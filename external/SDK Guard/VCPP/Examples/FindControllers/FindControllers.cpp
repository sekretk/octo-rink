// FindControllers.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <string>
#include <locale.h>
#include "ZGuard.h"
#include "ZPort.h"
#pragma comment(lib, "ZGuard.lib")
#include "Utils.h"

const ZP_PORT_TYPE CvtPortType = ZP_PORT_COM;
LPCWSTR CvtPortName = L"COM6";
//LPCWSTR CvtPortName = L"COM3";
//const ZP_PORT_TYPE CvtPortType = ZP_PORT_IP;
//LPCWSTR CvtPortName = L"90.11.11.56:1000";


HANDLE g_hCvt = 0;
INT g_nCtrCount;
BOOL g_fNotifyEnabled;
HANDLE g_hEvent = NULL;
BOOL g_fThreadActive;
HANDLE g_hThread = NULL;


BOOL CALLBACK EnumCtrsCB(PZG_FIND_CTR_INFO pInfo, INT nPos, INT nMax, PVOID pUserData)
{
	std::wstring sType;
	if (g_nCtrCount == 0)
		_tprintf(TEXT("\n-------------\n"));
	if ((pInfo->nType > ZG_CTR_UNDEF) && (pInfo->nType <= ZG_CTR_GUARDNET))
		sType = CtrTypeStrs[pInfo->nType];
	else
	{
		sType.resize(128);
		INT n = _stprintf_s(&sType[0], sType.size(), TEXT("Unknown[%.2X]"), pInfo->nTypeCode);
		sType.resize(n);
	}
	_tprintf(TEXT("%s, addr: %d, s/n: %d, v%d.%d, K: %d, E: %d, %s;\n"),
		sType.c_str(),
		pInfo->nAddr,
		pInfo->nSn,
		LOBYTE(pInfo->nVersion), HIBYTE(pInfo->nVersion),
		pInfo->nMaxKeys,
		pInfo->nMaxEvents,
		KeyModeStrs[(pInfo->nFlags & ZG_CTR_F_PROXIMITY) ? 1 : 0]
	);
	g_nCtrCount++;
	return TRUE;
}

void DoGuardFindCtrs()
{
	_tprintf(TEXT("Search controllers... "));
	g_nCtrCount = 0;
	if (!CheckZGError(ZG_Cvt_EnumControllers(g_hCvt, EnumCtrsCB, NULL), _T("ZG_Cvt_EnumControllers")))
		return;
	if (g_nCtrCount > 0)
	{
		_tprintf(TEXT("--\n"));
		_tprintf(TEXT("Found %d controllers.\n"), g_nCtrCount);
	}
	else
		_tprintf(TEXT("Controllers not found.\n"));
}

HRESULT CheckNotifyMsgs()
{
	HRESULT hr;
	UINT nMsg;
	LPARAM nMsgParam;
	while ((hr = ZG_Cvt_GetNextMessage(g_hCvt, &nMsg, &nMsgParam)) == S_OK)
	{
		switch (nMsg)
		{
		case ZG_N_CVT_CTR_INSERT:
		case ZG_N_CVT_CTR_REMOVE:
			{
				PZG_FIND_CTR_INFO pFInfo = (PZG_FIND_CTR_INFO)nMsgParam;
				LPCTSTR CtrActs[2] = {TEXT("Remove"), TEXT("Insert")};
				std::wstring sType;
				INT n;

				if ((pFInfo->nType > ZG_CTR_UNDEF) && (pFInfo->nType <= ZG_CTR_GUARDNET))
					sType = CtrTypeStrs[pFInfo->nType];
				else
				{
					sType.resize(128);
					n = _stprintf_s(&sType[0], sType.size(), TEXT("Unknown[%.2X]"), pFInfo->nTypeCode);
					sType.resize(n);
				}
				_tprintf(TEXT("%s %s, addr: %d, s/n: %d, v%d.%d, K: %d, E: %d, %s;\n"),
					CtrActs[nMsg == ZG_N_CVT_CTR_INSERT],
					sType.c_str(),
					pFInfo->nAddr,
					pFInfo->nSn,
					LOBYTE(pFInfo->nVersion), HIBYTE(pFInfo->nVersion),
					pFInfo->nMaxKeys,
					pFInfo->nMaxEvents,
					KeyModeStrs[(pFInfo->nFlags & ZG_CTR_F_PROXIMITY) ? 1 : 0]
				);
			}
			break;
		}
	}
	if (hr == ZP_S_NOTFOUND)
		hr = S_OK;
	return hr;
}

DWORD WINAPI NotifyThreadProc(LPVOID lpParameter)
{
	while (g_fThreadActive)
	{
		if (WaitForSingleObject(g_hEvent, INFINITE) == WAIT_OBJECT_0)
		{
			ResetEvent(g_hEvent);
			if (g_hCvt != NULL)
				CheckNotifyMsgs();
		}
	}
	return 0;
}

void StartNotifyThread()
{
	if (g_hThread != NULL)
		return;
	DWORD nThreadId;
	g_fThreadActive = TRUE;
	g_hThread = CreateThread(NULL, 0, NotifyThreadProc, NULL, 0, &nThreadId);
}

void StopNotifyThread()
{
	if (g_hThread == NULL)
		return;
	g_fThreadActive = FALSE;
	SetEvent(g_hEvent);
	WaitForSingleObject(g_hThread, INFINITE);
	CloseHandle(g_hThread);
	g_hThread = NULL;
}

void EnableNotification(BOOL fEnable=TRUE, BOOL fReport=TRUE)
{
	if (fEnable)
	{
		if (g_hEvent == NULL)
			g_hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
		_ZG_CVT_NOTIFY_SETTINGS rNS = {0};
		rNS.nNMask = ZG_NF_CVT_CTR_EXIST;
		rNS.hEvent		= g_hEvent;
		rNS.nScanCtrsPeriod = 1500; // Период сканирования контроллеров
		if (!CheckZGError(ZG_Cvt_SetNotification(g_hCvt, &rNS), _T("ZG_Cvt_SetNotification")))
			return;
		StartNotifyThread();
	}
	else
	{
		StopNotifyThread();
		if (!CheckZGError(ZG_Cvt_SetNotification(g_hCvt, NULL), _T("ZG_Cvt_SetNotification")))
			return;
	}
	g_fNotifyEnabled = fEnable;
	if (fReport)
		_tprintf(TEXT("Done.\n"));
}

int _tmain(int argc, _TCHAR* argv[])
{
	setlocale(LC_ALL, "Russian");

	// Проверяем версию SDK
	DWORD nVersion = ZG_GetVersion();
	if (((nVersion & 0xff) != ZG_SDK_VER_MAJOR) || (((nVersion >> 8) & 0xff) != ZG_SDK_VER_MINOR))
	{
		_tprintf(TEXT("Wrond version SDK Guard.\n"));
		getchar();
		return 0;
	}

	if (!CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP), _T("ZG_Initialize")))
		return 0;
	__try
	{
		_ZG_CVT_INFO rInfo;
		ZeroMemory(&rInfo, sizeof(rInfo));
		_ZG_CVT_OPEN_PARAMS rOp;
		ZeroMemory(&rOp, sizeof(rOp));
		rOp.nType = CvtPortType;
		rOp.pszName = CvtPortName;
		rOp.nSpeed = ZG_SPEED_19200;
		if (!CheckZGError(ZG_Cvt_Open(&g_hCvt, &rOp), _T("ZG_Cvt_Open")))
			return 0;
		g_fNotifyEnabled = FALSE;
		_tprintf(TEXT("\n"));
		DoGuardFindCtrs();
		_tprintf(TEXT("-----\n"));
		EnableNotification(TRUE, FALSE);
		__try
		{
			TCHAR szBuf[128];
			while (1)
			{
				_tprintf(TEXT("Enter command number:\n"));
				_tprintf(TEXT("1 - rescan\n"));
				_tprintf(TEXT("2 - %s notifications\n"), (g_fNotifyEnabled ? TEXT("Disable") : TEXT("Enable")));
				_tprintf(TEXT("0 - quit\n"));
				if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1)
				{
					_tprintf(TEXT("\n"));
					switch (_ttoi(szBuf))
					{
					case 1:
						DoGuardFindCtrs();
						break;
					case 2:
						EnableNotification(!g_fNotifyEnabled);
						break;
					case 0:
						return 0;
					default:
						_tprintf(TEXT("Invalid command.\n"));
					}
					_tprintf(TEXT("-----\n"));
				}
			}
		}
		__finally
		{
		}
	}
	__finally
	{
		StopNotifyThread();
		if (g_hCvt != 0)
			ZG_CloseHandle(g_hCvt);
		if (g_hEvent != NULL)
			CloseHandle(g_hEvent);
		ZG_Finalyze();
	}
	return 0;
}


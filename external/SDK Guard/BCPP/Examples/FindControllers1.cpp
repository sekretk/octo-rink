//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include <tchar.h>
#include "..\ZPort.h"
#include "..\ZGuard.h"
#pragma comment(lib, "..\ZGuard.lib")
#include "Utils.h"
//---------------------------------------------------------------------------

const ZP_PORT_TYPE CvtPortType = ZP_PORT_COM;
LPCWSTR CvtPortName = L"COM11";


HANDLE g_hCvt = 0;
INT g_nCtrCount;
BOOL g_fNotifyEnabled;
HANDLE g_hEvent = NULL;
BOOL g_fThreadActive;
HANDLE g_hThread = NULL;


BOOL CALLBACK EnumCtrsCB(PZG_FIND_CTR_INFO pInfo, INT nPos, INT nMax, PVOID pUserData)
{
	String sType;
	if ((pInfo->nType > ZG_CTR_UNDEF) && (pInfo->nType <= ZG_CTR_GUARDNET))
	   sType = CtrTypeStrs[pInfo->nType];
	else
		sType.printf(TEXT("Unknown[%.2X]"), pInfo->nTypeCode);
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
	_tprintf(TEXT("Search controllers...\n"));
	_tprintf(TEXT("-------------\n"));
	g_nCtrCount = 0;
	if (!CheckZGError(ZG_Cvt_EnumControllers(g_hCvt, EnumCtrsCB, NULL), TEXT("ZG_Cvt_EnumControllers")))
		return;
	if (g_nCtrCount > 0) {
		_tprintf(TEXT("--\n"));
		_tprintf(TEXT("Found %d controllers.\n"), g_nCtrCount);
	}
	else
		_tprintf(TEXT("Controllers not found.\n"));
}

HRESULT CheckNotifyEvent()
{
	HRESULT hr;
	UINT nMsg;
	LPARAM nMsgParam;
	while ((hr = ZG_Cvt_GetNextMessage(g_hCvt, &nMsg, &nMsgParam)) == S_OK) {
		switch (nMsg) {
		case ZG_N_CVT_CTR_INSERT:
		case ZG_N_CVT_CTR_REMOVE: {
				PZG_FIND_CTR_INFO pFInfo = PZG_FIND_CTR_INFO(nMsgParam);
				LPCTSTR CtrActs[2] = {TEXT("Remove"), TEXT("Insert")};
				String sType;

				if ((pFInfo->nType > ZG_CTR_UNDEF) && (pFInfo->nType <= ZG_CTR_GUARDNET))
				   sType = CtrTypeStrs[pFInfo->nType];
				else
					sType.printf(TEXT("Unknown[%.2X]"), pFInfo->nTypeCode);
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
	while (g_fThreadActive) {
		if (WaitForSingleObject(g_hEvent, INFINITE) == WAIT_OBJECT_0) {
			ResetEvent(g_hEvent);
			if (g_hCvt != NULL)
				CheckNotifyEvent();
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
	if (fEnable) {
		if (g_hEvent == NULL)
			g_hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
		_ZG_CVT_NOTIFY_SETTINGS rNS = {0};
		rNS.nNMask = ZG_NF_CVT_CTR_EXIST;
		rNS.hEvent = g_hEvent;
		rNS.nScanCtrsPeriod = 1500; // Период сканирования контроллеров
		if (!CheckZGError(ZG_Cvt_SetNotification(g_hCvt, &rNS), TEXT("ZG_Cvt_SetNotification")))
			return;
		StartNotifyThread();
	}
	else {
		StopNotifyThread();
		if (!CheckZGError(ZG_Cvt_SetNotification(g_hCvt, NULL), TEXT("ZG_Cvt_SetNotification")))
			return;
	}
	g_fNotifyEnabled = fEnable;
	if (fReport)
		_tprintf(TEXT("Done.\n"));
}

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
	setlocale(LC_ALL,"Rus");

	// Проверяем версию SDK
	DWORD nVersion = ZG_GetVersion();
	if (((nVersion & 0xff) != ZG_SDK_VER_MAJOR) || (((nVersion >> 8) & 0xff) != ZG_SDK_VER_MINOR)) {
		_tprintf(TEXT("Wrong version SDK Guard.\n"));
		getchar();
		return 0;
	}

	if (!CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP), TEXT("ZG_Initialize")))
		return 0;
	__try {
		_ZG_CVT_INFO rInfo;
		_ZG_CVT_OPEN_PARAMS rOp;
		ZeroMemory(&rInfo, sizeof(rInfo));
		ZeroMemory(&rOp, sizeof(rOp));
		rOp.nType = CvtPortType;
		rOp.pszName = CvtPortName;
		rOp.nSpeed = ZG_SPEED_57600;
		if (!CheckZGError(ZG_Cvt_Open(&g_hCvt, &rOp, &rInfo), TEXT("ZG_Cvt_Open")))
			return 0;
		g_fNotifyEnabled = FALSE;
		_tprintf(TEXT("\n"));
		DoGuardFindCtrs();
		EnableNotification(TRUE, FALSE);
		_tprintf(TEXT("-----\n"));
		TCHAR szBuf[128];
		while (1) {
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - rescan\n"));
			_tprintf(TEXT("2 - %s notifications\n"), (g_fNotifyEnabled ? TEXT("Disable") : TEXT("Enable")));
			_tprintf(TEXT("0 - quit\n"));
#if __BORLANDC__ >= 0x0620
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1) {
#else
			if (_tscanf(TEXT("%s"), szBuf) == 1) {
#endif
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf)) {
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
	__finally {
		StopNotifyThread();
		if (g_hCvt != 0)
			ZG_CloseHandle(g_hCvt);
		ZG_Finalyze();
	}
	return 0;
}
//---------------------------------------------------------------------------

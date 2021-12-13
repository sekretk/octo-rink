// CtrClock.cpp: определ€ет точку входа дл€ консольного приложени€.
//

#include "stdafx.h"
#include <locale.h>
#include "ZGuard.h"
#include "ZPort.h"
#pragma comment(lib, "ZGuard.lib")
#include "Utils.h"

//const ZP_PORT_TYPE CvtPortType = ZP_PORT_COM;
//LPCWSTR CvtPortName = L"COM6";
//const INT CtrAddr = 3;
const ZP_PORT_TYPE CvtPortType = ZP_PORT_IP;
LPCWSTR CvtPortName = L"90.11.11.56:1000";
const INT CtrAddr = 4;

HANDLE g_hCtr = NULL;
HANDLE g_hEvent = NULL;
BOOL g_fThreadActive;
HANDLE g_hThread = NULL;

void ShowClock()
{
	_ZG_CTR_CLOCK rCtrTime;

	if (!CheckZGError(ZG_Ctr_GetClock(g_hCtr, &rCtrTime), _T("ZG_Ctr_GetClock")))
		return;
	_tprintf(TEXT("%.2d.%.2d.%.4d %.2d:%.2d:%.2d (stopped: %s)\n"),
		rCtrTime.nDay,
		rCtrTime.nMonth,
		rCtrTime.nYear,
		rCtrTime.nHour,
		rCtrTime.nMinute,
		rCtrTime.nSecond,
		rCtrTime.fStopped ? TEXT("True") : TEXT("False"));
}

void SyncWithPC()
{
	_ZG_CTR_CLOCK rCtrTime;
	SYSTEMTIME rPcTime;

	ZeroMemory(&rCtrTime, sizeof(rCtrTime));
	GetLocalTime(&rPcTime);
	rCtrTime.nYear = rPcTime.wYear;
	rCtrTime.nMonth = rPcTime.wMonth;
	rCtrTime.nDay = rPcTime.wDay;
	rCtrTime.nHour = rPcTime.wHour;
	rCtrTime.nMinute = rPcTime.wMinute;
	rCtrTime.nSecond = rPcTime.wSecond;

	if (!CheckZGError(ZG_Ctr_SetClock(g_hCtr, &rCtrTime), _T("ZG_Ctr_SetClock")))
		return;
	_tprintf(TEXT("Synchronized.\n"));
}

HRESULT CheckNotifyMsgs()
{
	HRESULT hr;
	UINT nMsg;
	LPARAM nMsgParam;
	while ((hr = ZG_Ctr_GetNextMessage(g_hCtr, &nMsg, &nMsgParam)) == S_OK)
	{
		switch (nMsg)
		{
		case ZG_N_CTR_CLOCK:
			{
				INT64 nOffs = *PINT64(nMsgParam);
				_tprintf(TEXT("==> Clock desync: %d sec\n"), nOffs);
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
			if (g_hCtr != NULL)
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

int _tmain(int argc, _TCHAR* argv[])
{
	HANDLE hCvt = 0;

	setlocale(LC_ALL, "Russian");

	// ѕровер€ем версию SDK
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
		_ZG_CVT_OPEN_PARAMS rOp;
		ZeroMemory(&rOp, sizeof(rOp));
		rOp.nType = CvtPortType;
		rOp.pszName = CvtPortName;
		rOp.nSpeed = ZG_SPEED_57600;
		if (!CheckZGError(ZG_Cvt_Open(&hCvt, &rOp), _T("ZG_Cvt_Open")))
			return 0;
		if (!CheckZGError(ZG_Ctr_Open(&g_hCtr, hCvt, CtrAddr, 0, NULL), _T("ZG_Ctr_Open")))
			return 0;
		_ZG_CTR_NOTIFY_SETTINGS rNS = {0};
		TCHAR szBuf[128];

		g_hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
		rNS.nNMask = ZG_NF_CTR_CLOCK;
		rNS.hEvent = g_hEvent;
		rNS.nClockOffs = 5; // ƒопустимое расхождение часов в секундах
		rNS.nCheckStatePeriod = 1000;  // ѕериод проверки часов в миллисекундах
		if (!CheckZGError(ZG_Ctr_SetNotification(g_hCtr, &rNS), _T("ZG_Ctr_SetNotification")))
			return 0;
		StartNotifyThread();
		while (1)
		{
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - read clock\n"));
			_tprintf(TEXT("2 - synchronize with computer clock\n"));
			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1)
			{
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf))
				{
				case 1:
					ShowClock();
					break;
				case 2:
					SyncWithPC();
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
		StopNotifyThread();
		if (g_hCtr != 0)
			ZG_CloseHandle(g_hCtr);
		if (hCvt != 0)
			ZG_CloseHandle(hCvt);
		if (g_hEvent != NULL)
			CloseHandle(g_hEvent);
		ZG_Finalyze();
	}
	return 0;
}


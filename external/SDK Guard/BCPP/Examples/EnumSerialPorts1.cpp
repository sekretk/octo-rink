//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include <tchar.h>
#include "..\ZPort.h"
#include "..\ZGuard.h"
#pragma comment(lib, "..\ZGuard.lib")
#include "Utils.h"

//---------------------------------------------------------------------------
HANDLE g_hNotify = 0;
HANDLE g_hEvent = NULL;
BOOL g_fThreadActive;


HRESULT CheckNotifyMsgs()
{
	HRESULT hr;
	UINT nMsg;
	LPARAM nMsgParam;
	while ((hr = ZP_DD_GetNextMessage(g_hNotify, &nMsg, &nMsgParam)) == S_OK) {
		switch (nMsg) {
		case ZP_N_INSERT: {
				PZP_N_EXIST_INFO pInfo = (PZP_N_EXIST_INFO)nMsgParam;

				_tprintf(TEXT("Port insert: %s (%s); %s\n"),
					pInfo->rPort.szName,
					pInfo->rPort.szFriendly,
					(pInfo->rPort.nFlags & ZP_PIF_BUSY) ? TEXT("busy") : TEXT(""));
			}
			break;
		case ZP_N_REMOVE: {
				PZP_N_EXIST_INFO pInfo = (PZP_N_EXIST_INFO)nMsgParam;

				_tprintf(TEXT("Port removed: %s (%s)\n"),
					pInfo->rPort.szName,
					pInfo->rPort.szFriendly);
			}
			break;
		case ZP_N_CHANGE: {
				PZP_N_CHANGE_INFO pInfo = (PZP_N_CHANGE_INFO)nMsgParam;

				_tprintf(TEXT("Port changed (%.2Xh): %s (%s); %s\n"),
					pInfo->nChangeMask,
					pInfo->rPort.szName,
					pInfo->rPort.szFriendly,
					(pInfo->rPort.nFlags & ZP_PIF_BUSY) ? TEXT("busy") : TEXT(""));
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
			if (g_hNotify != NULL)
				CheckNotifyMsgs();
		}
	}
	return 0;
}

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
	setlocale(LC_ALL,"Rus");

	// Проверяем версию SDK
	DWORD nVersion, nVerMajor, nVerMinor, nVerBuild;
	nVersion = ZG_GetVersion();
	nVerMajor = nVersion & 0xff;
	nVerMinor = (nVersion >> 8) & 0xff;
	nVerBuild = (nVersion >> 16) & 0xff;
	_tprintf(TEXT("SDK Guard v%d.%d.%d\n"), nVerMajor, nVerMinor, nVerBuild);

	if ((nVerMajor != ZG_SDK_VER_MAJOR) || (nVerMinor != ZG_SDK_VER_MINOR)) {
		_tprintf(TEXT("Wrong version SDK Guard.\n"));
		getchar();
		return 0;
	}

	if (!CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP), TEXT("ZG_Initialize")))
		return 0;
	__try {
		_tprintf(TEXT("Enum serial ports...\n"));
		INT_PTR nPortCount = 0;
		HANDLE hList;
		if (!CheckZGError(ZG_GetPortInfoList(&hList, &nPortCount), _T("ZG_GetPortInfoList")))
			return 0;
		__try {
			_ZP_PORT_INFO rPI;
			for (int i = 0; i < nPortCount; i++) {
				ZP_GetPortInfo(hList, i, &rPI);
				_tprintf(TEXT("%d. %s %s (%s); %s\n"),
					(i + 1),
					PortTypeStrs[rPI.nType],
					rPI.szName,
					rPI.szFriendly,
					(rPI.nFlags & ZP_PIF_BUSY) ? TEXT("busy") : TEXT(""));
			}
		}
		__finally {
			ZG_CloseHandle(hList);
		}
		_tprintf(TEXT("--------------\n"));
		if (nPortCount > 0)
			 _tprintf(TEXT("Found %d ports.\n"), nPortCount);
		else
			_tprintf(TEXT("Ports not found.\n"));
		HANDLE hThread;
		DWORD nThreadId;
		g_hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
		g_fThreadActive = TRUE;
		hThread = CreateThread(NULL, 0, NotifyThreadProc, NULL, 0, &nThreadId);
		__try {
			_ZP_NOTIFY_SETTINGS rNS = {0};
			rNS.nNMask = ZP_NF_EXIST | ZP_NF_CHANGE;
			rNS.hEvent	= g_hEvent;
			if (!CheckZGError(ZG_SetNotification(&g_hNotify, &rNS, TRUE, FALSE), TEXT("ZG_SetNotification")))
				return 0;
			_tprintf(TEXT("Wait events...\n"));
			getchar();
		}
		__finally {
			g_fThreadActive = FALSE;
			SetEvent(g_hEvent);
			WaitForSingleObject(hThread, INFINITE);
			CloseHandle(hThread);
			CloseHandle(g_hEvent);
		}
	}
	__finally {
		if (g_hNotify != 0)
			ZG_CloseHandle(g_hNotify);
		ZG_Finalyze();
    }
	return 0;
}
//---------------------------------------------------------------------------

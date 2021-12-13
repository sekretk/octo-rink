// CtrLockTimes.cpp: определяет точку входа для консольного приложения.
//

#include "stdafx.h"
#include <locale.h>
#include "ZGuard.h"
#include "ZPort.h"
#if !defined(ZGUARD_LINKONREQUEST)
#pragma comment(lib, "../../ZGuard.lib")
#elif !defined(ZPORT_LINKONREQUEST)
#pragma comment(lib, "../../ZGuard.lib")
#endif // !ZGUARD_LINKONREQUEST
#include "Utils.h"


const ZP_PORT_TYPE CvtPortType = ZP_PORT_IP;
LPCWSTR CvtPortName = L"90.11.11.56:1000";
const INT CtrAddr = 4;

HANDLE g_hCtr = 0;
INT g_nMaxBanks;


void ShowLockTimes()
{
	INT i;
	DWORD nOpen, nLet, nMax;

	for (i = 0; i < g_nMaxBanks; i++)
	{
		if (!CheckZGError(ZG_Ctr_ReadLockTimes(g_hCtr, &nOpen, &nLet, &nMax, i), _T("ZG_Ctr_ReadLockTimes")))
			return;
		_tprintf(TEXT("------------\n"));
		_tprintf(TEXT("Bank %d:\n"), i);
		_tprintf(TEXT("Open (ms): %d\n"), nOpen);
		_tprintf(TEXT("Let (ms): %d\n"), nLet);
		_tprintf(TEXT("Max (ms): %d\n"), nMax);
	}
	_tprintf(TEXT("Done.\n"));
}

void DoSetLockTimes()
{
	INT nBankN, nOpen, nLet, nMax;
	_tprintf(TEXT("Enter bank#, open time (ms), let time (ms), max time (ms) (-1 not change):\n"));
	if (_tscanf_s(TEXT("%d, %d, %d, %d"), &nBankN, &nOpen, &nLet, &nMax) != 4)
	{
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	DWORD nMask = 0;
	if (nOpen != -1)
		nMask = 1;
	if (nLet != -1)
		nMask |= 2;
	if (nMax != -1)
		nMask |= 4;
	if (nMask == 0)
	{
		_tprintf(TEXT("Do noting.\n"));
		return;
	}
	_tprintf(TEXT("Writing...\n"));
	if (!CheckZGError(ZG_Ctr_WriteLockTimes(g_hCtr, nMask, nOpen, nLet, nMax, nBankN), _T("ZG_Ctr_WriteLockTimes")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoOpenLock(INT nLockN)
{
	if (!CheckZGError(ZG_Ctr_OpenLock(g_hCtr, nLockN), _T("ZG_Ctr_OpenLock")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoRestoreFactorySettings()
{
	INT i;
	_tprintf(TEXT("Writing (3000, 0, 0)...\n"));
	for (i = 0; i < g_nMaxBanks; i++)
	{
		if (!CheckZGError(ZG_Ctr_WriteLockTimes(g_hCtr, 0x7, 3000, 0, 0, i), _T("ZG_Ctr_WriteLockTimes")))
			return;
	}
	_tprintf(TEXT("Done.\n"));
}

void DoCtrLockTimesMenu()
{
	HANDLE hCvt = 0;

	if (!CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP), _T("ZG_Initialize")))
		return;
	__try
	{
		_ZG_CVT_OPEN_PARAMS rOp;
		ZeroMemory(&rOp, sizeof(rOp));
		rOp.nType = CvtPortType;
		rOp.pszName = CvtPortName;
		rOp.nSpeed = ZG_SPEED_57600;
		if (!CheckZGError(ZG_Cvt_Open(&hCvt, &rOp, NULL), _T("ZG_Cvt_Open")))
			return;
		_ZG_CTR_INFO rCtrInfo;
		ZeroMemory(&rCtrInfo, sizeof(rCtrInfo));
		if (!CheckZGError(ZG_Ctr_Open(&g_hCtr, hCvt, CtrAddr, 0, &rCtrInfo, ZG_CTR_UNDEF), _T("ZG_Ctr_Open")))
			return;
		g_nMaxBanks = (rCtrInfo.nFlags & ZG_CTR_F_2BANKS) ? 2 : 1;
		_tprintf(TEXT("%s addr: %d, s/n: %d, v%d.%d, Max_Banks: %d.\n"),
			CtrTypeStrs[rCtrInfo.nType],
			rCtrInfo.nAddr,
			rCtrInfo.nSn,
			LOBYTE(rCtrInfo.nVersion), HIBYTE(rCtrInfo.nVersion),
			g_nMaxBanks);
		TCHAR szBuf[128];
		while (1)
		{
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - show lock times\n"));
			_tprintf(TEXT("2 - set lock times...\n"));
			_tprintf(TEXT("3 - open lock (In)\n"));
			_tprintf(TEXT("4 - open lock (Out)\n"));
			_tprintf(TEXT("9 - Restore factory settings (all banks)\n"));
			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1)
			{
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf))
				{
				case 1:
					ShowLockTimes();
					break;
				case 2:
					DoSetLockTimes();
					break;
				case 3:
					DoOpenLock(0);
					break;
				case 4:
					DoOpenLock(1);
					break;
				case 9:
					DoRestoreFactorySettings();
					break;
				case 0:
					return;
				default:
					_tprintf(TEXT("Invalid command.\n"));
				}
				_tprintf(TEXT("-----\n"));
			}
		}
	}
	__finally
	{
		if (g_hCtr != NULL)
			ZG_CloseHandle(g_hCtr);
		if (hCvt != NULL)
			ZG_CloseHandle(hCvt);
		ZG_Finalyze();
	}
	getchar();
}

int _tmain(int argc, _TCHAR* argv[])
{
	setlocale(LC_ALL, "Russian");

	CZGuardLoader oZGL;

	if (!oZGL.IsLoaded())
	{
		_tprintf(TEXT("Load ZGuard error 0x%X.\n"), oZGL.m_nStatus);
		getchar();
		return 0;
	}
	DoCtrLockTimesMenu();

	return 0;
}


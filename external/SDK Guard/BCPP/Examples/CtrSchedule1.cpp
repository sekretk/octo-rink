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
LPCWSTR CvtPortName = L"COM6";
const INT CtrAddr = 3;

HANDLE g_hCtr = 0;
INT g_nCtrMaxBanks;


void ShowSchedule()
{
	INT i, j;
	PZG_CTR_TIMEZONE pTz;
	PZG_CTR_TIMEZONE aTZs = new _ZG_CTR_TIMEZONE[ZG_MAX_TIMEZONES];
	__try {
		for (i = 0; i < g_nCtrMaxBanks; i++) {
			if (!CheckZGError(ZG_Ctr_ReadTimeZones(g_hCtr, 0, aTZs, ZG_MAX_TIMEZONES, NULL, NULL, i), TEXT("ZG_Ctr_ReadTimeZones")))
				return;
			_tprintf(TEXT("------------\n"));
			_tprintf(TEXT("Bank %d:\n"), i);
			for (j = 0; j < ZG_MAX_TIMEZONES; j++) {
				pTz = &aTZs[j];
				_tprintf(TEXT("%d. Days of week: %.2Xh, Time: %.2d:%.2d - %.2d:%.2d\n"),
					j,
					pTz->nDayOfWeeks,
					pTz->nBegHour, pTz->nBegMinute,
					pTz->nEndHour, pTz->nEndMinute);
			}
		}
	}
	__finally {
		delete []aTZs;
	}
	_tprintf(TEXT("Done.\n"));
}

void DoSetTimeZone()
{
	INT nBankN, nTzIdx, nDows, nBegHour, nBegMin, nEndHour, nEndMin;
	_tprintf(TEXT("Enter bank#, timezone index, days of week (hex), time from (hh:mm), time to (hh:mm):\n"));
	if (_tscanf_s(TEXT("%d, %d, %X, %d:%d, %d:%d"),
		&nBankN, &nTzIdx, &nDows, &nBegHour, &nBegMin, &nEndHour, &nEndMin) != 7) {
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	_ZG_CTR_TIMEZONE rTz = {0};
	rTz.nDayOfWeeks = nDows;
	rTz.nBegHour = nBegHour;
	rTz.nBegMinute = nBegMin;
	rTz.nEndHour = nEndHour;
	rTz.nEndMinute = nEndMin;

	_tprintf(TEXT("Writing...\n"));
	if (!CheckZGError(ZG_Ctr_WriteTimeZones(g_hCtr, nTzIdx, &rTz, 1, NULL, NULL, nBankN), TEXT("ZG_Ctr_WriteTimeZones")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoRestoreFactorySettings()
{
	INT i;
	PZG_CTR_TIMEZONE pTz;
	PZG_CTR_TIMEZONE aTZs = new _ZG_CTR_TIMEZONE[ZG_MAX_TIMEZONES];
	__try {
		// Подготовка данных для записи в контроллер
		for (i = 0; i < ZG_MAX_TIMEZONES; i++) {
			pTz = &aTZs[i];
			pTz->nDayOfWeeks = 0x7F;
			pTz->nBegHour = 0;
			pTz->nBegMinute = 0;
			pTz->nEndHour = 23;
			pTz->nEndMinute = 59;
		}
		_tprintf(TEXT("Writing (0x7F, 00:00-23:59)...\n"));
		for (i = 0; i < g_nCtrMaxBanks; i++)
			if (!CheckZGError(ZG_Ctr_WriteTimeZones(g_hCtr, 0, aTZs, ZG_MAX_TIMEZONES, NULL, NULL, i), TEXT("ZG_Ctr_WriteTimeZones")))
				return;
	}
	__finally {
		delete []aTZs;
	}
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

	HANDLE hCvt = 0;
	if (!CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP), TEXT("ZG_Initialize")))
		return 0;
	__try {
		_ZG_CVT_OPEN_PARAMS rOp;
		ZeroMemory(&rOp, sizeof(rOp));
		rOp.nType = CvtPortType;
		rOp.pszName = CvtPortName;
		rOp.nSpeed = ZG_SPEED_57600;
		if (!CheckZGError(ZG_Cvt_Open(&hCvt, &rOp), TEXT("ZG_Cvt_Open")))
			return 0;
		_ZG_CTR_INFO rCtrInfo;
		ZeroMemory(&rCtrInfo, sizeof(rCtrInfo));
		if (!CheckZGError(ZG_Ctr_Open(&g_hCtr, hCvt, CtrAddr, 0, &rCtrInfo), TEXT("ZG_Ctr_Open")))
			return 0;
		g_nCtrMaxBanks = (rCtrInfo.nFlags & ZG_CTR_F_2BANKS) ? 2 : 1;
		_tprintf(TEXT("%s addr: %d, s/n: %d, v%d.%d, Max_Banks: %d\n"),
			CtrTypeStrs[rCtrInfo.nType],
			rCtrInfo.nAddr,
			rCtrInfo.nSn,
			LOBYTE(rCtrInfo.nVersion), HIBYTE(rCtrInfo.nVersion),
			g_nCtrMaxBanks);
		_tprintf(TEXT("-----\n"));
		TCHAR szBuf[128];
		while (1) {
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - Show schedule\n"));
			_tprintf(TEXT("6 - Set time zone...\n"));
			_tprintf(TEXT("9 - Restore factory settings (all banks)\n"));
			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1) {
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf)) {
				case 1:
					ShowSchedule();
					break;
				case 6:
					DoSetTimeZone();
					break;
				case 9:
					DoRestoreFactorySettings();
					break;
				default:
					_tprintf(TEXT("Invalid command.\n"));
				}
				_tprintf(TEXT("-----\n"));
			}
		}
	}
	__finally {
		if (g_hCtr != NULL)
			ZG_CloseHandle(g_hCtr);
		if (hCvt != NULL)
			ZG_CloseHandle(hCvt);
		ZG_Finalyze();
	}
	return 0;
}
//---------------------------------------------------------------------------

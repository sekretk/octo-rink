//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include <tchar.h>
#include "..\ZGuard.h"
#include "..\ZPort.h"
#pragma comment(lib, "..\ZGuard.lib")
#include "Utils.h"
//---------------------------------------------------------------------------


const ZP_PORT_TYPE CvtPortType = ZP_PORT_COM;
LPCWSTR CvtPortName = L"COM6";
const INT CtrAddr = 3;

HANDLE g_hCtr = NULL;


void ShowConfig()
{
	_ZG_CTR_ELECTRO_CONFIG rEC;
	if (!CheckZGError(ZG_Ctr_ReadElectroConfig(g_hCtr, &rEC), TEXT("ZG_Ctr_ReadElectroConfig")))
		return;
	_tprintf(TEXT("Power Config: %.2Xh\n"), rEC.nPowerConfig);
	_tprintf(TEXT("\t         Enabled: %s\n"), (rEC.nPowerConfig & ZG_EC_CF_ENABLED) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t        Schedule: %s\n"), (rEC.nPowerConfig & ZG_EC_CF_SCHEDULE) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t External reader: %s\n"), (rEC.nPowerConfig & ZG_EC_CF_EXT_READER) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t          Invert: %s\n"), (rEC.nPowerConfig & ZG_EC_CF_INVERT) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\tTurn off at exit: %s\n"), (rEC.nPowerConfig & ZG_EC_CF_EXIT_OFF) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t Opening of card: %s\n"), (rEC.nPowerConfig & ZG_EC_CF_CARD_OPEN) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("Power Delay (sec): %d\n"), rEC.nPowerDelay);
}

void ShowState()
{
	_ZG_CTR_ELECTRO_STATE rES;
	if (!CheckZGError(ZG_Ctr_GetElectroState(g_hCtr, &rES), TEXT("ZG_Ctr_WriteElectroConfig")))
		return;
	_tprintf(TEXT("Power Flags: %.2Xh\n"), rES.nPowerFlags);
	_tprintf(TEXT("\t         Enabled: %s\n"), (rES.nPowerFlags & ZG_EC_SF_ENABLED) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t        Schedule: %s\n"), (rES.nPowerFlags & ZG_EC_SF_SCHEDULE) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t     Open remote: %s\n"), (rES.nPowerFlags & ZG_EC_SF_REMOTE) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t           Delay: %s\n"), (rES.nPowerFlags & ZG_EC_SF_DELAY) ? _T("TRUE") : _T("FALSE"));
	_tprintf(TEXT("\t            Card: %s\n"), (rES.nPowerFlags & ZG_EC_SF_CARD) ? _T("TRUE") : _T("FALSE"));
}

void DoSetConfig()
{
	INT nEnabled, nSchedule, nExtReader, nInvert, nExitOff, nCardOpen, nPowerDelay;
	_tprintf(TEXT("Enter flag 'Enabled' (0 or 1), flag 'Schedule', flag 'External reader', \
				  flag 'Invert', flag 'Turn off at exit', flag 'Opening of card', Power Delay (sec):\n"));
	if (_tscanf_s(TEXT("%d, %d, %X, %d:%d, %d:%d"),
		&nEnabled, &nSchedule, &nExtReader, &nInvert, &nExitOff, &nCardOpen, &nPowerDelay) != 7) {
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	_ZG_CTR_ELECTRO_CONFIG rEC;
	rEC.nPowerConfig = 0;
	if (nEnabled)
		rEC.nPowerConfig = ZG_EC_CF_ENABLED;
	if (nSchedule)
		rEC.nPowerConfig |= ZG_EC_CF_SCHEDULE;
	if (nExtReader)
		rEC.nPowerConfig |= ZG_EC_CF_EXT_READER;
	if (nInvert)
		rEC.nPowerConfig |= ZG_EC_CF_INVERT;
	if (nExitOff)
		rEC.nPowerConfig |= ZG_EC_CF_EXIT_OFF;
	if (nCardOpen)
		rEC.nPowerConfig |= ZG_EC_CF_CARD_OPEN;
	rEC.nPowerDelay = nPowerDelay;
	if (!CheckZGError(ZG_Ctr_WriteElectroConfig(g_hCtr, &rEC, FALSE), TEXT("ZG_Ctr_WriteElectroConfig")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoSetPowerSchedule()
{
	INT nDows, nBegHour, nBegMin, nEndHour, nEndMin;
	_tprintf(TEXT("Enter days of week (hex), time from (hh:mm), time to (hh:mm):\n"));
	if (_tscanf_s(TEXT("%X, %d:%d, %d:%d"),
		&nDows, &nBegHour, &nBegMin, &nEndHour, &nEndMin) != 5) {
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
	if (!CheckZGError(ZG_Ctr_WriteTimeZones(g_hCtr, 6, &rTz, 1, NULL, NULL), TEXT("ZG_Ctr_WriteTimeZones")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoTogglePower()
{
	_ZG_CTR_ELECTRO_STATE rES;
	if (!CheckZGError(ZG_Ctr_GetElectroState(g_hCtr, &rES), TEXT("ZG_Ctr_GetElectroState")))
		return;
	BOOL fOn = !(rES.nPowerFlags & ZG_EC_SF_ENABLED);
	if (!CheckZGError(ZG_Ctr_SetElectroPower(g_hCtr, fOn), TEXT("ZG_Ctr_SetElectroPower")))
		return;
	_tprintf(TEXT("Done.\n"));
}

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
	setlocale(LC_ALL,"Rus");

	// ????????? ?????? SDK
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
		if (!(rCtrInfo.nFlags & ZG_CTR_F_ELECTRO)) {
			_tprintf(TEXT("ElectroControl function is not supported\n"));
			return 0;
		}
		_tprintf(TEXT("%s addr: %d, s/n: %d, v%d.%d.\n"),
			CtrTypeStrs[rCtrInfo.nType],
			rCtrInfo.nAddr,
			rCtrInfo.nSn,
			LOBYTE(rCtrInfo.nVersion), HIBYTE(rCtrInfo.nVersion));
		_tprintf(TEXT("-----\n"));
		TCHAR szBuf[128];
		while (1) {
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - Show config\n"));
			_tprintf(TEXT("2 - Show state\n"));
			_tprintf(TEXT("6 - Set config...\n"));
			_tprintf(TEXT("7 - Set power schedule...\n"));
			_tprintf(TEXT("8 - Toggle power\n"));
			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1) {
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf)) {
				case 1:
					ShowConfig();
					break;
				case 2:
					ShowState();
					break;
				case 6:
					DoSetConfig();
					break;
				case 7:
					DoSetPowerSchedule();
					break;
				case 8:
					DoTogglePower();
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
		if (g_hCtr != NULL)
			ZG_CloseHandle(g_hCtr);
		if (hCvt != NULL)
			ZG_CloseHandle(hCvt);
		ZG_Finalyze();
	}
	return 0;
}
//---------------------------------------------------------------------------

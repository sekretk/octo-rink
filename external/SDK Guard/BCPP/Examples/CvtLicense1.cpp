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
LPCWSTR CvtPortName = L"COM12";

HANDLE g_hCvt = 0;


void ShowLicense()
{
	_ZG_CVT_LIC_INFO rInfo;

	if (!CheckZGError(ZG_Cvt_GetLicense(g_hCvt, ZG_DEF_CVT_LICN, &rInfo), TEXT("ZG_Cvt_GetLicense")))
		return;
	_tprintf(TEXT("Status: %d\n"), rInfo.nStatus);
	if (rInfo.nMaxCtrs == 0xFF)
		_tprintf(TEXT("MaxCtrs: unlimited\n"));
	else
		_tprintf(TEXT("MaxCtrs: %d\n"), rInfo.nMaxCtrs);
	if (rInfo.nMaxKeys == 0xFFFF)
		_tprintf(TEXT("MaxKeys: unlimited\n"));
	else
		_tprintf(TEXT("MaxKeys: %d\n"), rInfo.nMaxKeys);
	if (rInfo.nMaxYear == 0xFFFF)
		_tprintf(TEXT("MaxDate: unlimited\n"));
	else
		_tprintf(TEXT("MaxDate: %.2d.%.2d.%.4d\n"),
			rInfo.nMaxDay, rInfo.nMaxMon, rInfo.nMaxYear);
	if (rInfo.nDownCountTime == 0xFFFF)
		_tprintf(TEXT("DownCountTime: unlimited\n"));
	else
		_tprintf(TEXT("DownCountTime: %d\n"), rInfo.nDownCountTime);
}

void ShowAllLicenses()
{
	_ZG_CVT_LIC_SINFO aLic[ZG_MAX_LICENSES];
	PZG_CVT_LIC_SINFO pLic;
	int nCount;

	if (!CheckZGError(ZG_Cvt_GetAllLicenses(g_hCvt, aLic, _countof(aLic), &nCount), TEXT("ZG_Cvt_GetAllLicenses")))
		return;
	if (nCount == 0) {
		_tprintf(TEXT("Installed licenses not found.\n"));
		return;
	}
	for (int i = 0; i < nCount; i++) {
		pLic = &aLic[i];
		_tprintf(TEXT("%.2d(%d/%d);\n"), pLic->nLicN, pLic->nMaxCtrs, pLic->nMaxKeys);
	}
}

int HexToBin(LPCTSTR pszHex, int nLen, LPBYTE pBuf, INT nBufSize)
{
	int nRes = (nLen / 2);
	int n;

	for (int i = 0; i < nRes; i++) {
		n = *pszHex++;
		n -= (n <= '9') ? '0' : ('a' - 10);
		pBuf[i] = (n << 4);
		n = *pszHex++;
		n -= (n <= '9') ? '0' : ('a' - 10);
		pBuf[i] += n;
	}
	return nRes;
}

void DoSetLicense()
{
	TCHAR szFilename[MAX_PATH];
	_tprintf(TEXT("Enter license filename (absolute path):\n"));
	if (_tscanf(TEXT("%s"), szFilename) != 1) {
		_tprintf(TEXT("Cancelled.\n"));
		return;
	}
	DWORD nFAttr = GetFileAttributes(szFilename);
	if ((nFAttr == INVALID_FILE_ATTRIBUTES) || (nFAttr & FILE_ATTRIBUTE_DIRECTORY)) {
		_tprintf(TEXT("File not found.\n"));
		return;
	}
	TCHAR szLicHex[1024];
	DWORD nLicHexLen;
	nLicHexLen = GetPrivateProfileString(TEXT("Lic"), TEXT("Txt"), NULL, szLicHex, _countof(szLicHex), szFilename);
	if ((nLicHexLen > 0) && (nLicHexLen < (_countof(szLicHex)-1))) {
		BYTE aLicData[512];
		INT nLicDataLen;
		nLicDataLen = HexToBin(szLicHex, nLicHexLen, aLicData, _countof(szLicHex));
		if (nLicDataLen == 0) {
			_tprintf(TEXT("No license data.\n"));
			return;
		}
		if (!CheckZGError(ZG_Cvt_SetLicenseData(g_hCvt, 5, aLicData, nLicDataLen, NULL), TEXT("ZG_Cvt_SetLicenseData")))
			return;
		_tprintf(TEXT("Successfully.\n"));
	}
	else
		_tprintf(TEXT("Incorrect data license.\n"));
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
		if (rInfo.nMode != ZG_GUARD_ADVANCED) {
			_tprintf(TEXT("Not Advanced.\n"));
			return 0;
		}
		ShowLicense();
		_tprintf(TEXT("-----\n"));
		TCHAR szBuf[128];
		while (1) {
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - show license\n"));
			_tprintf(TEXT("2 - show all licenses\n"));
			_tprintf(TEXT("6 - set license...\n"));
			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1) {
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf)) {
				case 1:
					ShowLicense();
					break;
				case 2:
					ShowAllLicenses();
					break;
				case 6:
					DoSetLicense();
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
		if (g_hCvt != 0)
			ZG_CloseHandle(g_hCvt);
		ZG_Finalyze();
	}
	return 0;
}
//---------------------------------------------------------------------------

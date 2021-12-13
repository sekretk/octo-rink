//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include <tchar.h>
#include "..\ZGuard.h"
#include "..\ZPort.h"
#pragma comment(lib, "..\ZGuard.lib")
#include "Utils.h"
//---------------------------------------------------------------------------


BOOL DoLoadFW(LPCTSTR pszFilename, PVOID* ppData, LPDWORD pCount)
{
	DWORD nFAttr = GetFileAttributes(pszFilename);
	if ((nFAttr == INVALID_FILE_ATTRIBUTES) || (nFAttr & FILE_ATTRIBUTE_DIRECTORY)) {
		_tprintf(TEXT("File not found.\n"));
		return FALSE;
	}
	HANDLE hFile;
	hFile = CreateFile(pszFilename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE) {
		_tprintf(TEXT("Cannot open file (%d).\n"), GetLastError());
		return FALSE;
	}
	DWORD nSize = GetFileSize(hFile, NULL);
	if (nSize == INVALID_FILE_SIZE) {
		CloseHandle(hFile);
		_tprintf(TEXT("GetFileSize error (%d).\n"), GetLastError());
		return FALSE;
	}
	*ppData = malloc(nSize);
	if (*ppData == NULL) {
		CloseHandle(hFile);
	   _tprintf(TEXT("Insufficient memory.\n"), GetLastError());
		return FALSE;
	}
	if (!ReadFile(hFile, *ppData, nSize, pCount, NULL)) {
		free(*ppData);
		CloseHandle(hFile);
	   _tprintf(TEXT("ReadFile error (%d).\n"), GetLastError());
		return FALSE;
	}
	CloseHandle(hFile);
	return TRUE;
}

BOOL CALLBACK UpdateFW_CB(INT nPos, INT nMax, PVOID pUserData)
{
	_tprintf(TEXT("\rUpdate %3d%%..."), (nPos * 100) / nMax);
	return TRUE;
}

void DoCvtUpdateFW()
{
	TCHAR szFilename[MAX_PATH];

	_tprintf(TEXT("Enter FW-filename:\n"));
	if (_tscanf_s(TEXT("%s"), szFilename, _countof(szFilename)) != 1) {
		_tprintf(TEXT("Cancelled.\n"));
		return;
	}
	PVOID pData;
	DWORD nCount;
	if (!DoLoadFW(szFilename, &pData, &nCount))
		return;
	HANDLE hList;
	INT_PTR nPortCount;
	if (!CheckZGError(ZG_GetPortInfoList(&hList, &nPortCount), _T("ZG_GetPortInfoList")))
		return;
	__try {
		_ZP_PORT_INFO rPI;
		for (INT_PTR i = 0; i < nPortCount; i++) {
			ZP_GetPortInfo(hList, i, &rPI);
			if ((rPI.nType == ZP_PORT_COM) || ((rPI.nType == ZP_PORT_FT) && (*rPI.szFriendly != '\0')))
				_tprintf(TEXT("%s, %s\n"),
					(rPI.nType == ZP_PORT_COM) ? rPI.szName : rPI.szFriendly,
					(rPI.nFlags & ZP_PIF_BUSY) ? TEXT("busy") : TEXT(""));
		}
	}
	__finally {
		ZG_CloseHandle(hList);
	}
	_tprintf(TEXT("--\n"));

	TCHAR szPortname[MAX_PATH];
	_tprintf(TEXT("Enter COM-port name:\n"));
	if (_tscanf_s(TEXT("%s"), szPortname, _countof(szPortname)) != 1) {
		free(pData);
		_tprintf(TEXT("Cancelled.\n"));
		return;
	}
	HRESULT nRet;
	_ZG_CVT_OPEN_PARAMS rOp;
	ZeroMemory(&rOp, sizeof(rOp));
	rOp.nType = ZP_PORT_COM;
	rOp.pszName = szPortname;
	rOp.nSpeed = ZG_SPEED_57600;
	nRet = ZG_UpdateCvtFirmware(&rOp, pData, nCount, UpdateFW_CB, NULL);
	free(pData);
	_tprintf(TEXT("\n"));
	if (!CheckZGError(nRet, TEXT("ZG_UpdateCvtFirmware")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoCtrUpdateFW()
{
	TCHAR szFilename[MAX_PATH];

	_tprintf(TEXT("Enter FW-filename:\n"));
	if (_tscanf_s(TEXT("%s"), szFilename, _countof(szFilename)) != 1) {
		_tprintf(TEXT("Cancelled.\n"));
		return;
	}
	PVOID pData;
	DWORD nCount;
	HANDLE hCvt = NULL;
	if (!DoLoadFW(szFilename, &pData, &nCount))
		return;
	__try {
		HANDLE hSearch;
		_ZP_SEARCH_PARAMS rSP;
		ZeroMemory(&rSP, sizeof(rSP));
		rSP.nFlags = ZP_SF_UNID;
		if (!CheckZGError(ZG_SearchDevices(&hSearch, &rSP, TRUE, FALSE), _T("ZG_SearchDevices")))
			return;
		__try {
			HRESULT hr;
			_ZG_ENUM_CVT_INFO rInfo;
			_ZP_PORT_INFO rPI;
			INT_PTR nPortCount;
			rInfo.cbSize = sizeof(_ZG_ENUM_CVT_INFO);
			while ((hr = ZP_FindNextDevice(hSearch, &rInfo, &rPI, 1, &nPortCount)) == S_OK) {
				if ((rPI.nType == ZP_PORT_COM) || ((rPI.nType == ZP_PORT_FT) && (*rPI.szFriendly != '\0'))) {
					if ((rInfo.cbSize == sizeof(_ZG_ENUM_CVT_INFO)) && (rInfo.nType != ZG_CVT_UNDEF))
						_tprintf(TEXT("%s, %s, %s s/n: %d, v%d.%d\n"),
							(rPI.nType == ZP_PORT_COM) ? rPI.szName : rPI.szFriendly,
							(rPI.nFlags & ZP_PIF_BUSY) ? TEXT("busy") : TEXT(""),
							CvtTypeStrs[rInfo.nType],
							rInfo.nSn,
							(rInfo.nVersion & 0xff), (rInfo.nVersion >> 8) & 0xff, (rInfo.nVersion >> 16) & 0xff);
					else
						_tprintf(TEXT("%s, %s\n"),
						(rPI.nType == ZP_PORT_COM) ? rPI.szName : rPI.szFriendly,
						(rPI.nFlags & ZP_PIF_BUSY) ? TEXT("busy") : TEXT(""));
				}
				rInfo.cbSize = sizeof(_ZG_ENUM_CVT_INFO);
			}
		}
		__finally {
			ZG_CloseHandle(hSearch);
		}
		_tprintf(TEXT("--\n"));

		TCHAR szPortname[MAX_PATH];
		_tprintf(TEXT("Enter COM-port name:\n"));
		if (_tscanf_s(TEXT("%s"), szPortname, _countof(szPortname)) != 1) {
			_tprintf(TEXT("Cancelled.\n"));
			return;
		}
		_ZG_CVT_OPEN_PARAMS rOp;
		ZeroMemory(&rOp, sizeof(rOp));
		rOp.nType = ZP_PORT_COM;
		rOp.pszName = szPortname;
		rOp.nSpeed = ZG_SPEED_19200;
		if (!CheckZGError(ZG_Cvt_Open(&hCvt, &rOp), TEXT("ZG_Cvt_Open")))
			return;

		TCHAR szBuf[16];
		INT nCtrSn;
		_tprintf(TEXT("Enter controller s/n:\n"));
		if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) != 1) {
			_tprintf(TEXT("Cancelled.\n"));
			return;
		}
		nCtrSn = _ttoi(szBuf);
		if ((nCtrSn <= 0) || (nCtrSn >= 65535)) {
			_tprintf(TEXT("Incorrect entry.\n"));
			return;
		}
		if (!CheckZGError(ZG_Cvt_UpdateCtrFirmware(hCvt, nCtrSn, pData, nCount, NULL, UpdateFW_CB, NULL), TEXT("ZG_Cvt_UpdateCtrFirmware")))
			return;
		_tprintf(TEXT("\n"));
		_tprintf(TEXT("Done.\n"));
	}
	__finally {
		if (hCvt != NULL)
			ZG_CloseHandle(hCvt);
		free(pData);
	}
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
		_tprintf(TEXT("-----\n"));
		TCHAR szBuf[128];
		while (1) {
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - Update Converter Firmware...\n"));
			_tprintf(TEXT("2 - Update Controller Firmware...\n"));
			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1) {
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf)) {
				case 1:
					DoCvtUpdateFW();
					break;
				case 2:
					DoCtrUpdateFW();
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
		ZG_Finalyze();
	}
	return 0;
}
//---------------------------------------------------------------------------

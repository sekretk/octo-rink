//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <tchar.h>
#include "..\ZPort.h"
#include "..\ZGuard.h"
#pragma comment(lib, "..\ZGuard.lib")
#include "Utils.h"
//---------------------------------------------------------------------------


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
		_tprintf(TEXT("Enum converters...\n"));
		INT_PTR nDevCount = 0;
		HANDLE hSearch;
		_ZP_SEARCH_PARAMS rSP;
		ZeroMemory(&rSP, sizeof(rSP));
		if (!CheckZGError(ZG_SearchDevices(&hSearch, &rSP, TRUE, TRUE), _T("ZG_SearchDevices")))
			return 0;
		__try {
			HRESULT hr;
			_ZG_ENUM_CVT_INFO rInfo;
			_ZP_PORT_INFO aPIs[2];
			PZP_PORT_INFO pPI;
			INT_PTR nPortCount;
			rInfo.cbSize = sizeof(_ZG_ENUM_CVT_INFO);
			while ((hr = ZP_FindNextDevice(hSearch, &rInfo, aPIs, _countof(aPIs), &nPortCount)) == S_OK)	{
				++nDevCount;
				if (rInfo.cbSize == sizeof(_ZG_ENUM_CVT_INFO)) {
					_tprintf(TEXT("%d. %s, s/n: %d, v%d.%d.%d, mode: %s;\n"),
						nDevCount,
						CvtTypeStrs[rInfo.nType],
						rInfo.nSn,
						(rInfo.nVersion & 0xff), (rInfo.nVersion >> 8) & 0xff, (rInfo.nVersion >> 16) & 0xff,
						GuardModeStrs[rInfo.nMode]);
				}
				else {
					_tprintf(TEXT("%d. model: %d, s/n: %d, v%d.%d.%d;\n"),
						nDevCount,
						rInfo.nModel,
						rInfo.nSn,
						(rInfo.nVersion & 0xff), (rInfo.nVersion >> 8) & 0xff, (rInfo.nVersion >> 16) & 0xf);
				}
				for (INT_PTR i = 0; i < nPortCount; i++) {
					pPI = &aPIs[i];
					_tprintf(TEXT("\t%s %s (%s); %s\n"),
						PortTypeStrs[pPI->nType],
						pPI->szName,
						pPI->szFriendly,
						(pPI->nFlags & ZP_PIF_BUSY) ? TEXT("busy") : TEXT(""));
				}
				rInfo.cbSize = sizeof(_ZG_ENUM_CVT_INFO);
			}
			if (!CheckZGError(hr, _T("ZP_FindNextDevice")))
				return 0;
		}
		__finally {
			ZG_CloseHandle(hSearch);
		}
		_tprintf(TEXT("--------------\n"));
		if (nDevCount > 0)
			 _tprintf(TEXT("Found %d converters\n"), nDevCount);
		else
			_tprintf(TEXT("Converters not found.\n"));
	}
	__finally {
		ZG_Finalyze();
	}
	getchar();
	return 0;
}
//---------------------------------------------------------------------------

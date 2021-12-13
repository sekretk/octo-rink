//---------------------------------------------------------------------------

#pragma hdrstop

#include "Utils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

LPCTSTR PortTypeStrs[] = {
	TEXT("Unknown"), TEXT("COM"), TEXT("FT"), TEXT("IP"), TEXT("IPS")};
LPCTSTR CvtTypeStrs[] = {
	TEXT("Unknown"), TEXT("Z-397"), TEXT("Z-397 Guard"), TEXT("Z-397 IP"), TEXT("Z-397 Web"), TEXT("Z5R Web")};
LPCTSTR GuardModeStrs[] = {
	TEXT("Unknown"), TEXT("Normal"), TEXT("Advanced"), TEXT("Test"), TEXT("Accept")};
LPCTSTR CtrTypeStrs[] = {
	TEXT(""),
	TEXT("Gate 2000"),
	TEXT("Matrix II Net"),
	TEXT("Z5R Net"),
	TEXT("Z5R Net 8000"),
	TEXT("Guard Net"),
	TEXT("Z-9 EHT Net"),
	TEXT("EuroLock EHT net"),
	TEXT("Z5R Web")
};
LPCTSTR KeyModeStrs[] = {
	TEXT("Touch Memory"),
	TEXT("Proximity")
};
LPCTSTR KeyTypeStrs[] = {
	TEXT(""),
	TEXT("Normal"),
	TEXT("Blocking"),
	TEXT("Master")
};

BOOL CheckZGError(HRESULT nStatus, LPCTSTR pszContext) {
	if (SUCCEEDED(nStatus))
		return TRUE;
	LPTSTR pBuffer = NULL;
	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
			((HRESULT_FACILITY(nStatus) == 4) ? FORMAT_MESSAGE_FROM_HMODULE : FORMAT_MESSAGE_FROM_SYSTEM),
		GetModuleHandle(_T("ZGuard.dll")),
		(DWORD)nStatus,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR)&pBuffer,
		0,
		NULL);
	if (pBuffer != NULL)
	{
		_tprintf(TEXT("%s fail: %s\n"), pszContext, pBuffer);
		LocalFree(pBuffer);
	}
	getchar();
	return FALSE;
}

tstring ZKeyNumToStr(const Z_KEYNUM& rNum, BOOL fProximity) {
	tstring s;
	if (fProximity) {
		s.resize(32);
#if __BORLANDC__ >= 0x0620
		int n = _stprintf_s((LPTSTR)s.data(), s.size(), TEXT("[%.2X%.2X] %.3d,%.5d"), rNum[5], rNum[4], rNum[3], *(PWORD)&rNum[1]);
#else
		int n = _stprintf((LPTSTR)s.data(), TEXT("[%.2X%.2X] %.3d,%.5d"), rNum[5], rNum[4], rNum[3], *(PWORD)&rNum[1]);
#endif
		s.resize(max(n, 0));
	}
	else {
		INT i, j;
		s.resize(rNum[0] * 2 + 1);
		j = 0;
		for (i = rNum[0]; i > 0; i--) {
#if __BORLANDC__ >= 0x0620
			_stprintf_s(&s[j], s.size() - j, TEXT("%.2X"), rNum[i]);
#else
			_stprintf(&s[j], TEXT("%.2X"), rNum[i]);
#endif
			j += 2;
		}
		s.resize(rNum[0] * 2);
	}
	return s;
}

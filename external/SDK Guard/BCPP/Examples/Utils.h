//---------------------------------------------------------------------------

#ifndef UtilsH
#define UtilsH
//---------------------------------------------------------------------------
#include "Windows.h"
#include <cstring.h>
#include <tchar.h>
#include <stdio.h>
#include "..\ZBase.h"
#include "..\ZGuard.h"

#ifdef _UNICODE
typedef wstring tstring;
#else
typedef string tstring;
#endif

extern LPCTSTR PortTypeStrs[6];
extern LPCTSTR CvtTypeStrs[6];
extern LPCTSTR GuardModeStrs[5];
extern LPCTSTR CtrTypeStrs[9];
extern LPCTSTR KeyModeStrs[2];
extern LPCTSTR KeyTypeStrs[4];

BOOL CheckZGError(HRESULT nStatus, LPCTSTR pszContext);
tstring ZKeyNumToStr(const Z_KEYNUM& rNum, BOOL fProximity);

#endif

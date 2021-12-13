// CtrKeys.cpp: определяет точку входа для консольного приложения.
//

#include "stdafx.h"
#include <string>
#include <locale.h>
#include "ZGuard.h"
#include "ZPort.h"
#if !defined(ZGUARD_LINKONREQUEST)
#pragma comment(lib, "../../ZGuard.lib")
#elif !defined(ZPORT_LINKONREQUEST)
#pragma comment(lib, "../../ZGuard.lib")
#endif // !ZGUARD_LINKONREQUEST
#include "Utils.h"


const ZP_PORT_TYPE CvtPortType = ZP_PORT_COM;
LPCWSTR CvtPortName = L"COM3";
const INT CtrAddr = 4;
//const ZP_PORT_TYPE CvtPortType = ZP_PORT_IP;
//LPCWSTR CvtPortName = L"90.11.11.56:1000";
//const INT CtrAddr = 4;

HANDLE g_hCtr = NULL;
INT g_nMaxBanks;
BOOL g_fProximity;
INT g_nFoundKeyIdx;
HANDLE g_hEvent = NULL;
BOOL g_fThreadActive;
HANDLE g_hThread = NULL;


void ShowKeys()
{
	INT i, nTop, j, nCount;
	_ZG_CTR_KEY aKeys[6];
	PZG_CTR_KEY pKey;
	
	for (i = 0; i < g_nMaxBanks; i++)
	{
		_tprintf(TEXT("------------\n"));
		_tprintf(TEXT("Bank %d:\n"), i);
		if (!CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, &nTop, i), _T("ZG_Ctr_GetKeyTopIndex")))
			return;
		if (nTop == 0)
		{
			_tprintf(TEXT("List is empty.\n"));
			continue;
		}
		for (j = 0; j < nTop; j++)
		{
			if ((j % _countof(aKeys)) == 0)
			{
				nCount = (nTop - j);
				if (nCount > _countof(aKeys))
					nCount = _countof(aKeys);    
				if (!CheckZGError(ZG_Ctr_ReadKeys(g_hCtr, j, aKeys, nCount, NULL, NULL, i), _T("ZG_Ctr_ReadKeys")))
					return;
			}    
			pKey = &aKeys[j % _countof(aKeys)];
			if (pKey->fErased)
				_tprintf(TEXT("%.4d empty.\n"), j);    
			else
			{
				_tprintf(TEXT("%.4d %s, %s, access: %.2Xh\n"), 
					j, 
					ZKeyNumToStr(pKey->rNum, g_fProximity).c_str(),
					KeyTypeStrs[pKey->nType],
					pKey->nAccess);
			}
		}
	}
	_tprintf(TEXT("Done.\n"));
}

BOOL TryHexToDec(TCHAR chHex, INT* pDec)
{
	if ((chHex >= L'0') && (chHex <= L'9'))
		*pDec = chHex - L'0';
	else if ((chHex >= L'a') && (chHex <= L'f'))
		*pDec = chHex - L'a' + 10;
	else if ((chHex >= L'A') && (chHex <= L'F'))
		*pDec = chHex - L'A' + 10;
	else
		return FALSE;
	return TRUE;
}

BOOL ParseKeyNum(Z_KEYNUM& rKeyNum, LPCTSTR pszText)
{
	INT nGroup, nNumber;
	ZeroMemory(&rKeyNum, sizeof(rKeyNum));
	if (_stscanf_s(pszText, TEXT("%d,%d"), &nGroup, &nNumber) == 2)
	{
		rKeyNum[0] = 3;
		*(PWORD)(&rKeyNum[1]) = nNumber;
		rKeyNum[3] = nGroup;
	}
	else
	{
		INT i, j, n, n2;
		i = _tcslen(pszText) - 2;
		j = 1;
		while (i >= 0)
		{
			if (!TryHexToDec(pszText[i], &n) || !TryHexToDec(pszText[i + 1], &n2))
				return FALSE;
			rKeyNum[j] = (n2 & 0xF) | ((n & 0xF) << 4);
			j++;
			if (j > 6)
				break;
			i -= 2;
		}
		rKeyNum[0] = (j - 1);
	}
	return TRUE;
}

BOOL CALLBACK FindKeyEnum(INT nIdx, PZG_CTR_KEY pKey, INT nPos, INT nMax, PVOID pUserData)
{
	PZ_KEYNUM pFindKey = (PZ_KEYNUM)pUserData;
	BOOL fFound;
	if ((*pFindKey)[0] < 6)
		fFound = (memcmp(&(*pFindKey)[1], &pKey->rNum[1], (*pFindKey)[0]) == 0);
	else
		fFound = (CompareZKeyNums(*pFindKey, pKey->rNum) == 0);
	if (fFound)
	{
		g_nFoundKeyIdx = nIdx;
		return FALSE;
	}
	return TRUE;
}

void DoFindKeyByNumber()
{
	INT nBankN;
	TCHAR szKeyNum[64];

	_tprintf(TEXT("Enter bank#, key number (-1 last key):\n"));
	if (_tscanf_s(TEXT("%d, %s"), 
		&nBankN, szKeyNum, _countof(szKeyNum)) != 2)
	{
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	Z_KEYNUM rFindNum;
	if (_tcscmp(szKeyNum, _T("-1")) == 0)
	{
		if (!CheckZGError(ZG_Ctr_ReadLastKeyNum(g_hCtr, &rFindNum), _T("ZG_Ctr_ReadLastKeyNum")))
			return;
	}
	else if (!ParseKeyNum(rFindNum, szKeyNum))
	{
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	g_nFoundKeyIdx = -1;
	if (!CheckZGError(ZG_Ctr_EnumKeys(g_hCtr, 0, FindKeyEnum, &rFindNum, nBankN), _T("ZG_Ctr_EnumKeys")))
		return;
	if (g_nFoundKeyIdx != -1)
		_tprintf(TEXT("Key %s found (index=%d).\n"), 
			ZKeyNumToStr(rFindNum, g_fProximity).c_str(), g_nFoundKeyIdx);
	else
		_tprintf(TEXT("Key %s not found.\n"), 
			ZKeyNumToStr(rFindNum, g_fProximity).c_str());
}

void DoSetKey()
{
	INT nBankN, nKeyIdx, nKeyType, nKeyAccess;
	TCHAR szKeyNum[64];

	_tprintf(TEXT("Enter bank#, key index (-1 top), type (1-normal,2-blocking,3-Master), access (hex), number (-1 last key):\n"));
	if (_tscanf_s(TEXT("%d, %d, %d, %x, %s"), 
			&nBankN, &nKeyIdx, &nKeyType, &nKeyAccess, szKeyNum, _countof(szKeyNum)) != 5)
	{
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	_ZG_CTR_KEY rKey = {0};

	if (nKeyIdx == -1)
	{
		if (!CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, &nKeyIdx, nBankN), _T("ZG_Ctr_GetKeyTopIndex")))
			return;
	}
	rKey.nType		= (ZG_CTR_KEY_TYPE)nKeyType;
	rKey.nAccess	= nKeyAccess;
	if (_tcscmp(szKeyNum, _T("-1")) == 0)
	{
		if (!CheckZGError(ZG_Ctr_ReadLastKeyNum(g_hCtr, &rKey.rNum), _T("ZG_Ctr_ReadLastKeyNum")))
			return;
	}
	else if (!ParseKeyNum(rKey.rNum, szKeyNum))
	{
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}

	if (!CheckZGError(ZG_Ctr_WriteKeys(g_hCtr, nKeyIdx, &rKey, 1, NULL, NULL, nBankN, TRUE), _T("ZG_Ctr_WriteKeys")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoClearKey()
{
	INT nBankN, nKeyIdx;

	_tprintf(TEXT("Enter bank#, key index (-1 key in tail):\n"));
	if (_tscanf_s(TEXT("%d, %d"), &nBankN, &nKeyIdx) != 2)
	{
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	if (nKeyIdx == -1)
	{
		INT nTop;
		if (!CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, &nTop, nBankN), _T("ZG_Ctr_GetKeyTopIndex")))
			return;
		if (nTop == 0)
		{
			_tprintf(TEXT("Key list is empty.\n"));
			return;
		}
		nKeyIdx = (nTop - 1);
	}
	if (!CheckZGError(ZG_Ctr_ClearKeys(g_hCtr, nKeyIdx, 1, NULL, NULL, nBankN, TRUE), _T("ZG_Ctr_ClearKeys")))
		return;
	_tprintf(TEXT("Done.\n"));
}

void DoClearAllKeys()
{
	INT nBankN;

	_tprintf(TEXT("Enter bank#:\n"));
	if (_tscanf_s(TEXT("%d"), &nBankN) != 1)
	{
		_tprintf(TEXT("Incorrect entry.\n"));
		return;
	}
	INT nTop;

	if (!CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, &nTop, nBankN), _T("ZG_Ctr_GetKeyTopIndex")))
		return;
	if (nTop == 0)
	{
		_tprintf(TEXT("Key list is empty.\n"));
		return;
	}
	_tprintf(TEXT("Clearing...\n"));
	if (!CheckZGError(ZG_Ctr_ClearKeys(g_hCtr, 0, nTop, NULL, NULL, nBankN, TRUE), _T("ZG_Ctr_ClearKeys")))
		return;
	_tprintf(TEXT("Done.\n"));
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
		case ZG_N_CTR_KEY_TOP:
			{
				PZG_N_KEY_TOP_INFO pInfo = PZG_N_KEY_TOP_INFO(nMsgParam);
				_tprintf(TEXT("==> Bank#%d: top index of key changed (%d -> %d).\n"), 
					pInfo->nBankN, pInfo->nOldTopIdx, pInfo->nNewTopIdx);
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

void DoCtrKeysMenu()
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
		g_fProximity = (rCtrInfo.nFlags & ZG_CTR_F_PROXIMITY) != 0;
		_tprintf(TEXT("%s addr: %d, s/n: %d, v%d.%d, Max_Banks: %d, Key_Mode: %s\n"),
			CtrTypeStrs[rCtrInfo.nType],
			rCtrInfo.nAddr,
			rCtrInfo.nSn,
			LOBYTE(rCtrInfo.nVersion), HIBYTE(rCtrInfo.nVersion),
			g_nMaxBanks,
			KeyModeStrs[g_fProximity]);

		g_hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
		_ZG_CTR_NOTIFY_SETTINGS rNS = {0};
		rNS.nNMask = ZG_NF_CTR_KEY_TOP;
		rNS.hEvent = g_hEvent;
		rNS.nCheckStatePeriod = 3000;  // Период проверки верхней границы ключей
		if (!CheckZGError(ZG_Ctr_SetNotification(g_hCtr, &rNS), _T("ZG_Ctr_SetNotification")))
			return;
		StartNotifyThread();
		_tprintf(TEXT("-----\n"));
		TCHAR szBuf[128];
		while (1)
		{
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - show keys\n"));
			_tprintf(TEXT("2 - find key by number...\n"));

			_tprintf(TEXT("6 - set key...\n"));
			_tprintf(TEXT("7 - clear key...\n"));
			_tprintf(TEXT("8 - clear all keys...\n"));

			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1)
			{
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf))
				{
				case 1:
					ShowKeys();
					break;
				case 2:
					DoFindKeyByNumber();
					break;
				case 6:
					DoSetKey();
					break;
				case 7:
					DoClearKey();
					break;
				case 8:
					DoClearAllKeys();
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
		StopNotifyThread();
		if (g_hCtr != NULL)
			ZG_CloseHandle(g_hCtr);
		if (hCvt != NULL)
			ZG_CloseHandle(hCvt);
		if (g_hEvent != NULL)
			CloseHandle(g_hEvent);
		ZG_Finalyze();
	}
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
	DoCtrKeysMenu();

	return 0;
}


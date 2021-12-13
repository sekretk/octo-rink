// CtrUpdateKeys.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <string>
#include <memory>
#include <vector>
#include <fstream>
#include <algorithm>
#include <assert.h>
#include <locale.h>
#include "ZGuard.h"
#include "ZPort.h"
#include "ZBase.h"
#pragma comment(lib, "ZGuard.lib")
#include "Utils.h"


using namespace std;

const ZP_PORT_TYPE CvtPortType = ZP_PORT_COM;	// Тип порта
LPCWSTR CvtPortName = L"COM3";					// Имя порта
const INT CtrAddr = 2;							// Адрес контроллера
LPCTSTR KeysCacheFileName_D = _T("ctr%d_keys.bin");	// Имя файла кэша ключей

HANDLE g_hCtr = 0;
INT g_nSn;			// С/н контроллера
BOOL g_fProximity;	// True, Proximity, иначе - Touch Memory
INT g_nMaxBanks;	// Количество банков
INT g_nMaxKeys;		// Максимум ключей
INT g_nOptRead;		// Количество ключей, считываемых за 1 запрос
INT g_nOptWrite;	// Количество ключей, записываемых за 1 запрос

typedef struct _MYKEY
{
	Z_KEYNUM m_KeyNum;
	ZG_CTR_KEY_TYPE m_nType;
	UINT m_nAccess;
}* PMYKEY;
typedef vector<_MYKEY> CMyKeyList;
typedef vector<_ZG_CTR_KEY> CZgKeyList;
typedef vector<bool> CBits;


INT_PTR FormatStr(std::wstring& s, LPCTSTR pszFormat, ...)
{
	INT_PTR nRes;
	va_list argList;
	va_start(argList, pszFormat);
	s.resize(512);
	s.begin();	// freeze the buffer
	nRes = _vstprintf_s(&s.at(0), s.size(), pszFormat, argList);
	s.resize(nRes);
	va_end(argList);
	return nRes;
}

void ShowKeys()
{
	INT i, nTop, j, nCount;
	unique_ptr<_ZG_CTR_KEY[]> aKeys(new _ZG_CTR_KEY[g_nOptRead]);
	PZG_CTR_KEY pKey;
	std::wstring sCacheName;
	ofstream f;
	FormatStr(sCacheName, KeysCacheFileName_D, g_nSn);
	f.open(sCacheName, ios::out | ios::app | ios::binary | ios::trunc);
	for (i = 0; i < g_nMaxBanks; i++)
	{
		_tprintf(TEXT("------------\n"));
		_tprintf(TEXT("Bank %d:\n"), i);
		if (!CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, &nTop, i), _T("ZG_Ctr_GetKeyTopIndex")))
			return;
		if (nTop == 0)
			_tprintf(TEXT("List is empty.\n"));
		for (j = 0; j < nTop; j++)
		{
			if ((j % g_nOptRead) == 0)
			{
				nCount = (nTop - j);
				if (nCount > g_nOptRead)
					nCount = g_nOptRead;    
				if (!CheckZGError(ZG_Ctr_ReadKeys(g_hCtr, j, &aKeys[0], nCount, NULL, NULL, i), _T("ZG_Ctr_ReadKeys")))
					return;
				f.write((LPCSTR)&aKeys[0], nCount * sizeof(_ZG_CTR_KEY));
			}    
			pKey = &aKeys[j % g_nOptRead];
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
		pKey = &aKeys[0];
		ZeroMemory(pKey, sizeof(_ZG_CTR_KEY));
		pKey->fErased = TRUE;
		for (j = nTop; j < g_nMaxKeys; j++)
			f.write((LPCSTR)&aKeys[0], sizeof(_ZG_CTR_KEY));
	}
	f.close();
	_tprintf(TEXT("Done.\n"));
}

void DoClearKeyCache()
{
	std::wstring sCacheName;
	FormatStr(sCacheName, KeysCacheFileName_D, g_nSn);
	if (!DeleteFile(sCacheName.c_str()))
	{
		_tprintf(TEXT("DeleteFile(\"%s\") fail: %d\n"), sCacheName.c_str(), GetLastError());
		getchar();
		return;
	}
	_tprintf(TEXT("Done.\n"));
}

BOOL CALLBACK ZgProcessCb(INT nPos, INT nMax, PVOID pUserData)
{
	_tprintf(TEXT("\r%5d / %d"), nPos, nMax);
	return TRUE;
}

// Загрузить текущий список ключей из кэша или из контроллера
BOOL GetCtrList(CZgKeyList* pList)
{
	pList->resize(g_nMaxKeys * g_nMaxBanks);
	std::wstring sCacheName;
	FormatStr(sCacheName, KeysCacheFileName_D, g_nSn);
	if (GetFileAttributes(sCacheName.c_str()) != INVALID_FILE_ATTRIBUTES)
	{
		ifstream f;
		f.open(sCacheName, ios::in | ios::binary);
		f.read((LPSTR)&pList->at(0), pList->size() * sizeof(_ZG_CTR_KEY));
		f.close();
	}
	else
	{
		_tprintf(TEXT("Load keys from controller...\n"));
		ofstream f;
		INT nTop, nPos;
		PZG_CTR_KEY pCK;

		f.open(sCacheName, ios::out | ios::app | ios::binary | ios::trunc);
		for (int i = 0; i < g_nMaxBanks; i++)
		{
			if (!CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, &nTop, i), _T("ZG_Ctr_GetKeyTopIndex")))
				return FALSE;
			nPos = (i * g_nMaxKeys);
			if (nTop > 0)
			{
				if (!CheckZGError(ZG_Ctr_ReadKeys(g_hCtr, 0, &pList->at(nPos), nTop, ZgProcessCb, NULL, i), _T("ZG_Ctr_ReadKeys")))
					return FALSE;
				f.write((LPCSTR)&pList->at(nPos), nTop * sizeof(_ZG_CTR_KEY));
			}
			if (nTop < g_nMaxKeys)
			{
				pCK = &pList->at(nPos + nTop);
				ZeroMemory(pCK, sizeof(_ZG_CTR_KEY));
				pCK->fErased = TRUE;
				f.write((LPCSTR)pCK, sizeof(_ZG_CTR_KEY));
				for (int j = nTop + 1; j < g_nMaxKeys; j++)
				{
					f.write((LPCSTR)pCK, sizeof(_ZG_CTR_KEY));
					(*pList)[nPos + j] = *pCK;
				}
			}
		}
		f.close();
		_tprintf(TEXT(" completed.\n"));
	}
	return TRUE;
}

BOOL ParseKeyNum(LPCSTR pszText, Z_KEYNUM& rKeyNum)
{
	INT nGroup, nNumber, nCode;
	ZeroMemory(&rKeyNum, sizeof(rKeyNum));
	if (sscanf_s(pszText, "[%X] %d,%d", &nCode, &nGroup, &nNumber) == 3)
	{
		rKeyNum[0] = 5;
		*(PWORD)(&rKeyNum[1]) = nNumber & 0xFFFF;
		rKeyNum[3] = nGroup & 0xff;
		*(PWORD)(&rKeyNum[4]) = nCode & 0xFFFF;
	}
	else if (sscanf_s(pszText, "%d,%d", &nGroup, &nNumber) == 2)
	{
		rKeyNum[0] = 3;
		*(PWORD)(&rKeyNum[1]) = nNumber & 0xFFFF;
		rKeyNum[3] = nGroup & 0xff;
	}
	else
	{
		INT i, j;
		char sz[3] = {0};
		PCHAR p;
		i = strlen(pszText) - 2;
		j = 1;
		while (i >= 0)
		{
			sz[0] = pszText[i];
			sz[1] = pszText[i + 1];
			rKeyNum[j] = (BYTE)strtol(sz, &p, 16);
			if (errno != 0)
				break;
			if (++j > 6)
				break;
			i -= 2;
		}
		rKeyNum[0] = (j - 1);
	}
	return (rKeyNum[0] > 0);
}

BOOL GetNewList(CMyKeyList* pList)
{
	TCHAR szFilename[MAX_PATH];
	_tprintf(TEXT("Enter keys filename:\n"));
	if (_tscanf_s(TEXT("%s"), szFilename, _countof(szFilename)) != 1)
		return FALSE;
	if (GetFileAttributes(szFilename) == INVALID_FILE_ATTRIBUTES)
	{
		_tprintf(TEXT("File not found.\n"));
		return FALSE;
	}
	ifstream f;
	string sLine;
	f.open(szFilename, ios::in);
	LPCSTR seps = ";";
	LPSTR pToken, pNextToken, p;
	int n;
	_MYKEY r;
	while (f.good())
	{
		getline(f, sLine);
		pToken = strtok_s((LPSTR)sLine.c_str(), seps, &pNextToken);
		if ((pToken == NULL) || !ParseKeyNum(pToken, r.m_KeyNum))
			continue;
		r.m_nType = ZG_KEY_NORMAL;
		r.m_nAccess = 0xff;
		pToken = strtok_s(NULL, seps, &pNextToken);
		if (pToken != NULL)
		{
			while (*pToken == ' ') ++pToken;
			switch (toupper(*pToken))
			{
			case 'B':
				r.m_nType = ZG_KEY_BLOCKING;
				break;
			case 'M':
				r.m_nType = ZG_KEY_MASTER;
				break;
			}
			pToken = strtok_s(NULL, seps, &pNextToken);
			if (pToken != NULL)
			{
				while (*pToken == ' ') ++pToken;
				n = strtol(pToken, &p, 16);
				if (errno == 0)
					r.m_nAccess = n;
			}
		}
		pList->push_back(r);
	}
	f.close();

	_tprintf(TEXT("'Loaded %d keys. Continue [y/n]?\n"), pList->size());
	TCHAR szBuf[8];
	return (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1) && (toupper(*szBuf) == 'Y');
}

bool MyKeysLesser(const _MYKEY& elem1, const _MYKEY& elem2)
{
	return CompareZKeyNums(elem1.m_KeyNum, elem2.m_KeyNum) < 0;
}

int FindEraised(const CZgKeyList& oList, INT nStart, INT nBank)
{
	INT n = nBank * g_nMaxKeys;
	assert((n + g_nMaxKeys) <= (int)oList.size());
	for (int i = 0; i < (int)oList.size(); i++)
		if (oList[i].fErased)
			return i;
	return -1;
}

void SetCtrList(const CZgKeyList& oList, const CBits& oSync)
{
	assert(oList.size() == oSync.size());
	assert(oList.size() == (g_nMaxBanks * g_nMaxKeys));
	BOOL fChanged = FALSE;
	int nIdx, nPos, nWIdx, nWPos, nWCnt, nEnd;
	// Записываем в контроллер
	for (int i = 0; i < g_nMaxBanks; i++)
	{
		nIdx = 0;
		nPos = (i * g_nMaxKeys);
		while (nIdx < g_nMaxKeys)
		{
			if (oSync[nPos])
			{
				nIdx++;
				nPos++;
				continue;
			}
			nWIdx = nIdx++;
			nWPos = nPos++;
			nWCnt = 1;
			nEnd = (i + 1) * g_nMaxKeys;
			while (nPos < nEnd)
			{
				if ((nPos - nWPos) >= g_nOptWrite)
					break;
				if (!oSync[nPos])
					nWCnt = (nPos - nWPos + 1);
				nIdx++;
				nPos++;
			}
			if (nWCnt > 0)
			{
				if (!CheckZGError(ZG_Ctr_WriteKeys(g_hCtr, nWIdx, const_cast<PZG_CTR_KEY>(&oList[nWPos]), nWCnt, NULL, NULL, i, FALSE), _T("ZG_Ctr_WriteKeys")))
					return;
				_tprintf(TEXT("Updated keys %d-%d (bank#%d)\n"), nWIdx, nWIdx + nWCnt - 1, i);
				fChanged = TRUE;
			}
		}
	}
	if (fChanged)
	{
		std::wstring sCacheName;
		FormatStr(sCacheName, KeysCacheFileName_D, g_nSn);
		ofstream f;
		f.open(sCacheName, ios::out | ios::app | ios::binary | ios::trunc);
		f.write((LPCSTR)&oList[0], oList.size() * sizeof(_ZG_CTR_KEY));
		f.close();
	}
	else
		_tprintf(TEXT("List of keys of controller is not changed.\n"));
}

void DoLoadKeysFromFile()
{
	CMyKeyList oNewList;
	CZgKeyList oCtrList;
	if (!GetNewList(&oNewList))
		return;
	if (!GetCtrList(&oCtrList))
		return;
	// Сортируем новый список по возрастанию номеров чтобы легче было искать по номеру
	sort(oNewList.begin(), oNewList.end(), MyKeysLesser);
	CBits oSync(oCtrList.size(), false);
	int nIdx;
	CMyKeyList::iterator it;
	_MYKEY rMK;
	// Удаляем из CtrList ключи, которых нет в NewList
	for (size_t i = 0; i < oCtrList.size(); i++)
	{
		_ZG_CTR_KEY& rCK = oCtrList[i];
		if (rCK.fErased)
		{
			oSync[i] = true;
			continue;
		}
		memcpy_s(&rMK.m_KeyNum, sizeof(rMK.m_KeyNum), &rCK.rNum, sizeof(rCK.rNum));
		it = lower_bound(oNewList.begin(), oNewList.end(), rMK, MyKeysLesser);
		if ((it != oNewList.end()) && (CompareZKeyNums(rMK.m_KeyNum, rCK.rNum) == 0))
		{
			rMK = (*it);
			if ((rCK.nType != rMK.m_nType) || (rCK.nAccess != rMK.m_nAccess))
			{
				rCK.nType = rMK.m_nType;
				rCK.nAccess = rMK.m_nAccess;
			}
			else
				oSync[i] = true;
			oNewList.erase(it);
		}
		else
			rCK.fErased = TRUE;
	}
	// Добавляем из NewList в CtrList ключи, которых нет в CtrList
	INT aNext[2] = {0};
	assert(_countof(aNext) >= g_nMaxBanks);
	for (size_t i = 0; i < oNewList.size(); i++)
	{
		rMK = oNewList[i];
		for (int j = 0; j < g_nMaxBanks; j++)
		{
			nIdx = FindEraised(oCtrList, aNext[j], j);
			if (nIdx == -1)
			{
				_tprintf(TEXT("Key list is full (bank:%d)\n"), j);
				return;
			}
			_ZG_CTR_KEY& rCK = oCtrList[nIdx];
			rCK.fErased = FALSE;
			memcpy_s(rCK.rNum, sizeof(rCK.rNum), rMK.m_KeyNum, sizeof(rMK.m_KeyNum));
			rCK.nType = rMK.m_nType;
			rCK.nAccess = rMK.m_nAccess;
			oSync[nIdx] = false;
			aNext[j] = (nIdx + 1);
		}
	}
	SetCtrList(oCtrList, oSync);
	_tprintf(TEXT("Done.\n"));
}

void DoSaveKeysToFile()
{
	TCHAR szFilename[MAX_PATH];
	_tprintf(TEXT("Enter keys filename:\n"));
	if (_tscanf_s(TEXT("%s"), szFilename, _countof(szFilename)) != 1)
		return;

	CZgKeyList oCtrList;
	if (!GetCtrList(&oCtrList))
		return;
	LPCSTR KeyTypeAbbrs[4] = {"", "N", "B", "M"};
	ofstream f;
	CHAR szBuf[128];
	CHAR szNum[16];
	std::wstring s;

	f.open(szFilename, ios::out | ios::trunc);
	for (size_t i = 0; i < oCtrList.size(); i++)
	{
		const _ZG_CTR_KEY& rCK = oCtrList[i];
		if (rCK.fErased)
			continue;
		s = ZKeyNumToStr(rCK.rNum, g_fProximity);
		szNum[WideCharToMultiByte(CP_ACP, 0, s.c_str(), s.length(), szNum, _countof(szNum), NULL, NULL)] = '\0';
		sprintf_s(szBuf, "%s; %s; %.2X\n", szNum, KeyTypeAbbrs[rCK.nType], rCK.nAccess);
		f << szBuf;
	}
	f.close();

	_tprintf(TEXT("Done.\n"));
}


int _tmain(int argc, _TCHAR* argv[])
{
	setlocale(LC_ALL, "Russian");

	// Проверяем версию SDK
	DWORD nVersion = ZG_GetVersion();
	if (((nVersion & 0xff) != ZG_SDK_VER_MAJOR) || (((nVersion >> 8) & 0xff) != ZG_SDK_VER_MINOR))
	{
		_tprintf(TEXT("Wrond version SDK Guard.\n"));
		getchar();
		return 0;
	}

	HANDLE hCvt = 0;

	// Инициализируем библиотеку
	if (!CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP), _T("ZG_Initialize")))
		return 0;
	__try
	{
		// Подключаемся к конвертеру
		_ZG_CVT_OPEN_PARAMS rOp;
		ZeroMemory(&rOp, sizeof(rOp));
		rOp.nType = CvtPortType;
		rOp.pszName = CvtPortName;
		rOp.nSpeed = ZG_SPEED_57600;
		if (!CheckZGError(ZG_Cvt_Open(&hCvt, &rOp), _T("ZG_Cvt_Open")))
			return 0;
		// Подключаемся к контроллеру
		_ZG_CTR_INFO rCtrInfo;
		ZeroMemory(&rCtrInfo, sizeof(rCtrInfo));
		if (!CheckZGError(ZG_Ctr_Open(&g_hCtr, hCvt, CtrAddr, 0, &rCtrInfo), _T("ZG_Ctr_Open")))
			return 0;
		// Запоминаем некоторые параметры контроллера
		g_nSn = rCtrInfo.nSn;
		g_fProximity = (rCtrInfo.nFlags & ZG_CTR_F_PROXIMITY) != 0;
		g_nMaxBanks = (rCtrInfo.nFlags & ZG_CTR_F_2BANKS) ? 2 : 1;
		g_nMaxKeys = rCtrInfo.nMaxKeys;
		g_nOptRead = rCtrInfo.nOptReadItems;
		g_nOptWrite = rCtrInfo.nOptWriteItems;
		// Выводим на экран информацию о контроллере
		_tprintf(TEXT("%s addr: %d, s/n: %d, v%d.%d, Max_Banks: %d\n"),
			CtrTypeStrs[rCtrInfo.nType],
			rCtrInfo.nAddr,
			rCtrInfo.nSn,
			LOBYTE(rCtrInfo.nVersion), HIBYTE(rCtrInfo.nVersion),
			g_nMaxBanks);

		_tprintf(TEXT("-----\n"));
		TCHAR szBuf[128];
		while (1)
		{
			_tprintf(TEXT("Enter command number:\n"));
			_tprintf(TEXT("1 - show keys\n"));					// Показать все ключи
			_tprintf(TEXT("6 - clear keys cache\n"));			// Очистить кэш ключей (удаление файла)
			_tprintf(TEXT("7 - set new keys from file...\n"));	// Установить новый список из файла в контроллер...
			_tprintf(TEXT("8 - save keys to file...\n"));		// Сохранить ключи в файл, загрузив из контроллера или из кэша

			_tprintf(TEXT("0 - quit\n"));
			if (_tscanf_s(TEXT("%s"), szBuf, _countof(szBuf)) == 1)
			{
				_tprintf(TEXT("\n"));
				switch (_ttoi(szBuf))
				{
				case 1:
					ShowKeys();
					break;
				case 6:
					DoClearKeyCache();
					break;
				case 7:
					DoLoadKeysFromFile();
					break;
				case 8:
					DoSaveKeysToFile();
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
	__finally
	{
		if (g_hCtr != NULL)
			ZG_CloseHandle(g_hCtr);
		if (hCvt != NULL)
			ZG_CloseHandle(hCvt);
		ZG_Finalyze();
	}
	return 0;
}


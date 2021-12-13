#ifndef __ZBASE_H
#define __ZBASE_H

#include <algorithm>
using namespace std;

// Номер ключа контроллера ([0] - длина номера ключа)
typedef BYTE Z_KEYNUM[16];
typedef Z_KEYNUM near *PZ_KEYNUM;


inline INT CompareZKeyNums(const Z_KEYNUM& _Left, const Z_KEYNUM& _Right)
{
	if (_Left[0] == _Right[0])
		return (_Left[0] == 0) ? 0 : memcmp(&_Left[1], &_Right[1], _Left[0]);
	else
	{
		INT nL = min(_Left[0], _Right[0]);
		INT nRes = ((nL == 0) ? 0 : memcmp(&_Left[1], &_Right[1], nL));
		if (nRes == 0)
			nRes = ((_Left[0] < _Right[0]) ? -1 : +1);
		return nRes;
	}
}

#endif /* __ZBASE_H */


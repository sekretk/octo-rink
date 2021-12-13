#ifndef __ZPORT_H
#define __ZPORT_H
// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the GUARDL_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// ZPORT_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.

#ifndef ZPORT_LINKONREQUEST

#ifdef ZPORT_EXPORTS
#define ZPORT_API(type) extern "C" __declspec(dllexport) type __stdcall 
#else
#ifdef ZPORT_STATIC
#define ZPORT_API(type) extern "C" type __stdcall 
#else
#define ZPORT_API(type) extern "C" __declspec(dllimport) type __stdcall 
#endif
#endif
#endif

#include "Windows.h"
#include <Unknwn.h>

#define ZP_SDK_VER_MAJOR	1
#define ZP_SDK_VER_MINOR	18

#ifndef DEPRECATED_ATTR
#if __BORLANDC__ >= 0x0620
#define DEPRECATED_ATTR  [[deprecated]]
#else
#define DEPRECATED_ATTR
#endif
#endif

#pragma pack(1)   // turn byte alignment on
#pragma option push -b

#define ZP_S_CANCELLED					((HRESULT)0x00040201L)	// �������� �������������
#define ZP_S_NOTFOUND					((HRESULT)0x00040202L)	// �� ������ (��� ������� ZP_FindSerialDevice)
#define ZP_S_TIMEOUT					((HRESULT)0x00040203L)
#define ZP_E_OPENNOTEXIST				((HRESULT)0x80040203L)	// ���� �� ����������
#define ZP_E_OPENPORT					((HRESULT)0x80040205L)	// ������ ������ �������� �����
#define ZP_E_PORTIO						((HRESULT)0x80040206L)	// ������ ����� (��������� �������� �� USB?)
#define ZP_E_PORTSETUP					((HRESULT)0x80040207L)	// ������ ��������� �����
#define ZP_E_LOADFTD2XX					((HRESULT)0x80040208L)	// ��������� ��������� FTD2XX.DLL
#define ZP_E_SOCKET						((HRESULT)0x80040209L)	// ������ �������
#define ZP_E_SERVERCLOSE				((HRESULT)0x8004020AL)	// ���������� ������ �� ������� �������
#define ZP_E_NOTINITALIZED				((HRESULT)0x8004020BL)	// �� ������������������� � ������� ZP_Initialize
#define ZP_E_INSUFFICIENTBUFFER			((HRESULT)0x8004020CL)	// ������ ������ ������� ���
#define ZP_E_NOCONNECT                   ((HRESULT)0x8004020DL)


#define ZP_MAX_PORT_NAME				31
#define ZP_MAX_REG_DEV				32

// ����� ��� ������� ZP_Initialize
#define ZP_IF_NO_MSG_LOOP		0x0001	// ���������� �� ����� ����� ��������� ��������� (Console or Service)
#define ZP_IF_LOG				0x0002	// ���������� ���

// ����� ��� ������� ZP_FindNotification
enum _ZP_DD_FLAGS
{
	ZP_NF_EXIST					= 0x0001, // ����������� � �����������/���������� ����� (ZP_N_INSERT / ZP_N_REMOVE)
	ZP_NF_CHANGE				= 0x0002, // ����������� � ��������� ���������� ����� (ZP_N_CHANGE)
	ZP_NF_ERROR					= 0x0008, // ����������� �� ������ � ����(thread), ����������� ����� (ZP_N_ERROR)
	ZP_NF_SDEVICE				= 0x0010, // ���������� ����������, ������������ �� USB
	ZP_NF_IPDEVICE				= 0x0020, // ���������� IP ���������� (���������� � ������ SERVER)
	ZP_NF_IPSDEVICE				= 0x0080, // ���������� IPS ���������� (���������� � ������ CLIENT)
	ZP_NF_COMPLETED				= 0x0040, // ����������� � ���������� ������������
	ZP_NF_DEVEXIST				= 0x0004, // ����������� � �����������/���������� ���������� (ZP_N_DEVINSERT / ZP_N_DEVREMOVE)
	ZP_NF_DEVCHANGE				= 0x0100, // ����������� � ��������� ���������� ���������� (ZP_N_DEVCHANGE)
	ZP_NF_UNIDCOM				= 0x1000, // ������ ������������ com-�����
	ZP_NF_USECOM				= 0x2000  // �� ����������� ������������ Com-����
};
typedef UINT ZP_DD_FLAGS;

// ����������� ������� ZP_FindNotification (wParam - MsgCode, lParam - MsgParam)
typedef enum _ZP_DD_NOTIFICATIONS
{
	ZP_N_INSERT					= 1, // ����������� �����. (PZP_N_EXIST_INFO)MsgParam - ���� � �����
	ZP_N_REMOVE					= 2, // ���������� �����. (PZP_N_EXIST_INFO)MsgParam - ���� � �����
	ZP_N_CHANGE					= 3, // ��������� ���������� �����. (PZP_N_CHANGE_INFO)MsgParam - ���� �� ����������
	ZP_N_ERROR					= 4, // ��������� ������ � ������. (PHRESULT)MsgParam - ��� ������
	ZP_N_COMPLETED				= 5, // ������������ ���������. (PINT)MsgParam - �����: b0-������ com-������, b1-������ ip-������, b2-���������� �� �����������, b3-�������������, b4-�������, <0-������
	ZP_N_DEVINSERT				= 6, // ����������� ����������. (PZP_N_EXIST_DEVINFO)MsgParam - ���� � ����������
	ZP_N_DEVREMOVE              = 7, // ���������� ����������. (PZP_N_EXIST_DEVINFO)MsgParam - ���� � ����������
	ZP_N_DEVCHANGE				= 8 // ��������� ���������� ����������. (PZP_N_CHANGE_DEVINFO)MsgParam - ���� �� ����������
} ZP_DD_NOTIFICATIONS;

// ��������� �� ��������� (����-���� � ������� � �������������)
#define ZP_IP_CONNECTTIMEOUT		4000		// ����-��� ����������� �� TCP ��� ����� ���� ZP_PORT_IP
#define ZP_IP_RESTOREPERIOD			3000		// ������ �������������� ��������� TCP-����� (��� ����� ���� ZP_PORT_IP)
#define ZP_IPS_CONNECTTIMEOUT		10000		// ����-��� ����������� �� TCP ��� ����� ���� ZP_PORT_IPS
#define ZP_USB_RESTOREPERIOD		3000		// ������ �������������� ��������� ����� (��� ������ ����� ZP_PORT_COM � ZP_PORT_FT)
#define ZP_DTC_FINDUSBPERIOD		5000		// ������ ������ USB-��������� (������ ����� com-������) (��� ��������� ���������)
#define ZP_DTC_FINDIPPERIOD			15000		// ������ ������ IP-���������� �� UDP (��� ��������� ���������)
#define ZP_DTC_SCANDEVPERIOD		INFINITE	// ������ ������������ ���������, ������� ������ (��� ��������� ���������)
#define ZP_SCAN_RCVTIMEOUT0			500			// ����-��� �������� ������� ����� ������ �� ������ ��� ������������ ���������
#define ZP_SCAN_RCVTIMEOUT			3000		// ����-��� �������� ������ �� ������ ��� ������������ ���������
#define ZP_SCAN_MAXTRIES			2			// �������� ������� ������� ��� ������������ ���������
#define ZP_SCAN_CHECKPERIOD			INFINITE	// ������ �������� �������� ������ ����� ��� ������������ ������
#define ZP_FINDIP_RCVTIMEOUT		1000		// ����-��� ������ ip-��������� �� UDP
#define ZP_FINDIP_MAXTRIES			1			// �������� ������� ������ ip-��������� �� UDP

// ��� �����
enum ZP_PORT_TYPE
{
	ZP_PORT_UNDEF = 0,
	ZP_PORT_COM,		// Com-����
	ZP_PORT_FT,			// FT-���� (����� ftd2xx.dll �� �/� USB, ������ ��� ���������, ������������ ���� �������)
	ZP_PORT_IP,			// Ip-���� (TCP-������)
	ZP_PORT_IPS			// IPS-���� (TCP-������)
};

// ��� �����
typedef WCHAR ZP_PORT_NAME[ZP_MAX_PORT_NAME + 1];

// ��������� �������� ���������� �������
typedef struct _ZP_WAIT_SETTINGS {
	UINT nReplyTimeout;			// ����-��� �������� ������
	INT nMaxTries;				// ���������� ������� ��������� ������
	HANDLE hAbortEvent;			// ���������� ������������ ������� Event ��� ������ �������
	UINT nReplyTimeout0;		// ����-��� �������� ������� ������� ������
	UINT nCheckPeriod;			// ������ �������� ����� � �� (���� =0 ��� =INFINITE, �� �� RX-�������)
	UINT nConnectTimeout;		// ����-��� ����������� �� TCP
	UINT nRestorePeriod;		// ������ � ������� ����� �������������� ������� ������������ ��������� TCP-�����
} *PZP_WAIT_SETTINGS;

// ����� ��� ZP_OPEN_PARAMS.nFlags
enum _ZP_PORT_OPEN_FLAGS
{
	ZP_POF_NO_WAIT_CONNECT	= 0x0001,	// �� ����� ���������� ��������� �����������
	ZP_POF_NO_CONNECT_ERR	= 0x0002,	// �� ���������� ������ � ������ ����� ��� �����
	ZP_POF_NO_DETECT_USB	= 0x0004	// �� ������������ �������� USB-��������� (��� ZP_PORT_FT � ZP_PORT_COM)
};
typedef UINT ZP_PO_FLAGS;
// ��������� �������� ����� (��� ������� ZP_Open)
typedef struct _ZP_PORT_OPEN_PARAMS {
	LPCWSTR szName;				// ��� �����
	ZP_PORT_TYPE nType;			// ��� �����
	UINT nBaud;					// �������� �����
	CHAR nEvChar;				// ���������� ������
	BYTE nStopBits;				// ���������� �������� �����
	UINT nConnectTimeout;		// ����-��� ����������� �� TCP
	UINT nRestorePeriod;		// ������ � ������� ����� �������������� ������� ������������ ��������� TCP-�����
	ZP_PO_FLAGS nFlags;			// ����� ZP_PF_...
}*PZP_PORT_OPEN_PARAMS;

// ����� ��� ����������� �����
typedef enum _ZP_PORT_FLAGS
{
	ZP_PNF_RXEVENT		= 0x0001,	// ������ ����� ������ �� ����������
	ZP_PNF_STATUS		= 0x0002	// ���������� ��������� ����������� �����
}ZP_PORT_FLAGS;
typedef UINT ZP_PN_FLAGS;

// ����� ����� (��� nFlags � ��������� ZP_PORT_INFO)
enum _ZP_PORT_INFO_FLAGS
{
	ZP_PIF_BUSY			= 0x0001,	// ���� �����
	ZP_PIF_USER			= 0x0002	// ����, ��������� ������������� (������ _ZP_PORT_ADDR)
};
typedef UINT ZP_PI_FLAGS;
// ���������� � �����
typedef struct _ZP_PORT_INFO {
	ZP_PORT_TYPE nType;			// ��� �����
	ZP_PORT_NAME szName;		// ��� �����
	ZP_PI_FLAGS nFlags;			// ����� ����� (ZP_PF_...)
	ZP_PORT_NAME szFriendly;	// ������������� ��� �����
	UINT nDevTypes;				// ����� ����� ���������
	WCHAR szOwner[64];			// �������� ����� (��� ������� ZP_EnumIpDevices)
} *PZP_PORT_INFO;

typedef struct _ZP_DEVICE_INFO {
	UINT cbSize;				// ������ ���������
	UINT nTypeId;
	UINT nModel;				// ������
	UINT nSn;					// �/�
	UINT nVersion;				// ������ ��������
} *PZP_DEVICE_INFO;

// ��������� �����������
enum ZP_CONNECTION_STATUS
{
	ZP_CS_DISCONNECTED = 0,		// ��������
	ZP_CS_CONNECTED,			// ���������
	ZP_CS_CONNECTING,			// ���� �����������... (��� ������ �����������)
	ZP_CS_RESTORATION			// �������������� �����... (��� ��������� ������������)
};

// ����� ��� _ZP_DDN_PORT_INFO.nChangeMask � _ZP_DDN_DEVICE_INFO.nChangeMask
enum _ZP_DDN_PORT_INFO_FLAGS
{
	ZP_CIF_BUSY			= 0x0004,	// ���������� ��������� "���� �����"
	ZP_CIF_FRIENDLY		= 0x0008,	// ���������� ������������� ��� �����
	ZP_CIF_OWNER		= 0x0020,	// ��������� �������� ����� (������ ��� IP ���������)
	ZP_CIF_MODEL		= 0x0080,	// ���������� ������ ����������
	ZP_CIF_SN			= 0x0100,	// ��������� �������� ����� ����������
	ZP_CIF_VERSION		= 0x0200,	// ���������� ������ �������� ����������
	ZP_CIF_DEVPARAMS	= 0x0400,	// ���������� ����������� ��������� ����������
	ZP_CIF_LIST			= 0x0800	// ��������� ������ ������ (��� _ZP_DDN_DEVICE_INFO) ��� ��������� (��� _ZP_DDN_PORT_INFO)
};
typedef UINT _ZP_DDNPI_FLAGS;
// ���������� � �����
typedef struct _ZP_DDN_PORT_INFO {
	_ZP_PORT_INFO rPort;
	PZP_DEVICE_INFO* aDevs;
	INT nDevCount;
	_ZP_DDNPI_FLAGS nChangeMask;	// ����� ��������� (ZP_CIF_...)
} *PZP_DDN_PORT_INFO;

// ���������� � ����������
typedef struct _ZP_DDN_DEVICE_INFO {
	PZP_DEVICE_INFO pInfo;
	PZP_PORT_INFO aPorts;
	INT nPortCount;
	UINT nChangeMask;				// ����� ��������� (ZP_CIF_...)
} *PZP_DDN_DEVICE_INFO;

// Callback-������� (������� ��������� ������)
typedef BOOL (CALLBACK* ZP_DEVICEPARSEPROC) (LPCVOID pReply, UINT nCount, PBOOL pPartially, 
	PZP_DEVICE_INFO pInfo, PZP_PORT_INFO pPortArr, INT nArrLen, PINT pPortCount);

typedef struct _ZP_DD_NOTIFY_SETTINGS {
	UINT nNMask;			// ����� ����� ����������� ZP_NF_

	HANDLE hEvent;          // ������� (������ �������������)
	HWND hWindow;				// ����, ����������� ��������� nWndMsgId
	UINT nWndMsgId;         // ��������� ��� �������� ���� hWnd

	DWORD nSDevTypes;		// ����� ����� ���������, ������������ � ����������������� �����
	DWORD nIpDevTypes;		// ����� ����� Ip-���������

	LPWORD aIps;			// ������ TCP-������ ��� ����������� ����������� � ������ "CLIENT" (���� NULL, �� �� ������������)
	INT nIpsCount;			// ���������� TCP-������
} *PZP_DD_NOTIFY_SETTINGS;

// ��������� ��� ��������� ���������
typedef struct _ZP_DD_GLOBAL_SETTINGS {
	UINT nCheckUsbPeriod;		// ������ �������� ��������� USB-������ (� �������������) (=0 �� ��������� 5000)
	UINT nCheckIpPeriod;		// ������ �������� ��������� IP-������ (� �������������) (=0 �� ��������� 15000)
	UINT nScanDevPeriod;		// ������ ������������ ��������� �� USB- � IP-������ (� �������������) (=0 �� ��������� �������=INFINITE)

	UINT nIpReqTimeout;			// ����-��� �������� ������ �� ip-���������� ��� ������ �� UDP
	INT nIpReqMaxTries;			// ���������� ������� ������ ip-���������� �� UDP
	_ZP_WAIT_SETTINGS rScanWS;	// ��������� �������� ��� ������������ ������
} *PZP_DD_GLOBAL_SETTINGS;

typedef struct _ZP_DEVICE {
	UINT nTypeId;				// ��� ����������
	LPCVOID pReqData;			// ������ ������� (����� ���� NULL)
	UINT nReqSize;				// ���������� ���� � �������
	ZP_DEVICEPARSEPROC pfnParse;// ������� ������� ������
	UINT nDevInfoSize;			// ������ ��������� ZP_DEVICE_INFO
} *PZP_DEVICE;

typedef struct _ZP_IP_DEVICE : _ZP_DEVICE {
	WORD nReqPort;				// UDP-���� ��� �������
	INT nMaxPort;				// �������� ������ � ���������� (����� � ����������)
} *PZP_IP_DEVICE;

typedef struct _ZP_USB_DEVICE : _ZP_DEVICE {
	PDWORD pVidPids;			// ������ Vid,Pid USB-��������� MAKELONG(vid, pid)
	INT nVidPidCount;			// ������ ������� Vid,Pid
	UINT nBaud;					// �������� �����
	CHAR chEvent;				// ������-������� ����� �������� (���� =0, ��� �������)
	BYTE nStopBits;				// �������� ���� (ONESTOPBIT=0, ONE5STOPBITS=1, TWOSTOPBITS=2)
	LPCWSTR pszBDesc;			// �������� ����������, ��������������� ����� (DEVPKEY_Device_BusReportedDeviceDesc)
} *PZP_USB_DEVICE;

typedef struct _ZP_PORT_ADDR {
	ZP_PORT_TYPE nType;	// ��� �����
	LPCWSTR pName;		// ��� �����
	DWORD nDevTypes;	// ����� ����� ���������
} *PZP_PORT_ADDR;

// ����� ��� ��������� ZP_SEARCH_PARAMS.nFlags
enum _ZP_SEARCH_PARAMS_FLAGS
{
	ZP_SF_USECOM		= 0x0001,	// ������������ COM-���� �� �����������
	ZP_SF_DETECTOR		= 0x0002,	// ������������ ��� ������� ������ ��������� ��������� ��������� (��������� �������� ZP_SetNotification)
	ZP_SF_IPS			= 0x0004,	// �������� � ������ ��������� IP-���������� � ������ CLIENT (�� �������� � ������ ZP_SF_DETECTOR)
	ZP_SF_UNID			= 0x0008,	// �������� � ������ ������������ ����������
	ZP_SF_UNIDCOM		= 0x0010	// ���������� ������������ com-�����
};
typedef UINT _ZP_SP_FLAGS;
// ��������� ������ ��������� (��� ������� ZP_SearchDevices)
typedef struct _ZP_SEARCH_PARAMS {
	UINT nDevMask;				// ����� ��������� ��� ������������ ������ (=0 �� ������, =0xffffffff ������ ��)
	UINT nIpDevMask;			// ����� IP ���������, ����������� � ������� UDP-������� (=0 �� ������, =0xffffffff ������ ��)
	PZP_PORT_ADDR pPorts;		// ������ ������
	INT nPCount;				// ������ ������ ������
	_ZP_SP_FLAGS nFlags;				// ����� ZP_SF_...
	PZP_WAIT_SETTINGS pWait;	// ��������� �������� ��� ������������ ������. ����� ���� =NULL.
	UINT nIpReqTimeout;			// ����-��� �������� ������ �� ip-���������� ��� ������ �� UDP
	INT nIpReqMaxTries;			// ���������� ������� ������ ip-���������� �� UDP
}*PZP_SEARCH_PARAMS;

#if !defined(_countof)
#if __BORLANDC__ >= 0x0620
#define _countof(array)		RTL_NUMBER_OF(array)
#else
#define _countof(array)		sizeof(array)/sizeof(array[0])
#endif
#endif

typedef INT ZP_STATUS DEPRECATED_ATTR;	// ��������� ���������� ������� DLL

#pragma deprecated(ZP_SUCCESS)
#define ZP_SUCCESS						S_OK
#pragma deprecated(ZP_E_CANCELLED)
#define ZP_E_CANCELLED					ZP_S_CANCELLED
#pragma deprecated(ZP_E_NOT_FOUND)
#define ZP_E_NOT_FOUND					ZP_S_NOTFOUND
#pragma deprecated(ZP_E_INVALID_PARAM)
#define ZP_E_INVALID_PARAM				E_INVALIDARG
#pragma deprecated(ZP_E_OPEN_NOT_EXIST)
#define ZP_E_OPEN_NOT_EXIST				ZP_E_OPENNOTEXIST
#pragma deprecated(ZP_E_OPEN_ACCESS)
#define ZP_E_OPEN_ACCESS				ZP_E_OPENACCESS
#pragma deprecated(ZP_E_OPEN_PORT)
#define ZP_E_OPEN_PORT					ZP_E_OPENPORT
#pragma deprecated(ZP_E_PORT_IO_ERROR)
#define ZP_E_PORT_IO_ERROR				ZP_E_PORTIO
#pragma deprecated(ZP_E_PORT_SETUP)
#define ZP_E_PORT_SETUP					ZP_E_PORTSETUP
#pragma deprecated(ZP_E_LOAD_FTD2XX)
#define ZP_E_LOAD_FTD2XX				ZP_E_LOADFTD2XX
#pragma deprecated(ZP_E_INIT_SOCKET)
#define ZP_E_INIT_SOCKET				ZP_E_SOCKET
#pragma deprecated(ZP_E_NOT_ENOUGH_MEMORY)
#define ZP_E_NOT_ENOUGH_MEMORY			E_OUTOFMEMORY
#pragma deprecated(ZP_E_UNSUPPORT)
#define ZP_E_UNSUPPORT					E_NOINTERFACE
#pragma deprecated(ZP_E_NOT_INITALIZED)
#define ZP_E_NOT_INITALIZED				ZP_E_NOTINITALIZED
#pragma deprecated(ZP_E_CREATE_EVENT)
#define ZP_E_CREATE_EVENT				E_FAIL
#pragma deprecated(ZP_E_OTHER)
#define ZP_E_OTHER						E_FAIL
#pragma deprecated(ZP_NF_BUSY)
#define ZP_NF_BUSY				ZP_NF_CHANGE
#pragma deprecated(ZP_NF_FRIENDLY)
#define ZP_NF_FRIENDLY			ZP_NF_CHANGE
#pragma deprecated(ZP_N_STATE_CHANGED)
#define ZP_N_STATE_CHANGED		ZP_N_CHANGE
#pragma deprecated(_ZP_OPEN_PARAMS)
#define _ZP_OPEN_PARAMS					_ZP_PORT_OPEN_PARAMS
#pragma deprecated(PZP_OPEN_PARAMS)
#define PZP_OPEN_PARAMS					PZP_PORT_OPEN_PARAMS
#pragma deprecated(_ZP_N_CHANGE_STATE)
#define _ZP_N_CHANGE_STATE				_ZP_DDN_PORT_INFO
#pragma deprecated(PZP_N_CHANGE_STATE)
#define PZP_N_CHANGE_STATE				PZP_DDN_PORT_INFO
#pragma deprecated(ZP_NF_USEVCOM)
#define ZP_NF_USEVCOM					ZP_NF_USECOM
#pragma deprecated(ZP_NF_WND_SYNC)
#define ZP_NF_WND_SYNC					ZP_NF_WNDSYNC
#pragma deprecated(ZP_NF_WNDSYNC)
#define ZP_NF_WNDSYNC				0x4000	
#pragma deprecated(ZP_NF_ONLY_NOTIFY)
#define ZP_NF_ONLY_NOTIFY				ZP_NF_ONLYNOTIFY
#pragma deprecated(ZP_NF_ONLYNOTIFY)
#define ZP_NF_ONLYNOTIFY			0x8000
#pragma deprecated(ZP_PF_BUSY2)
#define ZP_PF_BUSY2						4
#pragma deprecated(ZP_PF_BUSY)
#define ZP_PF_BUSY						ZP_PIF_BUSY
#pragma deprecated(ZP_PF_USER)
#define ZP_PF_USER						ZP_PIF_USER
#pragma deprecated(ZP_E_OPENACCESS)
#define ZP_E_OPENACCESS					E_ACCESSDENIED
#pragma deprecated(ZP_IF_ERROR_LOG)
#define ZP_IF_ERROR_LOG				ZP_IF_LOG
#pragma deprecated(ZP_PF_NOWAITCONNECT)
#define ZP_PF_NOWAITCONNECT				ZP_POF_NO_WAIT_CONNECT
#pragma deprecated(ZP_PF_NOCONNECTERR)
#define ZP_PF_NOCONNECTERR				ZP_POF_NO_CONNECT_ERR
#pragma deprecated(ZP_PF_NOUSBDETECT)
#define ZP_PF_NOUSBDETECT				ZP_POF_NO_DETECT_USB
#pragma deprecated(_ZP_N_EXIST_INFO)
#define _ZP_N_EXIST_INFO				_ZP_DDN_PORT_INFO
#pragma deprecated(PZP_N_EXIST_INFO)
#define PZP_N_EXIST_INFO				PZP_DDN_PORT_INFO
#pragma deprecated(_ZP_N_CHANGE_INFO)
#define _ZP_N_CHANGE_INFO				_ZP_DDN_PORT_INFO
#pragma deprecated(PZP_N_CHANGE_INFO)
#define PZP_N_CHANGE_INFO				PZP_DDN_PORT_INFO
#pragma deprecated(_ZP_N_EXIST_DEVINFO)
#define _ZP_N_EXIST_DEVINFO				_ZP_DDN_DEVICE_INFO
#pragma deprecated(PZP_N_EXIST_DEVINFO)
#define PZP_N_EXIST_DEVINFO				PZP_DDN_DEVICE_INFO
#pragma deprecated(_ZP_N_CHANGE_DEVINFO)
#define _ZP_N_CHANGE_DEVINFO			_ZP_DDN_DEVICE_INFO
#pragma deprecated(PZP_N_CHANGE_DEVINFO)
#define PZP_N_CHANGE_DEVINFO			PZP_DDN_DEVICE_INFO
#pragma deprecated(_ZP_NOTIFY_SETTINGS)
#define _ZP_NOTIFY_SETTINGS			_ZP_DD_NOTIFY_SETTINGS
#pragma deprecated(PZP_NOTIFY_SETTINGS)
#define PZP_NOTIFY_SETTINGS			PZP_DD_NOTIFY_SETTINGS
#pragma deprecated(_ZP_DETECTOR_SETTINGS)
#define _ZP_DETECTOR_SETTINGS			_ZP_DD_GLOBAL_SETTINGS
#pragma deprecated(PZP_DETECTOR_SETTINGS)
#define PZP_DETECTOR_SETTINGS			PZP_DD_GLOBAL_SETTINGS
#pragma deprecated(ZP_CIF_PORTS)
#define ZP_CIF_PORTS			ZP_CIF_LIST
#pragma deprecated(ZP_CIF_DEVICES)
#define ZP_CIF_DEVICES			ZP_CIF_LIST
#pragma deprecated(_ZP_S_DEVICE)
#define _ZP_S_DEVICE			_ZP_USB_DEVICE
#pragma deprecated(PZP_S_DEVICE)
#define PZP_S_DEVICE			PZP_USB_DEVICE

typedef BOOL (CALLBACK* ZP_ENUMPORTSPROC) (PZP_PORT_INFO pInfo, PVOID pUserData);
typedef BOOL (CALLBACK* ZP_ENUMDEVICEPROC) (PZP_DEVICE_INFO pInfo, PZP_PORT_INFO pPort, PVOID pUserData);
typedef BOOL (CALLBACK* ZP_NOTIFYPROC) (UINT nMsg, LPARAM nMsgParam, PVOID pUserData);

#pragma option pop
#pragma pack() // turn byte alignment off

#ifndef ZPORT_LINKONREQUEST

// ���������� ������ ����������
ZPORT_API(DWORD) ZP_GetVersion();

// �������������/����������� ����������
ZPORT_API(HRESULT) ZP_Initialize(void** ppvObject, UINT nFlags);
ZPORT_API(HRESULT) ZP_Finalyze();

ZPORT_API(HRESULT) ZP_CloseHandle(HANDLE hHandle);

// ������� ������ ������������ ������
ZPORT_API(HRESULT) ZP_GetPortInfoList(PHANDLE pHandle, PINT pCount, UINT nSerDevs=0xffffffff, UINT nFlags=0);
ZPORT_API(HRESULT) ZP_GetPortInfo(HANDLE hHandle, INT nIdx, PZP_PORT_INFO pInfo);
// ���� ����������, �������� ����� �(���) ��������� ip-���������� �� UDP
ZPORT_API(HRESULT) ZP_SearchDevices(PHANDLE pHandle, PZP_SEARCH_PARAMS pParams);
ZPORT_API(HRESULT) ZP_FindNextDevice(HANDLE hHandle, PZP_DEVICE_INFO pInfo, 
	PZP_PORT_INFO pPortArr, INT nArrLen, PINT pPortCount, UINT nTimeout=INFINITE);

// ����������� � �����������/���������� ���������
ZPORT_API(HRESULT) ZP_DD_SetNotification(PHANDLE pHandle, PZP_DD_NOTIFY_SETTINGS pSettings);
ZPORT_API(HRESULT) ZP_DD_GetNextMessage(HANDLE hHandle, LPUINT pMsg, LPARAM* pMsgParam);
ZPORT_API(HRESULT) ZP_DD_SetGlobalSettings(PZP_DD_GLOBAL_SETTINGS pSettings);
ZPORT_API(HRESULT) ZP_DD_GetGlobalSettings(PZP_DD_GLOBAL_SETTINGS pSettings);
ZPORT_API(HRESULT) ZP_DD_Refresh(UINT nWaitMs=0);

ZPORT_API(HRESULT) ZP_SetServiceCtrlHandle(SERVICE_STATUS_HANDLE hSvc);
ZPORT_API(VOID) ZP_DeviceEventNotify(DWORD nEvType, PVOID pEvData);

// ������� � ������
ZPORT_API(HRESULT) ZP_Port_Open(PHANDLE pHandle, PZP_PORT_OPEN_PARAMS pParams);
ZPORT_API(HRESULT) ZP_Port_SetBaudAndEvChar(HANDLE hHandle, UINT nBaud, CHAR chEvChar);
ZPORT_API(HRESULT) ZP_Port_GetBaudAndEvChar(HANDLE hHandle, LPUINT pBaud, PCHAR pEvChar);
ZPORT_API(HRESULT) ZP_Port_GetConnectionStatus(HANDLE hHandle, ZP_CONNECTION_STATUS* pValue);
ZPORT_API(HRESULT) ZP_Port_SetNotification(HANDLE hHandle, HANDLE hEvent, HWND hWnd, UINT nMsgId, ZP_PN_FLAGS nMsgMask);
ZPORT_API(HRESULT) ZP_Port_EnumMessages(HANDLE hHandle, PUINT pMsgs);

ZPORT_API(HRESULT) ZP_Port_Clear(HANDLE hHandle, BOOL fIn, BOOL fOut);
ZPORT_API(HRESULT) ZP_Port_Write(HANDLE hHandle, LPCVOID pBuf, UINT nCount);
ZPORT_API(HRESULT) ZP_Port_Read(HANDLE hHandle, LPVOID pBuf, UINT nCount, PUINT pRead);
ZPORT_API(HRESULT) ZP_Port_GetInCount(HANDLE hHandle, PUINT pCount);
ZPORT_API(HRESULT) ZP_Port_SetDtr(HANDLE hHandle, BOOL fState);
ZPORT_API(HRESULT) ZP_Port_SetRts(HANDLE hHandle, BOOL fState);

ZPORT_API(HRESULT) ZP_RegSerialDevice(PZP_USB_DEVICE pParams);
ZPORT_API(HRESULT) ZP_RegIpDevice(PZP_IP_DEVICE pParams);

#define ZP_L_ERROR		0
#define ZP_L_WARNING	1
#define ZP_L_INFO		2
#define MAKELOGTYPE(_type, _level)	(UINT(_level-1) * 3 + _type)
#ifdef ZP_LOG
ZPORT_API(HRESULT) ZP_SetLog(LPCWSTR pszSvrAddr, LPCWSTR pszFileName, UINT nFileTypeMask);
ZPORT_API(HRESULT) ZP_GetLog(LPWSTR pszSvrAddrBuf, UINT nSABufSize, LPWSTR pszFileNameBuf, UINT nFNBufSize, PUINT pFileTypeMask);
ZPORT_API(HRESULT) ZP_AddLog(WCHAR chSrc, UINT nType, UINT nMsgId, LPCWSTR pszText);
#else
inline HRESULT ZP_AddLog(WCHAR /*chSrc*/, UINT /*nType*/, UINT /*nMsgId*/, LPCWSTR /*pszText*/) { return 0; };
#endif // ZP_LOG
inline HRESULT ZP_CloseNotification(HANDLE hHandle) DEPRECATED_ATTR {
	return ZP_CloseHandle(hHandle);
}
inline HRESULT ZP_Open(PHANDLE pHandle, PZP_OPEN_PARAMS pParams) DEPRECATED_ATTR {
	return ZP_Port_Open(pHandle, pParams);
}
inline HRESULT ZP_Close(HANDLE hHandle) DEPRECATED_ATTR {
	return ZP_CloseHandle(hHandle);
}

inline HRESULT ZP_EnumSerialPorts(UINT nDevTypes,
	ZP_ENUMPORTSPROC pEnumProc, PVOID pUserData) DEPRECATED_ATTR {
	HANDLE hList;
	int nPortCount;
	HRESULT hr = ZP_GetPortInfoList(&hList, &nPortCount, nDevTypes);
	if (FAILED(hr))
		return hr;
	__try {
		_ZP_PORT_INFO rPI;
		for (int i = 0; i < nPortCount; i++) {
			if (FAILED(hr = ZP_GetPortInfo(hList, i, &rPI)))
				return hr;
			if (!pEnumProc(&rPI, pUserData))
				return ZP_S_CANCELLED;
		}
	}
	__finally {
		ZP_CloseHandle(hList);
    }
	return hr;
}

inline HRESULT ZP_EnumMessages(HANDLE hHandle, ZP_NOTIFYPROC pEnumProc, PVOID pUserData) DEPRECATED_ATTR {
	HRESULT hr;
	UINT nMsg;
	LPARAM nMsgParam;
	while ((hr = ZP_DD_GetNextMessage(hHandle, &nMsg, &nMsgParam)) == S_OK)
		pEnumProc(nMsg, nMsgParam, pUserData);
	if (hr == ZP_S_NOTFOUND)
		hr = S_OK;
	return hr;
}
inline HRESULT ZP_SetNotification(PHANDLE pHandle, PZP_DD_NOTIFY_SETTINGS pSettings) DEPRECATED_ATTR {
	return ZP_DD_SetNotification(pHandle, pSettings);
}
inline HRESULT ZP_GetNextMessage(HANDLE hHandle, LPUINT pMsg, LPARAM* pMsgParam) DEPRECATED_ATTR {
	return ZP_DD_GetNextMessage(hHandle, pMsg, pMsgParam);
}
inline HRESULT ZP_GetGlobalSettings(PZP_DD_GLOBAL_SETTINGS pSettings) DEPRECATED_ATTR {
	return ZP_DD_GetGlobalSettings(pSettings);
}
inline HRESULT ZP_SetGlobalSettings(PZP_DD_GLOBAL_SETTINGS pSettings) DEPRECATED_ATTR {
	return ZP_DD_SetGlobalSettings(pSettings);
}
inline HRESULT ZP_Refresh(UINT nWaitMs=0) DEPRECATED_ATTR {
	return ZP_DD_Refresh(nWaitMs);
}

#endif // ZPORT_LINKONREQUEST

#endif /* __ZPORT_H */


unit ZPort;

interface

{$IF CompilerVersion >= 17.0}
  {$DEFINE HAS_INLINE}
{$IFEND}

uses
  Windows;

// (rom) this is the switch to change between static and dynamic linking.
// (rom) it is enabled by default here.
// (rom) To disable simply change the '$' to a '.'.
//{$DEFINE ZP_LINKONREQUEST}

{$IF not Declared(NativeInt)}
Type
  NativeInt = Integer;
{$IFEND}
{$IF not Declared(PPVoid)}
Type
  PPVoid = ^Pointer;
{$IFEND}
Const
  ZP_SDK_VER_MAJOR        = 1;
  ZP_SDK_VER_MINOR        = 18;
Const
  ZP_S_CANCELLED          = HResult($00040201); // �������� �������������
  ZP_S_NOTFOUND           = HResult($00040202); // �� ������ (��� ������� ZP_FindSerialDevice)
  ZP_S_TIMEOUT            = HResult($00040203);
  ZP_E_OPENNOTEXIST       = HResult($80040203); // ���� �� ����������
  ZP_E_OPENPORT           = HResult($80040205); // ������ ������ �������� �����
  ZP_E_PORTIO             = HResult($80040206); // ������ ����� (��������� �������� �� USB?)
  ZP_E_PORTSETUP          = HResult($80040207); // ������ ��������� �����
  ZP_E_LOADFTD2XX         = HResult($80040208); // ��������� ��������� FTD2XX.DLL
  ZP_E_SOCKET             = HResult($80040209); // �� ������� ���������������� ������
  ZP_E_SERVERCLOSE        = HResult($8004020A); // ���������� ������ �� ������� �������
  ZP_E_NOTINITALIZED      = HResult($8004020B); // �� ������������������� � ������� ZP_Initialize
  ZP_E_INSUFFICIENTBUFFER = HResult($8004020C); // ������ ������ ������� ���
  ZP_E_NOCONNECT          = HResult($8004020D);

Const
  ZP_MAX_PORT_NAME        = 31;
  ZP_MAX_REG_DEV          = 32;

Const // ����� ��� ZP_Initialize
  ZP_IF_NO_MSG_LOOP       = $0001;  // ���������� �� ����� ����� ��������� ���������
                                // (���������� ���������� (console) ��� ������ Windows (service))
  ZP_IF_LOG               = $0002;  // ������ ���

Const  // ��������� �� ��������� (����-���� � ������� � �������������)
  ZP_IP_CONNECTTIMEOUT    = 4000;  // ����-��� ����������� �� TCP ��� ����� ���� ZP_PORT_IP
  ZP_IP_RESTOREPERIOD     = 3000;  // ������ �������������� ��������� TCP-����� (��� ����� ���� ZP_PORT_IP)
  ZP_IPS_CONNECTTIMEOUT   = 10000; // ����-��� ����������� �� TCP ��� ����� ���� ZP_PORT_IPS
  ZP_USB_RESTOREPERIOD    = 3000;  // ������ �������������� ��������� ����� (��� ������ ����� ZP_PORT_COM � ZP_PORT_FT)
  ZP_DTC_FINDUSBPERIOD    = 5000;  // ������ ������ USB-��������� (������ ����� com-������) (��� ��������� ���������)
  ZP_DTC_FINDIPPERIOD     = 15000; // ������ ������ IP-���������� �� UDP (��� ��������� ���������)
  ZP_DTC_SCANDEVPERIOD    = INFINITE; // ������ ������������ ���������, ������� ������ (��� ��������� ���������)
  ZP_SCAN_RCVTIMEOUT0     = 500;   // ����-��� �������� ������� ����� ������ �� ������ ��� ������������ ���������
  ZP_SCAN_RCVTIMEOUT      = 3000;  // ����-��� �������� ������ �� ������ ��� ������������ ���������
  ZP_SCAN_MAXTRIES        = 2;     // �������� ������� ������� ��� ������������ ���������
  ZP_SCAN_CHECKPERIOD     = INFINITE; // ������ �������� �������� ������ ����� ��� ������������ ������
  ZP_FINDIP_RCVTIMEOUT    = 1000;  // ����-��� ������ ip-��������� �� UDP
  ZP_FINDIP_MAXTRIES      = 1;     // �������� ������� ������ ip-��������� �� UDP

{$ALIGN 1}
{$MINENUMSIZE 4}
type
  TZP_PORT_TYPE = (
    ZP_PORT_UNDEF = 0,
    ZP_PORT_COM,	      // Com-����
    ZP_PORT_FT,		      // FT-���� (����� ftd2xx.dll �� �/� USB, ������ ��� ���������, ������������ ���� �������)
    ZP_PORT_IP,		      // Ip-���� (TCP-������)
    ZP_PORT_IPS         // Ip-���� (TCP-������)
  );

  // ��� �����
  TZP_PORT_NAME = array[0..ZP_MAX_PORT_NAME] of WideChar;

type
  // ��������� �������� ���������� �������
  TZP_WAIT_SETTINGS = record
    nReplyTimeout   : Cardinal;   // ����-��� �������� ������ �� ������ ����������
    nMaxTries         : Integer;    // ���������� ������� ��������� ������
    hAbortEvent     : THandle;    // ���������� ������������ ������� Event ��� ������ �������
    nReplyTimeout0  : Cardinal;   // ����-��� �������� ������� ������� ������
    nCheckPeriod    : Cardinal;   // ������ �������� ����� � �� (���� =0 ��� =INFINITE, �� �� RX-�������)
    nConnectTimeout : Cardinal;   // ����-��� ����������� �� TCP
    nRestorePeriod  : Cardinal;   // ������ � ������� ����� �������������� ������� ������������ ��������� TCP-�����
  end;
  PZP_WAIT_SETTINGS = ^TZP_WAIT_SETTINGS;

Const // ����� ��� ZP_OPEN_PARAMS.nFlags
  ZP_POF_NO_WAIT_CONNECT = $0001;     // �� ����� ���������� ��������� �����������
  ZP_POF_NO_CONNECT_ERR  = $0002;     // �� ���������� ������ � ������ ����� ��� �����
  ZP_POF_NO_DETECT_USB   = $0004;     // �� ������������ �������� USB-��������� (��� ZP_PORT_FT � ZP_PORT_COM)
type
  // ��������� �������� ����� (��� ������� ZP_Open)
  TZP_PORT_OPEN_PARAMS = record
    szName          : PWideChar;      // ��� �����
    nType           : TZP_PORT_TYPE;  // ��� �����
    nBaud           : DWord;          // �������� �����
    nEvChar         : AnsiChar;       // ���������� ������
    nStopBits       : Byte;           // ���������� �������� �����
    nConnectTimeout : Cardinal;       // ����-��� ����������� �� TCP
    nRestorePeriod  : Cardinal;       // ������ � ������� ����� �������������� ������� ������������ ��������� TCP-�����
    nFlags          : Cardinal;       // ����� ZP_PF_...
  end;
  PZP_PORT_OPEN_PARAMS = ^TZP_PORT_OPEN_PARAMS;

Const // ����� ��� ������� ZP_Port_SetNotification
  ZP_PNF_RXEVENT    = $0001;  // ������ ����� ������ �� ����������
  ZP_PNF_STATUS     = $0002;  // ���������� ��������� ����������� �����

Const // ����� ����� (��� nFlags � ��������� ZP_PORT_INFO)
  ZP_PIF_BUSY       = $0001;  // ���� �����
  ZP_PIF_USER       = $0002;  // ����, ��������� ������������� (������ _ZP_PORT_ADDR)
type
  // ���������� � �����
  TZP_PORT_INFO = record
    nType           : TZP_PORT_TYPE;	          // ��� �����
    szName          : TZP_PORT_NAME;	          // ��� �����
    nFlags          : Cardinal;		              // ����� ����� (ZP_PF_...)
    szFriendly      : TZP_PORT_NAME;	          // ������������� ��� �����
    nDevTypes       : Cardinal;                 // ����� ����� ���������
    szOwner         : array[0..63] of WideChar; // �������� ����� (��� ������� ZP_EnumIpDevices)
  end;
  PZP_PORT_INFO = ^TZP_PORT_INFO;

  TZP_DEVICE_INFO = record
    cbSize          : Cardinal;           // ������ ���������
    nTypeId         : Cardinal;
    nModel          : Cardinal;
    nSn        	    : Cardinal;
    nVersion        : Cardinal;
  end;
  PZP_DEVICE_INFO = ^TZP_DEVICE_INFO;

  // ��������� �����������
  TZP_CONNECTION_STATUS = (
    ZP_CS_DISCONNECTED = 0, // ��������
    ZP_CS_CONNECTED,        // ���������
    ZP_CS_CONNECTING,       // ���� �����������... (��� ������ �����������)
    ZP_CS_RESTORATION       // �������������� �����... (��� ��������� ������������)
  );

Const // ����� ��� ZP_N_CHANGE_INFO.nChangeMask � ZP_N_CHANGE_DEVINFO.nChangeMask
  ZP_CIF_BUSY       = $0004;  // ���������� ��������� "���� �����"
  ZP_CIF_FRIENDLY   = $0008;  // ���������� ������������� ��� �����
  ZP_CIF_OWNER      = $0020;  // ��������� �������� ����� (������ ��� IP ���������)
  ZP_CIF_MODEL      = $0080;  // ���������� ������ ����������
  ZP_CIF_SN         = $0100;  // ��������� �������� ����� ����������
  ZP_CIF_VERSION    = $0200;  // ���������� ������ �������� ����������
  ZP_CIF_DEVPARAMS  = $0400;  // ���������� ����������� ��������� ����������
  ZP_CIF_LIST       = $0800;  // ��������� ������ ������ (��� _ZP_DDN_DEVICE_INFO) ��� ��������� (��� _ZP_DDN_PORT_INFO)
Type
  // ���������� � �����
  TZP_DDN_PORT_INFO = record
    rPort           : TZP_PORT_INFO;
    aDevs           : ^PZP_DEVICE_INFO;
    nDevCount       : Integer;
    nChangeMask     : Cardinal;	// ����� ���������
  end;
  PZP_DDN_PORT_INFO = ^TZP_DDN_PORT_INFO;

  // ���������� � ����������
  TZP_DDN_DEVICE_INFO = record
    pInfo           : PZP_DEVICE_INFO;
    aPorts          : PZP_PORT_INFO;
    nPortCount      : Integer;
    nChangeMask     : Cardinal;
  end;
  PZP_DDN_DEVICE_INFO = ^TZP_DDN_DEVICE_INFO;

  TZP_DEVICEPARSEPROC = function(AReply: Pointer; ACount: Cardinal;
      var VPartially: LongBool; VInfo: PZP_DEVICE_INFO;
      VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer): LongBool; stdcall;

Const // ����� ��� TZP_NOTIFY_SETTINGS.nNMask
  ZP_NF_EXIST             = $0001;  // ����������� � �����������/���������� ����� (ZP_N_INSERT / ZP_N_REMOVE)
  ZP_NF_CHANGE            = $0002;  // ����������� � ��������� ���������� ����� (ZP_N_CHANGE)
  ZP_NF_ERROR             = $0008;  // ����������� �� ������ � ����(thread), ����������� ����� (ZP_N_ERROR)
  ZP_NF_SDEVICE           = $0010;  // ���������� � �����������, ������������ � ���������������� ������
  ZP_NF_IPDEVICE          = $0020;  // ���������� � �����������, ������������ � IP-������
  ZP_NF_IPSDEVICE         = $0080;  // ���������� ���������� ����� IPS-�����
  ZP_NF_COMPLETED         = $0040;  // ����������� � ���������� ������������
  ZP_NF_DEVEXIST          = $0004;  // ����������� � �����������/���������� ���������� (ZP_N_DEVINSERT / ZP_N_DEVREMOVE)
  ZP_NF_DEVCHANGE         = $0100;  // ����������� � ��������� ���������� ���������� (ZP_N_DEVCHANGE)
  ZP_NF_UNIDCOM           = $1000;  // ������ ������������ com-�����
  ZP_NF_USECOM            = $2000;  // �� ����������� ������������ Com-����

Const // ����������� ������� ZP_FindUsbNotification
  ZP_N_INSERT             = 1;	// ����������� ����� (PZP_N_EXIST_INFO(MsgParam) - ���� � �����)
  ZP_N_REMOVE             = 2;	// ���������� ����� (PZP_N_EXIST_INFO(MsgParam) - ���� � �����)
  ZP_N_CHANGE             = 3;	// ��������� ��������� ����� (PZP_N_CHANGE_INFO(MsgParam) - ���� �� ����������)
  ZP_N_ERROR              = 4;  // ��������� ������ � ���� (PHRESULT(MsgParam) - ��� ������)
  ZP_N_COMPLETED          = 5;  // ������������ ��������� (PINT(MsgParam) - �����: b0-������ com-������, b1-������ ip-������, b2-���������� �� �����������, <0-������)
  ZP_N_DEVINSERT          = 6;  // ����������� ���������� (PZP_N_EXIST_DEVINFO(MsgParam) - ���� � ����������)
  ZP_N_DEVREMOVE          = 7;  // ���������� ���������� (PZP_N_EXIST_DEVINFO(MsgParam) - ���� � ����������)
  ZP_N_DEVCHANGE          = 8;  // ��������� ���������� ���������� (PZP_N_CHANGE_DEVINFO(MsgParam) - ���� �� ����������)

Type
  TZP_DD_NOTIFY_SETTINGS = record
    nNMask          : Cardinal;	      // ����� ����� ����������� ZP_NF_

    hEvent          : THandle;        // ������� (������ �������������)
    hWindow         : HWnd;           // �������� ��� Callback-�������
    nWndMsgId       : Cardinal;       // ��������� ��� �������� ���� hWnd

    nSDevTypes      : Cardinal;	      // ����� ����� ���������, ������������ � ����������������� �����
    nIpDevTypes     : Cardinal;	      // ����� ����� Ip-���������

    aIps            : PCardinal;      // ������ TCP-������ ��� ����������� ����������� � ������ "CLIENT" (���� NULL, �� �� ������������)
    nIpsCount       : Integer;        // ���������� TCP-������
  end;
  PZP_DD_NOTIFY_SETTINGS = ^TZP_DD_NOTIFY_SETTINGS;

  TZP_DD_GLOBAL_SETTINGS = record
    nCheckUsbPeriod : Cardinal;	      // ������ �������� ��������� USB-������ (� �������������) (=0 �� ��������� 5000)
    nCheckIpPeriod  : Cardinal;	      // ������ �������� ��������� IP-������ (� �������������) (=0 �� ��������� 15000)
    nScanDevPeriod  : Cardinal;       // ������ ������������ ��������� �� USB- � IP-������ (� �������������) (=0 �� ��������� �������=INFINITE)

    nIpReqTimeout   : Cardinal;       // ����-��� �������� ������ �� ip-���������� ��� ������ �� UDP
    nIpReqMaxTries  : Integer;        // ���������� ������� ������ ip-���������� �� UDP
    rScanWS         : TZP_WAIT_SETTINGS; // ��������� �������� ��� ������������ ������
  end;
  PZP_DD_GLOBAL_SETTINGS = ^TZP_DD_GLOBAL_SETTINGS;

  TZP_DEVICE = record
    nTypeId         : Cardinal;             // ��� ����������
    pReqData        : Pointer;              // ������ ������� (����� ���� NULL)
    nReqSize        : Cardinal;             // ���������� ���� � �������
    pfnParse        : TZP_DEVICEPARSEPROC;  // ������� ������� ������
    nDevInfoSize    : Cardinal;             // ������ ��������� ZP_DEVICE_INFO
  end;
  PZP_DEVICE = ^TZP_DEVICE;

  TZP_IP_DEVICE = record
    rBase           : TZP_DEVICE;
    nReqPort        : Word;                 // UDP-���� ��� �������
    nMaxPort        : Integer;              // �������� ������ � ���������� (����� � ����������)
  end;
  PZP_IP_DEVICE = ^TZP_IP_DEVICE;

  TZP_USB_DEVICE = record
    rBase           : TZP_DEVICE;
    pVidPids        : PDWord;               // Vid,Pid USB-���������
    nVidPidCount    : Integer;              // ���������� Vid,Pid
    nBaud           : Cardinal;             // �������� �����
    chEvent         : AnsiChar;             // ������-������� ����� �������� (���� =0, ��� �������)
    nStopBits       : Byte;                 // �������� ���� (ONESTOPBIT=0, ONE5STOPBITS=1, TWOSTOPBITS=2)
    pszBDesc        : PWideChar;            // �������� ����������, ��������������� ����� (DEVPKEY_Device_BusReportedDeviceDesc)
  end;
  PZP_USB_DEVICE = ^TZP_USB_DEVICE;

  TZP_PORT_ADDR = record
    nType           : TZP_PORT_TYPE;
    pName           : PWideChar;
    nDevTypes       : Cardinal;
  end;
  PZP_PORT_ADDR = ^TZP_PORT_ADDR;

Const // ����� ��� ��������� ZP_SEARCH_PARAMS.nFlags
  ZP_SF_USECOM      = $0001;  // ������������ COM-���� �� �����������
  ZP_SF_DETECTOR    = $0002;  // ������������ ��� ������� ������ ��������� ��������� ��������� (��������� �������� ZP_FindNotification)
  ZP_SF_IPS         = $0004;  // �������� � ������ ��������� IP-���������� � ������ CLIENT
  ZP_SF_UNID        = $0008;  // �������� � ������ ������������ ����������
  ZP_SF_UNIDCOM     = $0010;  // ���������� ������������ com-�����
Type
  // ��������� ������ ��������� (��� ������� ZP_SearchDevices)
  TZP_SEARCH_PARAMS = record
    nDevMask        : Cardinal;             // ����� ��������� ��� ������������ ������ (=0 �� ������, =0xffffffff ������ ��)
    nIpDevMask      : Cardinal;             // ����� IP ���������, ����������� � ������� UDP-������� (=0 �� ������, =0xffffffff ������ ��)
    pPorts          : PZP_PORT_ADDR;        // ������ ������
    nPCount         : Integer;              // ������ ������ ������
    nFlags          : Cardinal;             // ����� ZP_SF_...
    pWait           : PZP_WAIT_SETTINGS;    // ��������� �������� ��� ������������ ������. ����� ���� =NULL.
    nIpReqTimeout   : Cardinal;             // ����-��� �������� ������ �� ip-���������� ��� ������ �� UDP
    nIpReqMaxTries    : Integer;              // ���������� ������� ������ ip-���������� �� UDP
  end;
  PZP_SEARCH_PARAMS =^ TZP_SEARCH_PARAMS;


// ���������� ��������� � ����
Const
  ZP_SUCCESS              = S_OK deprecated;
  ZP_E_CANCELLED          = ZP_S_CANCELLED deprecated;
  ZP_E_NOT_FOUND          = ZP_S_NOTFOUND deprecated;
  ZP_E_INVALID_PARAM      = E_INVALIDARG deprecated;
  ZP_E_OPEN_NOT_EXIST     = ZP_E_OPENNOTEXIST deprecated;
  ZP_E_OPEN_ACCESS        = E_ACCESSDENIED deprecated;
  ZP_E_OPENACCESS         = E_ACCESSDENIED deprecated;
  ZP_E_OPEN_PORT          = ZP_E_OPENPORT deprecated;
  ZP_E_PORT_IO_ERROR      = ZP_E_PORTIO deprecated;
  ZP_E_PORT_SETUP         = ZP_E_PORTSETUP deprecated;
  ZP_E_LOAD_FTD2XX        = ZP_E_LOADFTD2XX deprecated;
  ZP_E_INIT_SOCKET        = ZP_E_SOCKET deprecated;
  ZP_E_NOT_ENOUGH_MEMORY  = E_OUTOFMEMORY deprecated;
  ZP_E_UNSUPPORT          = E_NOINTERFACE deprecated;
  ZP_E_NOT_INITALIZED     = ZP_E_NOTINITALIZED deprecated;
  ZP_E_CREATE_EVENT       = E_FAIL deprecated;
  ZP_E_OTHER              = E_FAIL deprecated;
  ZP_NF_BUSY              = ZP_NF_CHANGE deprecated;
  ZP_NF_FRIENDLY          = ZP_NF_CHANGE deprecated;
  ZP_N_STATE_CHANGED      = ZP_N_CHANGE deprecated;
  ZP_PF_BUSY              = ZP_PIF_BUSY deprecated;
  ZP_PF_USER              = ZP_PIF_USER deprecated;
  ZP_PF_BUSY2             = 4 deprecated;
  ZP_NF_USEVCOM           = ZP_NF_USECOM deprecated;
  ZP_NF_WNDSYNC           = $4000 deprecated;
  ZP_NF_ONLYNOTIFY        = $8000 deprecated;
  ZP_NF_WND_SYNC          = $4000 deprecated;
  ZP_NF_ONLY_NOTIFY       = $8000 deprecated;
  ZP_IF_ERROR_LOG         = ZP_IF_LOG deprecated;
  ZP_PF_NOWAITCONNECT     = ZP_POF_NO_WAIT_CONNECT deprecated;
  ZP_PF_NOCONNECTERR      = ZP_POF_NO_CONNECT_ERR deprecated;
  ZP_PF_NOUSBDETECT       = ZP_POF_NO_DETECT_USB deprecated;

Const // ����� ��� ������� ZP_EnumSerialDevices, ZP_FindSerialDevice � ZP_EnumIpDevices
  ZP_SF_UPDATE  = 1 deprecated;  // �������� ������ ������
  ZP_SF_USEVCOM = 2 deprecated;  // �� ����������� ������������ Com-����

Type
  TZP_STATUS = Integer {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_N_CHANGE_STATE = TZP_DDN_PORT_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_N_CHANGE_STATE = PZP_DDN_PORT_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_N_EXIST_INFO = TZP_DDN_PORT_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_N_EXIST_INFO = PZP_DDN_PORT_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_N_CHANGE_INFO = TZP_DDN_PORT_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_N_CHANGE_INFO = PZP_DDN_PORT_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_N_EXIST_DEVINFO = TZP_DDN_DEVICE_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_N_EXIST_DEVINFO = PZP_DDN_DEVICE_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_N_CHANGE_DEVINFO = TZP_DDN_DEVICE_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_N_CHANGE_DEVINFO = PZP_DDN_DEVICE_INFO {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_ENUMPORTSPROC = function(Const AInfo: TZP_PORT_INFO; AUserData: Pointer): LongBool; stdcall;
  TZP_ENUMDEVICEPROC = function(AInfo: PZP_DEVICE_INFO;
      Const APort: TZP_PORT_INFO; AUserData: Pointer): LongBool; stdcall;
  TZP_NOTIFYPROC = function(AMsg: Cardinal; AMsgParam: NativeInt; AParam: Pointer): LongBool; stdcall;
  TZP_NOTIFY_SETTINGS = TZP_DD_NOTIFY_SETTINGS {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_NOTIFY_SETTINGS = PZP_DD_NOTIFY_SETTINGS {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_DETECTOR_SETTINGS = TZP_DD_GLOBAL_SETTINGS {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_DETECTOR_SETTINGS = PZP_DD_GLOBAL_SETTINGS {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_S_DEVICE = TZP_USB_DEVICE {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_S_DEVICE = PZP_USB_DEVICE {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_OPEN_PARAMS = TZP_PORT_OPEN_PARAMS {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  PZP_OPEN_PARAMS = PZP_PORT_OPEN_PARAMS {$IF CompilerVersion >= 17.0}deprecated{$IFEND};
  TZP_PORT_STATUS = TZP_CONNECTION_STATUS {$IF CompilerVersion >= 17.0}deprecated{$IFEND};

{$MINENUMSIZE 1}
{$ALIGN ON}

{$IFNDEF ZP_LINKONREQUEST}

// ���������� ������ ����������
function ZP_GetVersion(): Cardinal; stdcall;

// �������������/����������� ����������
function ZP_Initialize(VObject: PPVoid; AFlags: Cardinal): HResult; stdcall;
function ZP_Finalyze(): HResult; stdcall;

function ZP_CloseHandle(AHandle: THandle): HResult; stdcall;
// ������� ������ ������������ ������
function ZP_GetPortInfoList(var VHandle: THandle; var VCount: Integer;
    ASerDevs: Cardinal=$ffffffff; AFlags: Cardinal=0): HResult; stdcall;
function ZP_GetPortInfo(AHandle: THandle; AIdx: Integer; var VInfo: TZP_PORT_INFO): HResult; stdcall;
// ���� ����������, �������� ����� �(���) ��������� ip-���������� �� UDP
function ZP_SearchDevices(var VHandle: THandle; Const AParams: TZP_SEARCH_PARAMS): HResult; stdcall;
function ZP_FindNextDevice(AHandle: THandle; VInfo: PZP_DEVICE_INFO;
    VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer;
    ATimeout: Cardinal=INFINITE): HResult; stdcall;

// ����������� � �������� ����� (�����������/����������, ������/�����������)
function ZP_DD_SetNotification(var VHandle: THandle; Const ASettings: TZP_DD_NOTIFY_SETTINGS): HResult; stdcall;
function ZP_DD_GetNextMessage(AHandle: THandle; var VMsg: Cardinal; var VMsgParam: NativeInt): HResult; stdcall;
function ZP_DD_SetGlobalSettings(ASettings: PZP_DD_GLOBAL_SETTINGS): HResult; stdcall;
function ZP_DD_GetGlobalSettings(var VSettings: TZP_DD_GLOBAL_SETTINGS): HResult; stdcall;
function ZP_DD_Refresh(AWaitMs: Cardinal=0): HResult; stdcall;

function ZP_SetServiceCtrlHandle(ASvc: THandle): HResult; stdcall;
procedure ZP_DeviceEventNotify(AEvType: Cardinal; AEvData: Pointer); stdcall;

// ������� � ������
function ZP_Port_Open(var VHandle: THandle; AParams: PZP_PORT_OPEN_PARAMS): HResult; stdcall;
function ZP_Port_SetBaudAndEvChar(AHandle: THandle; ABaud: Cardinal; AEvChar: AnsiChar): HResult; stdcall;
function ZP_Port_GetBaudAndEvChar(AHandle: THandle; VBaud: PCardinal; VEvChar: PAnsiChar): HResult; stdcall;
function ZP_Port_GetConnectionStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult; stdcall;
function ZP_Port_SetNotification(AHandle: THandle; AEvent: THandle; AWindow: HWnd;
    AWndMsgId: Cardinal; AMsgMask: Cardinal): HResult; stdcall;
function ZP_Port_EnumMessages(AHandle: THandle; var VMsgs: Cardinal): HResult; stdcall;
function ZP_Port_Clear(AHandle: THandle; AIn, AOut: LongBool): HResult; stdcall;
function ZP_Port_Write(AHandle: THandle; Const ABuf; ACount: Cardinal): HResult; stdcall;
function ZP_Port_Read(AHandle: THandle; var VBuf; ACount: Cardinal; var VRead: Cardinal): HResult; stdcall;
function ZP_Port_GetInCount(AHandle: THandle; var VCount: Cardinal): HResult; stdcall;
function ZP_Port_SetDtr(AHandle: THandle; AState: LongBool): HResult; stdcall;
function ZP_Port_SetRts(AHandle: THandle; AState: LongBool): HResult; stdcall;

// ������ � ������������
function ZP_RegSerialDevice(Const AParams: TZP_USB_DEVICE): HResult; stdcall;
function ZP_RegIpDevice(Const AParams: TZP_IP_DEVICE): HResult; stdcall;

{$IFDEF ZP_LOG}
function ZP_SetLog(ASvrAddr, AFileName: PWideChar; AFileTypeMask: Cardinal): HResult; stdcall;
function ZP_GetLog(VSvrAddrBuf: PWideChar; ASABufSize: Cardinal;
    VFileNameBuf: PWideChar; AFNBufSize: Cardinal; var VFileTypeMask: Cardinal): HResult; stdcall;
function ZP_AddLog(ASrc: WideChar; AMsgType: Integer; AText: PWideChar): HResult; stdcall;
{$ENDIF !ZP_LOG}

// ���������� �������
function ZP_FindNotification(var VHandle: THandle; Const ASettings: TZP_DD_NOTIFY_SETTINGS): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_CloseNotification(AHandle: THandle): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_ProcessMessages(AHandle: THandle; AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_EnumMessages(AHandle: THandle; AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_Close(AHandle: THandle): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_EnumSerialPorts(ADevTypes: Cardinal; AEnumProc: TZP_ENUMPORTSPROC; AUserData: Pointer): HRESULT; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_EnumSerialDevices(ADevTypes: Cardinal;
    APorts: PZP_PORT_ADDR; APCount: Integer; AEnumProc: TZP_ENUMDEVICEPROC;
    AUserData: Pointer; AWait: PZP_WAIT_SETTINGS=nil; AFlags: Cardinal=1): HResult; deprecated;
function ZP_FindSerialDevice(ADevTypes: Cardinal; APorts: PZP_PORT_ADDR; APCount: Integer;
    VInfo: PZP_DEVICE_INFO; AInfoSize: Integer; var VPort: TZP_PORT_INFO;
    AWait: PZP_WAIT_SETTINGS=nil; AFlags: Cardinal=1): HResult; deprecated;
function ZP_EnumIpDevices(ADevTypes: Cardinal; AEnumProc: TZP_ENUMDEVICEPROC;
    AUserData: Pointer; AWait: PZP_WAIT_SETTINGS=nil; AFlags: Cardinal=1): HResult; deprecated;

function ZP_SetNotification(var VHandle: THandle; Const ASettings: TZP_DD_NOTIFY_SETTINGS): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_GetNextMessage(AHandle: THandle; var VMsg: Cardinal; var VMsgParam: NativeInt): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_SetDetectorSettings(ASettings: PZP_DD_GLOBAL_SETTINGS): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_GetDetectorSettings(var VSettings: TZP_DD_GLOBAL_SETTINGS): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_UpdateDetector(AWaitMs: Cardinal=0): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZP_Port_GetPortStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}

{$ELSE}
{$ENDIF !ZP_LINKONREQUEST}

Const
//  ZP_DLL_Name = 'ZPort.dll';
  ZP_DLL_Name = 'ZGuard.dll';
//  ZP_DLL_Name = 'ZReader.dll';

implementation

{$IFNDEF ZP_LINKONREQUEST}

function ZP_GetVersion(): Cardinal; stdcall;
        External ZP_DLL_Name name 'ZP_GetVersion';

function ZP_Initialize(VObject: PPVoid; AFlags: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Initialize';
function ZP_Finalyze(): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Finalyze';


function ZP_CloseHandle(AHandle: THandle): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_CloseHandle';
function ZP_GetPortInfoList(var VHandle: THandle; var VCount: Integer;
    ASerDevs: Cardinal; AFlags: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_GetPortInfoList';
function ZP_GetPortInfo(AHandle: THandle; AIdx: Integer; var VInfo: TZP_PORT_INFO): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_GetPortInfo';
function ZP_SearchDevices(var VHandle: THandle; Const AParams: TZP_SEARCH_PARAMS): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_SearchDevices';
function ZP_FindNextDevice(AHandle: THandle; VInfo: PZP_DEVICE_INFO;
    VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer;
    ATimeout: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_FindNextDevice';

function ZP_DD_SetNotification(var VHandle: THandle; Const ASettings: TZP_DD_NOTIFY_SETTINGS): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_DD_SetNotification';
function ZP_DD_GetNextMessage(AHandle: THandle; var VMsg: Cardinal; var VMsgParam: NativeInt): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_DD_GetNextMessage';
function ZP_DD_SetGlobalSettings(ASettings: PZP_DD_GLOBAL_SETTINGS): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_DD_SetGlobalSettings';
function ZP_DD_GetGlobalSettings(var VSettings: TZP_DD_GLOBAL_SETTINGS): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_DD_GetGlobalSettings';
function ZP_DD_Refresh(AWaitMs: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_DD_Refresh';
function ZP_SetServiceCtrlHandle(ASvc: THandle): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_SetServiceCtrlHandle';
procedure ZP_DeviceEventNotify(AEvType: Cardinal; AEvData: Pointer); stdcall;
        External ZP_DLL_Name name 'ZP_DeviceEventNotify';

function ZP_Port_Open(var VHandle: THandle; AParams: PZP_PORT_OPEN_PARAMS): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_Open';
function ZP_Port_SetBaudAndEvChar(AHandle: THandle; ABaud: Cardinal; AEvChar: AnsiChar): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_SetBaudAndEvChar';
function ZP_Port_GetBaudAndEvChar(AHandle: THandle; VBaud: PCardinal; VEvChar: PAnsiChar): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_GetBaudAndEvChar';
function ZP_Port_GetConnectionStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_GetConnectionStatus';
function ZP_Port_SetNotification(AHandle: THandle; AEvent: THandle; AWindow: HWnd;
    AWndMsgId: Cardinal; AMsgMask: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_SetNotification';
function ZP_Port_EnumMessages(AHandle: THandle; var VMsgs: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_EnumMessages';

function ZP_Port_Clear(AHandle: THandle; AIn, AOut: LongBool): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_Clear';
function ZP_Port_Write(AHandle: THandle; Const ABuf; ACount: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_Write';
function ZP_Port_Read(AHandle: THandle; var VBuf; ACount: Cardinal; var VRead: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_Read';
function ZP_Port_GetInCount(AHandle: THandle; var VCount: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_GetInCount';
function ZP_Port_SetDtr(AHandle: THandle; AState: LongBool): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_SetDtr';
function ZP_Port_SetRts(AHandle: THandle; AState: LongBool): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_Port_SetRts';

function ZP_RegSerialDevice(Const AParams: TZP_USB_DEVICE): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_RegSerialDevice';
function ZP_RegIpDevice(Const AParams: TZP_IP_DEVICE): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_RegIpDevice';
{$IFDEF ZP_LOG}
function ZP_SetLog(ASvrAddr, AFileName: PWideChar; AFileTypeMask: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_SetLog';
function ZP_GetLog(VSvrAddrBuf: PWideChar; ASABufSize: Cardinal;
    VFileNameBuf: PWideChar; AFNBufSize: Cardinal; var VFileTypeMask: Cardinal): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_GetLog';
function ZP_AddLog(ASrc: WideChar; AMsgType: Integer; AText: PWideChar): HResult; stdcall;
        External ZP_DLL_Name name 'ZP_AddLog';
{$ENDIF !ZP_LOG}

function ZP_FindNotification(var VHandle: THandle; Const ASettings: TZP_DD_NOTIFY_SETTINGS): HResult;
begin
  Result := ZP_SetNotification(VHandle, ASettings);
end;
function ZP_CloseNotification(AHandle: THandle): HResult;
begin
  Result := ZP_CloseHandle(AHandle);
end;
function ZP_ProcessMessages(AHandle: THandle; AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult;
begin
  Result := ZP_EnumMessages(AHandle, AEnumProc, AUserData);
end;

function ZP_EnumMessages(AHandle: THandle;
    AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult;
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
begin
  repeat
    Result := Zp_GetNextMessage(AHandle, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    AEnumProc(nMsg, nMsgParam, AUserData);
  until False;
  if Result = ZP_S_NOTFOUND then
    Result := S_OK;
end;
function ZP_Close(AHandle: THandle): HResult;
begin
  Result := ZP_CloseHandle(AHandle);
end;

function ZP_EnumSerialPorts(ADevTypes: Cardinal;
    AEnumProc: TZP_ENUMPORTSPROC; AUserData: Pointer): HRESULT;
var
  hPL: THandle;
  nCount, i: Integer;
  rPI: TZp_Port_Info;
begin
  Result := ZP_GetPortInfoList(hPL, nCount, ADevTypes);
  if Failed(Result) then
    Exit;
  try
    for i := 0 to nCount - 1 do
    begin
      ZP_GetPortInfo(hPL, i, rPI);
      if not AEnumProc(rPI, AUserData) then
        Exit(ZP_S_CANCELLED);
    end;
  finally
    ZP_CloseHandle(hPL);
  end;
end;
function ZP_EnumSerialDevices(ADevTypes: Cardinal;
    APorts: PZP_PORT_ADDR; APCount: Integer; AEnumProc: TZP_ENUMDEVICEPROC;
    AUserData: Pointer; AWait: PZP_WAIT_SETTINGS; AFlags: Cardinal): HResult;
var
  hSearch: THandle;
  rParams: TZp_Search_Params;
  pDI: PZp_Device_Info;
  rPI: TZp_Port_Info;
  nCount, nDISize: Integer;
begin
  FillChar(rParams, SizeOf(rParams), 0);
  rParams.nDevMask := ADevTypes;
  if (AFlags and ZP_SF_USEVCOM) <> 0 then
    rParams.nFlags := ZP_SF_USECOM;
  rParams.pPorts := APorts;
  rParams.nPCount := APCount;
  rParams.pWait := AWait;
  nDISize := (SizeOf(TZp_Device_Info) + 1024);
  pDI := AllocMem(nDISize);
  try
    Result := ZP_SearchDevices(hSearch, rParams);
    if Failed(Result) then
      Exit;
    try
      repeat
        pDI.cbSize := nDISize;
        Result := ZP_FindNextDevice(hSearch, pDI, @rPI, 1, nCount);
        if Result <> S_OK then
          break;
        if not AEnumProc(pDI, rPI, AUserData) then
          Exit(ZP_S_CANCELLED);
      until False;
    finally
      ZP_CloseHandle(hSearch);
    end;
  finally
    FreeMem(pDI);
  end;
end;
function ZP_FindSerialDevice(ADevTypes: Cardinal; APorts: PZP_PORT_ADDR; APCount: Integer;
    VInfo: PZP_DEVICE_INFO; AInfoSize: Integer; var VPort: TZP_PORT_INFO;
    AWait: PZP_WAIT_SETTINGS; AFlags: Cardinal): HResult;
var
  hSearch: THandle;
  rParams: TZp_Search_Params;
  pDI: PZp_Device_Info;
  rPI: TZp_Port_Info;
  nCount: Integer;
begin
  FillChar(rParams, SizeOf(rParams), 0);
  rParams.nDevMask := ADevTypes;
  if (AFlags and ZP_SF_USEVCOM) <> 0 then
    rParams.nFlags := ZP_SF_USECOM;
  rParams.pPorts := APorts;
  rParams.nPCount := APCount;
  rParams.pWait := AWait;
  pDI := AllocMem(AInfoSize);
  try
    Result := ZP_SearchDevices(hSearch, rParams);
    if Failed(Result) then
      Exit;
    try
      pDI.cbSize := AInfoSize;
      Result := ZP_FindNextDevice(hSearch, pDI, @rPI, 1, nCount);
      if Result <> S_OK then
        Exit;
      Move(pDI^, VInfo^, AInfoSize);
      VPort := rPI;
    finally
      Zp_CloseHandle(hSearch);
    end;
  finally
    FreeMem(pDI);
  end;
end;
function ZP_EnumIpDevices(ADevTypes: Cardinal; AEnumProc: TZP_ENUMDEVICEPROC;
    AUserData: Pointer; AWait: PZP_WAIT_SETTINGS; AFlags: Cardinal): HResult;
var
  hSearch: THandle;
  rParams: TZp_Search_Params;
  pDI: PZp_Device_Info;
  rPI: TZp_Port_Info;
  nCount, nDISize: Integer;
begin
  FillChar(rParams, SizeOf(rParams), 0);
  rParams.nIpDevMask := ADevTypes;
  rParams.pWait := AWait;
  nDISize := (SizeOf(TZp_Device_Info) + 1024);
  pDI := AllocMem(nDISize);
  try
    Result := ZP_SearchDevices(hSearch, rParams);
    if Failed(Result) then
      Exit;
    try
      repeat
        pDI.cbSize := nDISize;
        Result := ZP_FindNextDevice(hSearch, pDI, @rPI, 1, nCount);
        if Result <> S_OK then
          break;
        if not AEnumProc(pDI, rPI, AUserData) then
          Exit(ZP_S_CANCELLED);
      until False;
    finally
      ZP_CloseHandle(hSearch);
    end;
  finally
    FreeMem(pDI);
  end;
end;

function ZP_SetNotification(var VHandle: THandle; Const ASettings: TZP_DD_NOTIFY_SETTINGS): HResult;
begin
  Result := ZP_DD_SetNotification(VHandle, ASettings);
end;
function ZP_GetNextMessage(AHandle: THandle; var VMsg: Cardinal; var VMsgParam: NativeInt): HResult;
begin
  Result := ZP_DD_GetNextMessage(AHandle, VMsg, VMsgParam);
end;
function ZP_SetDetectorSettings(ASettings: PZP_DD_GLOBAL_SETTINGS): HResult;
begin
  Result := ZP_DD_SetGlobalSettings(ASettings);
end;
function ZP_GetDetectorSettings(var VSettings: TZP_DD_GLOBAL_SETTINGS): HResult;
begin
  Result := ZP_DD_GetGlobalSettings(VSettings);
end;
function ZP_UpdateDetector(AWaitMs: Cardinal): HResult;
begin
  Result := ZP_DD_Refresh(AWaitMs);
end;
function ZP_Port_GetPortStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult;
begin
  Result := ZP_Port_GetConnectionStatus(AHandle, VValue);
end;

{$ENDIF !ZP_LINKONREQUEST}

end.

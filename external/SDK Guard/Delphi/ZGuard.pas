unit ZGuard;

interface

{$IF CompilerVersion >= 17.0}
  {$DEFINE HAS_INLINE}
{$IFEND}

uses
  Windows, ZPort, ZBase;

Const
  // ����������� ������ SDK
  ZG_SDK_VER_MAJOR          = 3;
  ZG_SDK_VER_MINOR          = 33;

Const
  ZG_E_TOOLARGEMSG          = HResult($80040301); // ������� ������� ��������� ��� ��������
  ZG_E_NOANSWER             = HResult($80040303); // ��� ������
  ZG_E_BADANSWER            = HResult($80040304); // �������������� �����
  ZG_E_WRONGZPORT           = HResult($80040305); // �� ���������� ������ ZPort.dll
  ZG_E_CVTBUSY              = HResult($80040306); // ��������� ����� (��� �������� ���������� � ������ "Proxy")
  ZG_E_CVTERROR             = HResult($80040307); // ������ ������ ����������
  ZG_E_LICNOTFOUND          = HResult($80040308); // ������ ����������: ��� ����� ��������
  ZG_E_LICEXPIRED           = HResult($80040309); // ������� �������� �������
  ZG_E_LICONTROLLERS        = HResult($8004030A); // ������ ����������: ����������� �������� �� ���������� ������������
  ZG_E_LICREADKEYS          = HResult($8004030B); // ����������� �������� �� ����� ������ ��� ������
  ZG_E_LICWRITEKEYS         = HResult($8004030C); // ����������� �������� �� ����� ������ ��� ������
  ZG_E_LICEXPIRED2          = HResult($8004030D); // ���� �������� ����� (���������� ��� ��������� ���� � �����������)
  ZG_E_NOCONVERTER          = HResult($8004030E); // ��������� �� ������ (�������� �����) (��� ����������� ����� Proxy)
  ZG_E_NOCONTROLLER         = HResult($8004030F); // ���������� �� ������ (�������� �����)
  ZG_E_CTRNACK              = HResult($80040310); // ���������� ������� � ���������� �������
  ZG_E_FWBOOTLOADERNOSTART  = HResult($80040311); // ��������� �� ���������� (��� ��������)
  ZG_E_FWFILESIZE           = HResult($80040312); // ������������ ������ ����� (��� ��������)
  ZG_E_FWNOSTART            = HResult($80040313); // �� ��������� ����� ��������. ���������� ������������� ����������. (��� ��������)
  ZG_E_FWNOCOMPATIBLE       = HResult($80040314); // �� �������� ��� ����� ���������� (��� ��������)
  ZG_E_FWINVALIDDEVNUM      = HResult($80040315); // �� �������� ��� ����� ������ ���������� (��� ��������)
  ZG_E_FWTOOLARGE           = HResult($80040316); // ������� ������� ������ ������ �������� (��� ��������)
  ZG_E_FWSEQUENCEDATA       = HResult($80040317); // ������������ ������������������ ������ (��� ��������)
  ZG_E_FWDATAINTEGRITY      = HResult($80040318); // ����������� ������ �������� (��� ��������)

Const
  ZG_IF_ERROR_LOG         = $100; // ���������� ���
Const
  ZG_DEVTYPE_GUARD        = 1;
  ZG_DEVTYPE_Z397         = 2;
  ZG_DEVTYPE_COM          = 5;
  ZG_DEVTYPE_IPGUARD      = ZP_MAX_REG_DEV;
  ZG_DEVTYPE_CVTS         = $26;
  ZG_IPDEVTYPE_CVTS       = 1;

  ZG_DEF_CVT_LICN         = 5;  // ����� �������� ���������� �� ���������
  ZG_MAX_TIMEZONES        = 7;  // �������� ��������� ���
  ZG_MAX_LICENSES         = 16; // ������������ ���������� ��������, ������� ����� ���������� � ���������
  ZG_MODES_TZ0            = -2; // ����� ������ ��.���� ��� ������������ ������ �����������, ����� 2 ����
  ZG_DUAL_ZONE_TZ0        = -9; // ����� ������ ��.���� �� ������ ������, ����� 7 ���

// �������� �� ���������
Const
  ZG_CVT_SCANCTRSPERIOD   = 5000; // ������ ������������ ������������ (� �������������)
  ZG_CVT_SCANCTRSLASTADDR = 31;   // ��������� ����������� ����� ����������� (��� ��������������� ������ ������������)

{$ALIGN 1}
{$MINENUMSIZE 4}
type
  // ������ ����������
  TZG_CVT_TYPE = (
    ZG_CVT_UNDEF = 0,   // �� ����������
	  ZG_CVT_Z397,			  // Z-397
	  ZG_CVT_Z397_GUARD,	// Z-397 Guard
	  ZG_CVT_Z397_IP,	    // Z-397 IP
    ZG_CVT_Z397_WEB,    // Z-397 Web
    ZG_CVT_Z5R_WEB      // Z5R Web
  );

  // ����� ���������� Guard
  TZG_GUARD_MODE = (
	  ZG_GUARD_UNDEF = 0,   // �� ����������
	  ZG_GUARD_NORMAL,      // ����� "Normal" (�������� �������� ���������� Z397)
	  ZG_GUARD_ADVANCED,    // ����� "Advanced"
	  ZG_GUARD_TEST,        // ����� "Test" (��� ������������)
	  ZG_GUARD_ACCEPT       // ����� "Accept" (��� ������������)
  );
  PZG_GUARD_MODE = ^TZG_GUARD_MODE;

  // �������� ����������
  TZG_CVT_SPEED = (
	  ZG_SPEED_19200	= 19200,
	  ZG_SPEED_57600	= 57600
  );
  PZG_CVT_SPEED = ^TZG_CVT_SPEED;

  // ���������� � ����������, ������������ �������� ZG_EnumConverters
  TZG_ENUM_CVT_INFO = record
    rBase           : TZP_DEVICE_INFO;
    nType           : TZG_CVT_TYPE;   // ��� ����������
    nMode           : TZG_GUARD_MODE; // ����� ������ ���������� Guard
  end;
  PZG_ENUM_CVT_INFO = ^TZG_ENUM_CVT_INFO;

  // ���������� � ip-����������, ���������� ������� �� Udp
  TZG_ENUM_IPCVT_INFO = record
    rBase           : TZP_DEVICE_INFO;
    nType           : TZG_CVT_TYPE;   // ��� ����������
    nMode           : TZG_GUARD_MODE; // ����� ������ ���������� Guard
    nFlags          : Cardinal;       // �����: ��� 0 - "VCP", ��� 1 - "WEB", 0xFF - "All"
  end;
  PZG_ENUM_IPCVT_INFO = ^TZG_ENUM_IPCVT_INFO;

  TZG_CVT_INFO = record
    rBase           : TZP_DEVICE_INFO;
    nType           : TZG_CVT_TYPE;   // ��� ����������
    nSpeed          : TZG_CVT_SPEED;  // �������� ����������

    nMode           : TZG_GUARD_MODE; // ����� ������ ���������� Guard

    pszLinesBuf     : PWideChar;      // ����� ��� �������������� �����
    nLinesBufMax    : Integer;        // ������ ������ � ��������, ������� ����������� #0
  end;
  PZG_CVT_INFO = ^TZG_CVT_INFO;

Const // ����� ��� ZG_CVT_OPEN_PARAMS.nFlags
  ZG_OF_NOCHECKLIC    = $0001;            // �� ���������/��������� ��������
  ZG_OF_NOSCANCTRS    = $0002;            // �� ����������� �����������
type
  TZG_CVT_OPEN_PARAMS = record
    nType           : TZP_PORT_TYPE;  // ��� �����
    pszName         : PWideChar;      // ��� �����. ���� =NULL, �� ������������ hPort
    hPort           : THandle;        // ���������� �����, ���������� �������� ZP_Open
    nCvtType        : TZG_CVT_TYPE;   // ��� ����������. ���� =ZG_CVT_UNDEF, �� ���������������
    nSpeed          : TZG_CVT_SPEED;  // �������� ����������
    pWait           : PZP_WAIT_SETTINGS;// ��������� ��������. ����� ���� =nil.
    nStopBits       : Byte;
    nLicN           : Integer;        // ����� ��������. ���� =0, �� ������������ ZG_DEF_CVT_LICN
    pActCode        : PAnsiChar;      // ��� ��������� ��� ������ "Proxy"
    nSn             : Integer;        // �/� ���������� ��� ������ "Proxy"
    nFlags          : Cardinal;         // ����� ����������� (ZG_CTR_F_...)
  end;
  PZG_CVT_OPEN_PARAMS = ^TZG_CVT_OPEN_PARAMS;

  // ���������� � �������� ���������� Guard
  TZG_CVT_LIC_INFO = record
    nStatus         : Word;         // ������ ��������
    Reserved        : Word;         // ��������������� ��� ������������ ���������
    nMaxCtrs        : Integer;      // ������������ ���������� ������������
    nMaxKeys        : Integer;      // ������������ ���������� ������
    nMaxYear        : Word;         // ����: ��� (= 0xFFFF ���� ������������)
    nMaxMon         : Word;         // ����: �����
    nMaxDay         : Word;         // ����: ����
    nDownCountTime  : Word;         // ���������� ����� ����� �������� � �������
  end;
  PZG_CVT_LIC_INFO = ^TZG_CVT_LIC_INFO;

  // ������� ���������� � �������� ���������� Guard
  TZG_CVT_LIC_SINFO = record
    nLicN           : Integer;      // ����� ��������
    nMaxCtrs        : Integer;      // ������������ ���������� ������������
    nMaxKeys        : Integer;      // ������������ ���������� ������
  end;
  PZG_CVT_LIC_SINFO = ^TZG_CVT_LIC_SINFO;

  // ��� �����������
  TZG_CTR_TYPE = (
    ZG_CTR_UNDEF = 0,   // �� ����������
    ZG_CTR_GATE2K,      // Gate 2000
    ZG_CTR_MATRIX2NET,  // Matrix II Net
    ZG_CTR_Z5RNET,      // Z5R Net
    ZG_CTR_Z5RNET8K,    // Z5R Net 8000
    ZG_CTR_GUARDNET,    // Guard Net
    ZG_CTR_Z9,          // Z-9 EHT Net
    ZG_CTR_EUROLOCK,    // EuroLock EHT net
    ZG_CTR_Z5RWEB       // Z5R Web
  );
  // ������ �����������
  TZG_CTR_SUB_TYPE = (
    ZG_CS_UNDEF	= 0,    // �����������
    ZG_CS_DOOR,         // �����
    ZG_CS_TURNSTILE,    // ��������
    ZG_CS_GATEWAY,      // ����
    ZG_CS_BARRIER       // ��������
  );

Const // ����� �����������
  ZG_CTR_F_2BANKS       = $0001;  // 2 ����� / 1 ����
  ZG_CTR_F_PROXIMITY    = $0002;  // Proximity (Wiegand) / TouchMemory (Dallas)
  ZG_CTR_F_JOIN         = $0004;  // ����������� ���� ������
  ZG_CTR_F_X2           = $0008;  // �������� ������
  ZG_CTR_F_ELECTRO      = $0010;  // ������� ElectroControl (��� Matrix II Net)
  ZG_CTR_F_MODES        = $0020;  // ��������� ������� �������
  ZG_CTR_F_DUAL_ZONE    = $0040;  // ��������� ���� ������� ��������� ���

Type
// ���������� � ��������� �����������, ������������ �������� ZG_Cvt_FindNextCtr
  TZG_FIND_CTR_INFO = record
    nType           : TZG_CTR_TYPE;     // ��� �����������
    nTypeCode       : Byte;             // ��� ���� �����������
    nAddr           : Byte;             // ������� �����
    nSn             : Word;             // ��������� �����
    nVersion        : Word;             // ������ ��������
    nMaxKeys        : Integer;          // �������� ������
    nMaxEvents      : Integer;          // �������� �������
    nFlags          : Cardinal;         // ����� ����������� (ZG_CTR_F_...)
    nSubType        : TZG_CTR_SUB_TYPE; // ������ �����������
  end;
  PZG_FIND_CTR_INFO = ^TZG_FIND_CTR_INFO;

  // ���������� � �����������
  TZG_CTR_INFO = record
    nType           : TZG_CTR_TYPE;     // ��� �����������
    nTypeCode       : Byte;             // ��� ���� �����������
    nAddr           : Byte;             // ������� �����
    nSn             : Word;             // ��������� �����
    nVersion        : Word;             // ������ ��������
    nInfoLineCount  : Integer;          // ���������� ����� � �����������
    nMaxKeys        : Integer;          // �������� ������
    nMaxEvents      : Integer;          // �������� �������
    nFlags          : Cardinal;         // ����� ����������� (ZG_CTR_F_...)
    Reserved        : Word;
    pszLinesBuf     : PWideChar;        // ����� ��� �������������� �����
    nLinesBufMax    : Integer;          // ������ ������ � ��������, ������� ����������� #0
    nSubType        : TZG_CTR_SUB_TYPE; // ������ �����������
    nOptReadItems   : Integer;          // ���������� ���������, ������� ����� ���� ������� ����� �������� �����������
    nOptWriteItems  : Integer;          // ���������� ���������, ������� ����� ���� �������� ����� �������� �����������
  end;
  PZG_CTR_INFO = ^TZG_CTR_INFO;

  // ����� ������� �����������
  TZG_CTR_MODE = (
    ZG_MODE_UNDEF = 0,
    ZG_MODE_NORMAL,     // ������� ����� ������
    ZG_MODE_BLOCK,      // ���������� (��������� ����� ������ "�����������" �����)
    ZG_MODE_FREE,       // ��������� (����� ���������, ��� ���������� ����� ��������������)
    ZG_MODE_WAIT        // �������� (������� ����� ������, ��� ���������� ���������� ����� ������� � ����� "Free")
  );

  // ����� HOTEL
  TZG_HOTEL_MODE = (
    ZG_HMODE_UNDEF = 0,
    ZG_HMODE_NORMAL,    // �����
    ZG_HMODE_BLOCK,     // ����������
    ZG_HMODE_FREE,      // ��������� ������
    ZG_HMODE_RESERVED   // ������
  );

  // ��������� ���� �����������
  TZG_CTR_TIMEZONE = record
    nDayOfWeeks     : Byte;         // ��� ������
    nBegHour        : Byte;         // ������: ���
    nBegMinute      : Byte;         // ������: ������
    nEndHour        : Byte;         // �����: ���
    nEndMinute      : Byte;         // �����: ������
    Reserved        : array[0..2] of Byte;
    nMode           : TZG_Ctr_Mode; // ����� ����������� (������������ ������ ��� ��.��� ZG_MODES_TZ0..ZG_MODES_TZ0+1)
  end;
  PZG_CTR_TIMEZONE = ^TZG_CTR_TIMEZONE;

  // ��� ����� �����������
  TZG_CTR_KEY_TYPE = (
	  ZG_KEY_UNDEF = 0,       // �� ����������
	  ZG_KEY_NORMAL,          // �������
	  ZG_KEY_BLOCKING,        // �����������
	  ZG_KEY_MASTER           // ������
  );
Const
  ZG_KF_FUNCTIONAL          = $0002; // ��������������
  ZG_KF_DUAL                = $0004; // ������� �����
  ZG_KF_SHORTNUM            = $0020; // �������� �����. ���� fProximity=False, �� ���������� ����� ��������� ������ ������ 3 ����� ������ �����.
Type
  // ���� �����������
  TZG_CTR_KEY = record
    fErased         : LongBool;             // TRUE, ���� ���� �����
    rNum            : TZ_KEYNUM;            // ����� �����
    nType           : TZG_CTR_KEY_TYPE;     // ��� �����
    nFlags          : Cardinal;             // ����� ZG_KF_...
    nAccess         : Cardinal;             // ������ (����� ��������� ���)
    aData1          : array[0..3] of Byte;  // ������ ������ �����
  end;
  PZG_CTR_KEY = ^TZG_CTR_KEY;

  // ���� �����������
  TZG_CTR_CLOCK = record
    fStopped        : LongBool;         // True, ���� ���� �����������
    nYear           : Word;             // ���
    nMonth          : Word;             // �����
    nDay            : Word;             // ����
    nHour           : Word;             // ���
    nMinute         : Word;             // ������
    nSecond         : Word;             // �������
  end;
  PZG_CTR_CLOCK = ^TZG_CTR_CLOCK;

  // ��� ������� �����������
  TZG_CTR_EV_TYPE = (
    ZG_EV_UNKNOWN = 0,      // �� ����������
    ZG_EV_BUT_OPEN,			    // ������� ������� �������
    ZG_EV_KEY_NOT_FOUND,		// ���� �� ������ � ����� ������
    ZG_EV_KEY_OPEN,				  // ���� ������, ����� �������
    ZG_EV_KEY_ACCESS,			  // ���� ������, ������ �� ��������
    ZG_EV_REMOTE_OPEN,			// ������� ���������� �� ����
    ZG_EV_KEY_DOOR_BLOCK,		// ���� ������, ����� �������������
    ZG_EV_BUT_DOOR_BLOCK,		// ������� ������� ��������������� ����� �������
    ZG_EV_NO_OPEN,				  // ����� ��������
    ZG_EV_NO_CLOSE,				  // ����� ��������� �������� (timeout)
    ZG_EV_PASSAGE,				  // ������ ���������
    ZG_EV_SENSOR1,				  // �������� ������ 1
    ZG_EV_SENSOR2,				  // �������� ������ 2
    ZG_EV_REBOOT,				    // ������������ �����������
    ZG_EV_BUT_BLOCK,				// ������������� ������ ����������
    ZG_EV_DBL_PASSAGE,			// ������� �������� �������
    ZG_EV_OPEN,					    // ����� ������� ������
    ZG_EV_CLOSE,				    // ����� �������
    ZG_EV_POWEROFF,         // ������� �������
    ZG_EV_ELECTRO_ON,       // ��������� ��������������
    ZG_EV_ELECTRO_OFF,      // ���������� ��������������
    ZG_EV_LOCK_CONNECT,     // ��������� ����� (�������)
    ZG_EV_LOCK_DISCONNECT,  // ���������� ����� (�������)
    ZG_EV_MODE_STATE,        // ������������ ������� ������ (c� �����)
    ZG_EV_FIRE_STATE,       // ��������� ��������� ������
    ZG_EV_SECUR_STATE,      // ��������� ��������� ������
    ZG_EV_UNKNOWN_KEY,      // ����������� ����
    ZG_EV_GATEWAY_PASS,     // �������� ���� � ����
    ZG_EV_GATEWAY_BLOCK,    // ������������ ���� � ���� (�����)
    ZG_EV_GATEWAY_ALLOWED,  // �������� ���� � ����
    ZG_EV_ANTIPASSBACK,     // ������������ ������ (�����������)
    ZG_EV_HOTEL40,
    ZG_EV_HOTEL41
  );

  // ������� �����������
  TZG_CTR_EVENT = record
    nType           : TZG_CTR_EV_TYPE;  // ��� �������

    case Integer of
      0:
      (
        nCode       : Byte;                 // ��� ������� � �����������
        aParams     : array[0..6] of Byte;  // ��������� �������
      );
      1: ( aData    : array[0..7] of Byte; ); // ������ �������
      // (����������� ������� �������������, �������������� ���� �������,
      // ZG_Ctr_DecodePassEvent, ZG_Ctr_DecodeEcEvent, ZG_Ctr_DecodeUnkKeyEvent,
      // ZG_Ctr_DecodeFireEvent, ZG_Ctr_DecodeSecurEvent)
  end;
  PZG_CTR_EVENT = ^TZG_CTR_EVENT;

  // ����������� ������� �����������
  TZG_CTR_DIRECT = (
    ZG_DIRECT_UNDEF = 0,    // �� ����������
    ZG_DIRECT_IN,           // ����
    ZG_DIRECT_OUT           // �����
  );

  // ���� � ����� �������
  TZG_EV_TIME = record
    nMonth          : Byte;           // �����
    nDay            : Byte;           // ����
    nHour           : Byte;           // ���
    nMinute         : Byte;           // ������
    nSecond         : Byte;           // �������
    Reserved        : array[0..2] of Byte; // (���������������)
  end;
  PZG_EV_TIME = ^TZG_EV_TIME;

  // �������, ��������� ������� ElectroControl: ZG_EV_ELECTRO_ON, ZG_EV_ELECTRO_OFF
  TZG_EC_SUB_EV = (
    ZG_EC_EV_UNDEF = 0,     // �� ����������
    ZG_EC_EV_CARD_DELAY,    // ��������� �������� ����� � ������ ������� (��� �����) �������� ��������
    ZG_EC_EV_RESERVED1,     // (���������������)
    ZG_EC_EV_ON_NET,        // �������� �������� �� ����
    ZG_EC_EV_OFF_NET,       // ��������� �������� �� ����
    ZG_EC_EV_ON_SCHED,      // �������� �� ��������� ����
    ZG_EC_EV_OFF_SHED,      // ��������� �� ��������� ����
    ZG_EC_EV_CARD,          // ��������� �������� ����� � ������������ ����������
    ZG_EC_EV_RESERVED2,     // (���������������)
    ZG_EC_EV_OFF_TIMEOUT,   // ��������� ����� ��������� ��������
    ZG_EC_EV_OFF_EXIT       // ��������� �� ������������ ������� ������
  );

  // �������, ��������� ������� ZG_EV_FIRE_STATE
  TZG_FIRE_SUB_EV = (
    ZG_FR_EV_UNDEF = 0,     // �� ����������
    ZG_FR_EV_OFF_NET,       // ��������� �� ����
    ZG_FR_EV_ON_NET,        // �������� �� ����
    ZG_FR_EV_OFF_INPUT_F,   // ��������� �� ����� FIRE
    ZG_FR_EV_ON_INPUT_F,    // �������� �� ����� FIRE
    ZG_FR_EV_OFF_TEMP,      // ��������� �� ������� �����������
    ZG_FR_EV_ON_TEMP        // �������� �� ������� �����������
  );

  // �������, ��������� ������� ZG_EV_SECUR_STATE
  TZG_SECUR_SUB_EV = (
    ZG_SR_EV_UNDEF = 0,     // �� ����������
    ZG_SR_EV_OFF_NET,       // ��������� �� ����
    ZG_SR_EV_ON_NET,        // �������� �� ����
    ZG_SR_EV_OFF_INPUT_A,   // ��������� �� ����� ALARM
    ZG_SR_EV_ON_INPUT_A,    // �������� �� ����� ALARM
    ZG_FR_EV_OFF_TAMPERE,   // ��������� �� �������
    ZG_FR_EV_ON_TAMPERE,    // �������� �� �������
    ZG_FR_EV_OFF_DOOR,      // ��������� �� ������� �����
    ZG_FR_EV_ON_DOOR        // �������� �� ������� �����
  );

  // �������, ��������� ������� ZG_EV_MODE_STATE
  TZG_MODE_SUB_EV = (
    ZG_MD_EV_UNDEF = 0,
    ZG_MD_EV_RS485_ALLOW,   // ��������� �������� �� ����
    ZG_MD_EV_RS485_DENIED,  // �������� ��������� �� ����
    ZG_MD_EV_TZ_START,      // �������� ��������� ����
    ZG_MD_EV_TZ_FINISH,     // ���������� ��������� ����
    ZG_MD_EV_CARD_ALLOW,    // ��������� ������
    ZG_MD_EV_CARD_DENIED    // �������� ��������� ������
  );


Const
  // ������ ������� ��������� ��� ������� ZG_Ctr_ControlDevices
  ZG_DEV_RELE1          = 0;  // ���� ����� 1
  ZG_DEV_RELE2          = 1;  // ���� ����� 2
  ZG_DEV_SW3            = 2;  // ������� ���� SW3 (��) ����.5 ������� �5
  ZG_DEV_SW4            = 3;  // ������� ���� SW4 (��) ����.5 ������� �6
  ZG_DEV_SW0            = 4;  // ������� ���� SW0 (��) ����.1 ������� �4
  ZG_DEV_SW1            = 5;  // ������� ���� SW1 (��) ����.3 ������� �4
  ZG_DEV_K65            = 6;  // ����������� ���� (��) ����.6 ������� �5
  ZG_DEV_K66            = 7;  // ����������� ���� (��) ����.6 ������� �6

Const
  // ����� ������������ ��������������
  ZG_EC_CF_ENABLED      = $1;   // ������������� ���������� ��������
  ZG_EC_CF_SCHEDULE     = $2;   // ������������ ��������� ���� 6 ��� ��������� �������
  ZG_EC_CF_EXT_READER   = $4;   // ����������� �����������: �0� Matrix-II Net, �1� ������� �����������
  ZG_EC_CF_INVERT       = $8;   // ������������� ����������� �����
  ZG_EC_CF_EXIT_OFF     = $10;  // ������������� ������ �����
  ZG_EC_CF_CARD_OPEN    = $20;  // �� ����������� ������� ���������� ��� ������������ �����������

  // ����� ��������� ��������������
  ZG_EC_SF_ENABLED      = $1;   // ��������� ������� � 1 ���/0 ����
  ZG_EC_SF_SCHEDULE     = $2;   // ������� ��������� �� ��������� ����
  ZG_EC_SF_REMOTE       = $4;   // �������� �� ������� �� ����
  ZG_EC_SF_DELAY        = $8;   // ���� ��������� ��������
  ZG_EC_SF_CARD         = $10;  // ����� � ���� ������������ �����������

  ZG_EC_SCHEDULE_TZ     = 6;

Type
  // ������������ ���������� ���������������
  TZG_CTR_ELECTRO_CONFIG = record
    nPowerConfig    : Cardinal;         // ������������ ���������� ��������
    nPowerDelay     : Cardinal;         // ����� �������� � ��������
    rTz6            : TZG_CTR_TIMEZONE; // ��������� ���� �6 (������� �� 0)
  end;
  PZG_CTR_ELECTRO_CONFIG = ^TZG_CTR_ELECTRO_CONFIG;

  // ��������� ��������������
  TZG_CTR_ELECTRO_STATE = record
    nPowerFlags     : Cardinal;   // ����� ��������� ��������������
    nPowerConfig    : Cardinal;   // ������������ ���������� ��������
    nPowerDelay     : Cardinal;   // ����� �������� � ��������
  end;
  PZG_CTR_ELECTRO_STATE = ^TZG_CTR_ELECTRO_STATE;

Const
  // ����� ��������� ������ �����
  ZG_FR_F_ENABLED       = $1;   // ��������� ��������� ������ � 1 ���/0 ����
  ZG_FR_F_INPUT_F       = $2;   // ������� �������� ����� �� ����� FIRE
  ZG_FR_F_TEMP          = $4;   // ������� �������� ����� �� ���������� �����������
  ZG_FR_F_NET           = $8;   // ������� �������� ����� �� ������� �������

  // ����� ��� ����� ���������� ���������� ������ �����
  ZG_FR_SRCF_INPUT_F    = $1;   // �������� �������� ����� �� ����� FIRE
  ZG_FR_SRCF_TEMP       = $2;   // �������� �������� ����� �� ���������� �����������

Type
  // ����� ������
  TZG_SECUR_MODE = (
    ZG_SR_M_UNDEF = 0,    // �� ����������
    ZG_SR_M_SECUR_OFF,    // ��������� ����� ������
    ZG_SR_M_SECUR_ON,     // �������� ����� ������
    ZG_SR_M_ALARM_OFF,    // ��������� �������
    ZG_SR_M_ALARM_ON      // �������� �������
  );

Const
  // ����� ��������� ������ ������
  ZG_SR_F_ENABLED       = $1;   // ��������� ��������� ������ � 1 ���/0 ����
  ZG_SR_F_ALARM         = $2;   // ��������� �������
  ZG_SR_F_INPUT_A       = $4;   // ������� �� ����� ALARM
  ZG_SR_F_TAMPERE       = $8;   // ������� �� �������
  ZG_SR_F_DOOR          = $10;  // ������� �� ������� �����
  ZG_SR_F_NET           = $20;  // ������� �������� �� ����

  // ����� ��� ����� ���������� ���������� ������ ������
  ZG_SR_SRCF_INPUT_F    = $1;   // ��������� ������� �� ����� FIRE
  ZG_SR_SRCF_TAMPERE    = $2;   // ��������� ������� �� �������
  ZG_SR_SRCF_DOOR       = $4;   // ��������� ������� �� ������� �����

Type
// �������, ��������� ������� ZG_EV_HOTEL40, ZG_EV_HOTEL41
  TZG_HOTEL_SUB_EV = (
    ZG_H_EV_UNDEF = 0,    // �� ����������
    ZG_H_EV_FREECARD,     // ����� ��������
    ZG_H_EV_BLOCKCARD,    // ����� �����������
    ZG_H_EV_EXFUNC,       // �������������� �������
    ZG_H_EV_NEWRCARD,     // ������� ��������� �����
    ZG_H_EV_NETWORK,
    ZG_H_EV_TIMEZONE,
    ZG_H_EV_COUNTER,      // �������� �������
    ZG_H_EV_CRYPTOKEY,    // �������� ����������
    ZG_H_EV_PULSEZ,       // ����������� ������� � ������� 2� ������
    ZG_H_EV_STATE         // ��������� ������� -���� ������ ����� � ��������� ����� ��� ����� 2 �������
  );
Const
// ����� ��� ������� ZG_EV_HOTEL40, ZG_EV_HOTEL41
  ZG_HF_LATCH           = $0001; // �������
  ZG_HF_LATCH2          = $0002; // ��������
  ZG_HF_KEY             = $0004; // ����
  ZG_HF_CARD            = $0008; // �����

Const
  ZG_NF_CVT_CTR_EXIST       = $0001;     // ZG_N_CVT_CTR_INSERT / ZG_N_CVT_CTR_REMOVE
  ZG_NF_CVT_CTR_CHANGE      = $0002;     // ��������� ���������� ����������� ZG_N_CVT_CTR_CHANGE
  ZG_NF_CVT_ERROR           = $0004;     // ZG_N_CVT_ERROR
  ZG_CVTNF_CONNECTION_CHANGE= $0008;     // ZG_N_CVT_PORTSTATUS
  ZG_NF_CVT_CTR_DBL_CHECK   = $1000;  // ������ ��������� ���������� ������������
  ZG_NF_CVT_REASSIGN_ADDRS  = $2000;  // �������������� �������������� �������
    // ������������ (����� Guard Advanced) (�������� ������ � ZG_NF_CVT_CTR_EXIST)
  ZG_NF_CVT_RESCAN_CTRS     = $10000; // ������ ������ ������������ ������������ (��� �������� ����������, �� Guard)
  ZG_NF_CVT_ALT_SCAN        = $20000; // �������������� ����� ������������
  ZG_NF_CVT_NOGATE          = $40000; // �� ����������� GATE-����������� (���, ����� Eurolock)
  ZG_NF_CVT_NOEUROLOCK      = $80000; // �� ����������� Eurolock EHT net

Const
  ZG_N_CVT_CTR_INSERT       = 1; // ���������� ��������� PZG_FIND_CTR_INFO(MsgParam) - ���������� � �����������
  ZG_N_CVT_CTR_REMOVE       = 2; // ���������� �������� PZG_FIND_CTR_INFO(MsgParam) - ���������� � �����������
  ZG_N_CVT_CTR_CHANGE       = 3; // �������� ��������� ����������� PZG_N_CTR_CHANGE_INFO(MsgParam)
  ZG_N_CVT_ERROR            = 4; // �������� ������ � ���� (HRESULT*)nMsgParam - ��� ������
  ZG_CVTN_CONNECTION_CHANGE = 5; // ���������� ��������� �����������

Type
  TZG_N_CTR_CHANGE_INFO = record
    nChangeMask     : Cardinal;           // ����� ��������� (���0 addr, ���1 version, ���2 proximity)
    rCtrInfo        : TZG_FIND_CTR_INFO;  // ���������� ���������� � �����������
    nOldVersion     : Word;               // ������ �������� ������
    nOldAddr        : Byte;               // ������ �������� ������
    Reserved        : Byte;               // ��������������� ��� ������������ ���������
  end;
  PZG_N_CTR_CHANGE_INFO = ^TZG_N_CTR_CHANGE_INFO;

Type
  // ��������� ��� ����������� �� ����������
  TZG_CVT_NOTIFY_SETTINGS = record
    nNMask          : Cardinal;       // ����� ����� ����������� (ZG_NF_CVT_...)

    hEvent          : THandle;        // ������� (������ �������������)
    hWindow         : HWnd;           // �������� ��� Callback-�������
    nWndMsgId       : Cardinal;       // ��������� ��� �������� ���� hWnd

    nScanCtrsPeriod : Cardinal;       // ������ ������������ ������ ������������ � �� (=0 ������������ �������� �� ���������, 5000, =-1 �������)
    nScanCtrsLastAddr: Integer;       // ��������� ����������� ����� �����������
  end;
  PZG_CVT_NOTIFY_SETTINGS = ^TZG_CVT_NOTIFY_SETTINGS;

Const
  ZG_NF_CTR_NEW_EVENT     = $1;     // ZG_N_CTR_NEW_EVENT
  ZG_NF_CTR_CLOCK         = $2;     // ZG_N_CTR_CLOCK
  ZG_NF_CTR_KEY_TOP       = $4;     // ZG_N_CTR_KEY_TOP
  ZG_NF_CTR_ADDR_CHANGE   = $8;     // ZG_N_CTR_ADDR_CHANGE
  ZG_NF_CTR_ERROR         = $10;    // ZG_N_CTR_ERROR
  ZG_NF_CTR_WND_SYNC      = $4000;  // ���������������� � �������� ��������� Windows
  ZG_NF_CTR_ONLY_NOTIFY   = $8000;  // ������ ���������� � ���������� ����� ��������� � �������

// ����������� ������� ZG_Ctr_SetNotification (wParam - MsgCode, lParam - MsgParam)
Type
  TZG_N_NEW_EVENT_INFO = record
    nNewCount       : Integer;		// ���������� ����� �������
    nWriteIdx       : Integer;		// ��������� ������
    nReadIdx        : Integer;		// ��������� ������
    rLastNum        : TZ_KEYNUM;	// ����� ���������� ������������ �����
  end;
  PZG_N_NEW_EVENT_INFO = ^TZG_N_NEW_EVENT_INFO;
Const 
  ZG_N_CTR_NEW_EVENT      = 1;  // ����� ������� PZG_N_NEW_EVENT_INFO(MsgParam) - ����������

  ZG_N_CTR_CLOCK          = 2;  // �������� ���������������� � �������� PINT64(MsgParam)

Type
  TZG_N_KEY_TOP_INFO = record
    nBankN          : Integer;		// ����� ����� ������
    nNewTopIdx      : Integer;		// ����� �������� ������� ������� ������
    nOldTopIdx      : Integer;		// ������ �������� ������� ������� ������
  end;
  PZG_N_KEY_TOP_INFO = ^TZG_N_KEY_TOP_INFO;
Const
  ZG_N_CTR_KEY_TOP        = 3;  // ���������� ������� ������� ������ PZG_N_KEY_TOP_INFO(MsgParam) - ����������
  ZG_N_CTR_ADDR_CHANGE    = 4;  // ������� ������� ����� ����������� PByte(MsgParam) = ����� �����
  ZG_N_CTR_ERROR          = 5;  // �������� ������ � ���� PHRESULT(MsgParam) - ��� ������

Type
  // ��������� ��� ����������� �� �����������
  TZG_CTR_NOTIFY_SETTINGS = record
    nNMask          : Cardinal;       // ����� ����� ����������� (ZG_NF_CTR_...)

    hEvent          : THandle;        // ������� (������ �������������)
    hWindow         : HWnd;           // �������� ��� Callback-�������
    nWndMsgId       : Cardinal;       // ��������� ��� �������� ���� hWnd

    nReadEvIdx      : Integer;        // ��������� ������ �������
    nCheckStatePeriod: Cardinal;      // ������ �������� ��������� ����������� (����� �������, �����, ������� ������� ������) � �� (=0 ������������ �������� �� ���������, 1000)

    nClockOffs      : Cardinal;       // �������� ����� ����������� �� ����� �� � ��������
  end;
  PZG_CTR_NOTIFY_SETTINGS = ^TZG_CTR_NOTIFY_SETTINGS;

Type
  TZG_PROCESSCALLBACK = function(APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
  TZG_ENUMCTRSPROC = function(AInfo: PZG_FIND_CTR_INFO; APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
  TZG_ENUMCTRTIMEZONESPROC = function(AIdx: Integer; ATz: PZG_CTR_TIMEZONE; AUserData: Pointer): LongBool; stdcall;
  TZG_ENUMCTRKEYSPROC = function(AIdx: Integer; AKey: PZG_CTR_KEY; APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
  TZG_ENUMCTREVENTSPROC = function(AIdx: Integer; AEvent: PZG_CTR_EVENT; APos, AMax: Integer; AUserData: Pointer): LongBool; stdcall;

Const // ����� ��� ������� ZG_Cvt_EnumControllers
  ZG_F_UPDATE     = 1;  // �������� ������ ������
  ZG_F_REASSIGN   = 2;  // ������������� ������������� ������
  ZG_F_NOGATE     = 4;  // �� ������ GATE-�����������
  ZG_F_NOEUROLOCK = 8;  // �� ������ Eurolock EHT net

Const // ����� ��� ������� ZG_Cvt_SearchControllers
  ZG_CVSF_DETECTOR  = 1;  // ������������ ������� ������ ��������� ������������ ��������� (ZG_Cvt_SetNotification)
  ZG_CVSF_NOGATE    = 4;  // �� ������ GATE-�����������
  ZG_CVSF_NOEUROLOCK= 8;  // �� ������ �����: Eurolock EHT net � Z-9 EHT net

// ���������� ��������� � ����
Type
  TZG_STATUS = HResult;
  TZG_ENUMCVTSPROC = function(AInfo: PZG_ENUM_CVT_INFO; Const APort: TZP_PORT_INFO; AUserData: Pointer): LongBool; stdcall;
  TZG_ENUMIPCVTSPROC = function(AInfo: PZG_ENUM_IPCVT_INFO; Const APort: TZP_PORT_INFO; AUserData: Pointer): LongBool; stdcall;
Const
  ZG_SUCCESS                = S_OK deprecated;
  ZG_E_CANCELLED            = ZP_S_CANCELLED deprecated;
  ZG_E_NOT_FOUND            = ZP_S_NOTFOUND deprecated;

  ZG_E_INVALID_PARAM        = E_INVALIDARG deprecated;
  ZG_E_OPEN_NOT_EXIST       = ZP_E_OPENNOTEXIST deprecated;
  ZG_E_OPEN_ACCESS          = E_ACCESSDENIED deprecated;
  ZG_E_OPEN_PORT            = ZP_E_OPENPORT deprecated;
  ZG_E_PORT_IO_ERROR        = ZP_E_PORTIO deprecated;
  ZG_E_PORT_SETUP           = ZP_E_PORTSETUP deprecated;
  ZG_E_LOAD_FTD2XX          = ZP_E_LOADFTD2XX deprecated;
  ZG_E_INIT_SOCKET          = ZP_E_SOCKET deprecated;
  ZG_E_SERVERCLOSE          = ZP_E_SERVERCLOSE deprecated;
  ZG_E_NOT_ENOUGH_MEMORY    = E_OUTOFMEMORY deprecated;
  ZG_E_UNSUPPORT            = E_NOINTERFACE deprecated;
  ZG_E_NOT_INITALIZED       = ZP_E_NOTINITALIZED deprecated;
  ZG_E_CREATE_EVENT         = E_FAIL deprecated;

  ZG_E_TOO_LARGE_MSG        = ZG_E_TOOLARGEMSG deprecated;
  ZG_E_INSUFFICIENT_BUFFER  = ZP_E_INSUFFICIENTBUFFER deprecated;
  ZG_E_NO_ANSWER            = ZG_E_NOANSWER deprecated;
  ZG_E_BAD_ANSWER           = ZG_E_BADANSWER deprecated;
  ZG_E_ONLY_GUARD           = E_NOINTERFACE deprecated;
  ZG_E_WRONG_ZPORT_VERSION  = ZG_E_WRONGZPORT deprecated;
  ZG_E_CVT_BUSY             = ZG_E_CVTBUSY deprecated;
  ZG_E_G_ONLY_ADVANCED      = E_NOINTERFACE deprecated;
  ZG_E_G_OTHER              = ZG_E_CVTERROR deprecated;
  ZG_E_G_LIC_OTHER          = ZG_E_CVTERROR deprecated;
  ZG_E_G_LIC_NOT_FOUND      = ZG_E_LICNOTFOUND deprecated;
  ZG_E_G_LIC_EXPIRED        = ZG_E_LICEXPIRED deprecated;
  ZG_E_G_LIC_CTR_LIM        = ZG_E_LICONTROLLERS deprecated;
  ZG_E_G_LIC_RKEY_LIM       = ZG_E_LICREADKEYS deprecated;
  ZG_E_G_LIC_WKEY_LIM       = ZG_E_LICWRITEKEYS deprecated;
  ZG_E_G_LIC_EXPIRED2       = ZG_E_LICEXPIRED2 deprecated;
  ZG_E_G_BAD_CS             = ZG_E_CVTERROR deprecated;
  ZG_E_G_CTR_NOT_FOUND      = ZG_E_NOCONTROLLER deprecated;
  ZG_E_G_CMD_UNSUPPORT      = ZG_E_CVTERROR deprecated;

  ZG_E_CTR_NACK             = ZG_E_CTRNACK deprecated;
  ZG_E_CTR_TRANSFER         = ZG_E_NOCONTROLLER deprecated;
  ZG_E_BOOTLOADER_NOSTART   = ZG_E_FWBOOTLOADERNOSTART deprecated;
  ZG_E_FIRMWARE_FILESIZE    = ZG_E_FWFILESIZE deprecated;
  ZG_E_FIRMWARE_NOSTART     = ZG_E_FWNOSTART deprecated;

  ZG_E_FW_NO_COMPATIBLE     = ZG_E_FWNOCOMPATIBLE deprecated;
  ZG_E_FW_INVALID_DEV_NUM   = ZG_E_FWINVALIDDEVNUM deprecated;
  ZG_E_FW_TOOLARGE          = ZG_E_FWTOOLARGE deprecated;
  ZG_E_FW_SEQUENCE_DATA     = ZG_E_FWSEQUENCEDATA deprecated;
  ZG_E_FW_DATA_INTEGRITY    = ZG_E_FWDATAINTEGRITY deprecated;
  ZG_E_FW_OTHER             = E_FAIL deprecated;
  ZG_E_OTHER                = E_FAIL deprecated;

  ZG_NF_CVT_WND_SYNC        = $4000 deprecated;
  ZG_NF_CVT_ONLY_NOTIFY     = $8000 deprecated;

  ZG_KF_SHORT_NUM           = ZG_KF_SHORTNUM deprecated;
  ZG_KF_ANTIPASSBACK        = 2 deprecated;

{$MINENUMSIZE 1}
{$ALIGN ON}

function ZG_GetVersion(): Cardinal; stdcall;

function ZG_Initialize(AFlags: Cardinal): HResult; stdcall;
function ZG_Finalyze(): HResult; stdcall;

function ZG_CloseHandle(AHandle: THandle): HResult; {$IFDEF HAS_INLINE}inline;{$ENDIF}
//////////////////////////////////////////////////////////////////////////
// ������� ��� ������ � �����������
//

// ����������� ��� ������������ ����������
function ZG_GetPortInfoList(var VHandle: THandle; var VCount: Integer): HResult; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_SearchDevices(var VHandle: THandle; var AParams: TZP_SEARCH_PARAMS;
    ASerial: Boolean=True; AIP: Boolean=True): HResult; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_FindNextDevice(AHandle: THandle; var VInfo: TZG_ENUM_IPCVT_INFO;
    VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer;
    ATimeout: Cardinal=INFINITE): HResult; {$IFDEF HAS_INLINE}inline;{$ENDIF}

// ����������� � �����������/���������� ����������
function ZG_SetNotification(var VHandle: THandle; var ASettings: TZP_DD_NOTIFY_SETTINGS;
    ASerial: Boolean; AIp: Boolean): HResult;{$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_GetNextMessage(AHandle: THandle; var VMsg: Cardinal;
    var VMsgParam: NativeInt): HResult;{$IFDEF HAS_INLINE}inline;{$ENDIF}

// ���������� ������ �/� �����������, ������������ � Proxy-�������
function ZG_GetProxyConverters(VSnBuf: PWord; ABufSize: Integer;
    var VRCount: Integer; AIpAddr: PWideChar; AActCode: PAnsiChar;
    AWait: PZP_WAIT_SETTINGS=nil): HResult; stdcall;
// ��������� ����� �������� � ���������
function ZG_UpdateCvtFirmware(AParams: PZG_CVT_OPEN_PARAMS; Const AData; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;

// ���������/��������� ���������
function ZG_Cvt_Open(var VHandle: THandle; AParams: PZG_CVT_OPEN_PARAMS;
    VInfo: PZG_CVT_INFO=nil): HResult; stdcall;
// ����������� �� ����������, �� �������� ����, ���������� ���������� �����, ���������� �������� ZP_Open
function ZG_Cvt_DettachPort(AHandle: THandle; var VPortHandle: THandle): HResult; stdcall;
// ���������� ��������� �����������
function ZG_Cvt_GetConnectionStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult; stdcall;

// ����������/������������� ��������� �������� ���������� �������
function ZG_Cvt_SetWaitSettings(AHandle: THandle; Const ASetting: TZP_WAIT_SETTINGS): HResult; stdcall;
function ZG_Cvt_GetWaitSettings(AHandle: THandle; var VSetting: TZP_WAIT_SETTINGS): HResult; stdcall;

// ��������� ���� (���������������� ������� �������� � ������)
function ZG_Cvt_SetCapture(AHandle: THandle): HResult; stdcall;
// ��������� ���� (������������ ������� �������� � ������)
function ZG_Cvt_ReleaseCapture(AHandle: THandle): HResult; stdcall;
// ������� ���� ����������
function ZG_Cvt_Clear(AHandle: THandle): HResult; stdcall;
// ���������� ������ ����������
function ZG_Cvt_Send(AHandle: THandle; Const AData; ACount: Integer): HResult; stdcall;
// ���������� ����� �� ����������
function ZG_Cvt_Receive(AHandle: THandle; var VBuf; ABufSize: Integer;
    AMinRead: Integer; var VRCount: Integer): HResult; stdcall;
// ���������� ������ � ���������� ����� �� ����������
function ZG_Cvt_Exec(AHandle: THandle; Const AData; ACount: Integer;
    var VBuf; ABufSize: Integer; AMinRead: Integer; var VRCount: Integer): HResult; stdcall;

// ����������� ��� ������������ ����������� (��� �� Guard-����������� ����� �������������� ��������� ����������� � �����/����. ������������)
function ZG_Cvt_EnumControllers(AHandle: THandle;
    AEnumProc: TZG_ENUMCTRSPROC; AUserData: Pointer; AFlags: Cardinal=ZG_F_UPDATE): HResult; stdcall;
// ���� ����������� �� �������� ������
function ZG_Cvt_FindController(AHandle: THandle; AAddr: Byte; var VInfo: TZG_FIND_CTR_INFO;
    AFlags: Cardinal=ZG_F_UPDATE; AWait: PZP_WAIT_SETTINGS=nil): HResult; stdcall;

function ZG_Cvt_SearchControllers(AHandle: THandle; AMaxCtrs: Integer=MaxInt; AFlags: Cardinal=0): HResult; stdcall;
function ZG_Cvt_FindNextController(AHandle: THandle; VInfo: PZG_FIND_CTR_INFO): HResult; stdcall;

// ���������� ���������� � ����������
function ZG_Cvt_GetInformation(AHandle: THandle; var VInfo: TZG_CVT_INFO): HResult; stdcall;

// ����������� ����������� �� ����������
function ZG_Cvt_SetNotification(AHandle: THandle; ASettings: PZG_CVT_NOTIFY_SETTINGS): HResult; stdcall;
function ZG_Cvt_GetNextMessage(AHandle: THandle; var VMsg: Cardinal;
    var VMsgParam: NativeInt): HResult; stdcall;
function ZG_Cvt_GetScanCtrsState(AHandle: THandle; var VNextAddr: Integer): HResult; stdcall;

// ������������� � ��������� ����� ��������
function ZG_Cvt_UpdateFirmware(AHandle: THandle; Const AData; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;

// ���������� ���������� � �������� ����������
function ZG_Cvt_GetLicense(AHandle: THandle; ALicN: Byte;
    var VInfo: TZG_CVT_LIC_INFO): HResult; stdcall;
// ������������� ����� ��������
function ZG_Cvt_SetLicenseData(AHandle: THandle; ALicN: Byte;
    Const AData; ACount: Integer; VLicStatus: PWord=nil): HResult; stdcall;
// ������� ��� �������� � ����������
function ZG_Cvt_ClearAllLicenses(AHandle: THandle): HResult; stdcall;
// ���������� ���������� � ���� ���������, ������������� � ���������
function ZG_Cvt_GetAllLicenses(AHandle: THandle; VBuf: PZG_CVT_LIC_SINFO;
    VBufSize: Integer; var VCount: Integer): HResult; stdcall;

// �������� �/� ���������� � ����� (������ Guard)
function ZG_Cvt_GetShortInfo(AHandle: THandle; var VSn: Word; var VMode: TZG_GUARD_MODE): HResult; stdcall;
// �������� �������������� ������ ���������� (������ Guard)
function ZG_Cvt_GetLongInfo(AHandle: THandle;
    VSn: PWord; VVersion: PCardinal; VMode: PZG_GUARD_MODE;
    VBuf: PWideChar; ABufSize: Integer; VLen: PInteger): HResult; stdcall;

// ���������� � ���������� ����� ��������
function ZG_Cvt_UpdateCtrFirmware(AHandle: THandle; ASn: Word;
    Const AData; ACount: Integer; AInfoStr: PAnsiChar;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer;
    AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): HResult; stdcall;

// ���������� ����� ������� ����� ����������� �� �/� (� Advanced ������ �� ��������)
function ZG_Cvt_SetCtrAddrBySn(AHandle: THandle; ASn: Word; ANewAddr: Byte;
    AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): HResult; stdcall;
function ZG_Cvt_SetCtrAddr(AHandle: THandle; AOldAddr: Byte; ANewAddr: Byte;
    AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): HResult; stdcall;

// ��������� ���������� ����������� �� �������� ������
function ZG_Cvt_GetCtrInfoNorm(AHandle: THandle; AAddr: Byte; VTypeCode: PByte; VSn: PWord;
    VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal;
    AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): HResult; stdcall;
function ZG_Cvt_GetCtrInfoAdv(AHandle: THandle; AAddr: Byte; VTypeCode: PByte; VSn: PWord;
    VVersion: PWord; VFlags: PCardinal; VEvWrIdx: PInteger; VEvRdIdx: PInteger): HResult; stdcall;
// ����������� ���������� � ����������� �� ��������� ������.
function ZG_Cvt_GetCtrInfoBySn(AHandle: THandle; ASn: Word; VTypeCode: PByte; VAddr: PByte;
    VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal;
    AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): HResult; stdcall;
// ���������� �������������� ������ �����������
function ZG_Cvt_GetCtrInfoLine(AHandle: THandle; ASn: Word; ALineN: Integer;
    VBuf: PAnsiChar; ABufSize: Integer; VLen: PInteger=nil;
    AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): HResult; stdcall;
// �������� ������ �����������
function ZG_Cvt_GetCtrVersion(AHandle: THandle; AAddr: Byte; VVerData5: PByte;
    AWait: PZP_WAIT_SETTINGS=nil): HResult; stdcall;

//////////////////////////////////////////////////////////////////////////
// ������� ��� ������ � ������������
//

// ���������/��������� ����������
function ZG_Ctr_Open(var VHandle: THandle; ACvtHandle: THandle; AAddr: Byte; ASn: Word;
    VInfo: PZG_CTR_INFO=nil; AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): HResult; stdcall;
// ���������� ���������� � �����������
function ZG_Ctr_GetInformation(AHandle: THandle; var VInfo: TZG_CTR_INFO): HResult; stdcall;
// ����������� ����������� �� ����������� (���� = nil, ��������� ��� �����������)
function ZG_Ctr_SetNotification(AHandle: THandle; ASettings: PZG_CTR_NOTIFY_SETTINGS): HResult; stdcall;
function ZG_Ctr_GetNextMessage(AHandle: THandle; var VMsg: Cardinal;
    var VMsgParam: NativeInt): HResult; stdcall;
// ��������� �������� ������ ����������� (Guard �� ������������)
function ZG_Ctr_SetNewAddr(AHandle: THandle; ANewAddr: Byte): HResult; stdcall;
// ������� ���������� ����������� � ����� ������� ������� �����������
function ZG_Ctr_AssignAddr(AHandle: THandle; AAddr: Byte): HResult; stdcall;
// ��������� ����� �������� �����������
function ZG_Ctr_UpdateFirmware(AHandle: THandle; Const AData; ACount: Integer;
    AInfoStr: PAnsiChar; ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;

// ������� �����
function ZG_Ctr_OpenLock(AHandle: THandle; ALockN: Integer=0): HResult; stdcall;
// ���������� �����
function ZG_Ctr_CloseLock(AHandle: THandle): HResult; stdcall;
// �������� ����� ���������� ���������� ������
function ZG_Ctr_EnableEmergencyUnlocking(AHandle: THandle; AEnable: LongBool): HResult; stdcall;
// ���������� ��������� ������ ���������� ���������� ������
function ZG_Ctr_IsEmergencyUnlockingEnabled(AHandle: THandle; var VEnable: LongBool): HResult; stdcall;

// ������� �������� �����������
function ZG_Ctr_ReadRegs(AHandle: THandle; AAddr: Cardinal;
    ACount: Integer; var VBuf): HResult; stdcall;
// ������� ��������� ������
function ZG_Ctr_ReadPorts(AHandle: THandle; var VData: Cardinal): HResult; stdcall;
// ���������� �������� ������������ (nDevType - ����� ���������� ZG_DEV_..)
function ZG_Ctr_ControlDevices(AHandle: THandle; ADevType: Cardinal;
    AActive: LongBool; ATimeMs: Cardinal=0): HResult; stdcall;

// ������ / ������ ������ ����������� (nBankN - ����� �� ����: =0 ����, =1 �����, =2 �������
function ZG_Ctr_ReadData(AHandle: THandle; ABankN: Integer;
    AAddr: Cardinal; ACount: Integer; var VBuf;
    var VReaded: Integer; ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
function ZG_Ctr_WriteData(AHandle: THandle; ABankN: Integer;
    AAddr: Cardinal; Const AData; ACount: Integer; 
    var VWritten: Integer; ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;

// �������� ������� ��� ������ �����������
function ZG_Ctr_ReadLockTimes(AHandle: THandle; VOpenMs: PCardinal;
    VLetMs: PCardinal; VMaxMs: PCardinal; ABankN: Integer=0): HResult; stdcall;
// ���������� ������� ��� ������ �����������
function ZG_Ctr_WriteLockTimes(AHandle: THandle; AMask: Cardinal; AOpenMs: Cardinal;
    ALetMs: Cardinal; AMaxMs: Cardinal; ABankN: Integer=0): HResult; stdcall;

// �������� ���� ��� ��������� ��������� ���
function ZG_Ctr_ReadTimeZones(AHandle: THandle; AIdx: Integer;
    VBuf: PZG_CTR_TIMEZONE; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer; ABankN: Integer=0): HResult; stdcall;
// �������� ���� ��� ��������� ��������� ���
function ZG_Ctr_WriteTimeZones(AHandle: THandle; AIdx: Integer;
    ATzs: PZG_CTR_TIMEZONE; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer; ABankN: Integer=0): HResult; stdcall;
// ����������� ��������� ���� � �����������
function ZG_Ctr_EnumTimeZones(AHandle: THandle; AStart: Integer;
    AEnumProc: TZG_ENUMCTRTIMEZONESPROC; AUserData: Pointer; ABankN: Integer=0): HResult; stdcall;

// �������� ���� ��� ��������� ������
function ZG_Ctr_ReadKeys(AHandle: THandle; AIdx: Integer;
    VBuf: PZG_CTR_KEY; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer; ABankN: Integer=0): HResult; stdcall;
// �������� ���� ��� ��������� ������
function ZG_Ctr_WriteKeys(AHandle: THandle; AIdx: Integer;
    AKeys: PZG_CTR_KEY; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer;
    ABankN: Integer=0; AUpdateTop: LongBool=True): HResult; stdcall;
// ������� ���� ��� ��������� ������
function ZG_Ctr_ClearKeys(AHandle: THandle; AIdx: Integer; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer;
    ABankN: Integer=0; AUpdateTop: LongBool=True): HResult; stdcall;
// �������� ������ ������� ������� ������
function ZG_Ctr_GetKeyTopIndex(AHandle: THandle; var VIdx: Integer; ABankN: Integer=0): HResult; stdcall;
// ����������� ����� � �����������
function ZG_Ctr_EnumKeys(AHandle: THandle; AStart: Integer;
    AEnumProc: TZG_ENUMCTRKEYSPROC; AUserData: Pointer; ABankN: Integer=0): HResult; stdcall;

// �������� ���� �����������
function ZG_Ctr_GetClock(AHandle: THandle; var VClock: TZG_CTR_CLOCK): HResult; stdcall;
// ���������� ���� �����������
function ZG_Ctr_SetClock(AHandle: THandle; Const AClock: TZG_CTR_CLOCK): HResult; stdcall;

// �������� ����� ���������� ������������ �����
function ZG_Ctr_ReadLastKeyNum(AHandle: THandle; var VNum: TZ_KEYNUM): HResult; stdcall;
// �������� ��������� ������� � ����� ���������� ������������ ����� (���� *pWrIdx ��� *pRdIdx == -1, �� ��������� ������������)
function ZG_Ctr_ReadRTCState(AHandle: THandle; VClock: PZG_CTR_CLOCK;
    VWrIdx: PInteger; VRdIdx: PInteger; VNum: PZ_KEYNUM): HResult; stdcall;
function ZG_Ctr_ReadEventIdxs(AHandle: THandle; var VWrIdx: Integer; var VRdIdx: Integer): HResult; stdcall;
// ���������� ��������� �������
function ZG_Ctr_WriteEventIdxs(AHandle: THandle; AMask: Cardinal; AWrIdx, ARdIdx: Integer): HResult; stdcall;
// �������� ���� ��� ��������� �������
function ZG_Ctr_ReadEvents(AHandle: THandle; AIdx: Integer;
    VBuf: PZG_CTR_EVENT; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
// ����������� ������� �����������
// AStart - ������ ������� �������, ���� =-1, ������������ ��������� ������,
// ACount - ���������� ������������� �������, ���� =-1, �� ������������ ���������� ������� � nStart �� ��������� ������,
//			���� =MAXINT, �� ������������� ��� �������
function ZG_Ctr_EnumEvents(AHandle: THandle; AStart, ACount: Integer;
    AEnumProc: TZG_ENUMCTREVENTSPROC; AUserData: Pointer): HResult; stdcall;
// ������������� ������� �������
function ZG_Ctr_DecodePassEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VDirect: TZG_CTR_DIRECT;
    var VKeyIdx: Integer; var VKeyBank: Integer): HResult; stdcall;
// ������������� ������� ElectoControl: ZG_EV_ELECTRO_ON, ZG_EV_ELECTRO_OFF
function ZG_Ctr_DecodeEcEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VSubEvent: TZG_EC_SUB_EV;
    var VPowerFlags: Cardinal): HResult; stdcall;
// ������������� ������� �� ��������� �����: ZG_EV_UNKNOWN_KEY
function ZG_Ctr_DecodeUnkKeyEvent(AHandle: THandle; AData8: PByte;
    var VKeyNum: TZ_KEYNUM): HResult; stdcall;
// ������������� ������� ZG_EV_FIRE_STATE (��������� ��������� ������)
function ZG_Ctr_DecodeFireEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VSubEvent: TZG_FIRE_SUB_EV;
    var VFireFlags: Cardinal): HResult; stdcall;
// ������������� ������� ZG_EV_SECUR_STATE (��������� ��������� ������)
function ZG_Ctr_DecodeSecurEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VSubEvent: TZG_SECUR_SUB_EV;
    var VSecurFlags: Cardinal): HResult; stdcall;
// ������������� ������� "��������� ��������� �����": ZG_EV_MODE_STATE
function ZG_Ctr_DecodeModeEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VCurrMode: TZG_CTR_MODE;
    var VSubEvent: TZG_MODE_SUB_EV): HResult; stdcall;
function ZG_DecodeHotelEvent(AData8: PByte;
    var VTime: TZG_EV_TIME; var VMode: TZG_HOTEL_MODE;
    var VSubEvent: TZG_HOTEL_SUB_EV; var VFlags: Cardinal): HResult; stdcall;

// ���������� �������� ������� �� ����
function ZG_Ctr_SetFireMode(AHandle: THandle; AOn: LongBool): HResult; stdcall;
// ������ ��������� ��������� ������
function ZG_Ctr_GetFireInfo(AHandle: THandle; var VFireFlags: Cardinal;
    var VCurrTemp: Cardinal; var VSrcMask: Cardinal; var VLimitTemp: Cardinal): HResult; stdcall;
// ��������� ���������� ��������� ������
function ZG_Ctr_SetFireConfig(AHandle: THandle; ASrcMask, ALimitTemp: Cardinal;
    VFireFlags: PCardinal=nil; VCurrTemp: PCardinal=nil): HResult; stdcall;

// ���������� ������� ������ �� ����
function ZG_Ctr_SetSecurMode(AHandle: THandle; AMode: TZG_SECUR_MODE): HResult; stdcall;
// ������ ��������� ������ ������
function ZG_Ctr_GetSecurInfo(AHandle: THandle; var VSecurFlags: Cardinal;
    var VSrcMask: Cardinal; var VAlarmTime: Cardinal): HResult; stdcall;
// ��������� ���������� ������ ������
function ZG_Ctr_SetSecurConfig(AHandle: THandle; ASrcMask, AAlarmTime: Cardinal;
    VSecurFlags: PCardinal=nil): HResult; stdcall;

// ���������� �������� ������� �����������
function ZG_Ctr_SetCtrMode(AHandle: THandle; AMode: TZG_CTR_MODE): HResult; stdcall;
function ZG_Ctr_GetCtrModeInfo(AHandle: THandle; var VMode: TZG_CTR_MODE;
    var VFlags: Cardinal): HResult; stdcall;

// ��� ������� Matrix II Net FW v3.X
// ���������� ��������� �������������� (��.���� � �������� 6)
function ZG_Ctr_ReadElectroConfig(AHandle: THandle;
    var VConfig: TZG_CTR_ELECTRO_CONFIG): HResult; stdcall;
// ������������� ����� ��������� �������������� (��.���� � �������� 6)
function ZG_Ctr_WriteElectroConfig(AHandle: THandle;
    Const AConfig: TZG_CTR_ELECTRO_CONFIG; ASetTz: LongBool=True): HResult; stdcall;

// ���������� ������ ��������������
function ZG_Ctr_GetElectroState(AHandle: THandle;
    var VState: TZG_CTR_ELECTRO_STATE): HResult; stdcall;
// ���./����. ��������������
function ZG_Ctr_SetElectroPower(AHandle: THandle;
    AOn: LongBool): HResult; stdcall;


// ���������� �������
function ZG_EnumConverters(APorts: PZP_PORT_ADDR; APCount: Integer;
    AEnumProc: TZG_ENUMCVTSPROC; AUserData: Pointer;
    AWait: PZP_WAIT_SETTINGS=nil;
    AFlags: Cardinal=3(*(ZP_SF_UPDATE+ZP_SF_USEVCOM)*)): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_EnumIpConverters(AEnumProc: TZG_ENUMIPCVTSPROC; AUserData: Pointer;
    AWait: PZP_WAIT_SETTINGS=nil;
    AFlags: Cardinal=1(*ZP_SF_UPDATE*)): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_FindConverter(APorts: PZP_PORT_ADDR; APCount: Integer;
    var VInfo: TZG_ENUM_CVT_INFO; var VPort: TZP_PORT_INFO;
    AWait: PZP_WAIT_SETTINGS=nil;
    AFlags: Cardinal=3(*(ZP_SF_UPDATE+ZP_SF_USEVCOM)*)): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_EnumMessages(AHandle: THandle; AEnumProc: TZP_NOTIFYPROC;
    AUserData: Pointer): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_Cvt_EnumMessages(AHandle: THandle;
    AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_Ctr_EnumMessages(AHandle: THandle;
    AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}
function ZG_Cvt_GetPortStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult; deprecated; {$IFDEF HAS_INLINE}inline;{$ENDIF}


Const
  ZG_DLL_Name = 'ZGuard.dll';

implementation


function ZG_GetVersion(): Cardinal; stdcall;
        External ZG_DLL_Name name 'ZG_GetVersion';

function ZG_Initialize(AFlags: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Initialize';
function ZG_Finalyze(): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Finalyze';

function ZG_CloseHandle(AHandle: THandle): HResult;
begin
  Result := ZP_CloseHandle(AHandle);
end;
function ZG_GetPortInfoList(var VHandle: THandle; var VCount: Integer): HResult;
begin
  Result := ZP_GetPortInfoList(VHandle, VCount, ZG_DEVTYPE_CVTS);
end;
function ZG_SearchDevices(var VHandle: THandle; var AParams: TZP_SEARCH_PARAMS;
    ASerial, AIP: Boolean): HResult;
begin
  if ASerial then
    AParams.nDevMask := AParams.nDevMask or ZG_DEVTYPE_CVTS;
  if AIP then
    AParams.nIpDevMask := AParams.nIpDevMask or ZG_IPDEVTYPE_CVTS;
  Result := ZP_SearchDevices(VHandle, AParams);
end;
function ZG_FindNextDevice(AHandle: THandle; var VInfo: TZG_ENUM_IPCVT_INFO;
    VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer;
    ATimeout: Cardinal): HResult;
begin
  VInfo.rBase.cbSize := SizeOf(VInfo);
  Result := ZP_FindNextDevice(AHandle, @VInfo, VPortArr, AArrLen, VPortCount, ATimeout);
end;
function ZG_SetNotification(var VHandle: THandle; var ASettings: TZP_DD_NOTIFY_SETTINGS;
    ASerial, AIp: Boolean): HResult;
begin
  if ASerial then
    ASettings.nSDevTypes := ASettings.nSDevTypes or ZG_DEVTYPE_CVTS;
  if AIp then
    ASettings.nIpDevTypes := ASettings.nSDevTypes or ZG_IPDEVTYPE_CVTS;
  Result := ZP_DD_SetNotification(VHandle, ASettings);
end;
function ZG_GetNextMessage(AHandle: THandle; var VMsg: Cardinal;
    var VMsgParam: NativeInt): HResult;
begin
  Result := ZP_DD_GetNextMessage(AHandle, VMsg, VMsgParam);
end;

function ZG_GetProxyConverters(VSnBuf: PWord; ABufSize: Integer;
    var VRCount: Integer; AIpAddr: PWideChar; AActCode: PAnsiChar;
    AWait: PZP_WAIT_SETTINGS): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_GetProxyConverters';
function ZG_UpdateCvtFirmware(AParams: PZG_CVT_OPEN_PARAMS; Const AData; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_UpdateCvtFirmware';

function ZG_Cvt_Open(var VHandle: THandle; AParams: PZG_CVT_OPEN_PARAMS;
    VInfo: PZG_CVT_INFO): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_Open';
function ZG_Cvt_DettachPort(AHandle: THandle; var VPortHandle: THandle): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_DettachPort';
function ZG_Cvt_GetConnectionStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetConnectionStatus';

function ZG_Cvt_SetWaitSettings(AHandle: THandle; Const ASetting: TZP_WAIT_SETTINGS): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_SetWaitSettings';
function ZG_Cvt_GetWaitSettings(AHandle: THandle; var VSetting: TZP_WAIT_SETTINGS): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetWaitSettings';

function ZG_Cvt_SetCapture(AHandle: THandle): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_SetCapture';
function ZG_Cvt_ReleaseCapture(AHandle: THandle): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_ReleaseCapture';
function ZG_Cvt_Clear(AHandle: THandle): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_Clear';
function ZG_Cvt_Send(AHandle: THandle; Const AData; ACount: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_Send';
function ZG_Cvt_Receive(AHandle: THandle; var VBuf; ABufSize: Integer;
    AMinRead: Integer; var VRCount: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_Receive';
function ZG_Cvt_Exec(AHandle: THandle; Const AData; ACount: Integer;
    var VBuf; ABufSize: Integer; AMinRead: Integer; var VRCount: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_Exec';

function ZG_Cvt_EnumControllers(AHandle: THandle;
    AEnumProc: TZG_ENUMCTRSPROC; AUserData: Pointer; AFlags: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_EnumControllers';
function ZG_Cvt_FindController(AHandle: THandle; AAddr: Byte; var VInfo: TZG_FIND_CTR_INFO;
    AFlags: Cardinal; AWait: PZP_WAIT_SETTINGS): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_FindController';
function ZG_Cvt_SearchControllers(AHandle: THandle; AMaxCtrs: Integer=MaxInt; AFlags: Cardinal=0): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_SearchControllers';
function ZG_Cvt_FindNextController(AHandle: THandle; VInfo: PZG_FIND_CTR_INFO): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_FindNextController';

function ZG_Cvt_GetInformation(AHandle: THandle; var VInfo: TZG_CVT_INFO): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetInformation';
function ZG_Cvt_SetNotification(AHandle: THandle; ASettings: PZG_CVT_NOTIFY_SETTINGS): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_SetNotification';
function ZG_Cvt_GetNextMessage(AHandle: THandle; var VMsg: Cardinal;
    var VMsgParam: NativeInt): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetNextMessage';
function ZG_Cvt_GetScanCtrsState(AHandle: THandle; var VNextAddr: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetScanCtrsState';

function ZG_Cvt_UpdateFirmware(AHandle: THandle; Const AData; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_UpdateFirmware';

function ZG_Cvt_GetLicense(AHandle: THandle; ALicN: Byte; var VInfo: TZG_CVT_LIC_INFO): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetLicense';
function ZG_Cvt_SetLicenseData(AHandle: THandle; ALicN: Byte;
    Const AData; ACount: Integer; VLicStatus: PWord): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_SetLicenseData';
function ZG_Cvt_ClearAllLicenses(AHandle: THandle): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_ClearAllLicenses';
function ZG_Cvt_GetAllLicenses(AHandle: THandle; VBuf: PZG_CVT_LIC_SINFO;
    VBufSize: Integer; var VCount: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetAllLicenses';

function ZG_Cvt_GetShortInfo(AHandle: THandle; var VSn: Word; var VMode: TZG_GUARD_MODE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetShortInfo';
function ZG_Cvt_GetLongInfo(AHandle: THandle;
    VSn: PWord; VVersion: PCardinal; VMode: PZG_GUARD_MODE;
    VBuf: PWideChar; ABufSize: Integer; VLen: PInteger): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetLongInfo';
function ZG_Cvt_UpdateCtrFirmware(AHandle: THandle; ASn: Word;
    Const AData; ACount: Integer; AInfoStr: PAnsiChar;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer;
    AModel: TZG_CTR_TYPE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_UpdateCtrFirmware';
function ZG_Cvt_SetCtrAddrBySn(AHandle: THandle; ASn: Word; ANewAddr: Byte;
    AModel: TZG_CTR_TYPE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_SetCtrAddrBySn';
function ZG_Cvt_SetCtrAddr(AHandle: THandle; AOldAddr: Byte; ANewAddr: Byte;
    AModel: TZG_CTR_TYPE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_SetCtrAddr';
function ZG_Cvt_GetCtrInfoNorm(AHandle: THandle; AAddr: Byte; VTypeCode: PByte; VSn: PWord;
    VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal;
    AModel: TZG_CTR_TYPE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetCtrInfoNorm';
function ZG_Cvt_GetCtrInfoAdv(AHandle: THandle; AAddr: Byte; VTypeCode: PByte; VSn: PWord;
    VVersion: PWord; VFlags: PCardinal; VEvWrIdx: PInteger; VEvRdIdx: PInteger): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetCtrInfoAdv';
function ZG_Cvt_GetCtrInfoBySn(AHandle: THandle; ASn: Word; VTypeCode: PByte; VAddr: PByte;
    VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal;
    AModel: TZG_CTR_TYPE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetCtrInfoBySn';
function ZG_Cvt_GetCtrInfoLine(AHandle: THandle; ASn: Word; ALineN: Integer;
    VBuf: PAnsiChar; ABufSize: Integer; VLen: PInteger;
    AModel: TZG_CTR_TYPE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetCtrInfoLine';
function ZG_Cvt_GetCtrVersion(AHandle: THandle; AAddr: Byte; VVerData5: PByte; AWait: PZP_WAIT_SETTINGS): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Cvt_GetCtrVersion';

function ZG_Ctr_Open(var VHandle: THandle; ACvtHandle: THandle; AAddr: Byte; ASn: Word;
    VInfo: PZG_CTR_INFO; AModel: TZG_CTR_TYPE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_Open';
function ZG_Ctr_GetInformation(AHandle: THandle; var VInfo: TZG_CTR_INFO): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetInformation';
function ZG_Ctr_SetNotification(AHandle: THandle; ASettings: PZG_CTR_NOTIFY_SETTINGS): HResult;
        External ZG_DLL_Name name 'ZG_Ctr_SetNotification';
function ZG_Ctr_GetNextMessage(AHandle: THandle; var VMsg: Cardinal;
    var VMsgParam: NativeInt): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetNextMessage';
function ZG_Ctr_SetNewAddr(AHandle: THandle; ANewAddr: Byte): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetNewAddr';
function ZG_Ctr_AssignAddr(AHandle: THandle; AAddr: Byte): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_AssignAddr';
function ZG_Ctr_UpdateFirmware(AHandle: THandle; Const AData; ACount: Integer;
    AInfoStr: PAnsiChar; ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_UpdateFirmware';

function ZG_Ctr_OpenLock(AHandle: THandle; ALockN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_OpenLock';
function ZG_Ctr_CloseLock(AHandle: THandle): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_CloseLock';
function ZG_Ctr_EnableEmergencyUnlocking(AHandle: THandle; AEnable: LongBool): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_EnableEmergencyUnlocking';
function ZG_Ctr_IsEmergencyUnlockingEnabled(AHandle: THandle; var VEnable: LongBool): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_IsEmergencyUnlockingEnabled';

function ZG_Ctr_ReadRegs(AHandle: THandle; AAddr: Cardinal;
    ACount: Integer; var VBuf): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadRegs';
function ZG_Ctr_ReadPorts(AHandle: THandle; var VData: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadPorts';
function ZG_Ctr_ControlDevices(AHandle: THandle; ADevType: Cardinal;
    AActive: LongBool; ATimeMs: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ControlDevices';

function ZG_Ctr_ReadData(AHandle: THandle; ABankN: Integer;
    AAddr: Cardinal; ACount: Integer; var VBuf; 
    var VReaded: Integer; ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadData';
function ZG_Ctr_WriteData(AHandle: THandle; ABankN: Integer;
    AAddr: Cardinal; Const AData; ACount: Integer; 
    var VWritten: Integer; ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_WriteData';

function ZG_Ctr_ReadLockTimes(AHandle: THandle; VOpenMs: PCardinal;
    VLetMs: PCardinal; VMaxMs: PCardinal; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadLockTimes';
function ZG_Ctr_WriteLockTimes(AHandle: THandle; AMask: Cardinal; AOpenMs: Cardinal;
    ALetMs: Cardinal; AMaxMs: Cardinal; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_WriteLockTimes';

function ZG_Ctr_ReadTimeZones(AHandle: THandle; AIdx: Integer;
    VBuf: PZG_CTR_TIMEZONE; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadTimeZones';
function ZG_Ctr_WriteTimeZones(AHandle: THandle; AIdx: Integer;
    ATzs: PZG_CTR_TIMEZONE; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_WriteTimeZones';
function ZG_Ctr_EnumTimeZones(AHandle: THandle; AStart: Integer;
    AEnumProc: TZG_ENUMCTRTIMEZONESPROC; AUserData: Pointer; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_EnumTimeZones';

function ZG_Ctr_ReadKeys(AHandle: THandle; AIdx: Integer;
    VBuf: PZG_CTR_KEY; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadKeys';
function ZG_Ctr_WriteKeys(AHandle: THandle; AIdx: Integer;
    AKeys: PZG_CTR_KEY; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer;
    ABankN: Integer; AUpdateTop: LongBool): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_WriteKeys';
function ZG_Ctr_ClearKeys(AHandle: THandle; AIdx: Integer; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer;
    ABankN: Integer; AUpdateTop: LongBool): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ClearKeys';
function ZG_Ctr_GetKeyTopIndex(AHandle: THandle; var VIdx: Integer; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetKeyTopIndex';
function ZG_Ctr_EnumKeys(AHandle: THandle; AStart: Integer;
    AEnumProc: TZG_ENUMCTRKEYSPROC; AUserData: Pointer; ABankN: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_EnumKeys';

function ZG_Ctr_GetClock(AHandle: THandle; var VClock: TZG_CTR_CLOCK): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetClock';
function ZG_Ctr_SetClock(AHandle: THandle; Const AClock: TZG_CTR_CLOCK): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetClock';

function ZG_Ctr_ReadLastKeyNum(AHandle: THandle; var VNum: TZ_KEYNUM): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadLastKeyNum';
function ZG_Ctr_ReadRTCState(AHandle: THandle; VClock: PZG_CTR_CLOCK;
    VWrIdx: PInteger; VRdIdx: PInteger; VNum: PZ_KEYNUM): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadRTCState';
function ZG_Ctr_ReadEventIdxs(AHandle: THandle; var VWrIdx: Integer; var VRdIdx: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadEventIdxs';
function ZG_Ctr_WriteEventIdxs(AHandle: THandle; AMask: Cardinal; AWrIdx, ARdIdx: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_WriteEventIdxs';
function ZG_Ctr_ReadEvents(AHandle: THandle; AIdx: Integer;
    VBuf: PZG_CTR_EVENT; ACount: Integer;
    ACallback: TZG_PROCESSCALLBACK; AUserData: Pointer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadEvents';
function ZG_Ctr_EnumEvents(AHandle: THandle; AStart, ACount: Integer;
    AEnumProc: TZG_ENUMCTREVENTSPROC; AUserData: Pointer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_EnumEvents';
function ZG_Ctr_DecodePassEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VDirect: TZG_CTR_DIRECT;
    var VKeyIdx: Integer; var VKeyBank: Integer): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_DecodePassEvent';
function ZG_Ctr_DecodeEcEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VSubEvent: TZG_EC_SUB_EV;
    var VPowerFlags: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_DecodeEcEvent';
function ZG_Ctr_DecodeUnkKeyEvent(AHandle: THandle; AData8: PByte;
    var VKeyNum: TZ_KEYNUM): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_DecodeUnkKeyEvent';
function ZG_Ctr_DecodeFireEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VSubEvent: TZG_FIRE_SUB_EV;
    var VFireFlags: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_DecodeFireEvent';
function ZG_Ctr_DecodeSecurEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VSubEvent: TZG_SECUR_SUB_EV;
    var VSecurFlags: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_DecodeSecurEvent';
function ZG_Ctr_DecodeModeEvent(AHandle: THandle; AData8: PByte;
    var VTime: TZG_EV_TIME; var VCurrMode: TZG_CTR_MODE;
    var VSubEvent: TZG_MODE_SUB_EV): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_DecodeModeEvent';
function ZG_DecodeHotelEvent(AData8: PByte;
    var VTime: TZG_EV_TIME; var VMode: TZG_HOTEL_MODE;
    var VSubEvent: TZG_HOTEL_SUB_EV; var VFlags: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_DecodeHotelEvent';

function ZG_Ctr_SetFireMode(AHandle: THandle; AOn: LongBool): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetFireMode';
function ZG_Ctr_GetFireInfo(AHandle: THandle; var VFireFlags: Cardinal;
    var VCurrTemp: Cardinal; var VSrcMask: Cardinal; var VLimitTemp: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetFireInfo';
function ZG_Ctr_SetFireConfig(AHandle: THandle; ASrcMask, ALimitTemp: Cardinal;
    VFireFlags: PCardinal=nil; VCurrTemp: PCardinal=nil): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetFireConfig';

function ZG_Ctr_SetSecurMode(AHandle: THandle; AMode: TZG_SECUR_MODE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetSecurMode';
function ZG_Ctr_GetSecurInfo(AHandle: THandle; var VSecurFlags: Cardinal;
    var VSrcMask: Cardinal; var VAlarmTime: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetSecurInfo';
function ZG_Ctr_SetSecurConfig(AHandle: THandle; ASrcMask, AAlarmTime: Cardinal;
    VSecurFlags: PCardinal=nil): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetSecurConfig';

function ZG_Ctr_SetCtrMode(AHandle: THandle; AMode: TZG_CTR_MODE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetCtrMode';
function ZG_Ctr_GetCtrModeInfo(AHandle: THandle; var VMode: TZG_CTR_MODE;
    var VFlags: Cardinal): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetCtrModeInfo';

function ZG_Ctr_ReadElectroConfig(AHandle: THandle;
    var VConfig: TZG_CTR_ELECTRO_CONFIG): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_ReadElectroConfig';
function ZG_Ctr_WriteElectroConfig(AHandle: THandle;
    Const AConfig: TZG_CTR_ELECTRO_CONFIG; ASetTz: LongBool): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_WriteElectroConfig';

function ZG_Ctr_GetElectroState(AHandle: THandle;
    var VState: TZG_CTR_ELECTRO_STATE): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_GetElectroState';
function ZG_Ctr_SetElectroPower(AHandle: THandle;
    AOn: LongBool): HResult; stdcall;
        External ZG_DLL_Name name 'ZG_Ctr_SetElectroPower';

function ZG_EnumConverters(APorts: PZP_PORT_ADDR; APCount: Integer;
    AEnumProc: TZG_ENUMCVTSPROC; AUserData: Pointer;
    AWait: PZP_WAIT_SETTINGS; AFlags: Cardinal): HResult;
var
  hSearch: THandle;
  rParams: TZp_Search_Params;
  rDI: TZg_Enum_Cvt_Info;
  rPI: TZp_Port_Info;
  nCount: Integer;
begin
  FillChar(rParams, SizeOf(rParams), 0);
  rParams.nDevMask := ZG_DEVTYPE_CVTS;
  if (AFlags and ZP_SF_USEVCOM) <> 0 then
    rParams.nFlags := ZP_SF_USECOM;
  rParams.pPorts := APorts;
  rParams.nPCount := APCount;
  rParams.pWait := AWait;
  Result := ZP_SearchDevices(hSearch, rParams);
  if Failed(Result) then
    Exit;
  try
    repeat
      rDI.rBase.cbSize := SizeOf(rDI);
      Result := ZP_FindNextDevice(hSearch, @rDI, @rPI, 1, nCount);
      if Result <> S_OK then
        break;
      if not AEnumProc(@rDI, rPI, AUserData) then
        Exit(ZP_S_CANCELLED);
    until False;
  finally
    ZG_CloseHandle(hSearch);
  end;
end;

function ZG_EnumIpConverters(AEnumProc: TZG_ENUMIPCVTSPROC; AUserData: Pointer;
    AWait: PZP_WAIT_SETTINGS; AFlags: Cardinal): HResult;
var
  hSearch: THandle;
  rParams: TZp_Search_Params;
  rDI: TZg_Enum_IpCvt_Info;
  aPIs: array[0..1] of TZp_Port_Info;
  nCount, i: Integer;
begin
  FillChar(rParams, SizeOf(rParams), 0);
  rParams.nDevMask := ZG_DEVTYPE_CVTS;
  rParams.pWait := AWait;
  Result := ZP_SearchDevices(hSearch, rParams);
  if Failed(Result) then
    Exit;
  try
    repeat
      rDI.rBase.cbSize := SizeOf(rDI);
      Result := ZP_FindNextDevice(hSearch, @rDI, @aPIs, Length(aPIs), nCount);
      if Result <> S_OK then
        break;
      for i := 0 to nCount - 1 do
        if not AEnumProc(@rDI, aPIs[i], AUserData) then
          Exit(ZP_S_CANCELLED);
    until False;
  finally
    ZG_CloseHandle(hSearch);
  end;
end;

function ZG_FindConverter(APorts: PZP_PORT_ADDR; APCount: Integer;
    var VInfo: TZG_ENUM_CVT_INFO; var VPort: TZP_PORT_INFO;
    AWait: PZP_WAIT_SETTINGS; AFlags: Cardinal): HResult;
var
  hSearch: THandle;
  rParams: TZp_Search_Params;
  rDI: TZg_Enum_Cvt_Info;
  rPI: TZp_Port_Info;
  nCount: Integer;
begin
  FillChar(rParams, SizeOf(rParams), 0);
  rParams.nDevMask := ZG_DEVTYPE_CVTS;
  if (AFlags and ZP_SF_USEVCOM) <> 0 then
    rParams.nFlags := ZP_SF_USECOM;
  rParams.pPorts := APorts;
  rParams.nPCount := APCount;
  rParams.pWait := AWait;
  Result := ZP_SearchDevices(hSearch, rParams);
  if Failed(Result) then
    Exit;
  try
    rDI.rBase.cbSize := SizeOf(rDI);
    Result := ZP_FindNextDevice(hSearch, @rDI, @rPI, 1, nCount);
    if Result <> S_OK then
      Exit;
    VInfo := rDI;
    VPort := rPI;
  finally
    ZG_CloseHandle(hSearch);
  end;
end;
function ZG_EnumMessages(AHandle: THandle; AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult;
begin
  Result := ZP_EnumMessages(AHandle, AEnumProc, AUserData);
end;
function ZG_Cvt_EnumMessages(AHandle: THandle;
    AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult;
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
begin
  repeat
    Result := ZG_Cvt_GetNextMessage(AHandle, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    AEnumProc(nMsg, nMsgParam, AUserData);
  until False;
  if Result = ZP_S_NOTFOUND then
    Result := S_OK;
end;
function ZG_Ctr_EnumMessages(AHandle: THandle;
    AEnumProc: TZP_NOTIFYPROC; AUserData: Pointer): HResult;
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
begin
  repeat
    Result := ZG_Ctr_GetNextMessage(AHandle, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    AEnumProc(nMsg, nMsgParam, AUserData);
  until False;
  if Result = ZP_S_NOTFOUND then
    Result := S_OK;
end;
function ZG_Cvt_GetPortStatus(AHandle: THandle; var VValue: TZP_CONNECTION_STATUS): HResult;
begin
  Result := ZG_Cvt_GetConnectionStatus(AHandle, VValue);
end;

end.

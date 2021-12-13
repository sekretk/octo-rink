unit ZGClasses;

interface

uses
  Windows, SysUtils, Classes, Messages, Forms, Types,
  ZBase, ZGuard, ZPort, ZPClasses;

Type
  EZGError = class(EZPError)
  end;

  TZCvtCtrChangeEvent = procedure(ASender: TObject; Const AInfo: TZG_Find_Ctr_Info) of object;
  TCvtScanOption = (
    csoDblCheck,      // Дважды проверять отключение контроллеров
    csoReassignAddrs, // Автоматическое переназначение адресов
    csoAltScan,       // Альтернативный метод скарирования
    csoNoGate,        // Не сканировать GATE-контроллеры (все, кроме Eurolock)
    csoNoEurolock     // Не сканировать Eurolock EHT net
  );
  TCvtScanOptions = set of TCvtScanOption;

  TZConverter = class
  protected
    FPortName       : String;
    FPortType       : TZP_Port_Type;
    FReqTimeout     : Cardinal;
    FReqMaxTries      : Integer;
    FCvtType        : TZG_Cvt_TYPE;
    FSpeed          : TZG_Cvt_Speed;
    FGuardLicN      : Integer;
    FProxyActCode   : String;
    FProxyCvtSn     : Integer;
    FScanCtrsPeriod : Cardinal;
    FScanCtrsLastAddr: Integer;
    FScanOptions    : TCvtScanOptions;
    FNotifications  : Boolean;
    FOnCtrInsert    : TZCvtCtrChangeEvent;
    FOnCtrRemove    : TZCvtCtrChangeEvent;
    FOnError        : TZpErrorEvent;
    FOnConnectChange: TNotifyEvent;
    m_hCvt          : THandle;
    m_hWindow       : THandle;
  private
    procedure Open();
    procedure Close();
  public
    constructor Create(); overload;
    constructor Create(Const APortName: String; APortType: TZP_Port_Type); overload;
    destructor Destroy(); override;

    // Подключается к конвертеру, используя дескриптор порта, полученный функцией ZP_Open
    procedure AttachPort(APortHandle: THandle;
        APortType: TZP_Port_Type; VInfo: PZG_Cvt_Info=nil);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Отключается от конвертера, не закрывая порт, возвращает дескриптор порта, полученный функцией ZP_Open
    function DettachPort(): THandle;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    function GetConnectionStatus(): TZp_Connection_Status;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Устанавливает параметры ожидания исполнения функции
    procedure SetWaitSettings(Const ASetting: TZP_Wait_Settings);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Возвращает параметры ожидания исполнения функции
    procedure GetWaitSettings(var VSetting: TZP_Wait_Settings);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Захватить порт (приостанавливает фоновые операции с портом)
    procedure SetCapture();{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Отпустить порт (возобновляет фоновые операции с портом)
    procedure ReleaseCapture();{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Очищает порт конвертера
    procedure Clear();{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Отправляет запрос конвертеру
    procedure Send(Const ABuf; ACount: Integer);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Возвращает ответ от конвертера
    function Receive(var VBuf; ABufSize, AMinRead: Integer): Integer;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Отправляет запрос и возвращает ответ от конвертера
    function Exec(Const AData; ACount: Integer;
        var VBuf; ABufSize, AMinRead: Integer): Integer;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Перечисляет все подключенные контроллеры (только Guard)
    procedure EnumControllers(AEnumProc: TZG_EnumCtrsProc; AUserData: Pointer;
        AFlags: Cardinal=ZG_F_UPDATE);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Ищет контроллеры по сетевому адресу
    function FindController(AAddr: Byte; var VInfo: TZG_Find_Ctr_Info;
        AFlags: Cardinal=ZG_F_UPDATE; AWait: PZP_Wait_Settings=nil): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Возвращает информацию о конвертере
    procedure GetInformation(var VInfo: TZG_Cvt_Info);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Настраивает уведомления от конвертера
    procedure SetNotification(ASettings: PZG_Cvt_Notify_Settings);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure GetScanCtrsState(var VNextAddr: Integer);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Устанавливает в конвертер новую прошивку
    procedure UpdateFirmware(Const AData; ACount: Integer;
        ACallback: TZG_ProcessCallback; AParam: Pointer);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Возвращает информацию о лицензии конвертера
    procedure GetLicense(ALicN: Cardinal; var VInfo: TZG_Cvt_Lic_Info);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Устанавливает новую лицензию
    procedure SetLicenseData(ALicN: Cardinal;
        Const AData; ACount: Integer; VLicStatus: PWord=nil);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Очищает все лицензии в конвертере
    procedure ClearAllLicenses();{$IFDEF HAS_INLINE}inline;{$ENDIF}
    function GetAllLicenses(VBuf: PZG_Cvt_Lic_SInfo; VBufSize: Integer): Integer;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить с/н конвертера и режим (только Guard)
    procedure GetShortInfo(var VSn: Word; var VMode: TZG_Guard_Mode);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Получить информационные строки конвертера (только Guard)
    procedure GetLongInfo(VSn: PWord; VVersion: PCardinal; VMode: PZG_Guard_Mode;
        VBuf: PWideChar; ABufSize: Integer; VLen: PInteger);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Установить в контроллер новую прошивку
    procedure UpdateCtrFirmware(ASn: Word;
        Const AData; ACount: Integer; AInfoStr: PAnsiChar;
        ACallback: TZG_ProcessCallback; AUserData: Pointer; AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Установить новый сетевой адрес контроллера по с/н (в Advanced режиме не работает)
    procedure SetCtrAddrBySn(ASn: Word; ANewAddr: Byte; AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure SetCtrAddr(AOldAddr: Byte; ANewAddr: Byte; AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить версию контроллера
    procedure GetCtrVersion(AAddr: Byte; VData5: PByte; AWS: PZP_Wait_Settings=nil);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Запросить информацию контроллера по сетевому адресу
    procedure GetCtrInfoNorm(AAddr: Byte; VTypeCode: PByte; VSn: PWord;
        VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal;
        AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Запросить информацию контроллера по сетевому адресу
    procedure GetCtrInfoAdv(AAddr: Byte; VTypeCode: PByte; VSn: PWord;
        VVersion: PWord; VFlags: PCardinal; VEvWrIdx: PInteger; VEvRdIdx: PInteger);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Запросить информацию контроллера по заводскому номеру
    procedure GetCtrInfoBySn(ASn: Word; VTypeCode: PByte; VAddr: PByte;
        VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal;
        AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    function GetCtrInfoLine(ASn: Word; ALineN: Integer;
        VBuf: PAnsiChar; ABufSize: Integer;
        AModel: TZG_CTR_TYPE=ZG_CTR_UNDEF): Integer;{$IFDEF HAS_INLINE}inline;{$ENDIF}

  protected
    procedure WindowMethod(var Message: TMessage);
    procedure CmZGuard();
    procedure EnableNotifications(AEnable: Boolean;
        APeriod: Cardinal; ALastAddr: Integer; AOptions: TCvtScanOptions);
    class function ScanOptionsToMask(AOptions: TCvtScanOptions): Cardinal; static;

    procedure DoOnCtrInsert(Const AInfo: TZG_Find_Ctr_Info);
    procedure DoOnCtrRemove(Const AInfo: TZG_Find_Ctr_Info);
    procedure DoOnError(AErrCode: HResult);
    procedure DoOnConnectChange();
    function GetActive(): Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetNotifications(AValue: Boolean);
    procedure SetScanCtrsPeriod(AValue: Cardinal);
    procedure SetScanCtrsLastAddr(AValue: Integer);
    procedure SetScanOptions(AValue: TCvtScanOptions);
  public
    property Active: Boolean read GetActive write SetActive;

    property ScanCtrsPeriod: Cardinal read FScanCtrsPeriod write SetScanCtrsPeriod;
    property ScanCtrsLastAddr: Integer read FScanCtrsLastAddr write SetScanCtrsLastAddr;
    property ScanOptions: TCvtScanOptions read FScanOptions write SetScanOptions;
    property Notifications: Boolean read FNotifications write SetNotifications;
    property OnCtrInsert: TZCvtCtrChangeEvent read FOnCtrInsert write FOnCtrInsert;
    property OnCtrRemove: TZCvtCtrChangeEvent read FOnCtrRemove write FOnCtrRemove;
    property OnError: TZpErrorEvent read FOnError write FOnError;
    property OnConnectionChange: TNotifyEvent read FOnConnectChange write FOnConnectChange;

    property PortName: String read FPortName write FPortName;
    property PortType: TZP_Port_Type read FPortType write FPortType;
    property ReqTimeout: Cardinal read FReqTimeout write FReqTimeout;
    property ReqMaxTries: Integer read FReqMaxTries write FReqMaxTries;
    property CvtType: TZG_Cvt_Type read FCvtType write FCvtType;
    property Speed: TZG_Cvt_Speed read FSpeed write FSpeed;
    property GuardLicN: Integer read FGuardLicN write FGuardLicN;
    property ProxyActCode: String read FProxyActCode write FProxyActCode;
    property ProxyCvtSn: Integer read FProxyCvtSn write FProxyCvtSn;
    property Handle: THandle read m_hCvt;
  end;

  TZCtrNewEventEvent = procedure(ASender: TObject; Const AInfo: TZG_N_New_Event_Info) of object;
  TZCtrClockDesyncEvent = procedure(ASender: TObject; Const ADesyncSec: Int64) of object;
  TZCtrKeyTopChangeEvent = procedure(ASender: TObject; Const AInfo: TZG_N_Key_Top_Info) of object;

  TZCtrNtf = (cnNewEvent, cnClockDesync, cnKeyTopChange);
  TZCtrNtfs = set of TZCtrNtf;

  TZController = class
  protected
    FOnNewEvent     : TZCtrNewEventEvent;
    FOnClockDesync  : TZCtrClockDesyncEvent;
    FOnKeyTopChange : TZCtrKeyTopChangeEvent;
    FCvtH           : THandle;
    FAddr           : Integer;
    FSn             : Integer;
    FCtrType        : TZg_Ctr_Type;
    FReadEvIdx      : Integer;
    m_hCtr          : THandle;
    m_nNotifications: TZCtrNtfs;
    m_hWindow       : THandle;
  private
    procedure Open();
    procedure Close();
  public
    constructor Create();
    destructor Destroy(); override;

    // Возвращает информацию о контроллере
    procedure GetInformation(var VInfo: TZG_Ctr_Info);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Настраивает уведомления от контроллера (если = nil, отключить все уведомления)
    procedure SetNotification(ASettings: PZG_CTR_Notify_Settings);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Установка сетевого адреса контроллера (Guard не поддерживает)
    procedure SetNewAddr(ANewAddr: Byte);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Связать дескриптор контроллера с новым сетевым адресом контроллера
    procedure AssignAddr(AAddr: Byte);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Установка новой прошивки контроллеру
    procedure UpdateFirmware(Const AData; ACount: Integer;
        ACallback: TZG_ProcessCallback; AUserData: Pointer);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Открыть замок
    procedure OpenLock(ALockN: Integer);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Обесточить замки
    procedure CloseLock();{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Включить режим аварийного открывания дверей
    procedure EnableEmergencyUnlocking(AEnable: Boolean);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Определить состояние режима аварийного открывания дверей
    function IsEmergencyUnlockingEnabled(): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Считать регистры контроллера
    procedure ReadRegs(AAddr: Cardinal;
        ACount: Integer; var VBuf);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Считать состояние портов
    function ReadPorts(): Cardinal;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Управление внешними устройствами (nDevType - адрес устройства ZG_DEV_..)
    procedure ControlDevices(ADevType: Cardinal;
        AActive: Boolean; ATimeMs: Cardinal=0);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Чтение / запись памяти контроллера (nBankN - Адрес на шине: =0 Вход, =1 Выход, =2 События
    procedure ReadData(ABankN: Integer;
        AAddr: Cardinal; ACount: Integer; var VBuf);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure WriteData(ABankN: Integer;
        AAddr: Cardinal; Const ABuf; ACount: Integer);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить / Установить времена для дверей контроллера
    procedure ReadLockTimes(VOpenMs: PCardinal;
        VLetMs: PCardinal; VMaxMs: PCardinal; ABankN: Integer=0);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure WriteLockTimes(AMask: Cardinal; AOpenMs: Cardinal;
        ALetMs: Cardinal; AMaxMs: Cardinal; ABankN: Integer=0);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить / Записать одну или несколько временных зон
    function ReadTimeZones(AIdx: Integer;
        VBuf: PZG_Ctr_TimeZone; ACount: Integer;
        ACallback: TZG_ProcessCallback; AUserData: Pointer; ABankN: Integer=0): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    function WriteTimeZones(AIdx: Integer;
        ATzs: PZG_Ctr_TimeZone; ACount: Integer;
        ACallback: TZG_ProcessCallback; AUserData: Pointer; ABankN: Integer=0): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Перечислить временные зоны в контроллере
    function EnumTimeZones(AStart: Integer;
        AEnumProc: TZG_EnumCtrTimeZonesProc; AUserData: Pointer; ABankN: Integer=0): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить / Записать / Удалить один или несколько ключей
    function ReadKeys(AIdx: Integer;
        VBuf: PZG_Ctr_Key; ACount: Integer;
        ACallback: TZG_ProcessCallback; AUserData: Pointer; ABankN: Integer=0): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    function WriteKeys(AIdx: Integer;
        AKeys: PZG_Ctr_Key; ACount: Integer;
        ACallback: TZG_ProcessCallback; AUserData: Pointer;
        ABankN: Integer=0; AUpdateTop: Boolean=True): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    function ClearKeys(AIdx: Integer; ACount: Integer;
        ACallback: TZG_ProcessCallback; AUserData: Pointer; ABankN: Integer=0;
        AUpdateTop: Boolean=True): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Получить индекс верхней границы ключей
    function GetKeyTopIndex(ABankN: Integer=0): Integer;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Перечислить ключи в контроллере
    function EnumKeys(AStart: Integer;
        AEnumProc: TZG_EnumCtrKeysProc; AUserData: Pointer; ABankN: Integer=0): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить часы контроллера
    procedure GetClock(var VClock: TZG_Ctr_Clock);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Установить часы контроллера
    procedure SetClock(Const AClock: TZG_Ctr_Clock);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить номер последнего поднесенного ключа
    procedure ReadLastKeyNum(var VNum: TZ_KeyNum);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Получить указатели событий и номер последнего поднесенного ключа (если *pWrIdx или *pRdIdx == -1, то указатель некорректный)
    procedure ReadRTCState(VClock: PZG_CTR_CLOCK;
        VWrIdx: PInteger; VRdIdx: PInteger; VNum: PZ_KeyNum);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Получить указатели событий
    procedure ReadEventIdxs(var VWrIdx: Integer; var VRdIdx: Integer);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Установить указатели событий
    procedure WriteEventIdxs(AMask: Cardinal; AWrIdx, ARdIdx: Integer);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Получить одно или несколько событий
    function ReadEvents(AIdx: Integer;
        VBuf: PZG_Ctr_Event; ACount: Integer;
        ACallback: TZG_ProcessCallback; AUserData: Pointer): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Перечисляет события контроллера
    // AStart - индекс первого события, если =-1, используется указатель чтения,
    // ACount - количество перечисляемых событий, если =-1, то используется количество событий с nStart до указателя чтения,
    //			если =MAXINT, то перечисляются все события
    function EnumEvents(AStart, ACount: Integer;
        AEnumProc: TZG_EnumCtrEventsProc; AUserData: Pointer): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Декодирование событий прохода
    procedure DecodePassEvent(Const AData8;
        var VTime: TZG_Ev_Time; var VDirect: TZG_Ctr_Direct;
        var VKeyIdx: Integer; var VKeyBank: Integer);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Декодирование событий ElectoControl: ZG_EV_ELECTRO_ON, ZG_EV_ELECTRO_OFF
    procedure DecodeEcEvent(Const AData8;
        var VTime: TZG_Ev_Time; var VSubEvent: TZG_EC_Sub_Ev;
        var VPowerFlags: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Декодирование события со значением ключа: ZG_EV_UNKNOWN_KEY
    procedure DecodeUnkKeyEvent(Const AData8;
        var VKeyNum: TZ_KeyNum);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Декодирование события ZG_EV_FIRE_STATE (Изменение состояния Пожара)
    procedure DecodeFireEvent(Const AData8;
        var VTime: TZG_Ev_Time; var VSubEvent: TZG_Fire_Sub_Ev;
        var VFireFlags: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Декодирование события ZG_EV_SECUR_STATE (Изменение состояния Охрана)
    procedure DecodeSecurEvent(Const AData8;
        var VTime: TZG_Ev_Time; var VSubEvent: TZG_Secur_Sub_Ev;
        var VSecurFlags: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Декодирование события ZG_EV_MODE_STATE (Изменение состояния Режим)
    procedure DecodeModeEvent(Const AData8;
        var VTime: TZG_Ev_Time; var VMode: TZG_Ctr_Mode;
        var VSubEvent: TZG_Mode_Sub_Ev);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    class procedure DecodeHotelEvent(Const AData8;
        var VTime: TZG_Ev_Time; var VMode: TZG_Hotel_Mode;
        var VSubEvent: TZG_Hotel_Sub_Ev; var VFlags: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Управление пожарным режимом по сети
    procedure SetFireMode(AOn: Boolean);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Запрос состояния пожарного режима
    procedure GetFireInfo(var VFireFlags: Cardinal;
        var VCurrTemp: Cardinal; var VSrcMask: Cardinal; var VLimitTemp: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Установка параметров пожарного режима
    procedure SetFireConfig(ASrcMask, ALimitTemp: Cardinal;
        VFireFlags: PCardinal=nil; VCurrTemp: PCardinal=nil);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Управление режимом Охрана по сети
    procedure SetSecurMode(AMode: TZG_Secur_Mode);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Запрос состояния режима Охрана
    procedure GetSecurInfo(var VSecurFlags: Cardinal;
        var VSrcMask: Cardinal; var VAlarmTime: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Установка параметров режима Охрана
    procedure SetSecurConfig(ASrcMask, AAlarmTime: Cardinal;
        VSecurFlags: PCardinal=nil);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    // Управление режимом контроллера
    procedure SetCtrMode(AMode: TZG_Ctr_Mode);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure GetCtrModeInfo(var VMode: TZG_Ctr_Mode; var VFlags: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    // Для спец.прошики Matrix II Net
    procedure ReadElectroConfig(var VConfig: TZG_Ctr_Electro_Config);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure WriteElectroConfig(Const AConfig: TZG_Ctr_Electro_Config);{$IFDEF HAS_INLINE}inline;{$ENDIF}

    procedure GetElectroState(var VState: TZG_Ctr_Electro_State);{$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure SetElectroPower(AOn: Boolean);{$IFDEF HAS_INLINE}inline;{$ENDIF}

  protected
    procedure WindowMethod(var Message: TMessage);
    procedure CmZGuard();
    procedure EnableNotifications(AValue: TZCtrNtfs; AReadEvIdx: Integer);

    procedure DoOnNewEvent(Const AInfo: TZG_N_New_Event_Info);
    procedure DoOnClockDesync(Const ADiffSec: Int64);
    procedure DoOnKeyTopChange(Const AInfo: TZG_N_Key_Top_Info);
    procedure SetReadEvIdx(AValue: Integer);
    procedure SetNotifications(AValue: TZCtrNtfs);
    function GetActive(): Boolean;
    procedure SetActive(AValue: Boolean);
  public
    property ReadEvIdx: Integer read FReadEvIdx write SetReadEvIdx;
    property Notifications: TZCtrNtfs read m_nNotifications write SetNotifications;
    property OnNewEvent: TZCtrNewEventEvent read FOnNewEvent write FOnNewEvent;
    property OnClockDesync: TZCtrClockDesyncEvent read FOnClockDesync write FOnClockDesync;
    property OnKeyTopChange: TZCtrKeyTopChangeEvent read FOnKeyTopChange write FOnKeyTopChange;
    property Active: Boolean read GetActive write SetActive;
    property CvtH: THandle read FCvtH write FCvtH;
    property Addr: Integer read FAddr write FAddr;
    property Sn: Integer read FSn write FSn;
    property CtrType: TZg_Ctr_Type read FCtrType write FCtrType;
    property Handle: THandle read m_hCtr;
  end;

  TCvtDetector = class(TZPDetector)
  public
    constructor Create();
  end;

// Возвращает True, если версия DLL совместимая
function CheckZGVersion(): Boolean;{$IFDEF HAS_INLINE}inline;{$ENDIF}
// Проверяет код результата функции. Если ошибка, генерирует исключение
procedure CheckZGError(AErrCode: HResult);
// Возвращает текст ошибки по коду
function GetZGErrorText(AErrCode: HResult): String;

// Инициализация DLL
procedure ZGInitialize(AFlags: Cardinal);{$IFDEF HAS_INLINE}inline;{$ENDIF}
// Завершение DLL
procedure ZGFinalyze();{$IFDEF HAS_INLINE}inline;{$ENDIF}

// Возвращает список с/н конвертеров, подключенных к Proxy-серверу
function GetProxyConverters(var VSNs: TWordDynArray; Const AIpAddr, AActCode: String;
    AWait: PZP_Wait_Settings=nil): Integer;

// Устаревшие функции
procedure EnumSerialPorts(AEnumProc: TZP_EnumPortsProc; AUserData: Pointer); deprecated;
procedure EnumConverters(AEnumProc: TZG_EnumCvtsProc; AUserData: Pointer;
    APorts: PZP_Port_Addr=nil; APCount: Integer=0;
    AWait: PZP_Wait_Settings=nil; AFlags: Cardinal=3); deprecated;
function FindConverter(var VInfo: TZG_Enum_Cvt_Info; var VPort: TZP_Port_Info;
    APorts: PZP_Port_Addr=nil; APCount: Integer=0;
    AWait: PZP_Wait_Settings=nil; AFlags: Cardinal=3): Boolean; deprecated;
procedure EnumIpConverters(AEnumProc: TZG_EnumIpCvtsProc; AUserData: Pointer;
    AWait: PZP_Wait_Settings=nil; AFlags: Cardinal=1); deprecated;

implementation

Const
  CM_ZGUARD  = (WM_USER + 1);


constructor TZConverter.Create();
begin
  inherited Create();
  FScanCtrsPeriod := 5000;
  FScanCtrsLastAddr := 255;
  CheckZGError(ZG_Initialize(0));
end;

constructor TZConverter.Create(Const APortName: String; APortType: TZP_Port_Type);
begin
  inherited Create();
  FPortName := APortName;
  FPortType := APortType;
  FScanCtrsPeriod := 5000;
  FScanCtrsLastAddr := 255;
  CheckZGError(ZG_Initialize(0));
end;

destructor TZConverter.Destroy();
begin
  Close();
  ZG_Finalyze();
  if m_hWindow <> 0 then
    DeallocateHWnd(m_hWindow);
  Inherited Destroy();
end;

function TZConverter.GetActive(): Boolean;
begin
  Result := (m_hCvt <> 0);
end;

procedure TZConverter.DoOnCtrInsert(Const AInfo: TZG_Find_Ctr_Info);
begin
  if Assigned(FOnCtrInsert) then
    FOnCtrInsert(Self, AInfo);
end;

procedure TZConverter.DoOnCtrRemove(Const AInfo: TZG_Find_Ctr_Info);
begin
  if Assigned(FOnCtrRemove) then
    FOnCtrRemove(Self, AInfo);
end;

procedure TZConverter.DoOnError(AErrCode: HResult);
begin
  if Assigned(FOnError) then
    FOnError(Self, AErrCode);
end;

procedure TZConverter.DoOnConnectChange();
begin
  if Assigned(FOnConnectChange) then
    FOnConnectChange(Self);
end;

procedure TZConverter.Open();
var
  rOpen: TZG_Cvt_Open_Params;
  sCode: AnsiString;
  rWS: TZp_Wait_Settings;
  szName: array[0..ZP_MAX_PORT_NAME] of WideChar;
begin
  try
    FillChar(rOpen, SizeOf(rOpen), 0);
    StringToWideChar(FPortName, szName, Length(szName));
    rOpen.pszName := szName;
    rOpen.nType := FPortType;
    rOpen.nCvtType := FCvtType;
    rOpen.nSpeed := FSpeed;
    rOpen.nLicN := FGuardLicN;
    FillChar(rWS, SizeOf(rWS), 0);
    if FReqMaxTries <> 0 then
      rWS.nMaxTries := FReqMaxTries
    else
      rWS.nMaxTries := 1;
    if FReqTimeout <> 0 then
      rWS.nReplyTimeout := FReqTimeout
    else
      rWS.nReplyTimeout := 1000;
    if FPortType in [ZP_PORT_COM, ZP_PORT_FT] then
      rWS.nCheckPeriod := 50
    else
      rWS.nCheckPeriod := INFINITE;
    rWS.nRestorePeriod := 1000;
    rOpen.pWait := @rWS;
    if FPortType = ZP_PORT_IP then
    begin
      sCode := AnsiString(FProxyActCode);
      rOpen.pActCode := PAnsiChar(sCode);
      rOpen.nSn := FProxyCvtSn;
    end;
    CheckZGError(ZG_Cvt_Open(m_hCvt, @rOpen));
    if FNotifications then
      EnableNotifications(True, FScanCtrsPeriod, FScanCtrsLastAddr, FScanOptions);
  except
    Close();
    raise;
  end;
end;

procedure TZConverter.Close();
begin
  if m_hCvt = 0 then
    Exit;
  CheckZGError(ZG_CloseHandle(m_hCvt));
  m_hCvt := 0;
  if m_hWindow <> 0 then
  begin
    DeallocateHWnd(m_hWindow);
    m_hWindow := 0;
  end;
end;

procedure TZConverter.SetActive(AValue: Boolean);
begin
  if AValue = (m_hCvt <> 0) then
    exit;
  if AValue then Open() else Close();
end;

procedure TZConverter.SetScanCtrsPeriod(AValue: Cardinal);
begin
  if FScanCtrsPeriod = AValue then
    Exit;
  if (m_hCvt <> 0) and FNotifications then
    EnableNotifications(True, AValue, FScanCtrsLastAddr, FScanOptions);
  FScanCtrsPeriod := AValue;
end;

procedure TZConverter.SetScanCtrsLastAddr(AValue: Integer);
begin
  if FScanCtrsLastAddr = AValue then
    Exit;
  if (m_hCvt <> 0) and FNotifications then
    EnableNotifications(True, FScanCtrsPeriod, AValue, FScanOptions);
  FScanCtrsLastAddr := AValue;
end;

procedure TZConverter.SetScanOptions(AValue: TCvtScanOptions);
begin
  if FScanOptions = AValue then
    Exit;
  if (m_hCvt <> 0) and FNotifications then
    EnableNotifications(True, FScanCtrsPeriod, FScanCtrsLastAddr, AValue);
  FScanOptions := AValue;
end;

class function TZConverter.ScanOptionsToMask(AOptions: TCvtScanOptions): Cardinal;
begin
  Result := 0;
  if csoDblCheck in AOptions then Result := Result or ZG_NF_CVT_CTR_DBL_CHECK;
  if csoReassignAddrs in AOptions then Result := Result or ZG_NF_CVT_REASSIGN_ADDRS;
  if csoAltScan in AOptions then Result := Result or ZG_NF_CVT_ALT_SCAN;
  if csoNoGate in AOptions then Result := Result or ZG_NF_CVT_NOGATE;
  if csoNoEurolock in AOptions then Result := Result or ZG_NF_CVT_NOEUROLOCK;
end;

procedure TZConverter.CmZGuard();
var
  hr: HResult;
  nMsg: Cardinal;
  nMsgParam: NativeInt;
begin
  repeat
    hr := ZG_Cvt_GetNextMessage(m_hCvt, nMsg, nMsgParam);
    if hr <> S_OK then
      break;
    case nMsg of
      ZG_N_CVT_CTR_INSERT:        DoOnCtrInsert(PZG_Find_Ctr_Info(nMsgParam)^);
      ZG_N_CVT_CTR_REMOVE:        DoOnCtrRemove(PZG_Find_Ctr_Info(nMsgParam)^);
      ZG_N_CVT_ERROR:             DoOnError(HResult(Pointer(nMsgParam)^));
      ZG_CVTN_CONNECTION_CHANGE:  DoOnConnectChange();
    end;
  until False;
end;

procedure TZConverter.WindowMethod(var Message: TMessage);
begin
  with Message do
    if Msg = CM_ZGUARD then
      try
        CmZGuard();
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(m_hWindow, Msg, wParam, lParam);
end;

procedure TZConverter.EnableNotifications(AEnable: Boolean;
    APeriod: Cardinal; ALastAddr: Integer; AOptions: TCvtScanOptions);
var
  rNS: TZG_Cvt_Notify_Settings;
begin
  ASSERT(m_hCvt <> 0);
  if AEnable then
  begin
    if m_hWindow = 0 then
      m_hWindow := AllocateHWnd(WindowMethod);
    FillChar(rNS, SizeOf(rNS), 0);
    rNS.nNMask := ZG_NF_CVT_CTR_EXIST;
    if Assigned(FOnError) then
      Inc(rNS.nNMask, ZG_NF_CVT_ERROR);
    if Assigned(FOnConnectChange) then
      Inc(rNS.nNMask, ZG_CVTNF_CONNECTION_CHANGE);
    rNS.nNMask := rNS.nNMask or ScanOptionsToMask(AOptions);
    rNS.hWindow := m_hWindow;
    rNS.nWndMsgId := CM_ZGUARD;
    rNS.nScanCtrsPeriod := APeriod;
    rNS.nScanCtrsLastAddr := ALastAddr;
    CheckZGError(ZG_Cvt_SetNotification(m_hCvt, @rNS));
  end
  else
  begin
    ZG_Cvt_SetNotification(m_hCvt, nil);
    if m_hWindow <> 0 then
    begin
      DeallocateHWnd(m_hWindow);
      m_hWindow := 0;
    end;
  end;
end;

procedure TZConverter.SetNotifications(AValue: Boolean);
begin
  if AValue = FNotifications then
    Exit;
  if m_hCvt <> 0 then
    EnableNotifications(AValue, FScanCtrsPeriod, FScanCtrsLastAddr, FScanOptions);
  FNotifications := AValue;
end;

procedure TZConverter.AttachPort(APortHandle: THandle;
    APortType: TZP_Port_Type; VInfo: PZG_Cvt_Info);
var
  rOpen: TZG_Cvt_Open_Params;
begin
  ASSERT(m_hCvt = 0);
  try
    FillChar(rOpen, SizeOf(rOpen), 0);
    rOpen.nType := APortType;
    rOpen.hPort := APortHandle;
    rOpen.nCvtType := FCvtType;
    rOpen.nSpeed := FSpeed;
    rOpen.nLicN := FGuardLicN;
    CheckZGError(ZG_Cvt_Open(m_hCvt, @rOpen, VInfo));
    FPortType := APortType;
    if FNotifications then
      EnableNotifications(True, FScanCtrsPeriod, FScanCtrsLastAddr, FScanOptions);
  except
    DettachPort();
    raise;
  end;
end;

function TZConverter.DettachPort(): THandle;
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_DettachPort(m_hCvt, Result));
  m_hCvt := 0;
end;

function TZConverter.GetConnectionStatus(): TZp_Connection_Status;
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetConnectionStatus(m_hCvt, Result));
end;

procedure TZConverter.SetWaitSettings(Const ASetting: TZP_Wait_Settings);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_SetWaitSettings(m_hCvt, ASetting));
end;

procedure TZConverter.GetWaitSettings(var VSetting: TZP_Wait_Settings);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetWaitSettings(m_hCvt, VSetting));
end;

procedure TZConverter.SetCapture();
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_SetCapture(m_hCvt));
end;

procedure TZConverter.ReleaseCapture();
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_ReleaseCapture(m_hCvt));
end;

procedure TZConverter.Clear();
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_Clear(m_hCvt));
end;

procedure TZConverter.Send(Const ABuf; ACount: Integer);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_Send(m_hCvt, ABuf, ACount));
end;

function TZConverter.Receive(var VBuf; ABufSize, AMinRead: Integer): Integer;
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_Receive(m_hCvt, VBuf, ABufSize, AMinRead, Result));
end;

function TZConverter.Exec(Const AData; ACount: Integer;
    var VBuf; ABufSize, AMinRead: Integer): Integer;
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_Exec(m_hCvt, AData, ACount, VBuf, ABufSize, AMinRead, Result));
end;

procedure TZConverter.EnumControllers(AEnumProc: TZG_EnumCtrsProc; AUserData: Pointer;
    AFlags: Cardinal);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_EnumControllers(m_hCvt, AEnumProc, AUserData, AFlags));
end;

function TZConverter.FindController(AAddr: Byte; var VInfo: TZG_Find_Ctr_Info;
    AFlags: Cardinal; AWait: PZP_Wait_Settings): Boolean;
var
  nRet: Integer;
begin
  ASSERT(m_hCvt <> 0);
  nRet := ZG_Cvt_FindController(m_hCvt, AAddr, VInfo, AFlags, AWait);
  if nRet = ZP_S_NOTFOUND then
  begin
    Result := False;
    Exit;
  end;
  CheckZGError(nRet);
  Result := True;
end;

procedure TZConverter.GetInformation(var VInfo: TZG_Cvt_Info);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetInformation(m_hCvt, VInfo));
end;

procedure TZConverter.SetNotification(ASettings: PZG_Cvt_Notify_Settings);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_SetNotification(m_hCvt, ASettings));
end;

procedure TZConverter.GetScanCtrsState(var VNextAddr: Integer);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetScanCtrsState(m_hCvt, VNextAddr));
end;

procedure TZConverter.UpdateFirmware(Const AData; ACount: Integer;
    ACallback: TZG_ProcessCallback; AParam: Pointer);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_UpdateFirmware(m_hCvt, AData, ACount, ACallback, AParam));
end;

procedure TZConverter.GetLicense(ALicN: Cardinal; var VInfo: TZG_Cvt_Lic_Info);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetLicense(m_hCvt, ALicN, VInfo));
end;

procedure TZConverter.SetLicenseData(ALicN: Cardinal;
    Const AData; ACount: Integer; VLicStatus: PWord);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_SetLicenseData(m_hCvt, ALicN, AData, ACount, VLicStatus));
end;

procedure TZConverter.ClearAllLicenses();
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_ClearAllLicenses(m_hCvt));
end;

function TZConverter.GetAllLicenses(VBuf: PZG_Cvt_Lic_SInfo; VBufSize: Integer): Integer;
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetAllLicenses(m_hCvt, VBuf, VBufSize, Result));
end;

procedure TZConverter.GetShortInfo(var VSn: Word; var VMode: TZG_Guard_Mode);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetShortInfo(m_hCvt, VSn, VMode));
end;

procedure TZConverter.GetLongInfo(VSn: PWord; VVersion: PCardinal; VMode: PZG_Guard_Mode;
    VBuf: PWideChar; ABufSize: Integer; VLen: PInteger);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetLongInfo(m_hCvt, VSn, VVersion, VMode, VBuf, ABufSize, VLen));
end;

procedure TZConverter.UpdateCtrFirmware(ASn: Word;
    Const AData; ACount: Integer; AInfoStr: PAnsiChar;
    ACallback: TZG_ProcessCallback; AUserData: Pointer; AModel: TZG_CTR_TYPE);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_UpdateCtrFirmware(m_hCvt, ASn, AData, ACount, AInfoStr, ACallback, AUserData, AModel));
end;

procedure TZConverter.SetCtrAddrBySn(ASn: Word; ANewAddr: Byte; AModel: TZG_CTR_TYPE);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_SetCtrAddrBySn(m_hCvt, ASn, ANewAddr, AModel));
end;

procedure TZConverter.SetCtrAddr(AOldAddr: Byte; ANewAddr: Byte; AModel: TZG_CTR_TYPE);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_SetCtrAddr(m_hCvt, AOldAddr, ANewAddr, AModel));
end;

procedure TZConverter.GetCtrVersion(AAddr: Byte; VData5: PByte; AWS: PZP_Wait_Settings);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetCtrVersion(m_hCvt, AAddr, VData5, AWS));
end;

procedure TZConverter.GetCtrInfoNorm(AAddr: Byte; VTypeCode: PByte; VSn: PWord;
    VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal; AModel: TZG_CTR_TYPE);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetCtrInfoNorm(m_hCvt, AAddr, VTypeCode, VSn, VVersion, VInfoLines, VFlags, AModel));
end;

procedure TZConverter.GetCtrInfoAdv(AAddr: Byte; VTypeCode: PByte; VSn: PWord;
    VVersion: PWord; VFlags: PCardinal; VEvWrIdx: PInteger; VEvRdIdx: PInteger);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetCtrInfoAdv(m_hCvt, AAddr, VTypeCode, VSn, VVersion, VFlags, VEvWrIdx, VEvRdIdx));
end;

procedure TZConverter.GetCtrInfoBySn(ASn: Word; VTypeCode: PByte; VAddr: PByte;
    VVersion: PWord; VInfoLines: PInteger; VFlags: PCardinal; AModel: TZG_CTR_TYPE);
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetCtrInfoBySn(m_hCvt, ASn, VTypeCode, VAddr, VVersion, VInfoLines, VFlags, AModel));
end;

function TZConverter.GetCtrInfoLine(ASn: Word; ALineN: Integer;
    VBuf: PAnsiChar; ABufSize: Integer; AModel: TZG_CTR_TYPE): Integer;
begin
  ASSERT(m_hCvt <> 0);
  CheckZGError(ZG_Cvt_GetCtrInfoLine(m_hCvt, ASn, ALineN, VBuf, ABufSize, @Result, AModel));
end;

constructor TZController.Create();
begin
  inherited Create();
  FAddr := -1;
  CheckZGError(ZG_Initialize(0));
end;

destructor TZController.Destroy();
begin
  Close();
  ZG_Finalyze();
  if m_hWindow <> 0 then
    DeallocateHWnd(m_hWindow);
  Inherited Destroy();
end;

procedure TZController.DoOnNewEvent(Const AInfo: TZG_N_New_Event_Info);
begin
  if Assigned(FOnNewEvent) then
    FOnNewEvent(Self, AInfo);
end;

procedure TZController.DoOnClockDesync(Const ADiffSec: Int64);
begin
  if Assigned(FOnClockDesync) then
    FOnClockDesync(Self, ADiffSec);
end;

procedure TZController.DoOnKeyTopChange(Const AInfo: TZG_N_Key_Top_Info);
begin
  if Assigned(FOnKeyTopChange) then
    FOnKeyTopChange(Self, AInfo);
end;

procedure TZController.CmZGuard();
var
  hr: HResult;
  nMsg: Cardinal;
  nMsgParam: NativeInt;
begin
  repeat
    hr := ZG_Ctr_GetNextMessage(m_hCtr, nMsg, nMsgParam);
    if hr <> S_OK then
      break;
    case nMsg of
      ZG_NF_CTR_NEW_EVENT:  DoOnNewEvent(PZG_N_New_Event_Info(nMsgParam)^);
      ZG_NF_CTR_CLOCK:      DoOnClockDesync(PINT64(nMsgParam)^);
      ZG_NF_CTR_KEY_TOP:    DoOnKeyTopChange(PZG_N_Key_Top_Info(nMsgParam)^);
    end;
  until False;
end;

procedure TZController.WindowMethod(var Message: TMessage);
begin
  with Message do
    if Msg = CM_ZGUARD then
      try
        CmZGuard();
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(m_hWindow, Msg, wParam, lParam);
end;

procedure TZController.EnableNotifications(AValue: TZCtrNtfs; AReadEvIdx: Integer);
var
  rNS: TZG_Ctr_Notify_Settings;
begin
  ASSERT(m_hCtr <> 0);
  if AValue <> [] then
  begin
    if m_hWindow = 0 then
      m_hWindow := AllocateHWnd(WindowMethod);
    FillChar(rNS, SizeOf(rNS), 0);
    with rNS do
    begin
      nNMask := ZG_NF_CTR_WND_SYNC;
      if cnNewEvent in AValue then
        Inc(nNMask, ZG_NF_CTR_NEW_EVENT);
      if cnClockDesync in AValue then
        Inc(nNMask, ZG_NF_CTR_CLOCK);
      if cnKeyTopChange in AValue then
        Inc(nNMask, ZG_NF_CTR_KEY_TOP);

      rNS.hWindow := m_hWindow;
      rNS.nWndMsgId := CM_ZGUARD;
      rNS.nReadEvIdx := AReadEvIdx;
    end;
    CheckZGError(ZG_Ctr_SetNotification(m_hCtr, @rNS));
  end
  else
  begin
    CheckZGError(ZG_Ctr_SetNotification(m_hCtr, nil));
    if m_hWindow <> 0 then
    begin
      DeallocateHWnd(m_hWindow);
      m_hWindow := 0;
    end;
  end;
end;

procedure TZController.SetReadEvIdx(AValue: Integer);
begin
  if (m_hCtr <> 0) and (m_nNotifications <> []) then
    EnableNotifications(m_nNotifications, AValue);
  FReadEvIdx := AValue;
end;

procedure TZController.SetNotifications(AValue: TZCtrNtfs);
begin
  if m_nNotifications = AValue then
    Exit;
  if m_hCtr <> 0 then
    EnableNotifications(AValue, FReadEvIdx);
  m_nNotifications := AValue;
end;

function TZController.GetActive(): Boolean;
begin
  Result := (m_hCtr <> 0);
end;

procedure TZController.Open();
var
  rInfo: TZg_Ctr_Info;
begin
  try
    FillChar(rInfo, SizeOf(rInfo), 0);
    CheckZGError(ZG_Ctr_Open(m_hCtr, FCvtH, Byte(FAddr), FSn, @rInfo, FCtrType));
    FSn := rInfo.nSn;
    FAddr := rInfo.nAddr;
    FCtrType := rInfo.nType;
    if m_nNotifications <> [] then
      EnableNotifications(m_nNotifications, FReadEvIdx);
  except
    Close();
    raise;
  end;
end;

procedure TZController.Close();
begin
  if m_hCtr = 0 then
    Exit;
  ZG_CloseHandle(m_hCtr);
  if m_hWindow <> 0 then
  begin
    DeallocateHWnd(m_hWindow);
    m_hWindow := 0;
  end;
  m_hCtr := 0;
end;

procedure TZController.SetActive(AValue: Boolean);
begin
  if AValue = (m_hCtr <> 0) then
    exit;
  if AValue then Open() else Close();
end;

procedure TZController.GetInformation(var VInfo: TZG_Ctr_Info);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_GetInformation(m_hCtr, VInfo));
end;

procedure TZController.SetNotification(ASettings: PZG_CTR_Notify_Settings);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetNotification(m_hCtr, ASettings));
end;

procedure TZController.SetNewAddr(ANewAddr: Byte);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetNewAddr(m_hCtr, ANewAddr));
end;

procedure TZController.AssignAddr(AAddr: Byte);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_AssignAddr(m_hCtr, AAddr));
end;

procedure TZController.UpdateFirmware(Const AData; ACount: Integer;
    ACallback: TZG_ProcessCallback; AUserData: Pointer);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_UpdateFirmware(m_hCtr, AData, ACount, nil, ACallback, AUserData));
end;

procedure TZController.OpenLock(ALockN: Integer);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_OpenLock(m_hCtr, ALockN));
end;

procedure TZController.CloseLock();
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_CloseLock(m_hCtr));
end;

procedure TZController.EnableEmergencyUnlocking(AEnable: Boolean);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_EnableEmergencyUnlocking(m_hCtr, AEnable));
end;

function TZController.IsEmergencyUnlockingEnabled(): Boolean;
var
  fLBool: LongBool;
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_IsEmergencyUnlockingEnabled(m_hCtr, fLBool));
  Result := fLBool;
end;

procedure TZController.ReadRegs(AAddr: Cardinal;
    ACount: Integer; var VBuf);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadRegs(m_hCtr, AAddr, ACount, VBuf));
end;

function TZController.ReadPorts(): Cardinal;
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadPorts(m_hCtr, Result));
end;

procedure TZController.ControlDevices(ADevType: Cardinal;
    AActive: Boolean; ATimeMs: Cardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ControlDevices(m_hCtr, ADevType, AActive, ATimeMs));
end;

procedure TZController.ReadData(ABankN: Integer;
    AAddr: Cardinal; ACount: Integer; var VBuf);
var
  nReaded: Integer;
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadData(m_hCtr, ABankN, AAddr, ACount, VBuf, nReaded, nil, nil));
end;

procedure TZController.WriteData(ABankN: Integer;
    AAddr: Cardinal; Const ABuf; ACount: Integer);
var
  nWritten: Integer;
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_WriteData(m_hCtr, ABankN, AAddr, ABuf, ACount, nWritten, nil, nil));
end;

procedure TZController.ReadLockTimes(VOpenMs: PCardinal;
    VLetMs: PCardinal; VMaxMs: PCardinal; ABankN: Integer);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadLockTimes(m_hCtr, VOpenMs, VLetMs, VMaxMs, ABankN));
end;

procedure TZController.WriteLockTimes(AMask: Cardinal; AOpenMs: Cardinal;
    ALetMs: Cardinal; AMaxMs: Cardinal; ABankN: Integer);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_WriteLockTimes(m_hCtr, AMask, AOpenMs, ALetMs, AMaxMs, ABankN));
end;

function TZController.ReadTimeZones(AIdx: Integer;
    VBuf: PZG_Ctr_TimeZone; ACount: Integer;
    ACallback: TZG_ProcessCallback; AUserData: Pointer; ABankN: Integer): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_ReadTimeZones(m_hCtr, AIdx, VBuf, ACount, ACallback, AUserData, ABankN);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

function TZController.WriteTimeZones(AIdx: Integer;
    ATzs: PZG_Ctr_TimeZone; ACount: Integer;
    ACallback: TZG_ProcessCallback; AUserData: Pointer; ABankN: Integer): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_WriteTimeZones(m_hCtr, AIdx, ATzs, ACount, ACallback, AUserData, ABankN);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

function TZController.EnumTimeZones(AStart: Integer;
    AEnumProc: TZG_EnumCtrTimeZonesProc; AUserData: Pointer; ABankN: Integer): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_EnumTimeZones(m_hCtr, AStart, AEnumProc, AUserData, ABankN);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

function TZController.ReadKeys(AIdx: Integer;
    VBuf: PZG_Ctr_Key; ACount: Integer;
    ACallback: TZG_ProcessCallback; AUserData: Pointer; ABankN: Integer): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_ReadKeys(m_hCtr, AIdx, VBuf, ACount, ACallback, AUserData, ABankN);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

function TZController.WriteKeys(AIdx: Integer;
    AKeys: PZG_Ctr_Key; ACount: Integer;
    ACallback: TZG_ProcessCallback; AUserData: Pointer;
    ABankN: Integer; AUpdateTop: Boolean): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_WriteKeys(m_hCtr, AIdx, AKeys, ACount, ACallback, AUserData, ABankN, AUpdateTop);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

function TZController.ClearKeys(AIdx: Integer; ACount: Integer;
    ACallback: TZG_ProcessCallback; AUserData: Pointer;
    ABankN: Integer; AUpdateTop: Boolean): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_ClearKeys(m_hCtr, AIdx, ACount, ACallback, AUserData, ABankN, AUpdateTop);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

function TZController.GetKeyTopIndex(ABankN: Integer): Integer;
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_GetKeyTopIndex(m_hCtr, Result, ABankN));
end;

function TZController.EnumKeys(AStart: Integer;
    AEnumProc: TZG_EnumCtrKeysProc; AUserData: Pointer; ABankN: Integer): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_EnumKeys(m_hCtr, AStart, AEnumProc, AUserData, ABankN);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

procedure TZController.GetClock(var VClock: TZG_Ctr_Clock);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_GetClock(m_hCtr, VClock));
end;

procedure TZController.SetClock(Const AClock: TZG_Ctr_Clock);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetClock(m_hCtr, AClock));
end;

procedure TZController.ReadLastKeyNum(var VNum: TZ_KeyNum);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadLastKeyNum(m_hCtr, VNum));
end;

procedure TZController.ReadRTCState(VClock: PZG_CTR_CLOCK;
        VWrIdx: PInteger; VRdIdx: PInteger; VNum: PZ_KeyNum);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadRTCState(m_hCtr, VClock, VWrIdx, VRdIdx, VNum));
end;

procedure TZController.ReadEventIdxs(var VWrIdx: Integer; var VRdIdx: Integer);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadEventIdxs(m_hCtr, VWrIdx, VRdIdx));
end;

procedure TZController.WriteEventIdxs(AMask: Cardinal; AWrIdx, ARdIdx: Integer);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_WriteEventIdxs(m_hCtr, AMask, AWrIdx, ARdIdx));
end;

function TZController.ReadEvents(AIdx: Integer;
    VBuf: PZG_Ctr_Event; ACount: Integer;
    ACallback: TZG_ProcessCallback; AUserData: Pointer): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_ReadEvents(m_hCtr, AIdx, VBuf, ACount, ACallback, AUserData);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

function TZController.EnumEvents(AStart, ACount: Integer;
    AEnumProc: TZG_EnumCtrEventsProc; AUserData: Pointer): Boolean;
var
  nRet: TZg_Status;
begin
  ASSERT(m_hCtr <> 0);
  nRet := ZG_Ctr_EnumEvents(m_hCtr, AStart, ACount, AEnumProc, AUserData);
  CheckZGError(nRet);
  Result := (nRet = S_OK);
end;

procedure TZController.DecodePassEvent(Const AData8;
    var VTime: TZG_Ev_Time; var VDirect: TZG_Ctr_Direct;
    var VKeyIdx: Integer; var VKeyBank: Integer);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_DecodePassEvent(m_hCtr, PByte(@AData8), VTime, VDirect,
      VKeyIdx, VKeyBank));
end;

procedure TZController.DecodeEcEvent(Const AData8;
    var VTime: TZG_Ev_Time; var VSubEvent: TZG_EC_Sub_Ev;
    var VPowerFlags: Cardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_DecodeEcEvent(m_hCtr, PByte(@AData8), VTime, VSubEvent,
      VPowerFlags));
end;

procedure TZController.DecodeUnkKeyEvent(Const AData8; var VKeyNum: TZ_KeyNum);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_DecodeUnkKeyEvent(m_hCtr, PByte(@AData8), VKeyNum));
end;

procedure TZController.DecodeFireEvent(Const AData8;
    var VTime: TZG_Ev_Time; var VSubEvent: TZG_Fire_Sub_Ev;
    var VFireFlags: Cardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_DecodeFireEvent(m_hCtr, PByte(@AData8), VTime, VSubEvent, VFireFlags));
end;

procedure TZController.DecodeSecurEvent(Const AData8;
    var VTime: TZG_Ev_Time; var VSubEvent: TZG_Secur_Sub_Ev;
    var VSecurFlags: Cardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_DecodeSecurEvent(m_hCtr, PByte(@AData8), VTime, VSubEvent, VSecurFlags));
end;

procedure TZController.DecodeModeEvent(Const AData8;
    var VTime: TZG_Ev_Time; var VMode: TZG_Ctr_Mode;
    var VSubEvent: TZG_Mode_Sub_Ev);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_DecodeModeEvent(m_hCtr, PByte(@AData8), VTime, VMode, VSubEvent));
end;

class procedure TZController.DecodeHotelEvent(Const AData8;
    var VTime: TZG_Ev_Time; var VMode: TZG_Hotel_Mode;
    var VSubEvent: TZG_Hotel_Sub_Ev; var VFlags: Cardinal);
begin
  CheckZGError(ZG_DecodeHotelEvent(PByte(@AData8), VTime, VMode, VSubEvent, VFlags));
end;

procedure TZController.SetFireMode(AOn: Boolean);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetFireMode(m_hCtr, AOn));
end;

procedure TZController.GetFireInfo(var VFireFlags: Cardinal;
    var VCurrTemp: Cardinal; var VSrcMask: Cardinal; var VLimitTemp: Cardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_GetFireInfo(m_hCtr, VFireFlags, VCurrTemp, VSrcMask, VLimitTemp));
end;

procedure TZController.SetFireConfig(ASrcMask, ALimitTemp: Cardinal;
    VFireFlags: PCardinal; VCurrTemp: PCardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetFireConfig(m_hCtr, ASrcMask, ALimitTemp, VFireFlags, VCurrTemp));
end;

procedure TZController.SetSecurMode(AMode: TZG_Secur_Mode);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetSecurMode(m_hCtr, AMode));
end;

procedure TZController.GetSecurInfo(var VSecurFlags: Cardinal;
    var VSrcMask: Cardinal; var VAlarmTime: Cardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_GetSecurInfo(m_hCtr, VSecurFlags, VSrcMask, VAlarmTime));
end;

procedure TZController.SetSecurConfig(ASrcMask, AAlarmTime: Cardinal;
    VSecurFlags: PCardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetSecurConfig(m_hCtr, ASrcMask, AAlarmTime, VSecurFlags));
end;

procedure TZController.SetCtrMode(AMode: TZG_Ctr_Mode);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetCtrMode(m_hCtr, AMode));
end;

procedure TZController.GetCtrModeInfo(var VMode: TZG_Ctr_Mode; var VFlags: Cardinal);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_GetCtrModeInfo(m_hCtr, VMode, VFlags));
end;

procedure TZController.ReadElectroConfig(var VConfig: TZG_Ctr_Electro_Config);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_ReadElectroConfig(m_hCtr, VConfig));
end;

procedure TZController.WriteElectroConfig(Const AConfig: TZG_Ctr_Electro_Config);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_WriteElectroConfig(m_hCtr, AConfig));
end;

procedure TZController.GetElectroState(var VState: TZG_Ctr_Electro_State);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_GetElectroState(m_hCtr, VState));
end;

procedure TZController.SetElectroPower(AOn: Boolean);
begin
  ASSERT(m_hCtr <> 0);
  CheckZGError(ZG_Ctr_SetElectroPower(m_hCtr, AOn));
end;

constructor TCvtDetector.Create();
begin
  inherited Create();
  FSDevTypes := ZG_DEVTYPE_CVTS;
  FIpDevTypes := ZG_IPDEVTYPE_CVTS;
end;

function CheckZGVersion(): Boolean;
var
  nVersion: Cardinal;
begin
  nVersion := ZG_GetVersion();
  Result := ((nVersion and $ff) = ZG_SDK_VER_MAJOR) and
      (((nVersion shr 8) and $ff) = ZG_SDK_VER_MINOR);
end;

function GetZGErrorText(AErrCode: HResult): String;
var
  pBuffer: PChar;
  nLen, nFlags: Integer;
begin
  pBuffer := nil;
  if HResultFacility(AErrCode) = $4 then
    nFlags := FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_HMODULE
  else
    nFlags := FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM;
  nLen := FormatMessage(nFlags,
      Pointer(GetModuleHandle(ZG_DLL_Name)),
      Cardinal(AErrCode),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      PChar(@pBuffer), 0, nil);
  if pBuffer <> nil then
  begin
    SetString(Result, pBuffer, nLen);
    LocalFree(HLOCAL(pBuffer));
  end
  else
    Result := '';
end;

procedure CheckZGError(AErrCode: HResult);
begin
  if FAILED(AErrCode) then
    raise EZGError.Create(GetZGErrorText(AErrCode), AErrCode);
end;

procedure ZGInitialize(AFlags: Cardinal);
begin
  CheckZGError(ZG_Initialize(AFlags));
end;

procedure ZGFinalyze();
begin
  ZG_Finalyze();
end;

function GetProxyConverters(var VSNs: TWordDynArray; Const AIpAddr, AActCode: String;
    AWait: PZP_Wait_Settings): Integer;
var
  sCode: AnsiString;
  szIpAddr: array[0..63] of WideChar;
begin
  ASSERT((AIpAddr <> '') and (AActCode <> ''));
  Result := 0;
  CheckZGError(ZG_Initialize(0));
  try
    SetLength(VSNs, 256);
    sCode := AnsiString(AActCode);
    StringToWideChar(AIpAddr, szIpAddr, Length(szIpAddr));
    CheckZGError(ZG_GetProxyConverters(@VSNs[0], Length(VSNs), Result,
        szIpAddr, PAnsiChar(sCode), AWait));
  finally
    SetLength(VSNs, Result);
    ZG_Finalyze();
  end;
end;

procedure EnumSerialPorts(AEnumProc: TZP_EnumPortsProc; AUserData: Pointer);
begin
  CheckZGError(ZG_Initialize(0));
  try
    CheckZGError(ZP_EnumSerialPorts(ZG_DEVTYPE_CVTS, AEnumProc, AUserData));
  finally
    ZG_Finalyze();
  end;
end;
procedure EnumConverters(AEnumProc: TZG_EnumCvtsProc; AUserData: Pointer;
    APorts: PZP_Port_Addr; APCount: Integer;
    AWait: PZP_Wait_Settings; AFlags: Cardinal);
begin
  CheckZGError(ZG_Initialize(0));
  try
    CheckZGError(ZG_EnumConverters(APorts, APCount, AEnumProc, AUserData, AWait, AFlags));
  finally
    ZG_Finalyze();
  end;
end;

function FindConverter(var VInfo: TZG_Enum_Cvt_Info; var VPort: TZP_Port_Info;
    APorts: PZP_Port_Addr; APCount: Integer;
    AWait: PZP_Wait_Settings; AFlags: Cardinal): Boolean;
var
  nRet: HResult;
begin
  CheckZGError(ZG_Initialize(0));
  try
    nRet := ZG_FindConverter(APorts, APCount, VInfo, VPort, AWait, AFlags);
    CheckZGError(nRet);
    Result := (nRet = S_OK);
  finally
    ZG_Finalyze();
  end;
end;

procedure EnumIpConverters(AEnumProc: TZG_EnumIpCvtsProc; AUserData: Pointer;
    AWait: PZP_Wait_Settings; AFlags: Cardinal);
begin
  CheckZGError(ZG_Initialize(0));
  try
    CheckZGError(ZG_EnumIpConverters(AEnumProc, AUserData, AWait, AFlags));
  finally
    ZG_Finalyze();
  end;
end;

end.

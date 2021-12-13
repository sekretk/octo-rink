unit uConst;

interface

uses
  Windows, SysUtils, ZPort;

Type
  TDevType = (
    dev_UNDEF = 0,
    dev_Z397,         // Z-397
    dev_Z397_GUARD,   // Z-397 Guard
    dev_Z397_IP,      // Z-397 IP
    dev_Z397_Web,     // Z-397 Web
    dev_Z5R_Web,      // Z5r Web
    dev_Z2U,          // Z-2 USB
    dev_M3A,          // Matrix III Rd-All
    dev_Z2M,          // Z-2 USB MF
    dev_M3N,          // Matrix III Net
    dev_CPZ2MF,       // CP-Z-2MF
    dev_Z2EHR         // Z-2 EHR
  );
  TGuardMode = (
    gmUndef = 0,
    gmNormal,
    gmAdvanced,
    gmTest,
    gmAccept
  );
  TCvtInfo = packed record
    rBase: TZp_Device_Info;
    nGuardMode: TGuardMode;
  end;
  PCvtInfo = ^TCvtInfo;
Const
  YesNoStrs: array[Boolean] of String = ('no', 'yes');

  DevTypeStrs: array[TDevType] of String = (
    '???',
    'Z-397',
    'Z-397 Guard',
    'Z-397 IP',
    'Z-397 Web',
    'Z5R Web',
    'Z-2 USB',
    'Matrix III Rd-All',
    'Z-2 USB MF',
    'Matrix III Net',
    'CP-Z-2MF',
    'Z-2 EHR'
  );
  ConnectionStatusStrs: array[TZp_Connection_Status] of String = (
    'Disconnected',
    'Connected',
    'Connecting...',
    'Restoration...'
  );
Const
  cmdI: PAnsiChar = 'i'#13;

resourcestring
  StrSearching = 'Searching...';
  StrSearchComplite_D = 'Search is completed (%d ms).';
  StrPortInsert_SSS = '[%s] Found port %s (%s)';
  StrPortRemove_SSS = '[%s] Lost port %s (%s)';
  StrDevInsert_SDDS = '[%s] Found device model=%Xh s/n: %d (%s).';
  StrDevRemove_SDDS = '[%s] Lost device model=%Xh s/n: %d (%s).';

implementation

end.

unit uConst;

interface

uses
  ZPort, ZGuard;


Const
  CvtTypeStrs: array[TZG_CVT_TYPE] of String = (
      'Unknown', 'Z-397', 'Z-397 Guard', 'Z-397 IP', 'Z-397 Web', 'Z5R Web');
  CvtModeStrs: array[TZG_GUARD_MODE] of String = (
      'Unknown', 'Normal', 'Advanced', 'Test', 'Accept');
  ConnStStrs: array[TZp_Connection_Status] of String = (
    'Disconnected',
    'Connected',
    'Connecting...',
    'Restoration...'
  );
  CtrTypeStrs: array[TZG_CTR_TYPE] of  String = (
    '',
    'Gate 2000',
    'Matrix II Net',
    'Z5R Net',
    'Z5R Net 8000',
    'Guard Net',
    'Z-9 EHT Net',
    'Eurolock EHT Net',
    'Z5R Web'
  );
  KeyModeStrs: array[Boolean] of String = ('Touch Memory', 'Proximity');
  KeyTypeStrs: array[TZG_CTR_KEY_TYPE] of String = (
    '',
    'Normal',
    'Blocking',
    'Master'
  );
  EvTypeStrs: array[TZG_CTR_EV_TYPE] of String = (
    '',
    'Open button',                  // ������� ������� �������
    'Key not found',				        // ���� �� ������ � ����� ������
    'Open key',					            // ���� ������, ����� �������
    'Key unscheduled',			        // ���� ������, ������ �� ��������
    'Open RS485',					          // ������� ���������� �� ����
    'Door is blocked (key)',        // ���� ������, ����� �������������
    'Door is blocked (button)',	    // ������� ������� ��������������� ����� �������
    'Door is broken',				        // ����� ��������
    'Door is left open',			      // ����� ��������� �������� (timeout)
    'Passage',					            // ������ ���������
    'Sensor 1',					            // �������� ������ 1
    'Sensor 2',					            // �������� ������ 2
    'Controller is rebooted',       // ������������ �����������
    'Button is disabled',			      // ������������� ������ ����������
    'Double pass',				          // ������� �������� �������
    'Door opened',				          // ����� ������� ������
    'Door closed',				          // ����� �������
    'Power off',			              // ������� �������
    'Power is on (ElectroControl)', // ��������� ��������������
    'Power is off (ElectroControl)',// ��������� ��������������
    'Enable the lock (trigger)',		// ��������� ����� (�������)
    'Disable the lock (trigger)',	  // ���������� ����� (�������)
    'Changed state Mode',			      // ��������� ��������� �����
    'Changed state Fire',			      // ��������� ��������� ������
    'Changed state Security',		    // ��������� ��������� ������
    'Unknown key',		              // ����������� ����
    'Passage in the gateway',		    // �������� ���� � ����
    'Blocked the entrance to the gateway (busy)',// ������������ ���� � ���� (�����)
    'Allowed to enter the gateway', // �������� ���� � ����
    'AntiPassBack',		              // ������������ ������ (�����������)
    'Hotel40',
    'Hotel41'
  );
  DirectStrs: array[TZG_CTR_DIRECT] of String = (
    '   ',
    'IN ',  // ����
    'OUT'   // �����
  );
  EcSubEvStrs: array[TZG_EC_SUB_EV] of String = (
    '',
    'CARD_DELAY',       // ��������� �������� ����� � ������ ������� (��� �����) �������� ��������
    'RESERVED1',        // (���������������)
    'ON_NET',           // �������� �������� �� ����
    'OFF_NET',          // ��������� �������� �� ����
    'ON_SCHED',         // �������� �� ��������� ����
    'OFF_SHED',         // ��������� �� ��������� ����
    'CARD',             // ��������� �������� ����� � ������������ ����������
    'RESERVED2',        // (���������������)
    'OFF_TIMEOUT',      // ��������� ����� ��������� ��������
    'OFF_EXIT'          // ��������� �� ������������ ������� ������
  );
  FireSubEvStrs: array[TZG_FIRE_SUB_EV] of String = (
    '',
    'OFF_NET',          // ��������� �� ����
    'ON_NET',           // �������� �� ����
    'OFF_INPUT_F',      // ��������� �� ����� FIRE
    'ON_INPUT_F',       // �������� �� ����� FIRE
    'OFF_TEMP',         // ��������� �� ������� �����������
    'ON_TEMP'           // �������� �� ������� �����������
  );
  SecurSubEvStrs: array[TZG_SECUR_SUB_EV] of String = (
    '',
    'OFF_NET',          // ��������� �� ����
    'ON_NET',           // �������� �� ����
    'OFF_INPUT_A',      // ��������� �� ����� ALARM
    'ON_INPUT_A',       // �������� �� ����� ALARM
    'OFF_TAMPERE',      // ��������� �� �������
    'ON_TAMPERE',       // �������� �� �������
    'OFF_DOOR',         // ��������� �� ������� �����
    'ON_DOOR'           // �������� �� ������� �����
  );
  ModeSubEvStrs: array[TZG_MODE_SUB_EV] of String = (
    '',
    'RS485 allow',      // ��������� �������� �� ����
    'RS485 denied',     // �������� ��������� �� ����
    'TimeZone start',   // �������� ��������� ����
    'TimeZone finish',  // ���������� ��������� ����
    'Card allow',       // ��������� ������
    'Card denied'       // �������� ��������� ������
  );
  ModeStrs: array[TZG_CTR_MODE] of String = (
    '',
    'Norm',             // �����
    'Block',            // ����������
    'Free',             // ���������
    'Wait'              // ��������
  );
  HModeStrs: array[TZG_HOTEL_MODE] of String = (
    '',
    'Norm',             // �����
    'Block',            // ����������
    'Free',             // ���������
    '???'               // ���������������
  );
  HotelSubEvStrs: array[TZG_HOTEL_SUB_EV] of String = (
    '',
    'FreeCard',
    'BlockCard',
    'DopFunc',
    'NewRCard',
    'Network',
    'Timezone',
    'Counter',
    'CryptoKey',
    'Pulse Z',
    'State Change'
  );

resourcestring
  StrWrongSdkVer = 'Wrong version SDK.';
  StrEInitSdkFail_D = 'Initialize ZGuard.dll error(%d)';

  StrSearching = 'Searching...';
  StrSearchDone_D = 'Search is completed (%d ms).';
  StrPortInsert_SSS = '[%s] Found port %s (%s)';
  StrPortRemove_SSS = '[%s] Lost port %s (%s)';
  StrCvtInsert_SSDS = '[%s] Found converter %s s/n: %d (%s).';
  StrCvtRemove_SSDS = '[%s] Lost converter %s s/n: %d (%s).';

  StrUpdateCvtFW_D = 'Update converter firmware %3d%%...';

  StrEInvalidSn = 'Invalid S/n!';
  StrEInvalidLicData = 'Incorrect license data.';
  StrUpdateCtrFW_D = 'Update controller firmware %3d%%...';
  StrEInvalidCtrAddr = 'Incorrect controller address.';
  StrSearchingCtrs_D = 'Searching controllers %d%%...';
  StrUnlimited = 'unlimited';

  StrReadAllEvents_D = '[1/2] Read all events %d%%...';
  StrReadKeyNumForEvs_D = '[2/2] Read keynum for events %3d%%...';
  StrReadNewEvents_D = 'Read new events %3d%%...';
  StrUnkEvent_D = 'Unknown[%.2Xh]';
  StrPassEventOpt_DD = ' key_idx: %d, key_bank: %d';
  StrEcEventOpt_SD = ' sub_ev: %s, power_flags: %.2Xh';
  StrFireEventOpt_SD = ' sub_ev: %s, fire_flags: %.2Xh';
  StrSecurEventOpt_SD = ' sub_ev: %s, secur_flags: %.2Xh';
  StrModeEventOpt_SS = ' mode: %s, sub_ev: %s';
  StrHotelEventOpt_SSD = ' mode: %s, sub_ev: %s, flags: %.2Xh';

  StrEInvalidKeyNum = 'Invalid key number.';
  StrLbTz1_DSDDDD = '%d - %s %.2d:%.2d-%.2d:%.2d';
  StrLbTz2_DSDDDDSDDDD = '%d - In: %s %.2d:%.2d-%.2d:%.2d Out: %s %.2d:%.2d-%.2d:%.2d';

  StrEBankFull = 'Bank is full.';
  StrClearKeys_DD = 'Clear keys (bank%d) %%3d%%%%...';
  StrReally = 'Really?';
  StrReadKeys_DD = 'Read keys (bank%d) %%3d%%%%...';
  StrWriteKeys_DD = 'Write keys (bank%d) %%3d%%%%...';

  StrReadMemory_D = 'Read memory %3d%%...';
  StrCvtFWFilter = 'Firmware files (*.rom)|*.rom';
  StrLicFilter = 'License files (*.lic)|*.lic';

  StrConfirm = 'Really?';

  StrElPowerOn_DDD = 'Power on (%.2d:%.2d:%.2d)';
  StrElPowerOff_DDD = 'Power off (%.2d:%.2d:%.2d)';

  StrElStKey = 'The key is brought to the reader'; // ���� �������� � �����������
  StrElStKeyProcDelay = 'The key is brought to the reader (processing delays ...)'; // ���� �������� � ����������� (��������� ��������)
  StrSchedDows = 'MTWTFSS'; // ����� ���� ������

  StrIncorrectCvtSn = 'Incorrect s/n converter.'; // ������������ �/� ����������.
  StrIncorrectPortName = 'Incorrect port name.'; // ������������ ��� �����.
  StrIncorrectActCode = 'Incorrect activate code.'; // ������������ ��� ���������

implementation

end.

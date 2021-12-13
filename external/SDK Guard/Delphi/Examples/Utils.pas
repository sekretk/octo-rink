unit Utils;

interface

uses
  SysUtils,
  ZBase, ZPort, ZGuard;

const
  PortTypeStrs: array[TZP_PORT_TYPE] of String = ('Unknown', 'COM', 'FT', 'IP', 'IPS');
  BusyStrs: array[Boolean] of String = ('', 'busy');

  CvtTypeStrs: array[TZG_CVT_TYPE] of String = (
      'Unknown', 'Z-397', 'Z-397 Guard', 'Z-397 IP', 'Z-397 Web', 'Z5R Web');
  GuardModeStrs: array[TZG_GUARD_MODE] of String = (
      'Unknown', 'Normal', 'Advanced', 'Test', 'Accept');
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
  RdModeStrs: array[Boolean] of String = ('Dallas', 'Wiegand');
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

// ������������� ����� ����� � ������
function ZKeyNumToStr(const ANum: TZ_KEYNUM; AWiegand: Boolean): String;

implementation

function ZKeyNumToStr(const ANum: TZ_KEYNUM; AWiegand: Boolean): String;
var
  i: Integer;
begin
  if AWiegand then
    Result := format('%d, %d', [ANum[3], PWord(@ANum[1])^])
  else
  begin
    Result := '';
    for i := ANum[0] downto 1 do
      Result := Result + IntToHex(ANum[i], 2);
  end;
end;


end.

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
    'Open button',                  // Открыто кнопкой изнутри
    'Key not found',				        // Ключ не найден в банке ключей
    'Open key',					            // Ключ найден, дверь открыта
    'Key unscheduled',			        // Ключ найден, доступ не разрешен
    'Open RS485',					          // Открыто оператором по сети
    'Door is blocked (key)',        // Ключ найден, дверь заблокирована
    'Door is blocked (button)',	    // Попытка открыть заблокированную дверь кнопкой
    'Door is broken',				        // Дверь взломана
    'Door is left open',			      // Дверь оставлена открытой (timeout)
    'Passage',					            // Проход состоялся
    'Sensor 1',					            // Сработал датчик 1
    'Sensor 2',					            // Сработал датчик 2
    'Controller is rebooted',       // Перезагрузка контроллера
    'Button is disabled',			      // Заблокирована кнопка открывания
    'Double pass',				          // Попытка двойного прохода
    'Door opened',				          // Дверь открыта штатно
    'Door closed',				          // Дверь закрыта
    'Power off',			              // Пропало питание
    'Power is on (ElectroControl)', // Включение электропитания
    'Power is off (ElectroControl)',// Включение электропитания
    'Enable the lock (trigger)',		// Включение замка (триггер)
    'Disable the lock (trigger)',	  // Отключение замка (триггер)
    'Changed state Mode',			      // Изменение состояния Режим
    'Changed state Fire',			      // Изменение состояния Пожара
    'Changed state Security',		    // Изменение состояния Охраны
    'Unknown key',		              // Неизвестный ключ
    'Passage in the gateway',		    // Совершен вход в шлюз
    'Blocked the entrance to the gateway (busy)',// Заблокирован вход в шлюз (занят)
    'Allowed to enter the gateway', // Разрешен вход в шлюз
    'AntiPassBack',		              // Заблокирован проход (Антипассбек)
    'Hotel40',
    'Hotel41'
  );
  DirectStrs: array[TZG_CTR_DIRECT] of String = (
    '   ',
    'IN ',  // Вход
    'OUT'   // Выход
  );
  EcSubEvStrs: array[TZG_EC_SUB_EV] of String = (
    '',
    'CARD_DELAY',       // Поднесена валидная карта с другой стороны (для входа) запущена задержка
    'RESERVED1',        // (зарезервировано)
    'ON_NET',           // Включено командой по сети
    'OFF_NET',          // Выключено командой по сети
    'ON_SCHED',         // Включено по временной зоне
    'OFF_SHED',         // Выключено по временной зоне
    'CARD',             // Поднесена валидная карта к контрольному устройству
    'RESERVED2',        // (зарезервировано)
    'OFF_TIMEOUT',      // Выключено после отработки таймаута
    'OFF_EXIT'          // Выключено по срабатыванию датчика выхода
  );
  FireSubEvStrs: array[TZG_FIRE_SUB_EV] of String = (
    '',
    'OFF_NET',          // Выключено по сети
    'ON_NET',           // Включено по сети
    'OFF_INPUT_F',      // Выключено по входу FIRE
    'ON_INPUT_F',       // Включено по входу FIRE
    'OFF_TEMP',         // Выключено по датчику температуры
    'ON_TEMP'           // Включено по датчику температуры
  );
  SecurSubEvStrs: array[TZG_SECUR_SUB_EV] of String = (
    '',
    'OFF_NET',          // Выключено по сети
    'ON_NET',           // Включено по сети
    'OFF_INPUT_A',      // Выключено по входу ALARM
    'ON_INPUT_A',       // Включено по входу ALARM
    'OFF_TAMPERE',      // Выключено по тамперу
    'ON_TAMPERE',       // Включено по тамперу
    'OFF_DOOR',         // Выключено по датчику двери
    'ON_DOOR'           // Включено по датчику двери
  );
  ModeSubEvStrs: array[TZG_MODE_SUB_EV] of String = (
    '',
    'RS485 allow',      // Установка командой по сети
    'RS485 denied',     // Отказано оператору по сети
    'TimeZone start',   // Началась временная зона
    'TimeZone finish',  // Окончилась временная зона
    'Card allow',       // Установка картой
    'Card denied'       // Отказано изменению картой
  );
  ModeStrs: array[TZG_CTR_MODE] of String = (
    '',
    'Norm',             // Норма
    'Block',            // Блокировка
    'Free',             // Свободный
    'Wait'              // Ожидание
  );
  HModeStrs: array[TZG_HOTEL_MODE] of String = (
    '',
    'Norm',             // Норма
    'Block',            // Блокировка
    'Free',             // Свободный
    '???'               // Зарезервировано
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

// Преобразовать номер ключа в строку
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

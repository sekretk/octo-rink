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

  StrElStKey = 'The key is brought to the reader'; // Ключ поднесен к считывателю
  StrElStKeyProcDelay = 'The key is brought to the reader (processing delays ...)'; // Ключ поднесен к считывателю (обработка задержки)
  StrSchedDows = 'MTWTFSS'; // Маска дней недели

  StrIncorrectCvtSn = 'Incorrect s/n converter.'; // Некорректный с/н конвертера.
  StrIncorrectPortName = 'Incorrect port name.'; // Некорректный имя порта.
  StrIncorrectActCode = 'Incorrect activate code.'; // Некорректный код активации

implementation

end.

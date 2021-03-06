using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IvTx.IronLogic.Facade
{
    public static class ZGuardDictionary
    {
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };
        public static readonly string[] KeyModeStrs = { "Touch Memory", "Proximity" };

        public static readonly string[] EvTypeStrs =
        {
            "",
            "Открыто кнопкой изнутри",
            "Ключ не найден в банке ключей",
            "Ключ найден, дверь открыта",
            "Ключ найден, доступ не разрешен",
            "Открыто оператором по сети",
            "Ключ найден, дверь заблокирована",
            "Попытка открыть заблокированную дверь кнопкой",
            "Дверь взломана",
            "Дверь оставлена открытой (timeout)",
            "Проход состоялся",
            "Сработал датчик 1",
            "Сработал датчик 2",
            "Перезагрузка контроллера",
            "Заблокирована кнопка открывания",
            "Попытка двойного прохода",
            "Дверь открыта штатно",
            "Дверь закрыта",
            "Пропало питание",
            "Включение электропитания",
            "Включение электропитания",
            "Включение замка (триггер)",
            "Отключение замка (триггер)",
            "Изменение состояния Режим",
            "Изменение состояния Пожара",
            "Изменение состояния Охраны",
            "Неизвестный ключ",
            "Совершен вход в шлюз",
            "Заблокирован вход в шлюз (занят)",
            "Разрешен вход в шлюз",
            "Заблокирован проход (Антипассбек)",
            "Hotel40",
            "Hotel41"
        };

        public static readonly string[] DirectStrs =
        {
            "",
            "Вход",
            "Выход"
        };

        public static readonly string[] EcSubEvStrs =
        {
            "",
            "Поднесена карта для входа",
            "(зарезервировано)",
            "Включено командой по сети",
            "Выключено командой по сети",
            "Включено по временной зоне",
            "Выключено по временной зоне",
            "Поднесена карта к контрольному устройству",
            "(зарезервировано)",
            "Выключено после отработки таймаута",
            "Выключено по срабатыванию датчика выхода"
        };

        public static readonly string[] FireSubEvStrs =
        {
            "",
            "Выключено по сети",
            "Включено по сети",
            "Выключено по входу FIRE",
            "Включено по входу FIRE",
            "Выключено по датчику температуры",
            "Включено по датчику температуры"
        };

        public static readonly string[] SecurSubEvStrs =
        {
            "",
            "Выключено по сети",
            "Включено по сети",
            "Выключено по входу ALARM",
            "Включено по входу ALARM",
            "Выключено по тамперу",
            "Включено по тамперу",
            "Выключено по датчику двери",
            "Включено по датчику двери"
        };

        public static readonly string[] ModeSubEvStrs =
        {
            "",
            "Установка командой по сети",
            "Отказано оператору по сети",
            "Началась временная зона",
            "Окончилась временная зона",
            "Установка картой",
            "Отказано изменению картой"
        };

        public static readonly string[] ModeStrs =
        {
            "",
            "Обычный",
            "Блокировка",
            "Свободный",
            "Ожидание"
        };

        public static readonly string[] HModeStrs =
        {
            "",
            "Обычный",
            "Блокировка",
            "Свободный",
            "???"
        };

        public static readonly string[] HotelSubEvStrs =
        {
            "",
            "Карта открытия",
            "Карта блокирующая",
            "Дополнительная функция",
            "создана резервная карта",
            "Network",
            "TimeZone",
            "обновлен счетчик",
            "обновлен криптоключ",
            "Pulse Z",
            "Изменено состояние"
        };
    }
}

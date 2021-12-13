using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IvTx.IronLogic.Facade.Enums
{
    public enum EventType
    {
        [Description("")]
        Zero = 0,
        [Description("Открыто кнопкой изнутри")]
        OpenByButton = 1,
        [Description("Ключ не найден в банке ключей")]
        KeyNotFoundInStorage = 2,
        [Description("Ключ найден, дверь открыта")]
        KeyFoundAccessGranted = 3,
        [Description("Ключ найден, доступ не разрешен")]
        KeyFoundAccessDenied = 4,
        [Description("Открыто оператором по сети")]
        OpenByNetwork = 5,
        [Description("Ключ найден, дверь заблокирована")]
        KeyFoundDoorBlocked = 6,
        [Description("Попытка открыть заблокированную дверь кнопкой")]
        BlockedDoorOpenAttempt = 7,
        [Description("Дверь взломана")]
        DoorHacked = 8,
        [Description("Дверь оставлена открытой (timeout)")]
        DoorLeftOpened = 9,
        [Description("Проход состоялся")]
        EntranceSuccess = 10,
        [Description("Сработал датчик 1")]
        Sensor1Alert = 11,
        [Description("Сработал датчик 2")]
        Sensor2Alert = 12,
        [Description("Перезагрузка контроллера")]
        CtrlReboot = 13,
        [Description("Заблокирована кнопка открывания")]
        ButtonBlocked = 14,
        [Description("Попытка двойного прохода")]
        DoubleEntranceAtempt = 15,
        [Description("Дверь открыта штатно")]
        DoorOpened = 16,
        [Description("Дверь закрыта")]
        DoorClosed = 17,
        [Description("Пропало питание")]
        LostPower = 18,
        [Description("Включение электропитания")]
        PowerRestore1 = 19,
        [Description("Включение электропитания")]
        PowerRestore2 = 20,
        [Description("Включение замка (триггер)")]
        LockOn = 21,
        [Description("Отключение замка (триггер)")]
        LockOff = 22,
        [Description("Изменение состояния Режим")]
        ModeChanged = 23,
        [Description("Изменение состояния Пожара")]
        FireModeChanged = 24,
        [Description("Изменение состояния Охраны")]
        SecureModeChanged = 25,
        [Description("Неизвестный ключ")]
        UnknownKey = 26,
        [Description("Совершен вход в шлюз")]
        GateEntrance = 27,
        [Description("Заблокирован вход в шлюз (занят)")]
        GateEntranceBlocked = 28,
        [Description("Разрешен вход в шлюз")]
        GateEntranceGranted = 29,
        [Description("Заблокирован проход (Антипассбек)")]
        AntiPassBack = 30,
        [Description("Hotel40")]
        Hotel40 = 31,
        [Description("Hotel41")]
        Hotel41 = 32
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Threading;
using ZGuard;
using ZPort;

namespace CtrEvents
{
    class Program
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
        

        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM3";
        public const Byte CtrAddr = 3;

        public static IntPtr m_hCtr;
        public static int m_nCtrMaxEvents;
        public static bool m_fProximity;
        public static UInt32 m_nCtrFlags;
        public static bool m_fCtrNotifyEnabled;
        public static int m_nAppReadEventIdx;


        static void ShowEvents(int nStart, int nCount)
        {
            ZG_CTR_EVENT[] aEvents = new ZG_CTR_EVENT[6];
            ZG_CTR_EVENT rEv;
            int i = 0;
            int nIdx, nCnt;
            int hr;

            while (i < nCount)
            {
                nIdx = (nStart + i) % m_nCtrMaxEvents;
                nCnt = (nCount - i);
                if (nCnt > aEvents.Length)
                    nCnt = aEvents.Length;
                if ((nIdx + nCnt) > m_nCtrMaxEvents)
                    nCnt = (m_nCtrMaxEvents - nIdx);
                hr = ZGIntf.ZG_Ctr_ReadEvents(m_hCtr, nIdx, aEvents, nCnt, null, IntPtr.Zero);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_ReadEvents ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                for (int j = 0; j < nCnt; j++)
                {
                    rEv = aEvents[j];
                    switch (rEv.nType)
                    {
                        case ZG_CTR_EV_TYPE.ZG_EV_ELECTRO_ON:
                        case ZG_CTR_EV_TYPE.ZG_EV_ELECTRO_OFF:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_EC_SUB_EV nSubEvent = new ZG_EC_SUB_EV();
                                UInt32 nPowerFlags = 0;
                                ZGIntf.ZG_Ctr_DecodeEcEvent(m_hCtr, rEv.aData, ref rTime, ref nSubEvent, ref nPowerFlags);
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Power flags: {8:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond,
                                    EvTypeStrs[(int)rEv.nType],
                                    EcSubEvStrs[(int)nSubEvent], nPowerFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_FIRE_STATE:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_FIRE_SUB_EV nSubEvent = new ZG_FIRE_SUB_EV();
                                UInt32 nFireFlags = 0;
                                ZGIntf.ZG_Ctr_DecodeFireEvent(m_hCtr, rEv.aData, ref rTime, ref nSubEvent, ref nFireFlags);
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Fire flags: {8:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond,
                                    EvTypeStrs[(int)rEv.nType],
                                    FireSubEvStrs[(int)nSubEvent], nFireFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_SECUR_STATE:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_SECUR_SUB_EV nSubEvent = new ZG_SECUR_SUB_EV();
                                UInt32 nSecurFlags = 0;
                                ZGIntf.ZG_Ctr_DecodeSecurEvent(m_hCtr, rEv.aData, ref rTime, ref nSubEvent, ref nSecurFlags);
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Security flags: {8:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond,
                                    EvTypeStrs[(int)rEv.nType],
                                    SecurSubEvStrs[(int)nSubEvent], nSecurFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_MODE_STATE:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_CTR_MODE nMode = new ZG_CTR_MODE();
                                ZG_MODE_SUB_EV nSubEvent = new ZG_MODE_SUB_EV();
                                ZGIntf.ZG_Ctr_DecodeModeEvent(m_hCtr, rEv.aData, ref rTime, ref nMode, ref nSubEvent);
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Mode: {7} Sub_event: {8}",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond,
                                    EvTypeStrs[(int)rEv.nType],
                                    ModeStrs[(int)nMode],
                                    ModeSubEvStrs[(int)nSubEvent]);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_UNKNOWN_KEY:
                            {
                                Byte[] rKeyNum = new Byte[16];
                                ZGIntf.ZG_Ctr_DecodeUnkKeyEvent(m_hCtr, rEv.aData, rKeyNum);
                                Console.WriteLine("{0}.  Key \"{1}\"",
                                    nIdx + j,
                                    ZGIntf.CardNumToStr(rKeyNum, m_fProximity));
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_HOTEL40:
                        case ZG_CTR_EV_TYPE.ZG_EV_HOTEL41:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_HOTEL_MODE nMode = new ZG_HOTEL_MODE();
                                ZG_HOTEL_SUB_EV nSubEvent = new ZG_HOTEL_SUB_EV();
                                UInt32 nFlags = new UInt32();
                                ZGIntf.ZG_DecodeHotelEvent(rEv.aData, ref rTime, ref nMode, ref nSubEvent, ref nFlags);
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Mode: {7} Sub_event: {8} flags: {9:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond,
                                    EvTypeStrs[(int)rEv.nType],
                                    HModeStrs[(int)nMode],
                                    HotelSubEvStrs[(int)nSubEvent],
                                    nFlags);
                            }
                            break;
                        default:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_CTR_DIRECT nDirect = new ZG_CTR_DIRECT();
                                int nKeyIdx = 0;
                                int nKeyBank = 0;
                                ZGIntf.ZG_Ctr_DecodePassEvent(m_hCtr, rEv.aData, ref rTime, ref nDirect, ref nKeyIdx, ref nKeyBank);
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} {7} (key_idx: {8}, bank#: {9})",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond,
                                    DirectStrs[(int)nDirect],
                                    EvTypeStrs[(int)rEv.nType],
                                    nKeyIdx, nKeyBank);
                            }
                            break;
                    }
                }
                i += nCnt;
            }
        }

        static int CheckNotifyMsgs()
        {
            int hr;
            UInt32 nMsg = 0;
            IntPtr nMsgParam = IntPtr.Zero;
            while ((hr = ZGIntf.ZG_Ctr_GetNextMessage(m_hCtr, ref nMsg, ref nMsgParam)) == ZGIntf.S_OK)
            {
                switch (nMsg)
                {
                    case ZGIntf.ZG_N_CTR_NEW_EVENT:
                        {
                            ZG_N_NEW_EVENT_INFO pInfo = (ZG_N_NEW_EVENT_INFO)Marshal.PtrToStructure(nMsgParam, typeof(ZG_N_NEW_EVENT_INFO));
                            Console.WriteLine("==> Новые события: {0}.", pInfo.nNewCount);
                            ShowEvents(pInfo.nReadIdx, pInfo.nNewCount);
                            m_nAppReadEventIdx = pInfo.nWriteIdx;
                        }
                        break;
                }
            }
            if (hr == ZPIntf.ZP_S_NOTFOUND)
                hr = ZGIntf.S_OK;
            return hr;
        }

        static ManualResetEvent m_oEvent = null;
        static bool m_fThreadActive;
        static Thread m_oThread = null;
        static void DoNotifyWork()
        {
            while (m_fThreadActive)
            {
                if (m_oEvent.WaitOne())
                {
                    m_oEvent.Reset();
                    if (m_hCtr != IntPtr.Zero)
                        CheckNotifyMsgs();
                }
            }
        }

        static void StartNotifyThread()
        {
            if (m_oThread != null)
                return;
            m_fThreadActive = true;
            m_oThread = new Thread(DoNotifyWork);
            m_oThread.Start();
        }
        static void StopNotifyThread()
        {
            if (m_oThread == null)
                return;
            m_fThreadActive = false;
            m_oEvent.Set();
            // Wait until oThread finishes. Join also has overloads
            // that take a millisecond interval or a TimeSpan object.
            m_oThread.Join();
            m_oThread = null;
        }

        static void DoShowNewEvents()
        {
            int hr;
            int nWrIdx = 0;
            int nRdIdx = 0;
            hr = ZGIntf.ZG_Ctr_ReadEventIdxs(m_hCtr, ref nWrIdx, ref nRdIdx);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_ReadEventIdxs ({0}).", hr);
                Console.ReadLine();
                return;
            }
            int nNewCount;
            if (nWrIdx >= m_nAppReadEventIdx)
                nNewCount = (nWrIdx - m_nAppReadEventIdx);
            else
                nNewCount = (m_nCtrMaxEvents - m_nAppReadEventIdx + nWrIdx);
            if (nNewCount == 0)
                Console.WriteLine("Нет новых событий ({0}-{1}).", nRdIdx, nWrIdx);
            else
                Console.WriteLine("Доступно {0} новых событий ({1}-{2}).", nNewCount, nRdIdx, nWrIdx);
            int nShowCount;
            while (nNewCount > 0)
            {
                nShowCount = 25;
                if (nShowCount > nNewCount)
                    nShowCount = nNewCount;
                ShowEvents(m_nAppReadEventIdx, nShowCount);
                nNewCount -= nShowCount;
                m_nAppReadEventIdx = (m_nAppReadEventIdx + nShowCount) % m_nCtrMaxEvents;
                Console.WriteLine("Нажмите Enter для продолжения или 'x' для прерывания.");
                String s = Console.ReadLine();
                if (s == "x")
                {
                    Console.WriteLine("Прервано.");
                    return;
                }
            }
            Console.WriteLine("Успешно.");
        }

        static void DoShowAllEvents()
        {
            int hr;
            int nWrIdx = 0;
            int nRdIdx = 0;
            hr = ZGIntf.ZG_Ctr_ReadEventIdxs(m_hCtr, ref nWrIdx, ref nRdIdx);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_ReadEventIdxs ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Указатели событий: ук.чтения={0}, ук.записи={1}.", nRdIdx, nWrIdx);
            int nIdx, nTotalCount, nShowCount;
            nIdx = nWrIdx;
            nTotalCount = m_nCtrMaxEvents;
            while (nTotalCount > 0)
            {
                nShowCount = 25;
                if (nShowCount > nTotalCount)
                    nShowCount = nTotalCount;
                ShowEvents(nIdx, nShowCount);
                nTotalCount -= nShowCount;
                nIdx = (nIdx + nShowCount) % m_nCtrMaxEvents;
                Console.WriteLine("Нажмите Enter для продолжения или 'x' для прерывания.");
                String s = Console.ReadLine();
                if (s == "x")
                {
                    Console.WriteLine("Прервано.");
                    return;
                }
            }
            Console.WriteLine("Успешно.");
        }

        static void ToggleEventNotifyer()
        {
            bool fEnable = !m_fCtrNotifyEnabled;
            int hr;

            if (fEnable)
            {
                if (m_oEvent == null)
                    m_oEvent = new ManualResetEvent(false);
                ZG_CTR_NOTIFY_SETTINGS rNS = new ZG_CTR_NOTIFY_SETTINGS(
                    ZGIntf.ZG_NF_CTR_NEW_EVENT, m_oEvent.SafeWaitHandle, IntPtr.Zero, 0,
                    m_nAppReadEventIdx, 300, 0);
                hr = ZGIntf.ZG_Ctr_SetNotification(m_hCtr, rNS);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_SetNotification ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                StartNotifyThread();
            }
            else
            {
                StopNotifyThread();
                hr = ZGIntf.ZG_Ctr_SetNotification(m_hCtr, null);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_SetNotification ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
            }
            m_fCtrNotifyEnabled = fEnable;
            Console.WriteLine("Успешно.");
        }

        static void DoRestoreFactorySettings()
        {
            int hr;
            Console.WriteLine("Запись (0, 0)...");
            hr = ZGIntf.ZG_Ctr_WriteEventIdxs(m_hCtr, 0x3, 0, 0);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_WriteEventIdxs ({0}).", hr);
                Console.ReadLine();
                return;
            }
            m_nAppReadEventIdx = 0;
            Console.WriteLine("Успешно.");
        }

        static void Main(string[] args)
        {
            // Проверяем версию SDK
            UInt32 nVersion = ZGIntf.ZG_GetVersion();
            if ((((nVersion & 0xFF)) != ZGIntf.ZG_SDK_VER_MAJOR) || (((nVersion >> 8) & 0xFF) != ZGIntf.ZG_SDK_VER_MINOR))
            {
                Console.WriteLine("Неправильная версия SDK Guard.");
                Console.ReadLine();
                return;
            }

            IntPtr hCvt = new IntPtr(0);
            m_hCtr = new IntPtr(0);
            int hr;
            hr = ZGIntf.ZG_Initialize(ZPIntf.ZP_IF_NO_MSG_LOOP);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Initialize ({0}).", hr);
                Console.ReadLine();
                return;
            }
            try
            {
                ZG_CVT_INFO rInfo = new ZG_CVT_INFO();
                ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                rOp.nPortType = CvtPortType;
                rOp.pszName = CvtPortName;
                rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;
                hr = ZGIntf.ZG_Cvt_Open(ref hCvt, ref rOp, rInfo);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                ZG_CTR_INFO rCtrInfo = new ZG_CTR_INFO();
                hr = ZGIntf.ZG_Ctr_Open(ref m_hCtr, hCvt, CtrAddr, 0, ref rCtrInfo);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                m_nCtrMaxEvents = rCtrInfo.nMaxEvents;
                m_fProximity = ((rCtrInfo.nFlags & ZGIntf.ZG_CTR_F_PROXIMITY) != 0);
                m_nCtrFlags = rCtrInfo.nFlags;
                Console.WriteLine("{0} адрес: {1}, с/н: {2}, v{3}.{4}, Количество событий: {5}, Тип ключей: {6}.", 
                    CtrTypeStrs[(int)rCtrInfo.nType],
                    rCtrInfo.nAddr,
                    rCtrInfo.nSn,
                    rCtrInfo.nVersion & 0xff, (rCtrInfo.nVersion >> 8) & 0xff,
                    rCtrInfo.nMaxEvents,
                    KeyModeStrs[m_fProximity ? 1 : 0]);
                m_fCtrNotifyEnabled = false;
                int nWrIdx = 0;
                int nRdIdx = 0;
                hr = ZGIntf.ZG_Ctr_ReadEventIdxs(m_hCtr, ref nWrIdx, ref nRdIdx);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_ReadEventIdxs ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                m_nAppReadEventIdx = nWrIdx;
                Console.WriteLine("-----");
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - показать новые события");
                    Console.WriteLine("2 - показать все события ({0})", m_nCtrMaxEvents);
                    Console.WriteLine("3 - {0} уведомления о новых событиях.", 
                        m_fCtrNotifyEnabled ? "Выключить" : "Включить");
                    Console.WriteLine("9 - Восстановить заводские настройки (сброс индексов)");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    Console.WriteLine();
                    if (s != "")
                    {
                        switch (Convert.ToInt32(s))
                        {
                            case 1:
                                DoShowNewEvents();
                                break;
                            case 2:
                                DoShowAllEvents();
                                break;
                            case 3:
                                ToggleEventNotifyer();
                                break;
                            case 9:
                                DoRestoreFactorySettings();
                                break;
                            case 0:
                                return;
                            default:
                                Console.WriteLine("Неверная команда.");
                                break;
                        }
                    }
                    Console.WriteLine("-----");
                }
            }
            finally
            {
                StopNotifyThread();
                if (m_hCtr != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hCtr);
                if (hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(hCvt);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

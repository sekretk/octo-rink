using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ZGuard;
using ZPort;

namespace M2nElectroControl
{
    class Program
    {
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };

        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM3";
        public const Byte CtrAddr = 3;

        public static IntPtr m_hCtr;

        static void ShowConfig()
        {
            int hr;
            ZG_CTR_ELECTRO_CONFIG rEC = new ZG_CTR_ELECTRO_CONFIG();
            hr = ZGIntf.ZG_Ctr_ReadElectroConfig(m_hCtr, ref rEC);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_ReadElectroConfig ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Конфигурация питания: {0:X2}h.", rEC.nPowerConfig);
            Console.WriteLine("                 Активировано: {0}.", (rEC.nPowerConfig & ZGIntf.ZG_EC_CF_ENABLED) != 0);
            Console.WriteLine("                По расписанию: {0}.", (rEC.nPowerConfig & ZGIntf.ZG_EC_CF_SCHEDULE) != 0);
            Console.WriteLine("          Внешний считыватель: {0}.", (rEC.nPowerConfig & ZGIntf.ZG_EC_CF_EXT_READER) != 0);
            Console.WriteLine("          Инвертировать выход: {0}.", (rEC.nPowerConfig & ZGIntf.ZG_EC_CF_INVERT) != 0);
            Console.WriteLine("   Задействовать датчик двери: {0}.", (rEC.nPowerConfig & ZGIntf.ZG_EC_CF_EXIT_OFF) != 0);
            Console.WriteLine("         Не блокировать карты: {0}.", (rEC.nPowerConfig & ZGIntf.ZG_EC_CF_CARD_OPEN) != 0);
            Console.WriteLine("Время задержки в секундах: {0}.", rEC.nPowerDelay);
        }

        static void ShowState()
        {
            int hr;
            ZG_CTR_ELECTRO_STATE rES = new ZG_CTR_ELECTRO_STATE();
            hr = ZGIntf.ZG_Ctr_GetElectroState(m_hCtr, ref rES);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_GetElectroState ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Флаги питания: {0:X2}h.", rES.nPowerFlags);
            Console.WriteLine("\t           Питание включено: {0}.", (rES.nPowerFlags & ZGIntf.ZG_EC_SF_ENABLED) != 0);
            Console.WriteLine("\t    Включение по расписанию: {0}.", (rES.nPowerFlags & ZGIntf.ZG_EC_SF_SCHEDULE) != 0);
            Console.WriteLine("\t Включение командой по сети: {0}.", (rES.nPowerFlags & ZGIntf.ZG_EC_SF_REMOTE) != 0);
            Console.WriteLine("\t    Идет отработка задержки: {0}.", (rES.nPowerFlags & ZGIntf.ZG_EC_SF_DELAY) != 0);
            Console.WriteLine("\t   Карта в поле считывателя: {0}.", (rES.nPowerFlags & ZGIntf.ZG_EC_SF_CARD) != 0);
        }

        static void DoSetConfig()
        {
            string s;

            Console.WriteLine("Введите флаг 'Активировано' (0 или 1), флаг 'По расписанию', " +
                "флаг 'Внешний считыватель', флаг 'Инвертировать выход', " +
                "флаг 'Задействовать датчик двери', флаг 'Не блокировать карты', Время задержки (сек):");
            s = Console.ReadLine();
            string[] aValues = s.Split(',');
            if (aValues.Length < 7)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            int hr;
            ZG_CTR_ELECTRO_CONFIG rEC = new ZG_CTR_ELECTRO_CONFIG();

            rEC.nPowerConfig = 0;
            if (Convert.ToInt32(aValues[0]) != 1)
                rEC.nPowerConfig = ZGIntf.ZG_EC_CF_ENABLED;
            if (Convert.ToInt32(aValues[1]) != 1)
                rEC.nPowerConfig |= ZGIntf.ZG_EC_CF_SCHEDULE;
            if (Convert.ToInt32(aValues[2]) != 1)
                rEC.nPowerConfig |= ZGIntf.ZG_EC_CF_EXT_READER;
            if (Convert.ToInt32(aValues[3]) != 1)
                rEC.nPowerConfig |= ZGIntf.ZG_EC_CF_INVERT;
            if (Convert.ToInt32(aValues[4]) != 1)
                rEC.nPowerConfig |= ZGIntf.ZG_EC_CF_EXIT_OFF;
            if (Convert.ToInt32(aValues[5]) != 1)
                rEC.nPowerConfig |= ZGIntf.ZG_EC_CF_CARD_OPEN;
            rEC.nPowerDelay = Convert.ToUInt32(aValues[6]);
            hr = ZGIntf.ZG_Ctr_WriteElectroConfig(m_hCtr, ref rEC, false);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_WriteElectroConfig ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Успешно.");
        }

        static void DoSetPowerSchedule()
        {
            string s;

            Console.WriteLine("Введите маску дней недели (hex), время начала (чч:мм), время конца (чч:мм):");
            s = Console.ReadLine();
            string[] aValues = s.Split(',');
            if (aValues.Length < 3)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            ZG_CTR_TIMEZONE rTz = new ZG_CTR_TIMEZONE();
            rTz.nDayOfWeeks = Convert.ToByte(aValues[0], 16);
            string[] aTimes;
            aTimes = aValues[1].Split(':');
            if (aTimes.Length != 2)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            rTz.nBegHour = Convert.ToByte(aTimes[0]);
            rTz.nBegMinute = Convert.ToByte(aTimes[1]);
            aTimes = aValues[2].Split(':');
            if (aTimes.Length != 2)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            rTz.nEndHour = Convert.ToByte(aTimes[0]);
            rTz.nEndMinute = Convert.ToByte(aTimes[1]);
            Console.WriteLine("Запись...");
            int hr;
            hr = ZGIntf.ZG_Ctr_WriteTimeZones(m_hCtr, 6, ref rTz, 1, null, IntPtr.Zero);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_WriteTimeZones ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Успешно.");
        }

        static void DoTogglePower()
        {
            int hr;
            ZG_CTR_ELECTRO_STATE rES = new ZG_CTR_ELECTRO_STATE();
            hr = ZGIntf.ZG_Ctr_GetElectroState(m_hCtr, ref rES);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_GetElectroState ({0}).", hr);
                Console.ReadLine();
                return;
            }
            bool fOn = ((rES.nPowerFlags & ZGIntf.ZG_EC_SF_ENABLED) == 0);
            hr = ZGIntf.ZG_Ctr_SetElectroPower(m_hCtr, fOn);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_SetElectroPower ({0}).", hr);
                Console.ReadLine();
                return;
            }
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
                ZG_CVT_INFO rCvtInfo = new ZG_CVT_INFO();
                ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                rOp.nPortType = CvtPortType;    // тип порта
                rOp.pszName = CvtPortName;      // имя порта
                rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;    // скорость конвертера
                hr = ZGIntf.ZG_Cvt_Open(ref hCvt, ref rOp, rCvtInfo);
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
                if ((rCtrInfo.nFlags & ZGIntf.ZG_CTR_F_ELECTRO) == 0)
                {
                    Console.WriteLine("Функция ElectroControl не поддерживается.");
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine("{0} адрес: {1}, с/н: {2}, v{3}.{4}.",
                    CtrTypeStrs[(int)rCtrInfo.nType],
                    rCtrInfo.nAddr,
                    rCtrInfo.nSn,
                    rCtrInfo.nVersion & 0xff, (rCtrInfo.nVersion >> 8) & 0xff);
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - Показать конфигурацию");
                    Console.WriteLine("2 - Показать статус");
                    Console.WriteLine("6 - Установить конфигурацию...");
                    Console.WriteLine("7 - Установить расписание...");
                    Console.WriteLine("8 - Переключить питание");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    Console.WriteLine();
                    switch (Convert.ToInt32(s))
                    {
                        case 1:
                            ShowConfig();
                            break;
                        case 2:
                            ShowState();
                            break;
                        case 6:
                            DoSetConfig();
                            break;
                        case 7:
                            DoSetPowerSchedule();
                            break;
                        case 8:
                            DoTogglePower();
                            break;
                        case 0:
                            return;
                        default:
                            Console.WriteLine("Неверная команда.");
                            break;
                    }
                    Console.WriteLine("-----");
                }
            }
            finally
            {
                if (m_hCtr != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hCtr);
                if (hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(hCvt);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

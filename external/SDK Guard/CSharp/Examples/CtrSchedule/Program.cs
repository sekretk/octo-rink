using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ZGuard;
using ZPort;

namespace CtrSchedule
{
    class Program
    {
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };

        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM3";
        public const Byte CtrAddr = 3;

        public static IntPtr m_hCtr;
        public static int m_nCtrMaxBanks;

        static void ShowSchedule()
        {
            int hr;
            int i, j;
            ZG_CTR_TIMEZONE[] aTZs = new ZG_CTR_TIMEZONE[ZGIntf.ZG_MAX_TIMEZONES];

            for (i = 0; i < m_nCtrMaxBanks; i++)
            {
                hr = ZGIntf.ZG_Ctr_ReadTimeZones(m_hCtr, 0, aTZs, aTZs.Length, null, IntPtr.Zero, i);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_ReadTimeZones ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine("------------");
                Console.WriteLine("Банк № {0}:", i);
                for (j = 0; j < aTZs.Length; j++)
                {
                    Console.WriteLine("{0}. Дни недели: {1:X2}h, Время прохода: {2:D2}:{3:D2} - {4:D2}:{5:D2}", 
                        j,
                        aTZs[j].nDayOfWeeks,
                        aTZs[j].nBegHour, aTZs[j].nBegMinute,
                        aTZs[j].nEndHour, aTZs[j].nEndMinute);
                }
            }
            Console.WriteLine("Успешно.");
        }

        static void DoSetTimeZone()
        {
            string s;

            Console.WriteLine("Введите № банка, индекс временной зоны, " +
                "маску дней недели (hex), время начала (чч:мм), время конца (чч:мм):");
            s = Console.ReadLine();
            string[] aValues = s.Split(',');
            if (aValues.Length < 5)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            int nBankN = Convert.ToInt32(aValues[0]);
            int nTzIdx = Convert.ToInt32(aValues[1]);
            ZG_CTR_TIMEZONE rTz = new ZG_CTR_TIMEZONE();
            rTz.nDayOfWeeks = Convert.ToByte(aValues[2], 16);
            string[] aTimes;
            aTimes = aValues[3].Split(':');
            if (aTimes.Length != 2)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            rTz.nBegHour = Convert.ToByte(aTimes[0]);
            rTz.nBegMinute = Convert.ToByte(aTimes[1]);
            aTimes = aValues[4].Split(':');
            if (aTimes.Length != 2)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            rTz.nEndHour = Convert.ToByte(aTimes[0]);
            rTz.nEndMinute = Convert.ToByte(aTimes[1]);
            Console.WriteLine("Запись...");
            int hr;
            hr = ZGIntf.ZG_Ctr_WriteTimeZones(m_hCtr, nTzIdx, ref rTz, 1, null, IntPtr.Zero, nBankN);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_WriteTimeZones ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Успешно.");
        }

        static void DoRestoreFactorySettings()
        {
            ZG_CTR_TIMEZONE[] aTzs = new ZG_CTR_TIMEZONE[ZGIntf.ZG_MAX_TIMEZONES];
            int i;

            // Подготовка данных для записи в контроллер
            for (i = 0; i < aTzs.Length; i++)
            {
                aTzs[i].nDayOfWeeks = 0x7F;
                aTzs[i].nBegHour = 0;
                aTzs[i].nBegMinute = 0;
                aTzs[i].nEndHour = 23;
                aTzs[i].nEndMinute = 59;
            }
            int hr;
            Console.WriteLine("Запись (0x7F, 00:00-23:59)...");
            for (i = 0; i < m_nCtrMaxBanks; i++)
            {
                hr = ZGIntf.ZG_Ctr_WriteTimeZones(m_hCtr, 0, aTzs, aTzs.Length, null, IntPtr.Zero, i);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_WriteTimeZones ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
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
                m_nCtrMaxBanks = ((rCtrInfo.nFlags & ZGIntf.ZG_CTR_F_2BANKS) != 0) ? 2 : 1;
                Console.WriteLine("{0} адрес: {1}, с/н: {2}, v{3}.{4}, Количество банков: {5}.",
                    CtrTypeStrs[(int)rCtrInfo.nType],
                    rCtrInfo.nAddr,
                    rCtrInfo.nSn,
                    rCtrInfo.nVersion & 0xff, (rCtrInfo.nVersion >> 8) & 0xff,
                    m_nCtrMaxBanks);
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - показать расписание");
                    Console.WriteLine("2 - установить временную зону...");
                    Console.WriteLine("3 - восстановить заводские настройки (для всех банков)");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    if (s != "")
                    {
                        Console.WriteLine();
                        switch (Convert.ToInt32(s))
                        {
                            case 1:
                                ShowSchedule();
                                break;
                            case 2:
                                DoSetTimeZone();
                                break;
                            case 3:
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
                if (m_hCtr != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hCtr);
                if (hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(hCvt);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

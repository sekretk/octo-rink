using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ZGuard;
using ZPort;

namespace CtrLockTimes
{
    class Program
    {
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };

        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM9";
        public const Byte CtrAddr = 2;

        public static IntPtr m_hCtr;
        public static int m_nCtrMaxBanks;

        static void ShowLockTimes()
        {
            int hr;
            int i;
            UInt32 nOpen = 0;
            UInt32 nLet = 0;
            UInt32 nMax = 0;

            for (i = 0; i < m_nCtrMaxBanks; i++)
            {
                hr = ZGIntf.ZG_Ctr_ReadLockTimes(m_hCtr, ref nOpen, ref nLet, ref nMax, i);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_ReadLockTimes ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine("------------");
                Console.WriteLine("Банк № {0}:", i);
                Console.WriteLine("Время открытия (мс): {0}", nOpen);
                Console.WriteLine("Время контроля закрытой двери (мс): {0}", nLet);
                Console.WriteLine("Время контроля открытой двери (мс): {0}", nMax);
            }
            Console.WriteLine("Успешно.");
        }

        static void DoSetLockTimes()
        {
            string s;

            Console.WriteLine("Введите № банка, время открытия (мс), время контроля закрытой двери (мс), время контроля открытой двери (мс) (-1 не менять):");
            s = Console.ReadLine();
            string[] aValues = s.Split(',');
            if (aValues.Length < 4)
            {
                Console.WriteLine("Некорректный ввод.");
                return;
            }
            int nBankN, nOpen, nLet, nMax;
            UInt32 nMask = 0;
            nBankN = Convert.ToInt32(aValues[0]);
            nOpen = Convert.ToInt32(aValues[1]);
            nLet = Convert.ToInt32(aValues[2]);
            nMax = Convert.ToInt32(aValues[3]);
            if (nOpen != -1)
                nMask = 1;
            if (nLet != -1)
                nMask |= 2;
            if (nMax != -1)
                nMask |= 4;
            Console.WriteLine("Запись...");
            int hr;
            hr = ZGIntf.ZG_Ctr_WriteLockTimes(m_hCtr, nMask, (UInt32)nOpen, (UInt32)nLet, (UInt32)nMax, nBankN);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_WriteLockTimes ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Успешно.");
        }

        static void DoOpenLock(int nLockN)
        {
            int hr;
            hr = ZGIntf.ZG_Ctr_OpenLock(m_hCtr, nLockN);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_OpenLock ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Успешно.");
        }

        static void DoRestoreFactorySettings()
        {
            Console.WriteLine("Запись (3000, 0, 0)...");
            int hr;
            int i;
            for (i = 0; i < m_nCtrMaxBanks; i++)
            {
                hr = ZGIntf.ZG_Ctr_WriteLockTimes(m_hCtr, 0x7, 3000, 0, 0, i);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_WriteLockTimes (банк№ {0}) ({1}).", i, hr);
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
                    Console.WriteLine("1 - показать времена замков");
                    Console.WriteLine("2 - установить времена замков...");
                    Console.WriteLine("3 - открыть замок (Вход)");
                    Console.WriteLine("4 - открыть замок (Выход)");
                    Console.WriteLine("9 - восстановить заводские настройки (для всех банков)");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    if (s != "")
                    {
                        Console.WriteLine();
                        switch (Convert.ToInt32(s))
                        {
                            case 1:
                                ShowLockTimes();
                                break;
                            case 2:
                                DoSetLockTimes();
                                break;
                            case 3:
                                DoOpenLock(0);
                                break;
                            case 4:
                                DoOpenLock(1);
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
                if (m_hCtr != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hCtr);
                if (hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(hCvt);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

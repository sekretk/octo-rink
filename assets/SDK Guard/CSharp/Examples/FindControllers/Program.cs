using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Threading;
using ZGuard;
using ZPort;

namespace FindControllers
{
    class Program
    {
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };
        public static readonly string[] KeyModeStrs = { "Touch Memory", "Proximity" };

        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM9";

        public static IntPtr m_hCvt = IntPtr.Zero;
        public static int g_nCtrCount;
        public static bool m_fNotifyEnabled;

        static bool EnumCtrsCB(ref ZG_FIND_CTR_INFO pInfo, int nPos, int nMax, IntPtr pUserData)
        {
            Console.WriteLine("{0}, адрес: {1}, с/н: {2}, v{3}.{4}, кл.: {5}, соб.: {6}, {7};",
                CtrTypeStrs[(int)pInfo.nType],
                pInfo.nAddr,
                pInfo.nSn,
                pInfo.nVersion & 0xff, (pInfo.nVersion >> 8) & 0xff,
                pInfo.nMaxKeys,
                pInfo.nMaxEvents,
                KeyModeStrs[((pInfo.nFlags & ZGIntf.ZG_CTR_F_PROXIMITY) != 0) ? 1 : 0]);
            g_nCtrCount++;
            return true;
        }

        static void DoGuardFindCtrs()
        {
            int hr;

            Console.WriteLine("Поиск контроллеров...");
            Console.WriteLine("-------------");
            g_nCtrCount = 0;
            hr = ZGIntf.ZG_Cvt_EnumControllers(m_hCvt, EnumCtrsCB, IntPtr.Zero);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Cvt_EnumControllers ({0}).", hr);
                Console.ReadLine();
                return;
            }
            if (g_nCtrCount > 0)
            {
                Console.WriteLine("-------------");
                Console.WriteLine("Найдено {0} контроллеров.", g_nCtrCount);
            }
            else
                Console.WriteLine("Контроллеры не найдены.");
        }

        static int CheckNotifyMsgs()
        {
            int hr;
            UInt32 nMsg = 0;
            IntPtr nMsgParam = IntPtr.Zero;
            while ((hr = ZGIntf.ZG_Cvt_GetNextMessage(m_hCvt, ref nMsg, ref nMsgParam)) == ZGIntf.S_OK)
            {
                switch (nMsg)
                {
                    case ZGIntf.ZG_N_CVT_CTR_INSERT:
                    case ZGIntf.ZG_N_CVT_CTR_REMOVE:
                        {
                            ZG_FIND_CTR_INFO pInfo = (ZG_FIND_CTR_INFO)Marshal.PtrToStructure(nMsgParam, typeof(ZG_FIND_CTR_INFO));
                            Console.WriteLine("{0}  {1}, адрес: {2}, с/н: {3}, v{4}.{5}, кл.: {6}, соб.: {7}, {8};",
                                (nMsg == ZGIntf.ZG_N_CVT_CTR_INSERT) ? "Включен" : "Выключен",
                                CtrTypeStrs[(int)pInfo.nType],
                                pInfo.nAddr,
                                pInfo.nSn,
                                pInfo.nVersion & 0xff, (pInfo.nVersion >> 8) & 0xff,
                                pInfo.nMaxKeys,
                                pInfo.nMaxEvents,
                                KeyModeStrs[((pInfo.nFlags & ZGIntf.ZG_CTR_F_PROXIMITY) != 0) ? 1 : 0]);
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
                    if (m_hCvt != IntPtr.Zero)
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

        static void EnableNotification(bool fEnable=true, bool fReport=true)
        {
            int hr;
            if (fEnable)
            {
                if (m_oEvent == null)
                    m_oEvent = new ManualResetEvent(false);
                ZG_CVT_NOTIFY_SETTINGS rNS = new ZG_CVT_NOTIFY_SETTINGS(
                    ZGIntf.ZG_NF_CVT_CTR_EXIST, m_oEvent.SafeWaitHandle, IntPtr.Zero, 0,
                    1500,  // Период сканирования контроллеров
                    0);
                hr = ZGIntf.ZG_Cvt_SetNotification(m_hCvt, rNS);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_SetNotification ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                StartNotifyThread();
            }
            else
            {
                StopNotifyThread();
                hr = ZGIntf.ZG_Cvt_SetNotification(m_hCvt, null);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_SetNotification ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
            }
            m_fNotifyEnabled = fEnable;
            if (fReport)
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
                //rOp.nCvtType = ZG_CVT_TYPE.ZG_CVT_Z397;

                hr = ZGIntf.ZG_Cvt_Open(ref m_hCvt, ref rOp, rInfo);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine();
                DoGuardFindCtrs();
                EnableNotification(true, false);
                Console.WriteLine("-----");
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - пересканировать");
                    Console.WriteLine("2 - {0} уведомления.",
                        m_fNotifyEnabled ? "Выключить" : "Включить");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    if (s != "")
                    {
                        Console.WriteLine();
                        switch (Convert.ToInt32(s))
                        {
                            case 1:
                                DoGuardFindCtrs();
                                break;
                            case 2:
                                EnableNotification(!m_fNotifyEnabled);
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
                if (m_hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hCvt);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

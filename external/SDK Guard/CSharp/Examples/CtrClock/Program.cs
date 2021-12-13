using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Threading;
using ZGuard;
using ZPort;

namespace CtrClock
{
    class Program
    {
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };

        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM3";
        public const Byte CtrAddr = 3;

        public static IntPtr m_hCtr;

        static void ShowClock()
        {
            int hr;
            ZG_CTR_CLOCK rCtrTime = new ZG_CTR_CLOCK();

            hr = ZGIntf.ZG_Ctr_GetClock(m_hCtr, ref rCtrTime);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_GetClock ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("{0:D2}.{1:D2}.{2:D4} {3:D2}:{4:D2}:{5:D2} (stopped: {6})",
                rCtrTime.nDay,
                rCtrTime.nMonth,
                rCtrTime.nYear,
                rCtrTime.nHour,
                rCtrTime.nMinute,
                rCtrTime.nSecond,
                rCtrTime.fStopped);
        }

        static void SyncWithPC()
        {
            int hr;
            ZG_CTR_CLOCK rCtrTime = new ZG_CTR_CLOCK();
            DateTime rPcTime = DateTime.Now;

            rCtrTime.nYear = (ushort)rPcTime.Year;
            rCtrTime.nMonth = (ushort)rPcTime.Month;
            rCtrTime.nDay = (ushort)rPcTime.Day;
            rCtrTime.nHour = (ushort)rPcTime.Hour;
            rCtrTime.nMinute = (ushort)rPcTime.Minute;
            rCtrTime.nSecond = (ushort)rPcTime.Second;

            hr = ZGIntf.ZG_Ctr_SetClock(m_hCtr, ref rCtrTime);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_SetClock ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Время синхронизировано.");
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
                    case ZGIntf.ZG_N_CTR_CLOCK:
                        {
                            Int64 nOffs = (Int64)Marshal.PtrToStructure(nMsgParam, typeof(Int64));
                            Console.WriteLine("Рассинхронизация часов {0} сек.", nOffs);
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
                ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                rOp.nPortType = CvtPortType;
                rOp.pszName = CvtPortName;
                rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;
                ZG_CVT_INFO rInfo = new ZG_CVT_INFO();
                hr = ZGIntf.ZG_Cvt_Open(ref hCvt, ref rOp, rInfo);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                ZG_CTR_INFO rCtrInfo = new ZG_CTR_INFO();
                //string s2 = new string('\0', 1000);
                //rCtrInfo.pszLinesBuf = s2;
                //rCtrInfo.nLinesBufMax = 1000;
                hr = ZGIntf.ZG_Ctr_Open(ref m_hCtr, hCvt, CtrAddr, 0, ref rCtrInfo);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine("{0} адрес: {1}, с/н: {2}, v{3}.{4}.",
                    CtrTypeStrs[(int)rCtrInfo.nType],
                    rCtrInfo.nAddr,
                    rCtrInfo.nSn,
                    rCtrInfo.nVersion & 0xff, (rCtrInfo.nVersion >> 8) & 0xff);

                m_oEvent = new ManualResetEvent(false);
                ZG_CTR_NOTIFY_SETTINGS rNS = new ZG_CTR_NOTIFY_SETTINGS(
                    ZGIntf.ZG_NF_CTR_CLOCK, m_oEvent.SafeWaitHandle, IntPtr.Zero, 0,
                    0, 
                    1000, // Период проверки часов в миллисекундах
                    5 // Допустимое расхождение часов в секундах
                );
                hr = ZGIntf.ZG_Ctr_SetNotification(m_hCtr, rNS);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_FindNotification ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                StartNotifyThread();
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - прочитать часы");
                    Console.WriteLine("2 - синхронизировать с часами ПК");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    Console.WriteLine();
                    switch (Convert.ToInt32(s))
                    {
                        case 1:
                            ShowClock();
                            break;
                        case 2:
                            SyncWithPC();
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

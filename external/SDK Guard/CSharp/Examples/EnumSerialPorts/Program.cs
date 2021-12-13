using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Threading;
using ZGuard;
using ZPort;

namespace EnumSerialPorts
{
    class Program
    {
        static string[] PortTypeStrs = { "Неизвестный", "COM", "FT", "IP", "IPS" };

        static IntPtr m_hNotify = IntPtr.Zero;


        static int CheckNotifyMsgs()
        {
            int hr;
            UInt32 nMsg = 0;
            IntPtr nMsgParam = IntPtr.Zero;
            while ((hr = ZPIntf.ZP_DD_GetNextMessage(m_hNotify, ref nMsg, ref nMsgParam)) == ZPIntf.S_OK)
            {
                switch (nMsg)
                {
                    case ZPIntf.ZP_N_INSERT:
                        {
                            ZP_DDN_PORT_INFO pInfo = (ZP_DDN_PORT_INFO)Marshal.PtrToStructure(nMsgParam, typeof(ZP_DDN_PORT_INFO));
                            Console.WriteLine("Порт подключен: {0} ({1}){2};",
                               pInfo.rPort.szName,
                               pInfo.rPort.szFriendly,
                               ((pInfo.rPort.nFlags & ZPIntf.ZP_PIF_BUSY) != 0) ? " занят" : "");
                        }
                        break;
                    case ZPIntf.ZP_N_REMOVE:
                        {
                            ZP_DDN_PORT_INFO pInfo = (ZP_DDN_PORT_INFO)Marshal.PtrToStructure(nMsgParam, typeof(ZP_DDN_PORT_INFO));
                            Console.WriteLine("Порт отключен: {0} ({1})",
                              pInfo.rPort.szName,
                              pInfo.rPort.szFriendly);
                        }
                        break;
                    case ZPIntf.ZP_N_CHANGE:
                        {
                            ZP_DDN_PORT_INFO pInfo = (ZP_DDN_PORT_INFO)Marshal.PtrToStructure(nMsgParam, typeof(ZP_DDN_PORT_INFO));
                            Console.WriteLine("Порт изменен ({0:X2}): {1} ({2}){3};",
                                pInfo.nChangeMask,
                                pInfo.rPort.szName,
                                pInfo.rPort.szFriendly,
                                ((pInfo.rPort.nFlags & ZPIntf.ZP_PIF_BUSY) != 0) ? " занят" : "");
                        }
                        break;
                }
            }
            if (hr == ZPIntf.ZP_S_NOTFOUND)
                hr = ZPIntf.S_OK;
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
                    if (m_hNotify != null)
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
            UInt32 nVerMajor = (nVersion & 0xFF);
            UInt32 nVerMinor = ((nVersion >> 8) & 0xFF);
            UInt32 nVerBuild = ((nVersion >> 16) & 0xFF);
            Console.WriteLine("SDK Guard v{0}.{1}.{2}", nVerMajor, nVerMinor, nVerBuild);
            if ((nVerMajor != ZGIntf.ZG_SDK_VER_MAJOR) || (nVerMinor != ZGIntf.ZG_SDK_VER_MINOR))
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
                Console.WriteLine("Поиск последовательных портов...");
                int nPortCount = 0;
                IntPtr hList = new IntPtr();
                hr = ZGIntf.ZG_GetPortInfoList(ref hList, ref nPortCount);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_EnumSerialPorts ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                try
                {
                    ZP_PORT_INFO rPI = new ZP_PORT_INFO();
                    for (int i = 0; i < nPortCount; i++)
                    {
                        ZPIntf.ZP_GetPortInfo(hList, i, ref rPI);
                        Console.WriteLine("{0}. {1} ({2}); {3}",
                           (i + 1),
                           rPI.szName,
                           rPI.szFriendly,
                           ((rPI.nFlags & ZPIntf.ZP_PIF_BUSY) != 0) ? "Занят" : "");
                    }
                }
                finally
                {
                    ZGIntf.ZG_CloseHandle(hList);
                }
                Console.WriteLine("--------------");
                if (nPortCount > 0)
                    Console.WriteLine("Найдено {0} портов.", nPortCount);
                else
                    Console.WriteLine("Порты не найдены.");

                // Настраиваем уведомления
                m_oEvent = new ManualResetEvent(false);
                ZP_DD_NOTIFY_SETTINGS rNS = new ZP_DD_NOTIFY_SETTINGS();
                rNS.nNMask = ZPIntf.ZP_NF_EXIST | ZPIntf.ZP_NF_CHANGE;
                rNS.hEvent = m_oEvent.SafeWaitHandle;
                hr = ZGIntf.ZG_SetNotification(ref m_hNotify, ref rNS, true, false);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_SetNotification ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                StartNotifyThread();
                Console.WriteLine("Ожидание событий...");
                Console.ReadLine();
            }
            finally
            {
                StopNotifyThread();
                if (m_hNotify != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hNotify);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

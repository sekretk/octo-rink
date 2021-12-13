using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using ZGuard;
using ZPort;

namespace EnumConverters
{
    class Program
    {
        static string[] PortTypeStrs = { "Неизвестный", "COM", "FT", "IP", "IPS" };
        static string[] CvtTypeStrs = { "Неизвестный", "Z-397", "Z-397 Guard", "Z-397 IP", "Z-397 Web", "Z5R Web" };
        static string[] GuardModeStrs = { "Неизвестный", "Normal", "Advanced", "Test", "Accept" };
        public static readonly string[] KeyModeStrs = { "Touch Memory", "Proximity" };
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };

        public static int g_nCtrCount;
        public static IntPtr m_hCvt = IntPtr.Zero;
        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM9";

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
                // Перечисляем конвертеры
                int nDevCount = 0;
                ZP_WAIT_SETTINGS rWS = new ZP_WAIT_SETTINGS(300, 1, IntPtr.Zero, 0, 0, 0, 0);
                ZP_SEARCH_PARAMS rSP = new ZP_SEARCH_PARAMS();
                Console.WriteLine("Перечисление конвертеров...");
                IntPtr hSearch = IntPtr.Zero;
                hr = ZGIntf.ZG_SearchDevices(ref hSearch, ref rSP);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_SearchDevices ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                ZG_ENUM_IPCVT_INFO rInfo = new ZG_ENUM_IPCVT_INFO();
                ZP_PORT_INFO[] aPIs = new ZP_PORT_INFO[2];
                int nPortCount = 0;

                while ((hr = ZGIntf.ZG_FindNextDevice(hSearch, ref rInfo, aPIs, aPIs.Length, ref nPortCount)) == ZGIntf.S_OK)
                {
                    if (rInfo.nType != ZG_CVT_TYPE.ZG_CVT_UNDEF)
                    {
                        Console.WriteLine("{0}. {1}, с/н: {2}, v{3}.{4}.{5}, режим: {6};",
                            ++nDevCount,
                            CvtTypeStrs[(int)rInfo.nType],
                            rInfo.rBase.nSn,
                            rInfo.rBase.nVersion & 0xff, (rInfo.rBase.nVersion >> 8) & 0xff, (rInfo.rBase.nVersion >> 16) & 0xff,
                            GuardModeStrs[(int)rInfo.nMode]);
                    }
                    else
                        Console.WriteLine("{0}. Неизвестное устройство;", ++nDevCount);
                    for (int i = 0; i < nPortCount; i++)
                    {
                        Console.WriteLine("\t{0}. {1} ({2}){3};",
                            (i + 1),
                            aPIs[i].szName,
                            aPIs[i].szFriendly,
                            ((aPIs[i].nFlags & ZPIntf.ZP_PIF_BUSY) != 0) ? " занят" : "");
                    }

                    ZG_CVT_INFO rsInfo = new ZG_CVT_INFO();
                    ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                    rOp.nPortType = CvtPortType;
                    rOp.pszName = CvtPortName;
                    rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;

                    hr = ZGIntf.ZG_Cvt_Open(ref m_hCvt, ref rOp, rsInfo);
                    if (hr < 0)
                    {
                        Console.WriteLine("Ошибка ZG_Cvt_Open ({0}).", hr);
                        Console.ReadLine();
                        return;
                    }
                    Console.WriteLine();
                    DoGuardFindCtrs();
                }
                Console.WriteLine("--------------");
                if (nDevCount > 0)
                    Console.WriteLine("Найдено {0} конвертеров.", nDevCount);
                else
                    Console.WriteLine("Конвертеры не найдены.");
            }
            finally
            {
                if (m_hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hCvt);

                ZGIntf.ZG_Finalyze();
            }
            Console.ReadLine();
        }
    }
}

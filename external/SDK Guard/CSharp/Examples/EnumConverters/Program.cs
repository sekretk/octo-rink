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
                }
                Console.WriteLine("--------------");
                if (nDevCount > 0)
                    Console.WriteLine("Найдено {0} конвертеров.", nDevCount);
                else
                    Console.WriteLine("Конвертеры не найдены.");
            }
            finally
            {
                ZGIntf.ZG_Finalyze();
            }
            Console.ReadLine();
        }
    }
}

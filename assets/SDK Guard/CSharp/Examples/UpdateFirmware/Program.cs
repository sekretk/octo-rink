using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using ZGuard;
using ZPort;

namespace UpdateFirmware
{
    class Program
    {
        static string[] CvtTypeStrs = { "Неизвестный", "Z-397", "Z-397 Guard", "Z-397 IP", "Z-397 Web", "Z5R Web" };


        public static byte[] DoLoadFW(string filePath)
        {
            byte[] buffer;
            FileStream fileStream = new FileStream(filePath, FileMode.Open, FileAccess.Read);
            try
            {
                int length = (int)fileStream.Length;  // get file length
                buffer = new byte[length];            // create buffer

                fileStream.Read(buffer, 0, length);
            }
            finally
            {
                fileStream.Close();
            }
            return buffer;
        }

        static bool UpdateFW_CB(int nPos, int nMax, IntPtr pUserData)
        {
            Console.Write("\rПрошивка {0,3}%...", (nPos * 100) / nMax);
            return true;
        }

        static void DoCvtUpdateFW()
        {
            string s;

            Console.WriteLine("Введите имя файла прошивки:");
            s = Console.ReadLine();
            if (s == "")
            {
                Console.WriteLine("Отменено.");
                return;
            }
            if (!System.IO.File.Exists(s))
            {
                Console.WriteLine("Файл не найден.");
                return;
            }
            byte[] pData = DoLoadFW(s);
            int nPortCount = 0;
            IntPtr hPIL = IntPtr.Zero;

            ZGIntf.ZG_GetPortInfoList(ref hPIL, ref nPortCount);
            try
            {
                ZP_PORT_INFO rPI = new ZP_PORT_INFO();
                for (int i = 0; i < nPortCount; i++)
                {
                    ZPIntf.ZP_GetPortInfo(hPIL, i, ref rPI);
                    if ((rPI.nType == ZP_PORT_TYPE.ZP_PORT_COM) || 
                            ((rPI.nType == ZP_PORT_TYPE.ZP_PORT_FT) && (rPI.szFriendly != "")))
                        Console.WriteLine("{0} {1}", 
                            (rPI.nType == ZP_PORT_TYPE.ZP_PORT_COM) ? rPI.szName : rPI.szFriendly,
                            ((rPI.nFlags & ZPIntf.ZP_PIF_BUSY) != 0) ? "занят" : "");
                }
            }
            finally
            {
                ZGIntf.ZG_CloseHandle(hPIL);
            }
            Console.WriteLine("--");

            Console.WriteLine("Введите имя COM-порта:");
            s = Console.ReadLine();
            if (s == "")
            {
                Console.WriteLine("Отменено.");
                return;
            }
            int hr;
            ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
            rOp.nPortType = ZP_PORT_TYPE.ZP_PORT_COM;    // тип порта
            rOp.pszName = s;      // имя порта
            rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;    // скорость конвертера
            hr = ZGIntf.ZG_UpdateCvtFirmware(ref rOp, pData, pData.Length, UpdateFW_CB, IntPtr.Zero);
            Console.WriteLine();
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_UpdateCvtFirmware ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Завершено.");
        }

        static bool CvtEnum(ref ZG_ENUM_CVT_INFO pInfo, ref ZP_PORT_INFO pPort, IntPtr pUserData)
        {
            if ((pPort.nType == ZP_PORT_TYPE.ZP_PORT_FT) && (pPort.szFriendly != ""))
            {
                if (pInfo.nType != ZG_CVT_TYPE.ZG_CVT_UNDEF)
                    Console.WriteLine("{0}, {1}, {2} с/н: {3}, v{4}.{5}",
                        pPort.szFriendly,
                        ((pPort.nFlags & 1) != 0) ? "занят" : "",
                        CvtTypeStrs[(int)pInfo.nType],
                        pInfo.rBase.nSn,
                        pInfo.rBase.nVersion & 0xff, (pInfo.rBase.nVersion >> 8) & 0xff);
                else
                    Console.WriteLine("{0}, {1}", pPort.szFriendly, ((pPort.nFlags & 1) != 0) ? "занят" : "");
            }
            return true;
        }

        static void DoCtrUpdateFW()
        {
            string s;

            Console.WriteLine("Введите имя файла прошивки:");
            s = Console.ReadLine();
            if (s == "")
            {
                Console.WriteLine("Отменено.");
                return;
            }
            if (!System.IO.File.Exists(s))
            {
                Console.WriteLine("Файл не найден.");
                return;
            }
            byte[] pData = DoLoadFW(s);
            IntPtr hSD = IntPtr.Zero;
            ZP_SEARCH_PARAMS rSP = new ZP_SEARCH_PARAMS();
            ZG_ENUM_IPCVT_INFO rDI = new ZG_ENUM_IPCVT_INFO();
            ZP_PORT_INFO[] aPIs = new ZP_PORT_INFO[1];
            int nPortCount = 0;
            string sComPort;
            //ZGIntf.ZG_EnumConverters(null, 0, CvtEnum, IntPtr.Zero);
            ZGIntf.ZG_SearchDevices(ref hSD, ref rSP, true, false);
            try
            {
                while (ZGIntf.ZG_FindNextDevice(hSD, ref rDI, aPIs, aPIs.Length, ref nPortCount) == ZGIntf.S_OK)
                {
                    if ((aPIs[0].nType == ZP_PORT_TYPE.ZP_PORT_COM) ||
                            ((aPIs[0].nType == ZP_PORT_TYPE.ZP_PORT_FT) && (aPIs[0].szFriendly != "")))
                    {
                        sComPort = (aPIs[0].nType == ZP_PORT_TYPE.ZP_PORT_COM) ? aPIs[0].szName : aPIs[0].szFriendly;
                        if (rDI.nType != ZG_CVT_TYPE.ZG_CVT_UNDEF)
                            Console.WriteLine("{0}  {1} с/н: {2}, v{3}.{4}.{5}{6}",
                                sComPort,
                                CvtTypeStrs[(int)rDI.nType],
                                rDI.rBase.nSn,
                                rDI.rBase.nVersion & 0xff, (rDI.rBase.nVersion >> 8) & 0xff, (rDI.rBase.nVersion >> 16) & 0xff,
                                ((aPIs[0].nFlags & ZPIntf.ZP_PIF_BUSY) != 0) ? " занят" : "");
                        else
                            Console.WriteLine("{0}{1};",
                                sComPort, 
                                ((aPIs[0].nFlags & ZPIntf.ZP_PIF_BUSY) != 0) ? " занят" : "");
                    }
                }
            }
            finally
            {
                ZGIntf.ZG_CloseHandle(hSD);
            }
            Console.WriteLine("--");

            Console.WriteLine("Введите имя COM-порта:");
            s = Console.ReadLine();
            if (s == "")
            {
                Console.WriteLine("Отменено.");
                return;
            }
            IntPtr hCvt = IntPtr.Zero;
            try
            {
                int hr;
                ZG_CVT_INFO rInfo = new ZG_CVT_INFO();
                ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                rOp.nPortType = ZP_PORT_TYPE.ZP_PORT_COM;    // тип порта
                rOp.pszName = s;      // имя порта
                rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_19200;    // скорость конвертера
                hr = ZGIntf.ZG_Cvt_Open(ref hCvt, ref rOp, rInfo);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine("Введите с/н контроллера (0 для отмены):");
                s = Console.ReadLine();
                if (s == "")
                {
                    Console.WriteLine("Отменено.");
                    return;
                }
                int nCtrSn = Convert.ToInt32(s);
                if ((nCtrSn <= 0) || (nCtrSn >= 65535))
                {
                    Console.WriteLine("Некорректный ввод.");
                    return;
                }
                hr = ZGIntf.ZG_Cvt_UpdateCtrFirmware(hCvt, (UInt16)nCtrSn, pData, pData.Length, "", UpdateFW_CB, IntPtr.Zero);
                Console.WriteLine();
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_UpdateCtrFirmware ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine("Завершено.");
            }
            finally
            {
                if (hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(hCvt);
            }
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
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - Обновить прошивку конвертера...");
                    Console.WriteLine("2 - Обновить прошивку контроллера...");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    Console.WriteLine();
                    switch (Convert.ToInt32(s))
                    {
                        case 1:
                            DoCvtUpdateFW();
                            break;
                        case 2:
                            DoCtrUpdateFW();
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
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

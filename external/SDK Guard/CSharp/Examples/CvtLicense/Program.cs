using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Globalization;
using ZGuard;
using ZPort;

namespace CvtLicense
{
    public class Win32
    {
            [DllImport("Kernel32.dll", CharSet = CharSet.Auto)]
            public static extern int GetPrivateProfileString(String sSection, String sKey, String sDefault,
                StringBuilder sResult, int nSize, String sFile);
    }

    class Program
    {
        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM12";

        public static IntPtr m_hCvt;

        static void ShowLicense()
        {
            int hr;
            ZG_CVT_LIC_INFO rInfo = new ZG_CVT_LIC_INFO();

            hr = ZGIntf.ZG_Cvt_GetLicense(m_hCvt, ZGIntf.ZG_DEF_CVT_LICN, ref rInfo);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Cvt_GetLicense ({0}).", hr);
                Console.ReadLine();
                return;
            }
            Console.WriteLine("Статус: {0}", rInfo.nStatus);
            if (rInfo.nMaxCtrs == 0xFF)
                Console.WriteLine("Максимум контроллеров: не ограничено");
            else
                Console.WriteLine("Максимум контроллеров: {0}", rInfo.nMaxCtrs);
            if (rInfo.nMaxKeys == 0xFFFF)
                Console.WriteLine("Максимум ключей: не ограничено");
            else
                Console.WriteLine("Максимум ключей: {0}", rInfo.nMaxKeys);
            if (rInfo.nMaxYear == 0xFFFF)
                Console.WriteLine("Максимальная дата: не ограничено");
            else
                Console.WriteLine("Максимальная дата: {0}.{1}.{2}",
                    rInfo.nMaxDay, rInfo.nMaxMon, rInfo.nMaxYear);
            if (rInfo.nDownCountTime == 0xFFFF)
                Console.WriteLine("Счетчик: не ограничено");
            else
                Console.WriteLine("Счетчик: {0}", rInfo.nDownCountTime);
        }

        static void ShowAllLicenses()
        {
            int hr;
            ZG_CVT_LIC_SINFO[] aLic = new ZG_CVT_LIC_SINFO[ZGIntf.ZG_MAX_LICENSES];
            int nCount = 0;

            hr = ZGIntf.ZG_Cvt_GetAllLicenses(m_hCvt, aLic, aLic.Length, ref nCount);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Cvt_GetAllLicenses ({0}).", hr);
                Console.ReadLine();
                return;
            }
            if (nCount == 0)
            {
                Console.WriteLine("Installed licenses not found.");
                return;
            }
            for (int i = 0; i < nCount; i++)
            {
                Console.WriteLine("{0:D2}({1}/{2});", aLic[i].nLicN, aLic[i].nMaxCtrs, aLic[i].nMaxKeys);
            }
        }

        static void DoSetLicense()
        {
            string sFilename;
            Console.WriteLine("Введите имя файла лицензии (абсолютный путь):");
            sFilename = Console.ReadLine();
            if (sFilename == "")
            {
                Console.WriteLine("Отменено.");
                return;
            }
            if (!System.IO.File.Exists(sFilename))
            {
                Console.WriteLine("Файл не найден.");
                return;
            }
            sFilename = System.IO.Path.GetFullPath(sFilename);
            StringBuilder sLicHex = new StringBuilder(1024);
            int nLicHexLen;
            Byte[] aLicData;
            int nLicDataLen;
            UInt16 nLicStatus = 0;

            nLicHexLen = Win32.GetPrivateProfileString("Lic", "Txt", null, sLicHex, 1024, sFilename);
            if ((nLicHexLen > 0) && (nLicHexLen < (sLicHex.Capacity - 1)))
            {
                nLicDataLen = nLicHexLen / 2;
                if (nLicDataLen == 0)
                {
                    Console.WriteLine("Нет данных лицензии.");
                    return;
                }
                aLicData = new Byte[nLicDataLen];

                int j = 0;
                for (int i = 0; i < nLicHexLen; i++)
                    aLicData[j++] = byte.Parse(string.Concat(sLicHex[i], sLicHex[++i]), NumberStyles.HexNumber);

                int hr;
                hr = ZGIntf.ZG_Cvt_SetLicenseData(m_hCvt, 5, aLicData, nLicDataLen, ref nLicStatus);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_SetLicenseData ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                Console.WriteLine("Успешно.");
            }
            else
                Console.WriteLine("Некорректные данные лицензии.");
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
                // настраиваем структуру для информации о конвертере
                ZG_CVT_INFO rInfo = new ZG_CVT_INFO();
                rInfo.pszLinesBuf = new string('\0', 256);
                rInfo.nLinesBufMax = rInfo.pszLinesBuf.Length;
                // настраиваем структуру для параметров открытия конвертера
                ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                rOp.nPortType = CvtPortType;    // тип порта
                rOp.pszName = CvtPortName;      // имя порта
                rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;    // скорость конвертера
                // настраиваем параметры ожидания
                ZP_WAIT_SETTINGS rWS = new ZP_WAIT_SETTINGS(1000, 1, IntPtr.Zero, 0, 0, 0, 0);
                rOp.pWait = Marshal.AllocHGlobal(Marshal.SizeOf(rWS));
                try
                {
                    Marshal.StructureToPtr(rWS, rOp.pWait, true);
                    // открываем конвертер
                    hr = ZGIntf.ZG_Cvt_Open(ref m_hCvt, ref rOp, rInfo);
                }
                finally
                {
                    Marshal.FreeHGlobal(rOp.pWait);
                }
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }
                if (rInfo.nMode != ZG_GUARD_MODE.ZG_GUARD_ADVANCED)
                {
                    Console.WriteLine("Конвертер не в режиме Advanced.");
                    return;
                }
                ShowLicense();
                Console.WriteLine("-----");
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - показать параметры текущей лизензии");
                    Console.WriteLine("2 - показать все установленные лизензии");
                    Console.WriteLine("6 - установить новую лизензию...");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    if (s != "")
                    {
                        Console.WriteLine();
                        switch (Convert.ToInt32(s))
                        {
                            case 1:
                                ShowLicense();
                                break;
                            case 2:
                                ShowAllLicenses();
                                break;
                            case 6:
                                DoSetLicense();
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
                if (m_hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(m_hCvt);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}

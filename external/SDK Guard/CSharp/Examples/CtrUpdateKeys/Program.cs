using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;
using System.Globalization;
using ZGuard;
using ZPort;

namespace CtrUpdateKeys
{
    class Program
    {
        public static readonly string[] CtrTypeStrs = { "", "Gate 2000", "Matrix II Net", "Z5R Net", "Z5R Net 8000", "Guard Net", "Z-9 EHT Net", "EuroLock EHT net", "Z5R Web" };
        public static readonly string[] KeyTypeStrs = { "", "Обычный", "Блокирующий", "Мастер" };
        public static readonly string[] KeyTypeAbbrs = { "", "N", "B", "M" };

        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;   // Тип порта
        public const string CvtPortName = "COM12";                          // Имя порта
        public const Byte CtrAddr = 3;                                      // Адрес контроллера
        public const string KeysCacheFileName_D = "ctr{0}_keys.bin";         // Имя файла кэша ключей

        public static IntPtr m_hCtr;        // Дескриптор контроллера
        public static int m_nSn;            // С/н контроллера
        public static bool m_fProximity;    // True, Proximity, иначе - Touch Memory
        public static int m_nMaxBanks;      // Количество банков
        public static int m_nMaxKeys;       // Максимум ключей
        public static int m_nOptRead;       // Количество ключей, считываемых за 1 запрос
        public static int m_nOptWrite;      // Количество ключей, записываемых за 1 запрос

        [StructLayout(LayoutKind.Sequential, Pack = 1)]
        public struct MYKEY
        {
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16)]
            public Byte[] m_Num;                    // Номер ключа
            public ZG_CTR_KEY_TYPE m_nType;         // Тип ключа
            public UInt32 m_nAccess;                // Доступ (маска временных зон)
        }

        // Преобразовать строку в номер ключа
        static bool ParseKeyNum(ref Byte[] rKeyNum, string sText)
        {
            Int32 n, n2;
            n = sText.IndexOf(',');
            if (n != -1)
            {
                string[] aValues2 = sText.Split(new Char [] {',', '[', ']'}, System.StringSplitOptions.RemoveEmptyEntries);

                 n2 = Int32.Parse(aValues2[1]);
                //n2 = (n + 1);
                //while ((n2 < sText.Length) && sText[n2]])
            }
            string[] aValues = sText.Split(',');
            if (aValues.Length == 2)
            {
                Byte nGroup = Convert.ToByte(aValues[0]);
                UInt16 nNumber = Convert.ToUInt16(aValues[1]);
                rKeyNum[0] = 3;
                rKeyNum[1] = (Byte)nNumber;
                rKeyNum[2] = (Byte)(nNumber >> 8);
                rKeyNum[3] = nGroup;
                n = sText.IndexOf('[');
                if (n != -1)
                {
                    n2 = sText.IndexOf(']', n + 1);
                    int nValue;
                    if ((n2 != -1) && int.TryParse(sText.Substring(n + 1, n2 - n - 1), NumberStyles.HexNumber, CultureInfo.InvariantCulture, out nValue))
                    {
                        rKeyNum[4] = (Byte)nValue;
                        rKeyNum[5] = (Byte)(nValue >> 8);
                        rKeyNum[0] = 5;
                    }
                }
            }
            else
            {
                int j = 1;
                for (int i = sText.Length - 2; i >= 0; i -= 2)
                {
                    rKeyNum[j] = byte.Parse(string.Concat(sText[i], sText[i + 1]), NumberStyles.HexNumber);
                    if (++j > 6)
                        break;
                }
                rKeyNum[0] = (Byte)(j - 1);
            }
            return true;
        }

        static byte[] StructureToByteArray(object obj)
        {
            int len = Marshal.SizeOf(obj);
            byte[] arr = new byte[len];
            IntPtr ptr = Marshal.AllocHGlobal(len);
            Marshal.StructureToPtr(obj, ptr, true);
            Marshal.Copy(ptr, arr, 0, len);
            Marshal.FreeHGlobal(ptr);
            return arr;
        }
        static void ByteArrayToStructure(byte[] bytearray, ref object obj)
        {
            int len = Marshal.SizeOf(obj);
            IntPtr i = Marshal.AllocHGlobal(len);
            Marshal.Copy(bytearray, 0, i, len);
            obj = Marshal.PtrToStructure(i, obj.GetType());
            Marshal.FreeHGlobal(i);
        }

        // Показать все ключи
        static void ShowKeys()
        {
            int hr;
            int i, j, nCount;
            int nTop = 0;
            ZG_CTR_KEY[] aKeys = new ZG_CTR_KEY[m_nOptRead];
            ZG_CTR_KEY pKey;
            byte[] aBytes;
            FileStream f = new FileStream(String.Format(KeysCacheFileName_D, m_nSn), FileMode.Create, FileAccess.Write);

            for (i = 0; i < m_nMaxBanks; i++)
            {
                Console.WriteLine("------------");
                Console.WriteLine("Банк № {0}:", i);

                hr = ZGIntf.ZG_Ctr_GetKeyTopIndex(m_hCtr, ref nTop, i);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_GetKeyTopIndex (банк № {0}) ({1}).", i, hr);
                    Console.ReadLine();
                    return;
                }
                if (nTop == 0)
                    Console.WriteLine("список пуст.");
                for (j = 0; j < nTop; j++)
                {
                    if ((j % aKeys.Length) == 0)
                    {
                        nCount = (nTop - j);
                        if (nCount > aKeys.Length)
                            nCount = aKeys.Length;
                        hr = ZGIntf.ZG_Ctr_ReadKeys(m_hCtr, j, aKeys, nCount, null, IntPtr.Zero, i);
                        if (hr < 0)
                        {
                            Console.WriteLine("Ошибка ZG_Ctr_ReadKeys (банк № {0}) ({1}).", i, hr);
                            Console.ReadLine();
                            return;
                        }
                        for (int k = 0; k < nCount; k++)
                        {
                            aBytes = StructureToByteArray(aKeys[k]);
                            f.Write(aBytes, 0, aBytes.Length);
                        }
                    }
                    pKey = aKeys[j % aKeys.Length];
                    if (pKey.fErased)
                        Console.WriteLine("{0} стерт.", j);
                    else
                    {
                        Console.WriteLine("{0} {1}, {2}, доступ: {3:X2}h.",
                            j,
                            ZGIntf.CardNumToStr(pKey.rNum, m_fProximity),
                            KeyTypeStrs[(int)pKey.nType],
                            pKey.nAccess);
                    }
                }
                ZG_CTR_KEY rKey = new ZG_CTR_KEY();
                rKey.fErased = true;
                aBytes = StructureToByteArray(rKey);
                for (j = nTop; j < m_nMaxKeys; j++)
                    f.Write(aBytes, 0, aBytes.Length);
            }
            f.Close();
            Console.WriteLine("Успешно.");
        }

        // Очистить кэш ключей
        static void DoClearKeyCache()
        {
            System.IO.File.Delete(String.Format(KeysCacheFileName_D, m_nSn));
            Console.WriteLine("Успешно.");
        }

        static bool ZgProcessCb(int nPos, int nMax, IntPtr pUserData)
        {
            Console.Write("\r{0,5} / {1}", nPos, nMax);
            return true;
        }

        // Загрузить текущий список ключей из кэша или из контроллера
        static bool GetCtrList(ref ZG_CTR_KEY[] aList)
        {
            aList = new ZG_CTR_KEY[m_nMaxKeys * m_nMaxBanks];
            string sCacheName = String.Format(KeysCacheFileName_D, m_nSn);
            if (System.IO.File.Exists(sCacheName))
            {
                FileStream f = new FileStream(sCacheName, FileMode.Open, FileAccess.Read);
                byte[] aBytes = new byte[Marshal.SizeOf(aList[0])];
                object o = new ZG_CTR_KEY();
                for (int i = 0; i < aList.Length; i++)
                {
                    f.Read(aBytes, 0, aBytes.Length);
                    ByteArrayToStructure(aBytes, ref o);
                    aList[i] = (ZG_CTR_KEY)o;
                }
                f.Close();
            }
            else
            {
                Console.WriteLine("Чтение ключей из контроллера...");
                FileStream f = new FileStream(sCacheName, FileMode.Create, FileAccess.Write);
                int hr, nTop, nPos;
                byte[] aBytes;
                nTop = 0;
                for (int i = 0; i < m_nMaxBanks; i++)
                {
                    hr = ZGIntf.ZG_Ctr_GetKeyTopIndex(m_hCtr, ref nTop, i);
                    if (hr < 0)
                    {
                        Console.WriteLine("Ошибка ZG_Ctr_GetKeyTopIndex (банк № {0}) ({1}).", i, hr);
                        Console.ReadLine();
                        return false;
                    }
                    nPos = (i * m_nMaxKeys);
                    if (nTop > 0)
                    {
                        ZG_CTR_KEY[] aKeys = new ZG_CTR_KEY[nTop];
                        hr = ZGIntf.ZG_Ctr_ReadKeys(m_hCtr, 0, aKeys, nTop, ZgProcessCb, default(IntPtr), i);
                        if (hr < 0)
                        {
                            Console.WriteLine("Ошибка ZG_Ctr_ReadKeys (банк № {0}) ({1}).", i, hr);
                            Console.ReadLine();
                            return false;
                        }
                        aKeys.CopyTo(aList, nPos);
                        for (int j = 0; j < nTop; j++)
                        {
                            aBytes = StructureToByteArray(aKeys[j]);
                            f.Write(aBytes, 0, aBytes.Length);
                        }
                    }
                    if (nTop < m_nMaxKeys)
                    {
                        ZG_CTR_KEY rKey = new ZG_CTR_KEY();
                        rKey.fErased = true;
                        aBytes = StructureToByteArray(rKey);
                        for (int j = nTop; j < m_nMaxKeys; j++)
                        {
                            f.Write(aBytes, 0, aBytes.Length);
                            aList[nPos + j] = rKey;
                        }
                    }
                }
                f.Close();
                Console.WriteLine(" завершено.");
            }
            return true;
        }

        // Загрузить новый список ключей из текстового файла
        static bool GetNewList(ref System.Collections.Generic.List<MYKEY> oList)
        {
            string sFilename;
            Console.WriteLine("Введите имя файла со списком ключей:");
            sFilename = Console.ReadLine();
            if (sFilename == "")
            {
                Console.WriteLine("Отменено.");
                return false;
            }
            if (!System.IO.File.Exists(sFilename))
            {
                Console.WriteLine("Файл не найден.");
                return false;
            }
            sFilename = System.IO.Path.GetFullPath(sFilename);
            StreamReader sr = new StreamReader(sFilename);
            string line;
            string[] aValues;
            MYKEY rMK;
            int nValue;
            while ((line = sr.ReadLine()) != null)
            {
                aValues = line.Split(';');
                if (aValues.Length == 0)
                    continue;
                rMK = new MYKEY();
                rMK.m_Num = new Byte[16];
                if (!ParseKeyNum(ref rMK.m_Num, aValues[0]))
                    continue;
                rMK.m_nType = ZG_CTR_KEY_TYPE.ZG_KEY_NORMAL;
                rMK.m_nAccess = 0xff;
                if (aValues.Length >= 1)
                {
                    aValues[1] = aValues[1].Trim().ToUpper();
                    if (aValues[1] == "B")
                        rMK.m_nType = ZG_CTR_KEY_TYPE.ZG_KEY_BLOCKING;
                    else if (aValues[1] == "M")
                        rMK.m_nType = ZG_CTR_KEY_TYPE.ZG_KEY_MASTER;
                    if ((aValues.Length >= 2) && Int32.TryParse(aValues[2].Trim(), NumberStyles.HexNumber, CultureInfo.InvariantCulture, out nValue))
                        rMK.m_nAccess = (Byte)nValue;
                }
                oList.Add(rMK);
            }
            sr.Close();
            Console.WriteLine("Загружено {0} ключей. Продолжить [y/n]?", oList.Count);
            return (Console.ReadLine().ToUpper() == "Y");
        }

        static int CompareZKeyNums(Byte[] Left, Byte[] Right)
        {
            int n = Math.Min(Left[0], Right[0]);
            for (int i = n; i > 0; i--)
                if (Left[i] != Right[i])
                    return (Left[i] > Right[i]) ? 1 : -1;
            if (Left[0] != Right[0])
                return (Left[0] > Right[0]) ? 1 : -1;
            return 0;
        }

        // Поиск свободной ячейки в списке ключей Sdk
        static int FindEraised(ref ZG_CTR_KEY[] aList, int nStart, int nBank)
        {
            int n = (nBank * m_nMaxKeys);
            int nEnd = (n + m_nMaxKeys);
            for (int i = (n + nStart); i < nEnd; i++)
                if (aList[i].fErased)
                    return i;
            return -1;
        }

        // Установить список ключей в контроллер и затем сохранить в кэш
        static void SetCtrList(ref ZG_CTR_KEY[] aList, ref bool[] aSync)
        {
            bool fChanged = false;
            int nIdx, nPos, nWIdx, nWPos, nWCnt, nEnd, hr;
            for (int i = 0; i < m_nMaxBanks; i++ )
            {
                nIdx = 0;
                nPos = (i * m_nMaxKeys);
                while (nIdx < m_nMaxKeys)
                {
                    if (aSync[nPos])
                    {
                        nIdx++;
                        nPos++;
                        continue;
                    }
                    nWIdx = nIdx++;
                    nWPos = nPos++;
                    nWCnt = 1;
                    nEnd = (i + 1) * m_nMaxKeys;
                    while (nPos < nEnd)
                    {
                        if ((nPos - nWPos) >= m_nOptWrite)
                            break;
                        if (!aSync[nPos])
                            nWCnt = (nPos - nWPos + 1);
                        nIdx++;
                        nPos++;
                    }
                    if (nWCnt > 0)
                    {
                        ZG_CTR_KEY[] aKeys = new ZG_CTR_KEY[nWCnt];
                        Array.Copy(aList, nWPos, aKeys, 0, nWCnt);
                        hr = ZGIntf.ZG_Ctr_WriteKeys(m_hCtr, nWIdx, aKeys, aKeys.Length, null, default(IntPtr), i);
                        if (hr < 0)
                        {
                            Console.WriteLine("Ошибка ZG_Ctr_WriteKeys (банк № {0}) ({1}).", i, hr);
                            Console.ReadLine();
                            return;
                        }
                        Console.WriteLine("Обновлены ключи {0}-{1} (банк № {2}).", nWIdx, nWIdx + nWCnt - 1, i);
                        fChanged = true;
                    }
                }
            }
            if (fChanged)
            {
                // Обновляем кэш ключей в файле
                FileStream f = new FileStream(String.Format(KeysCacheFileName_D, m_nSn), FileMode.Create, FileAccess.Write);
                Byte[] aBytes;
                for (int i = 0; i < aList.Length; i++)
                {
                    aBytes = StructureToByteArray(aList[i]);
                    f.Write(aBytes, 0, aBytes.Length);
                }
                f.Close();
            }
            else
                Console.WriteLine("Список ключей контроллера не изменился.");
        }

        // Функция сравнения для сортировки списка ключей по их номерам
        public class MyKeysComparer : IComparer<MYKEY>
        {
            public int Compare(MYKEY x, MYKEY y)
            {
                return CompareZKeyNums(x.m_Num, y.m_Num);
            }
        }
        // Установить новый список ключей из файла
        static void DoLoadKeysFromFile()
        {
            System.Collections.Generic.List<MYKEY> oNewList = new System.Collections.Generic.List<MYKEY>();
            ZG_CTR_KEY[] aCtrList = null;
            if (!GetNewList(ref oNewList))
                return;
            if (!GetCtrList(ref aCtrList))
                return;
            MyKeysComparer mkc = new MyKeysComparer();
            oNewList.Sort(mkc);
            bool[] aSync = new bool[aCtrList.Length];
            ZG_CTR_KEY rCK;
            MYKEY rMK = new MYKEY();
            int nIdx;
            // Удаляем из CtrList ключи, которых нет в NewList
            for (int i = 0; i < aCtrList.Length; i++)
            {
                rCK = aCtrList[i];
                if (rCK.fErased)
                {
                    aSync[i] = true;
                    continue;
                }
                rMK.m_Num = rCK.rNum;
                nIdx = oNewList.BinarySearch(rMK, mkc);
                if (nIdx != -1)
                {
                    rMK = oNewList[nIdx];
                    if ((rCK.nType != rMK.m_nType) || (rCK.nAccess != rMK.m_nAccess))
                    {
                        aCtrList[i].nType = rMK.m_nType;
                        aCtrList[i].nAccess = rMK.m_nAccess;
                    }
                    else
                        aSync[i] = true;
                    oNewList.RemoveAt(nIdx);
                }
                else
                    aCtrList[i].fErased = true;
            }
            // Добавляем из NewList в CtrList ключи, которых нет в CtrList
            int[] aNext = new int[m_nMaxBanks];
            for (int i = 0; i < aNext.Length; i++)
                aNext[i] = 0;
            for (int i = 0; i < oNewList.Count; i++)
            {
                rMK = oNewList[i];
                for (int j = 0; j < m_nMaxBanks; j++)
                {
                    nIdx = FindEraised(ref aCtrList, aNext[j], j);
                    if (nIdx == -1)
                    {
                        Console.WriteLine("Список ключей переполнен (банк: {0}).", j);
                        Console.ReadLine();
                        return;
                    }
                    rCK = aCtrList[nIdx];
                    rCK.fErased = false;
                    rCK.rNum = rMK.m_Num;
                    rCK.nType = rMK.m_nType;
                    rCK.nAccess = rMK.m_nAccess;
                    aCtrList[nIdx] = rCK;
                    aSync[nIdx] = false;
                    aNext[j] = (nIdx + 1);
                }
            }
            SetCtrList(ref aCtrList, ref aSync);
            Console.WriteLine("Успешно.");
        }

        static void DoSaveKeysToFile()
        {
            string sFilename;
            Console.WriteLine("Введите имя файла со списком ключей:");
            sFilename = Console.ReadLine();
            if (sFilename == "")
            {
                Console.WriteLine("Отменено.");
                return;
            }
            ZG_CTR_KEY[] aCtrList = null;
            if (!GetCtrList(ref aCtrList))
                return;
            StreamWriter outfile = new StreamWriter(sFilename);
            for (int i = 0; i < m_nMaxKeys; i++)
            {
                ZG_CTR_KEY rCK = aCtrList[i];
                if (rCK.fErased)
                    continue;
                outfile.WriteLine(String.Format("{0}; {1}; {2:X2}", ZGIntf.CardNumToStr(rCK.rNum, m_fProximity), KeyTypeAbbrs[(int)rCK.nType], rCK.nAccess));
            }
            outfile.Close();
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
            IntPtr hCvt = new IntPtr(0);
            m_hCtr = new IntPtr(0);
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
                // Запоминаем некоторые параметры контроллера
                m_nSn = rCtrInfo.nSn;
                m_fProximity = ((rCtrInfo.nFlags & ZGIntf.ZG_CTR_F_PROXIMITY) != 0);
                m_nMaxBanks = ((rCtrInfo.nFlags & ZGIntf.ZG_CTR_F_2BANKS) != 0) ? 2 : 1;
                m_nMaxKeys = rCtrInfo.nMaxKeys;
                m_nOptRead = rCtrInfo.nOptReadItems;
                m_nOptWrite = rCtrInfo.nOptWriteItems;
                // Выводим на экран информацию о контроллере
                Console.WriteLine("{0} адрес: {1}, с/н: {2}, v{3}.{4}, Количество банков: {5}.",
                    CtrTypeStrs[(int)rCtrInfo.nType],
                    rCtrInfo.nAddr,
                    rCtrInfo.nSn,
                    rCtrInfo.nVersion & 0xff, (rCtrInfo.nVersion >> 8) & 0xff,
                    m_nMaxBanks);
                Console.WriteLine("-----");
                string s;
                while (true)
                {
                    Console.WriteLine("Введите номер команды:");
                    Console.WriteLine("1 - показать ключи");
                    Console.WriteLine("6 - Очистить кэш ключей (удалить файл)");
                    Console.WriteLine("7 - Установить новый список из файла в контроллер...");
                    Console.WriteLine("8 - Сохранить ключи в файл, загрузив из контроллера или из кэша");
                    Console.WriteLine("0 - выход");
                    s = Console.ReadLine();
                    if (s != "")
                    {
                        Console.WriteLine();
                        switch (Convert.ToInt32(s))
                        {
                            case 1:
                                ShowKeys();
                                break;
                            case 6:
                                DoClearKeyCache();
                                break;
                            case 7:
                                DoLoadKeysFromFile();
                                break;
                            case 8:
                                DoSaveKeysToFile();
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

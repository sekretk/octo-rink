using System;
using System.IO;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Xml;
using Microsoft.Win32;
using System.Windows.Forms;
using System.Resources;
using System.Collections;
using System.Data;

namespace MILibrary
{
    /// <summary>
    /// Класс представляет собой набор инструментов для работы с ресурсами продуктов Microinvest, получение настроек подключения, переменные локали и т.д.
    /// </summary>
    static public class miSettings
    {
        [DllImport("KERNEL32.DLL", EntryPoint = "GetPrivateProfileStringW", SetLastError = true, CharSet = CharSet.Unicode,
                                                ExactSpelling = true, CallingConvention = CallingConvention.StdCall)]

        private static extern int GetPrivateProfileString(string lpAppName, string lpKeyName,
            string lpDefault, StringBuilder lpReturnString, int nSize, string lpFilename);

        private static StringBuilder sb = new StringBuilder(256); 

        private static LogWriter log = new LogWriter("Класс настроек Microinvest.");


        /// <summary>
        /// Метод пытается найти в реестре путь к дирректории настроек программного комплекса Microinvest Warehouse Pro.
        /// </summary>
        /// <returns>Путь к каталогу либо пустую строку.</returns>
        public static string GetWHProSettingsPath()
        {
            try
            {
                RegistryKey readKey = Registry.LocalMachine.OpenSubKey("SOFTWARE\\Microinvest\\Warehouse Pro\\");
                return (string)readKey.GetValue("Install Path");
            }
            catch
            {
                return String.Empty;
            }
        }

        /// <summary>
        /// Метод пытается найти в реестре путь к каталогу запускных файлов программы Microinvest Warehouse Pro.
        /// </summary>
        /// <returns>Путь к дирректории либо пустую строку при ошибке</returns>
        public static string GetWHProBinPath()
        {
            try
            {
                RegistryKey readKey = Registry.LocalMachine.OpenSubKey("SOFTWARE\\Microinvest\\Warehouse Pro\\");
                return (string)readKey.GetValue("Install Bin Path");
            }
            catch
            {
                return String.Empty;
            }
        }
        
        /// <summary>
        /// Метод возвращает путь к файлу LiteApp.config
        /// </summary>
        /// <returns>Полный путь к файлу</returns>
        public static string GetWHLightSettingsPath()
        {
            return System.Environment.GetEnvironmentVariable("ALLUSERSPROFILE") + "\\Application Data\\Microinvest\\Warehouse Pro Light\\LiteApp.config";            
        }

        /// <summary>
        /// Проверяет установлен ли Microinvest Warehouse Pro.
        /// </summary>
        /// <returns>true если файл app.config доступ, в противном случае false</returns>
        public static bool isProEnable()
        {
            return File.Exists(GetWHProSettingsPath() + "App.config");            
        }

        /// <summary>
        /// Проверяет доступен ли файл настроек Warehouse Light
        /// </summary>
        /// <returns>true если файл существует, в противном случае false</returns>
        public static bool isLightEnable()
        {
            return File.Exists(GetWHLightSettingsPath());
        }

        /// <summary>
        /// Метод возвращает настройки используемые Warehouse Light
        /// </summary>
        /// <returns>Шаблон настроек</returns>
        public static SettingsTemplate LightSettings()
        {
            SettingsTemplate settings = new SettingsTemplate();
            try
            {
                //Получает тип БД, если не MS SQL Server или Access то выставляет настройки по-умолчанию.
                GetPrivateProfileString("DatabaseType", "value", "", sb, sb.Capacity, GetWHLightSettingsPath());
                if (sb.ToString() == "0") 
                    settings.type  = ConnectType.Access;
                else 
                    if (sb.ToString() == "3") 
                        settings.type  = ConnectType.SQLServer;
                    else
                        {
                            settings.SetDefault();
                            return settings;
                        }

                //Использование учетных записей MS SQL Server
                settings.sqlAuthentication = true;

                //Имя сервера БД MS Server
                GetPrivateProfileString("ServerName", "value", "", sb, sb.Capacity, GetWHLightSettingsPath());
                settings.serverName = sb.ToString();

                //Название БД в экземляре MS SQL Server
                GetPrivateProfileString("DatabaseName", "value", "", sb, sb.Capacity, GetWHLightSettingsPath());
                settings.dbName = sb.ToString();

                //Имя пользователя SQL Server
                GetPrivateProfileString("UserName", "value", "", sb, sb.Capacity, GetWHLightSettingsPath());
                settings.user = sb.ToString();

                //Пароль пользователя SQL Server
                GetPrivateProfileString("Password", "value", "", sb, sb.Capacity, GetWHLightSettingsPath());
                settings.pwd = sb.ToString();

                //Путь к БД MS Access
                GetPrivateProfileString("DatabasePath", "value", "", sb, sb.Capacity, GetWHLightSettingsPath());
                settings.dbFilePath = sb.ToString();

            }
            catch
            {
                settings.SetDefault();
            }
            return settings;
        }

        /// <summary>
        /// Возврашает набор настроек использеумых Warehouse Pro. В случае невозможности получения настроек буду возвращены настройки по умолчанию (заданы в классе SettingsTemplate). Настройки берутся ТОЛЬКО из умолчательной конфигурации программы, т.е. app.config.
        /// </summary>
        /// <returns>Шаблон настроек</returns>
        public static SettingsTemplate ProSettings()
        {
            SettingsTemplate settings = new SettingsTemplate();
            try
            {
                //Получает тип БД, если не MS SQL Server или Access то выставляет настройки по-умолчанию.  
                switch (Crypto.DecryptText(XMLReader(GetWHProSettingsPath() + "App.config", "databasetype")))
                {
                    case "0": settings.type = ConnectType.Access; break;
                    case "3": settings.type = ConnectType.SQLServer; break;
                    default: settings.SetDefault(); return settings;
                }

                //Использование учетных записей MS SQL Server
                settings.sqlAuthentication = true;

                //Имя сервера БД MS Server                
                settings.serverName = settings.type == ConnectType.SQLServer ? Crypto.DecryptText(XMLReader(GetWHProSettingsPath() + "App.config", "servername")) : "";

                //Название БД в экземляре MS SQL Server                
                settings.dbName = settings.type == ConnectType.SQLServer ? Crypto.DecryptText(XMLReader(GetWHProSettingsPath() + "App.config", "databasename")) : "";

                //Имя пользователя SQL Server                
                settings.user = settings.type == ConnectType.SQLServer ? Crypto.DecryptText(XMLReader(GetWHProSettingsPath() + "App.config", "username")) : "";

                //Пароль пользователя SQL Server                
                settings.pwd = settings.type == ConnectType.SQLServer ? Crypto.DecryptText(XMLReader(GetWHProSettingsPath() + "App.config", "userpassword")) : "";

                //Путь к БД MS Access                
                settings.dbFilePath = settings.type == ConnectType.Access ? Crypto.DecryptText(XMLReader(GetWHProSettingsPath() + "App.config", "databasepath")) : "";

            }
            catch
            {
                settings.SetDefault();
            }
            return settings;
        }

        private static string XMLReader(string fileName, string keyName)
        {
            try
            {
                XmlDocument xmlDocument = new XmlDocument();
                xmlDocument.Load(fileName);
                XmlNode xmlNode = xmlDocument.GetElementsByTagName("configuration")[0];
                xmlNode = xmlNode.FirstChild.FirstChild;
                while (xmlNode.Attributes["key"].Value != keyName)
                    xmlNode = xmlNode.NextSibling;
                return xmlNode.Attributes["value"].Value;
            }
            catch
            {
                return "";
            }            
        }

        /// <summary>
        /// Пытается найти файл ресурсов русской локали Warehouse Pro, файл Dictionary.ru.xml
        /// </summary>
        /// <returns>Путь к файлу локали, либо пустую строку при ошибке</returns>
        public static string ruCultureLocationFile()
        {
            return GetWHProBinPath()!=String.Empty?GetWHProBinPath() + "Resources\\Dictionary.ru.xml":String.Empty;
        }


        /// <summary>
        /// Метод создает DateSet содержащий в таблице location полный список локалей Warehouse Pro в колонках key и value
        /// </summary>
        /// <param name="fileName">Путь к каталогу ресурсов приложения</param>
        /// <returns>DateSet с единственной заполненной таблицей location содержащей столбцы key и value</returns>
        /// <example>DataSet ds = GetFullCulture(@"c:\Program Files\Microinvest\Warehouse Pro\Resources\Dictionary.ru.xml");</example>
        public static DataSet GetFullCulture(string fileName)
        {
            DataSet ds = new DataSet(); 
            ds.Tables.Add("location");
            ds.Tables["location"].Columns.Add(new DataColumn("key"));
            ds.Tables["location"].Columns.Add(new DataColumn("value"));

            try
            {
                ResXResourceReader rsxr = new ResXResourceReader(fileName);
                foreach (DictionaryEntry d in rsxr)
                {
                    ds.Tables["location"].Rows.Add(d.Key.ToString(), d.Value.ToString());
                }
                //Close the reader.
                rsxr.Close();
                return ds;
            }
            catch
            {
                return null;
            }            
        }

        /// <summary>
        /// Метод находит в полученном файле ресурсов заданный параметр и возвращает его значение.
        /// </summary>
        /// <param name="fileName">Файл ресурсов Warehouse Pro</param>
        /// <param name="key">Название икомого параметра</param>
        /// <returns>Значение заданного параметра либо пустую строку в случае ошибки или отсутсвия такого параметра</returns>
        public static string GetCultureElement(string fileName, string key)
        {
            try
            {
                DataSet ds = GetFullCulture(fileName);
                if (ds == null) return "";
                DataRow[] rows = ds.Tables["location"].Select("[key] like '" + key + "'");
                if (rows.Length > 0) return rows[0]["value"].ToString();
            }
            catch
            {
                return "";
            }
            return "";
        }

        /// <summary>
        /// Мотод сохраняет набор переменных локализации в файловую систему.
        /// </summary>
        /// <param name="ds">Тип DataSet с таблице location и двумя столбцами: key и value</param>
        /// <param name="filename">Путь к файлу для сохранения</param>
        /// <returns>true если сохранение пройдет без ошибок, в противном случае false</returns>
        public static bool SaveCulture(DataSet ds, string filename)
        {
            try
            {
                ResXResourceWriter rsxw = new ResXResourceWriter(filename);
                foreach (DataRow row in ds.Tables["location"].Rows)
                {
                    rsxw.AddResource(row[0].ToString(), row[1, DataRowVersion.Current].ToString());
                }
                //Close the writer.                
                rsxw.Close();
                return true;
            }
            catch
            {
                return false;
            }
        }
    }
}

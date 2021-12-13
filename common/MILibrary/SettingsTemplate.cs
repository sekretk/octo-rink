using System;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{
    /// <summary>
    /// Класс - шаблон настроек подключения, предназначен для сохранения в файловую систему и последующего восстановления.
    /// </summary>
    public class SettingsTemplate
    {
        /// <summary>
        /// Тип подключения к БД
        /// </summary>
        public ConnectType type;

        /// <summary>
        /// Имя сервера БД
        /// </summary>
        public string serverName;
        /// <summary>
        /// Название БД SQL Server
        /// </summary>
        public string dbName;
        /// <summary>
        /// Имя пользователя SQL Server
        /// </summary>
        public string user;
        /// <summary>
        /// Пароль пользователя SQL Server
        /// </summary>
        public string pwd;
        /// <summary>
        /// Путь к файлу БД MS Access
        /// </summary>
        public string dbFilePath;
        /// <summary>
        /// Пометка использования аутентификации MS SQL Server
        /// </summary>
        public bool sqlAuthentication;
        /// <summary>
        /// Заполнить настройки значениями по-умолчанию
        /// </summary>
        public void SetDefault()
        {
            type = ConnectType.Access;
            serverName = "localhost"; 
            dbName = "microinvest";
            user = "sa";
            pwd = "";
            sqlAuthentication = false;
            dbFilePath = "";
        }
    }
}

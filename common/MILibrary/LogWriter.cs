using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Windows.Forms;
using System.Diagnostics;
using System.Data;
using System.Data.SqlClient;
using System.Data.OleDb;

namespace MILibrary
{
    /// <summary>
    /// Предопределено 5 типов событий: information, caution, auditerror, fatalerror and other
    /// </summary>
    public enum EventType : byte
    {
        /// <summary>
        /// Информационное сообщение, регистрирует изменение настроек, получение данных, приложение работает в штатном режиме.
        /// </summary>
        Information = 0,
        /// <summary>
        /// Предупреждения, когда разработчиком учтена работа в нештатных усовиях, но это понижает устойчивость и безопасность приложения, обычно в коде учтены варианты работы при этой ошибке.
        /// </summary>
        Caution = 1,
        /// <summary>
        /// Ошибки прохождения аутентификации и прочих проверок прав.
        /// </summary>
        AuditError = 2,
        /// <summary>
        /// Ошибка после которой приложение не может работать корректно, часто дублирует сообщение пользователю.
        /// </summary>
        FatalError = 3,
        /// <summary>
        /// Другие типы событий
        /// </summary>
        Other = 4
    };

    /// <summary>
    /// Уровень регистрирования событий в журнале, предопределены 3 уровня: Critical (запись только событий FatalError), Smooth (запись FatalError и Caution),
    /// и Laborious (запись всех событий) 
    /// </summary>
    public enum LogLevel : byte
    {
        /// <summary>
        /// запись только событий FatalError
        /// </summary>
        Critical = 2,

        /// <summary>
        /// запись FatalError и Caution
        /// </summary>
        Smooth = 1,

        /// <summary>
        /// запись всех событий
        /// </summary>
        Laborious = 0
    }

    /// <summary>
    /// Тип протокола, какие ресурсы используются для хранения протокола: файловая система, системный журнал или база данных.
    /// </summary>
    public enum LogType : byte
    {
        File = 0,
        System = 1,
        DB = 2
    }

    /// <summary>
    /// Класс предназначен для протоколирования работы, имеет возможность записывать события в системный лог, в файловую систему и на сервер БД.
    /// </summary>
    public class LogWriter
    {
        private string logFileName = Application.StartupPath + "\\appLog.log";
        EventLog systemEventLog = new EventLog();
        LogType currentType;
        LogLevel currentLevel;
        private string application;
        IDbConnection cnc;

        public LogWriter():this("Неизвестно")
        {     
        }

        public LogWriter(string appType):this(appType, LogLevel.Laborious)
        {            
        }

        public LogWriter(string appType, LogType logType)
            : this(appType, logType, null)
        {
        }

        public LogWriter(string appType, LogType logType, IDbConnection connection):this(appType, logType, LogLevel.Laborious, connection)
        {
        }

        public LogWriter(string appType, LogLevel logLevel):this(appType, LogType.File, logLevel, null)
        {
        }

        public LogWriter(string appType, LogType logType, LogLevel logLevel, IDbConnection connection)
        {
            currentType = logType;
            application = appType;
            currentLevel = logLevel;
            cnc = connection;

            switch (currentType)
            {
                case LogType.System:
                    try
                    {
                        if (!EventLog.SourceExists(application))
                            EventLog.CreateEventSource(application, "Приложения ООО Ивтекс");
                        systemEventLog.Source = application;
                        Write("Работа с системным журналом открыта.", EventType.Information);
                        currentLevel = LogLevel.Critical;
                    }
                    catch (Exception ex)
                    {
                        currentType = LogType.File;
                        Write("Не удалось произвести запись в системный журнала. Текст ошибки: " + ex.Message, EventType.Caution);
                        goto default;
                    }
                    break;
                case LogType.DB:
                    try
                    {
                        if (!isTableExist(connection)) CreateLogTable(connection); cnc = connection;
                        Write("Сессия работы с протоколом событий на сервере базы данных успешно открыта", EventType.Information);
                    }
                    catch (Exception ex)
                    {
                        currentType = LogType.File;
                        Write("Не удалось произвести запись журнала в базу данных. Текст ошибки: " + ex.Message, EventType.Caution);
                        goto default;
                    }
                    break;
                default:
                        currentType = LogType.File;
                        Write("Сессия работа с протоколом событий открыта.", EventType.Information);
                    break;
            }
        }

        /// <summary>
        /// Записывает переданное сообщение в файл протокола работы.
        /// </summary>
        /// <param name="message"> Text of log event </param>        
        public void Write (string message)
        {
            Write(message, EventType.Information);
        }

        /// <summary>
        /// Logging your message with logType to log source
        /// </summary>
        /// <param name="eventMessage">text of message</param>
        /// <param name="type">event type</param>
        /// <returns>true if it can write event, false if it has problem</returns>
        /// <remarks>author: Boyko Constantine, data: 11/11/2010</remarks>        
        public void Write(string eventMessage, EventType type)
        {
            switch (currentLevel)
            {
                case LogLevel.Laborious:
                    if ( type == EventType.Other || type == EventType.Information || type == EventType.AuditError)
                        {
                        if (currentType == LogType.File)
                            WriteToFile(eventMessage, type);
                        if (currentType == LogType.System)
                            WriteToSysteLog(eventMessage, type);
                        if (currentType == LogType.DB)
                            WriteToDB(eventMessage, type);
                        }
                    goto case LogLevel.Smooth;

                case LogLevel.Smooth: 
                     if (type == EventType.Caution) 
                    {
                        if (currentType == LogType.File)
                            WriteToFile(eventMessage, type);
                        if (currentType == LogType.System)
                            WriteToSysteLog(eventMessage, type);
                        if (currentType == LogType.DB)
                            WriteToDB(eventMessage, type);
                    }
                     goto default;
                    
                default:
                    
                    if (type == EventType.FatalError) 
                    {
                        if (currentType == LogType.File)
                            WriteToFile(eventMessage, type);
                        if (currentType == LogType.System)
                            WriteToSysteLog(eventMessage, type);
                        if (currentType == LogType.DB)
                            WriteToDB(eventMessage, type);
                    }

                    break;
            }        
        }

        private void WriteToDB(string message, EventType eventType)
        {
            try
            {
                if (cnc is SqlConnection)
                {
                    SqlCommand cmd = ((SqlConnection)cnc).CreateCommand();
                    cmd.CommandText = "INSERT INTO IvtexApplicationsLog VALUES (GETDATE(), '" + eventType.ToString() + "', '" + application + "', '" + message + "')";
                    cmd.ExecuteNonQuery();
                }
                else
                {
                    Exception ex = new Exception("Тип базы данных отличен от MS SQL Server");
                    throw ex;
                }

            }
            catch (Exception ex)
            {
                currentType = LogType.File;
                Write("Не удалось записть в БД. Протоколирование перенесено в файл. Ошибка: " + ex.Message, EventType.Caution);
            }
        }

        private void WriteToFile(string message, EventType eventType)
        {
            using (StreamWriter sw = new StreamWriter(logFileName, true, Encoding.GetEncoding(1251)))
                    sw.WriteLine(String.Format("{0,19}\t{1,30}\t{2}\t{3}", DateTime.Now.ToString(), application, message, eventType.ToString()));
        }

        private void WriteToSysteLog(string message, EventType eventType)
        {
            try
            {
                switch (eventType)
                {
                    case EventType.AuditError:
                        EventLog.WriteEntry(application, message, EventLogEntryType.FailureAudit);
                        break;
                    case EventType.Caution:
                        EventLog.WriteEntry(application, message, EventLogEntryType.Warning);
                        break;
                    case EventType.FatalError:
                        EventLog.WriteEntry(application, message, EventLogEntryType.Error);
                        break;
                    case EventType.Information:
                        EventLog.WriteEntry(application, message, EventLogEntryType.Information);
                        break;
                    case EventType.Other:
                        EventLog.WriteEntry(application, message, EventLogEntryType.Information);
                        break;               
                }
            }
            catch (Exception ex)
            {
                currentType = LogType.File;
                Write("Не удалось записть в системный журнал. Протоколирование перенесено в файл. Ошибка: " + ex.Message, EventType.Caution);
            }
        }

        private bool isTableExist(IDbConnection connection)
        {
            try
            {
                if (connection is SqlConnection)
                {
                    SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                    cmd.CommandText = "select case when OBJECT_ID('IvtexApplicationsLog', 'U') IS NOT NULL then 1 ELSE 0 END";
                    return cmd.ExecuteScalar().ToString() == "1";
                }
                else
                    return false;
            }
            catch
            {
                return false;
            }
        }

        private bool CreateLogTable(IDbConnection connection)
        {
            try
            {
                IDbCommand cmd = connection.CreateCommand();
                cmd.CommandText = @"CREATE TABLE IvtexApplicationsLog([date] [datetime] NULL, 
                                [type] [varchar](50) NULL, [application] [varchar](50) NULL, [message] [text] NULL)";
                cmd.ExecuteNonQuery();                
                return true;
            }
            catch
            {
                return false;
            }


        }

    }
}

using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.Serialization.Formatters.Binary;
using System.Data;
using System.Data.OleDb;
using System.Data.SqlClient;
using System.Windows.Forms;

namespace MILibrary
{
    /// <summary>
    /// Перечисление типов подключения к источнику данных
    /// </summary>
    public enum ConnectType : byte
    {
        /// <summary>
        /// Подключение к БД MS Access
        /// </summary>
        Access = 0, 
        /// <summary>
        /// Подключение к БД MS SqlServer
        /// </summary>
        SQLServer = 1,
        /// <summary>
        /// Подключение к БД MySQL
        /// </summary>
        MySQL = 2
    }

    /// <summary>
    /// Класс подключения к БД
    /// </summary>
    public class Connection : IDisposable
    {
        private readonly string settingsFileName;                

        #region Свойства работающие с параметрами подключения

        /// <summary>
        /// Возвращает тип подключения
        /// </summary>
        public ConnectType ConnectionType
        {
            get { return connectionType; }
        }

        private string accessFilePath;
        private string sqlServerName;
        private string sqlDBName;
        private bool sqlAuth;
        private string sqlUser;
        private string sqlPwd;
        private ConnectType connectionType;
        private LogWriter log = new LogWriter("Подключение к БД");
        private SqlConnection sqlCnc = new SqlConnection();
        private OleDbConnection oledbCnc = new OleDbConnection();

        /// <summary>
        /// Возвращает или устанавливает подключение к SQL Server
        /// </summary>
        public SqlConnection sConnection
        {
            get
            {
                return this.sqlCnc;
            }
            set
            {
                if (value != null)
                {
                    this.sqlCnc.Close();
                    this.sqlCnc = value;
                    connectionType = ConnectType.SQLServer;
                    sqlCnc.Open();
                }
            }
        }

        /// <summary>
        /// Восвращает или задает подключение к oleDB
        /// </summary>
        public OleDbConnection aConnection
        {
            get 
            { 
                return this.oledbCnc; 
            }
            set
            {
                if (value != null)
                {
                    this.oledbCnc.Close();
                    this.oledbCnc = value;
                }
            }
        }

        /// <summary>
        /// Возвращает true если есть активное подключение к источнику данных.
        /// </summary>
        public bool IsConnected
        {
            get
            {
                return (sqlCnc.State == ConnectionState.Open)||(oledbCnc.State == ConnectionState.Open);
            }
        }

        /// <summary>
        /// Возвращает путь к БД MS Access
        /// </summary>
        public string aPath
        {
            get { return accessFilePath; }
        }

        #endregion

        /// <summary>
        /// Конструктор по умолчанию, пытается использовать файл ConnectSettings.xml для получени настроек
        /// </summary>
        public Connection():this(Application.StartupPath + "\\ConnectionSettings.xml")
        {          
        }

        /// <summary>
        /// Конструктор, которому явно указан файл настроек
        /// </summary>
        /// <param name="setupFile">Путь к файлу настроек</param>
        public Connection(string setupFile)
        {
            settingsFileName = setupFile == "" ? Application.StartupPath + "\\ConnectionSettings.xml" : setupFile;
            sqlCnc.StateChange += new System.Data.StateChangeEventHandler(sqlCnc_StateChange);
            oledbCnc.StateChange += new System.Data.StateChangeEventHandler(oledbCnc_StateChange);
            LoadSettings(settingsFileName);
            Connect();    
        }

        /// <summary>
        /// Метод подписан на событие изменение статуса oledb подлючения, пишет в лог о событии и закрывает второе соединение если оно было открыто.
        /// </summary>
        /// <param name="sender">Объект вызвавший событие</param>
        /// <param name="e">Аргумент события</param>
        void oledbCnc_StateChange(object sender, System.Data.StateChangeEventArgs e)
        {
            //Случай когда соединение было разорвано во время работы
            if (e.CurrentState == System.Data.ConnectionState.Closed && e.OriginalState == System.Data.ConnectionState.Open && connectionType == ConnectType.Access)
            {   
                Connect();
                log.Write("Соединение с БД MS Access было разорвано. Будет предпринята попытка восстановить соединение.");                
            }

            //Случай когда соединение открывается
            if (e.CurrentState == System.Data.ConnectionState.Open && e.OriginalState == System.Data.ConnectionState.Closed)
            {
                log.Write("Открыто подключение к БД MS Access по пути: " + accessFilePath);
                if (sqlCnc.State == System.Data.ConnectionState.Open)
                {
                    log.Write("Соединение с БД MS SQL Server будет закрыто, т.к. было открыто соединение с MS Access. Возможно поддержка только одного активного соединения.");
                    CloseConnection(sqlCnc);
                }
            }
        }

        /// <summary>
        /// Метод подписан на событие изменение статуса sql подлючения, пишет в лог о событии и закрывает второе соединение если оно было открыто.
        /// </summary>
        /// <param name="sender">Объект вызвавший событие</param>
        /// <param name="e">Аргумент события</param>
        void sqlCnc_StateChange(object sender, System.Data.StateChangeEventArgs e)
        {
            //Случай когда соединение было разорвано во время работы
            if (e.CurrentState == System.Data.ConnectionState.Closed && e.OriginalState == System.Data.ConnectionState.Open && connectionType == ConnectType.SQLServer)
            {
                Connect();
                log.Write("Соединение с БД MS SQL Server было разорвано. Будет предпринята попытка восстановить соединение.");                
            }

            //Случай когда соединение открывается
            if (e.CurrentState == System.Data.ConnectionState.Open && e.OriginalState == System.Data.ConnectionState.Closed)
            {
                log.Write("Открыто подключение к БД MS Access по пути: " + accessFilePath);
                if (oledbCnc.State == System.Data.ConnectionState.Open)
                {
                    log.Write("Соединение с БД MS Access будет закрыто, т.к. было открыто соединение с MS SQL Server. Возможно поддержка только одного активного соединения.");
                    CloseConnection(oledbCnc);
                }
            }
        }

        /// <summary>
        /// Закрывает соединение реализующее интерфейс IDbConnection
        /// </summary>
        /// <param name="connection"></param>
        private void CloseConnection(IDbConnection connection)
        {            
            if (connection is SqlConnection)
                log.Write("Соединение с БД MS SQL Server закрыто. Строка соединения: " + connection.ConnectionString);
            if (connection is OleDbConnection)
                log.Write("Соединение с БД MS Access закрыто. Строка соединения: " + connection.ConnectionString);            
            connection.Close();
        }

        /// <summary>
        /// Метод раскрывает окно с настройками подключения.
        /// </summary>
        public void ChangeConnection()
        {
            LoginForm lForm = new LoginForm();            
            bool isError = true;
            while (isError)
            {

                #region Заполнение полей формы подключения текущими данными.
                    
                    lForm.cncType = connectionType;                    
                    lForm.accessFilePath = this.accessFilePath;                    
                    lForm.sqlServerName = sqlServerName;
                    lForm.sqlDBName = sqlDBName;
                    lForm.sqlAuth = sqlAuth;
                    lForm.sqlUser = sqlUser;
                    lForm.sqlPwd = sqlPwd;                    

                #endregion

                lForm.ShowDialog();
                if (lForm.DialogResult == DialogResult.Cancel)
                {
                    log.Write("Подключение отменено пользователем. Нажата кнопка Esc или закрыта форма.");
                    return;
                }

                #region Взять настройки с формы регистрации подключения

                    connectionType = lForm.cncType;
                    if (connectionType == ConnectType.Access)
                        this.accessFilePath = lForm.accessFilePath;
                    if (connectionType == ConnectType.SQLServer)
                    {
                        sqlServerName = lForm.sqlServerName;
                        sqlDBName = lForm.sqlDBName;
                        sqlAuth = lForm.sqlAuth;
                        if (sqlAuth)
                        {
                            sqlUser = lForm.sqlUser;
                            sqlPwd = lForm.sqlPwd;
                        }
                    }

                #endregion

                isError = Connect()!="OK";

                if (isError) 
                {
                    if (MessageBox.Show("Не удалось подключиться с таким настройками. Хотите продолжить работу с предыдущими настройками?", "Ошибка подключения", MessageBoxButtons.OKCancel) == DialogResult.OK) 
                        isError = false;
                }
                else
                    SaveSettings(settingsFileName);

            } // end While (isError)

            
        }

        /// <summary>
        /// Возвращает открытое соединение типа реализующее интрефейс IDbConnection
        /// </summary>
        /// <returns>Активное соединение</returns>
        public IDbConnection ActiveConnection
        {
            get
            {
                if (oledbCnc.State == ConnectionState.Open)
                    return oledbCnc;
                if (sqlCnc.State == ConnectionState.Open)
                    return sqlCnc;
                return null;
            }
        }

        /// <summary>
        /// Метод создает подключение и окрывает его в соответствии с текущими значениями параметров
        /// </summary>
        /// <returns>Строку "OK" есть все в порядке либо сообщение об ошибке.</returns>
        public string Connect()
        {
            if (connectionType == ConnectType.Access) // подключение к базе данных MS Access
            {
                #region Подключение к Access БД
                try
                {
                    oledbCnc = new OleDbConnection();
                    oledbCnc.ConnectionString = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + this.accessFilePath + ";Jet OLEDB:Database Password=Microinvest6380";
                    oledbCnc.Open();
                    sqlCnc.Close();                     
                }
                catch (Exception ex)
                {                    
                    log.Write(ex.Message);
                    log.Write("Не удалось создать подключение к Access базе данных. Отображено меню настроек подключения");
                    return ex.Message;
                }
                #endregion
            }
            else //Попытка подключение к SQL Server
            {
                #region Подключение к MS SQL Server

                try
                {
                    sqlCnc = new SqlConnection();

                    if (!sqlAuth)
                        sqlCnc = new SqlConnection("Data Source=" + sqlServerName + ";Initial Catalog=" + sqlDBName + ";Integrated Security=SSPI;");
                    else
                        sqlCnc = new SqlConnection("Server=" + sqlServerName + ";Database=" + sqlDBName + ";User ID=" + sqlUser + ";Password=" + sqlPwd + ";Trusted_Connection=False;");
                    sqlCnc.Open();
                    oledbCnc.Close();                                            
                    if (sqlAuth)
                        log.Write("Открыто подключение к БД MS SQL Server " + sqlServerName + " к базе данных " + sqlDBName + " используя реквизиты SQL пользователя " + sqlUser);
                    else
                        log.Write("Открыто подключение к БД MS SQL Server " + sqlServerName + " к базе данных " + sqlDBName + " используя Windows аутентификацию");
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message);
                    log.Write(ex.Message);
                    log.Write("Не удалось создать подключение к SQL Server " + sqlServerName);
                    return ex.Message;
                }
                #endregion
            }
            return IsConnected?"OK":"ERROR";
        }

        /// <summary>
        /// Закрывает все текущие соединения.
        /// </summary>
        public void Disconnect()
        {
            if (oledbCnc.State == ConnectionState.Open) oledbCnc.Close();
            if (sqlCnc.State == ConnectionState.Open) oledbCnc.Close();
        }

        /// <summary>
        /// Метод сохраняет полученный объект типа SettingsTemplate по пути fileName
        /// </summary>
        /// <param name="overview">Сохраняемый объект</param>
        /// <param name="fileName">Путь к файлу настроек</param>
        private void WriteXML(SettingsTemplate overview, string fileName)
        {
            System.Xml.Serialization.XmlSerializer writer =
                new System.Xml.Serialization.XmlSerializer(typeof(SettingsTemplate));

            System.IO.StreamWriter file = new System.IO.StreamWriter(fileName);
            writer.Serialize(file, overview);
            file.Close();
        }

        /// <summary>
        /// Получение информации о соединении
        /// </summary>
        /// <returns>Строку содержащую тип подключения и реквизиты.</returns>
        public string ConnectionInfo
        {
            get
            {
                try
                {
                if (ConnectionType == ConnectType.Access)
                    return String.Format("Статус: {0}\r\nТип: {1}\r\nПуть к БД: {2}", IsConnected ? "ОК" : "ОШИБКА", ConnectionType.ToString(), accessFilePath);
                if (ConnectionType == ConnectType.SQLServer)
                    return "Статус: " + (IsConnected ? "ОК\r\n" : "ОШИБКА\r\n") + "Тип: " + ConnectionType.ToString() + "\r\n" + "Сервер: " + sqlServerName + "\r\n" + "БД: " + sqlDBName + "\r\n" + "Пользователь: " + sqlUser;
                return "Соединение не создано, либо его тип отличается от MS SQL Server или Access";
                }
                catch 
                {
                    return "ОШИБКА";
                }
            }
        }

        /// <summary>
        /// Метод восстанавливает объект типа SettingsTemplate из файла
        /// </summary>
        /// <returns>Объект типа SettingsTemplate содержащий настройки подключения</returns>
        /// <param name="fileName">Путь к файлу нстроек</param>
        /// <remarks>Если не удается загрузить настройки создает класс и запускает в нем метод SetDefault</remarks>
        private SettingsTemplate ReadXML(string fileName)
        {
            System.Xml.Serialization.XmlSerializer reader = new System.Xml.Serialization.XmlSerializer(typeof(SettingsTemplate));
            SettingsTemplate overview = new SettingsTemplate();
            if (File.Exists(fileName))
            {
                try
                {
                    System.IO.StreamReader file = new System.IO.StreamReader(fileName);
                    overview = (SettingsTemplate)reader.Deserialize(file);
                    file.Close();
                }
                catch
                {
                    overview.SetDefault();
                }
            }
            else
                overview.SetDefault();
            return overview;
        }

        /// <summary>
        /// Загружает настройки из файла
        /// </summary>
        /// <param name="fileName">Путь к файлу</param>
        private void LoadSettings(string fileName)
        {
            SettingsTemplate settings = ReadXML(fileName);
            this.connectionType = settings.type;
            this.sqlServerName = settings.serverName;
            this.sqlDBName = settings.dbName;
            this.accessFilePath = settings.dbFilePath;
            this.sqlUser = settings.user;
            this.sqlPwd = Crypto.DecryptText(settings.pwd);
            this.sqlAuth = settings.sqlAuthentication;
        }

        /// <summary>
        /// Сохраняет текущие настройки в файл
        /// </summary>
        /// <param name="fileName">Путь к файлу</param>
        private void SaveSettings(string fileName)
        {
            SettingsTemplate settings = new SettingsTemplate();
            settings.type = this.connectionType;
            settings.serverName = this.sqlServerName;
            settings.dbName = this.sqlDBName;
            settings.dbFilePath = this.accessFilePath;
            settings.user = this.sqlUser;
            settings.pwd = Crypto.EncryptText(this.sqlPwd);
            settings.sqlAuthentication = this.sqlAuth;
            WriteXML(settings, fileName);
        }

        /// <summary>
        /// Закрывает все текущие соединения и сохраняет настройки.
        /// </summary>
        public void Dispose()
        {
            if (sqlCnc.State == System.Data.ConnectionState.Open)
            {
                sqlCnc.Close();
                log.Write("Соединение с SQL Server " + this.sqlServerName + " закрыто");
            }
            if (oledbCnc.State == System.Data.ConnectionState.Open)
            {
                this.oledbCnc.Close();
                log.Write("Соединение c файлом MS Access " + this.accessFilePath + " закрыто");
            }
            SaveSettings(settingsFileName);
        }
    }
}

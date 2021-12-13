using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Timers;

namespace MILibrary
{
    public partial class LoginForm : Form
    {
        private LogWriter log = new LogWriter("Экранная форма настроек подключения");

        public LoginForm()
        {            
            InitializeComponent();            
            this.KeyPreview = true; //Form handles keyinput

        }

        private void useSQLAuthCheckBox_CheckedChanged(object sender, EventArgs e)
        {            
            this.Text = "Подключение к " + this.cncType.ToString();

            if (this.useSQLAuthCheckBox.Checked)
                this.Size = new System.Drawing.Size(386, 260);                
            else
                this.Size = new System.Drawing.Size(386, 200);
        }

        private void connectBtn_Click(object sender, EventArgs e)
        {            
            this.Text = "Подключение к " + this.cncType.ToString();           
                if (this.cncType == ConnectType.Access)
                {
                    if (accessFilePath == "")
                    {
                        MessageBox.Show("Введите или выберите файл базы данных");
                        accessPathTextBox.Focus();
                    }
                    else
                    {
                        if (!File.Exists(accessFilePath))
                        {
                            MessageBox.Show("Файла:" + accessFilePath + " не существует.");
                            log.Write("Попытка подключения у несуществующему файлу mdb по пути: " + accessFilePath);
                            accessPathTextBox.Text = "";
                            accessPathTextBox.Focus();
                        }
                        else                        
                            this.DialogResult = DialogResult.OK;                        
                    }

                }
                else //SQL server connect occur
                {
                    bool a = serverNameTextBox.Text == "", b = dbNameTextBox.Text == "", c = useSQLAuthCheckBox.Checked, E = userTextBox.Text == "", d = pwdTextBox.Text == "";
                    if (a || b)
                        MessageBox.Show("Поля `Имя SQL Server` и `Имя базы данных` не могут быть пустым");
                    else
                    if (c & ((d & E) | (!d & E) | (d & !E)))
                        MessageBox.Show("Поля `Имя пользователя` и `Пароль пользователя` не могут быть пустым");                    
                    else
                        this.DialogResult = DialogResult.OK;                    
                }
        }        

        private void checkBox1_CheckStateChanged(object sender, EventArgs e)
        {
            
            this.Text = "Подключение к " + this.cncType.ToString();
           
                accessPathTextBox.Enabled = true;
                serverNameTextBox.Enabled = true;
                dbNameTextBox.Enabled = true;
                userTextBox.Enabled = true;
                pwdTextBox.Enabled = true;
                openFileDialogBtn.Enabled = true;
                useSQLAuthCheckBox.Enabled = true;
                serverTypeComboBox.Enabled = true;                
            
        }

        private void serverTypeComboBox_SelectedValueChanged(object sender, EventArgs e)
        {           
            this.Text = "Подключение к " + this.cncType.ToString();
            if (serverTypeComboBox.SelectedIndex == 0)
            {
                accessPathTextBox.Enabled = false;
                serverNameTextBox.Enabled = true;
                dbNameTextBox.Enabled = true;
                userTextBox.Enabled = true;
                pwdTextBox.Enabled = true;
                openFileDialogBtn.Enabled = false;
                useSQLAuthCheckBox.Enabled = true;
            }
            else
            {
                accessPathTextBox.Enabled = true;
                serverNameTextBox.Enabled = false;
                dbNameTextBox.Enabled = false;
                userTextBox.Enabled = false;
                pwdTextBox.Enabled = false;
                openFileDialogBtn.Enabled = true;
                useSQLAuthCheckBox.Enabled = false;
            }
            this.Text = "Подключение к " + this.cncType.ToString();            
        }

        #region Свойства класса возвращающие реквизиты подключения

        /// <summary>
        /// Возвращает или задает тип подключения на форме. Отразится на переключателе типов.
        /// </summary>
        public ConnectType cncType
        {
            get 
            {
                ConnectType temp;
                switch (serverTypeComboBox.SelectedIndex)
                {
                    case 1:
                        temp = ConnectType.Access;
                        break;
                    case 0:
                        temp = ConnectType.SQLServer;
                        break;
                    default:
                        temp = ConnectType.SQLServer;
                        break;                
                }
                return temp;
            }

            set
            {
                switch (value)
                {
                    case ConnectType.SQLServer: 
                            serverTypeComboBox.SelectedIndex = 0;
                        break;
                    case ConnectType.Access:
                            serverTypeComboBox.SelectedIndex = 1;
                        break;
                    default: 
                        serverTypeComboBox.SelectedIndex = 0; 
                        break;
                }                    
            }
        }

        /// <summary>
        /// Задает или возвращает путь к базе данных MS Access
        /// </summary>
        public string accessFilePath
        {
            get 
            { 
                return accessPathTextBox.Text; 
            }

            set
            {
                accessPathTextBox.Text = value;
            }
        }

        /// <summary>
        /// Возвращает или задает наименование сервера БД MS SQL Server
        /// </summary>
        public string sqlServerName
        {
            get 
            { 
                return serverNameTextBox.Text; 
            }

            set
            {
                serverNameTextBox.Text = value;
            }
        }

        /// <summary>
        /// Задает или возвращает имя БД в экземпляре MS SQL Server
        /// </summary>
        public string sqlDBName
        {
            get 
            { 
                return dbNameTextBox.Text;             
            }

            set
            {
                dbNameTextBox.Text = value;             
            }
        }

        /// <summary>
        /// Задает или возвращает пометку о использовании SQL аутентификации при подключении
        /// </summary>
        public bool sqlAuth
        {
            get 
            { 
                return useSQLAuthCheckBox.Checked; 
            }
            set
            {
                useSQLAuthCheckBox.Checked = value; 
            }
        }

        /// <summary>
        /// Задает или возвращает имя пользователя MS SQL Server
        /// </summary>
        public string sqlUser
        {
            get 
            { 
                return userTextBox.Text; 
            }

            set
            {
                userTextBox.Text = value;
            }
        }

        /// <summary>
        /// Возвращает или задает пароль пользователя БД
        /// </summary>
        public string sqlPwd
        {
            get 
            { 
                return pwdTextBox.Text; 
            }

            set
            {
                pwdTextBox.Text = value;
            }
        }

        #endregion

        private void LoginForm_KeyDown(object sender, KeyEventArgs e)
        {            
            this.Text = "Подключение к " + this.cncType.ToString();

            if (e.KeyCode == Keys.Escape)
            {
                this.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            }           
        }

        private void serverTypeComboBox_EnabledChanged(object sender, EventArgs e)
        {           
            this.Text = "Подключение к " + this.cncType.ToString();

            if (serverTypeComboBox.SelectedIndex == 0)
            {
                accessPathTextBox.Enabled = false;
                serverNameTextBox.Enabled = true;
                dbNameTextBox.Enabled = true;
                userTextBox.Enabled = true;
                pwdTextBox.Enabled = true;
                openFileDialogBtn.Enabled = false;
                useSQLAuthCheckBox.Enabled = true;
            }
            else
            {                    
                accessPathTextBox.Enabled = true;
                serverNameTextBox.Enabled = false;
                dbNameTextBox.Enabled = false;
                userTextBox.Enabled = false;
                pwdTextBox.Enabled = false;
                openFileDialogBtn.Enabled = true;
                useSQLAuthCheckBox.Enabled = false;
            }
            
        }

        private void openFileDialogBtn_Click(object sender, EventArgs e)
        {
            
            this.Text = "Подключение к " + this.cncType.ToString(); 

            OpenFileDialog openFileDialog1 = new OpenFileDialog();

            openFileDialog1.InitialDirectory = "c:\\";
            openFileDialog1.Filter = "mdb files (*.mdb)|*.mdb";
            openFileDialog1.FilterIndex = 1;
            openFileDialog1.RestoreDirectory = true;

            if (openFileDialog1.ShowDialog() == DialogResult.OK)            
                accessPathTextBox.Text = openFileDialog1.FileName;
        }

        private void LoginForm_Shown(object sender, EventArgs e)
        {           
            this.Text = "Подключение к " + this.cncType.ToString();
        }

        private void useWHsettingsBtn_Click(object sender, EventArgs e)
        {
            try
            {
                SettingsTemplate settings = miSettings.ProSettings();
                cncType = settings.type;
                accessFilePath = settings.dbFilePath;
                sqlAuth = settings.sqlAuthentication;
                sqlServerName = settings.serverName;
                sqlDBName = settings.dbName;
                sqlUser = settings.user;
                sqlPwd = settings.pwd;
            }
            catch (Exception ex)
            {
                log.Write("Не удалось взять настройки из WH Pro");
                log.Write(ex.Message);
                MessageBox.Show(ex.Message);
            }            
        }

        private void useLightSettingsBtn_Click(object sender, EventArgs e)
        {           
            try
            {
                SettingsTemplate settings = miSettings.LightSettings();

                cncType = settings.type;
                accessFilePath = settings.dbFilePath;
                sqlAuth = settings.sqlAuthentication;
                sqlServerName = settings.serverName;
                sqlDBName = settings.dbName;
                sqlUser = settings.user;
                sqlPwd = settings.pwd;
            }
            catch (Exception ex)
            {
                log.Write("Не удалось взять настройки из WH Light");
                log.Write(ex.Message);
                MessageBox.Show(ex.Message);                
            }
        }

        private void LoginForm_Load(object sender, EventArgs e)
        {
            useWHsettingsBtn.Enabled = miSettings.isProEnable();
            useLightSettingsBtn.Enabled = miSettings.isLightEnable();
            if (this.useSQLAuthCheckBox.Checked)
                this.Size = new System.Drawing.Size(386, 260);
            else
                this.Size = new System.Drawing.Size(386, 200);
        }
    }
}

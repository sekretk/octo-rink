using System;
using System.Data;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{
    public class UserLightPermission
    {
        IDbConnection cnc;
        User user;

        /// <summary>
        /// Подключение к источнику данных. При измении данные о пользователе и его правах сбрасываются.
        /// </summary>
        public IDbConnection Connection
        {
            get 
            { 
                return cnc; 
            }
            set 
            { 
                Drop(); 
                cnc = value; 
            }
        }        

        /// <summary>
        /// Объект-пользователь, чьи разрешения отражает класс
        /// </summary>
        public User User
        {
            get { return user; }
            set { Load(value); }
        }

        /// <summary>
        /// Конструктор по умолчанию
        /// </summary>
        public UserLightPermission()
        {
            Drop();
        }

        /// <summary>
        /// Конструктор с передачей объета-пользователя
        /// </summary>
        /// <param name="usr">Пользователь, разрешения которого будут активны</param>
        public UserLightPermission(User usr)
        {
            if (usr != null)
                user = usr;
        }

        /// <summary>
        /// Конструктор с заполнением прав
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="usr">Пользователь, разрешения которого будут активны</param>
        public UserLightPermission(IDbConnection connection, User usr)
        {
            cnc = connection;
            user = usr;
            Load();
        }

        /// <summary>
        /// Помощь
        /// </summary>
        public bool Help;

        /// <summary>
        /// Выделение строки
        /// </summary>
        public bool SelectRow;

        /// <summary>
        /// Оплата наличными
        /// </summary>
        public bool CashPay;

        /// <summary>
        /// Оплата по счету
        /// </summary>
        public bool AccountPay;

        /// <summary>
        /// Оплата pos теминалом
        /// </summary>
        public bool POSPay;

        /// <summary>
        /// Оплата наличными - нефискальный чек
        /// </summary>
        public bool CashPayNonFiscal;

        /// <summary>
        /// Оплата наличными - кухонный принтер
        /// </summary>
        public bool CashPayKitchenPrt;

        /// <summary>
        /// Оплата ваучером
        /// </summary>
        public bool VoucherPay;

        /// <summary>
        /// Комбинированная оплата
        /// </summary>
        public bool CombPay;

        /// <summary>
        /// Оплата предоплата
        /// </summary>
        public bool PrePay;

        /// <summary>
        /// Оплата записывается как задолженность
        /// </summary>
        public bool DebitPay;

        /// <summary>
        /// Возврат
        /// </summary>
        public bool Return;

        /// <summary>
        /// Отложенные чеки (Сохранить)
        /// </summary>
        public bool Save;

        /// <summary>
        /// Проверка цен
        /// </summary>
        public bool PriceCheck;

        /// <summary>
        /// Вкл/Выкл печати кассового чека
        /// </summary>
        public bool ReceiptPrt;

        /// <summary>
        /// Сброс таблицы
        /// </summary>
        public bool ClearTable;

        /// <summary>
        /// Видео
        /// </summary>
        public bool Video;

        /// <summary>
        /// Добавить новый товар
        /// </summary>
        public bool AddGood;

        /// <summary>
        /// Вкл/Выкл печать счет-фактуры
        /// </summary>
        public bool InvoicePrt;

        /// <summary>
        /// Новый партнер
        /// </summary>
        public bool AddPartner;

        /// <summary>
        /// Менеджер документов
        /// </summary>
        public bool DocManager;

        /// <summary>
        /// Смена активного партнера и сброс таблицы
        /// </summary>
        public bool PartnerChange;

        /// <summary>
        /// Панель управления
        /// </summary>
        public bool ControlPanel;

        /// <summary>
        /// Программирование электронных весов
        /// </summary>
        public bool ScaleProg;

        /// <summary>
        /// X-отчет
        /// </summary>
        public bool xReport;

        /// <summary>
        /// Z-отчет
        /// </summary>
        public bool zReport;

        /// <summary>
        /// Удалить выделенную строку
        /// </summary>
        public bool DeleteRow;

        /// <summary>
        /// Выключить компьютер
        /// </summary>
        public bool PCTurnOff;

        /// <summary>
        /// Настройки
        /// </summary>
        public bool Settings;

        /// <summary>
        /// Записать изменения в БД
        /// </summary>
        public void SaveToDB()
        {
            if (user == null)
                return;

            if (cnc == null)
                return;

            try
            {

                string query = "DELETE FROM UsersSecurity WHERE UserID = " + user.ID.ToString();

                DbWorks.ExecuteQuery(cnc, query);

                if (!Help)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite034"));

                if (!SelectRow)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite035"));

                if (!CashPay)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite029"));

                if (!AccountPay)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite028"));

                if (!POSPay)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite031"));

                if (!CashPayNonFiscal)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite030"));

                if (!CashPayKitchenPrt)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite053"));

                if (!VoucherPay)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite052"));

                if (!CombPay)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite051"));

                if (!PrePay)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite058"));

                if (!DebitPay)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite120"));

                if (!Return)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite032"));

                if (!Save)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite048"));

                if (!PriceCheck)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite022"));

                if (!ReceiptPrt)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite039"));

                if (!ClearTable)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite065"));

                if (!Video)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite023"));

                if (!AddGood)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite063"));

                if (!InvoicePrt)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite040"));

                if (!AddPartner)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite020"));

                if (!DocManager)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite056"));

                if (!PartnerChange)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite024"));

                if (!ControlPanel)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite050"));

                if (!ScaleProg)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite064"));

                if (!xReport)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite062"));

                if (!zReport)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite061"));

                if (!DeleteRow)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite026"));

                if (!PCTurnOff)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite060"));

                if (!Settings)
                    DbWorks.ExecuteQuery(cnc, String.Format("INSERT INTO UsersSecurity VALUES({0}, '{1}', 0)", user.ID, "lite004"));
            }
            catch { }
        }

        /// <summary>
        /// Устанавливает переданного параметром пользователя текущим и загружает его разрешения
        /// </summary>
        /// <param name="newUser">Новый пользователь</param>
        public void Load(User newUser)
        {
            if (newUser == null)
                return;

            user = newUser;

            Load();
        }

        /// <summary>
        /// Загружает разрешения текущего пользователя
        /// </summary>
        public void Load()
        {
            if (cnc == null)
                return;

            try
            {
                string query = "SELECT ControlName, State FROM UsersSecurity WHERE UserID = " + user.ID.ToString();
                DataSet ds = DbWorks.GetDataSet(cnc, query);

                if (ds.Tables.Count == 0)
                    return;

                DataTable dt = ds.Tables[0];

                Drop();

                foreach (DataRow row in dt.Rows)
                {
                    switch ((string)row["ControlName"])
                    {
                        case "lite034": Help = false; break;
                        case "lite035": SelectRow = false; break;
                        case "lite029": CashPay = false; break;
                        case "lite028": AccountPay = false; break;
                        case "lite031": POSPay = false; break;
                        case "lite030": CashPayNonFiscal = false; break;
                        case "lite053": CashPayKitchenPrt = false; break;
                        case "lite052": VoucherPay = false; break;
                        case "lite051": CombPay = false; break;
                        case "lite058": PrePay = false; break;
                        case "lite120": DebitPay = false; break;
                        case "lite032": Return = false; break;
                        case "lite048": Save = false; break;
                        case "lite022": PriceCheck = false; break;
                        case "lite039": ReceiptPrt = false; break;
                        case "lite065": ClearTable = false; break;
                        case "lite023": Video = false; break;
                        case "lite063": AddGood = false; break;
                        case "lite040": InvoicePrt = false; break;
                        case "lite020": AddPartner = false; break;
                        case "lite056": DocManager = false; break;
                        case "lite024": PartnerChange = false; break;
                        case "lite050": ControlPanel = false; break;
                        case "lite064": ScaleProg = false; break;
                        case "lite062": xReport = false; break;
                        case "lite061": zReport = false; break;
                        case "lite026": DeleteRow = false; break;
                        case "lite060": PCTurnOff = false; break;
                        case "lite004": Settings = false; break;
                    }
                }
            }
            catch { }
        }

        /// <summary>
        /// Очищает данные о пользователе и разрешения выставляет в положение разрешено
        /// </summary>
        public void Drop()
        {
            user = null;

            Help = true;
            SelectRow = true;
            CashPay = true;
            AccountPay = true;
            POSPay = true;
            CashPayNonFiscal = true;
            CashPayKitchenPrt = true;
            VoucherPay = true;
            CombPay = true;
            PrePay = true;
            DebitPay = true;
            Return = true;
            Save = true;
            PriceCheck = true;
            ReceiptPrt = true;
            ClearTable = true;
            Video = true;
            AddGood = true;
            InvoicePrt = true;
            AddPartner = true;
            DocManager = true;
            PartnerChange = true;
            ControlPanel = true;
            ScaleProg = true;
            xReport = true;
            zReport = true;
            DeleteRow = true;
            PCTurnOff = true;
            Settings = true;
        }
    }
}

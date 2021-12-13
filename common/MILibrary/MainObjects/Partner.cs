using System;
using System.Data;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{
    /// <summary>
    /// Информация о партнере
    /// </summary>
    public class Partner
    {
        /// <summary>
        /// Идентификатор партнера
        /// </summary>
        public int ID;

        /// <summary>
        /// Наименование партнера
        /// </summary>
        public string Company;

        /// <summary>
        /// Наименование партнера для печати
        /// </summary>
        public string Company2;

        /// <summary>
        /// Код партнера
        /// </summary>
        public string Code;

        /// <summary>
        /// Материально ответсвенное лицо
        /// </summary>
        public string MOL;

        /// <summary>
        /// Материально ответсвенное лицо для печати
        /// </summary>
        public string MOL2;

        /// <summary>
        /// Город партнера
        /// </summary>
        public string City;

        /// <summary>
        /// Город партнера для печати
        /// </summary>
        public string City2;

        /// <summary>
        /// Адрес партнера
        /// </summary>
        public string Address;

        /// <summary>
        /// Адрес партнера для печати
        /// </summary>
        public string Address2;

        /// <summary>
        /// Телефон партнера
        /// </summary>
        public string Phone;

        /// <summary>
        /// Телефон партнера для печати
        /// </summary>
        public string Phone2;

        /// <summary>
        /// Факс партнера
        /// </summary>
        public string Fax;

        /// <summary>
        /// Адрес электронной почты партнера
        /// </summary>
        public string eMail;

        /// <summary>
        /// КПП партнера
        /// </summary>
        public string TaxNo;

        /// <summary>
        /// ИНН партнера
        /// </summary>
        public string Bulstat;

        /// <summary>
        /// Наименование банка партнера
        /// </summary>
        public string BankName;

        /// <summary>
        /// БИК банка партнера
        /// </summary>
        public string BankCode;

        /// <summary>
        /// Расчетный счет партнера
        /// </summary>
        public string BankAcct;

        /// <summary>
        /// Корреспондентский счет банка
        /// </summary>
        public string BankVATAcct;

        /// <summary>
        /// Ценовая группа партнера
        /// </summary>
        public int PriceGroup;

        /// <summary>
        /// Тип партнера
        /// </summary>
        public int Type;

        /// <summary>
        /// Номер клиентской карты партнера
        /// </summary>
        public string CardNumber;

        /// <summary>
        /// Скидка партнера по умолчанию
        /// </summary>
        public double Discount;

        /// <summary>
        /// Количество дней для отложеного платежа
        /// </summary>
        public int PaymentDays;

        /// <summary>
        /// ОКПД партнера
        /// </summary>
        public string Note1;

        /// <summary>
        /// ОКПО партнера
        /// </summary>
        public string Note2;

        /// <summary>
        /// Флаг для частого использования
        /// </summary>
        public int IsVeryUsed;

        /// <summary>
        /// Флаг логического удаления
        /// </summary>
        public int Deleted;

        /// <summary>
        /// Задает флаг логического удаления "Партнер не доступен"
        /// </summary>
        /// <param name="connection">Активное подключение к БД</param>
        /// <param name="newValue">Новое значение. 0 - доступен, -1 - не доступен</param>
        /// <returns>true если удалось изменить, false если нет</returns>
        public bool SetDeleted(IDbConnection connection, int newValue)
        {
            if (newValue == 0 || newValue == -1)
            {
                try
                {
                    DbWorks.ExecuteQuery(connection, String.Format("UPDATE Partners SET Deleted = {0} WHERE ID = {1}", newValue, ID));
                    Deleted = newValue;
                    return true;
                }
                catch
                {
                    return false;
                }
            }
            else
                return false;

        }

        /// <summary>
        /// Идентификатор пользователя создавшего партнера
        /// </summary>
        public int UserID;

        /// <summary>
        /// Иднтификатор группы партнера
        /// </summary>
        public int GroupID;

        /// <summary>
        /// Возвращает строку суммарной информации о партнере
        /// </summary>
        public string PartnerInfo
        {
            get
            {
                return String.Format("ID: {0}; Code: {1}; Company: {2}; Company2: {3}; MOL: {4}; MOL2: {5}; City: {6}; " +
                    "City2: {7}; Address: {8}; Address2: {9}; Phone: {10}; Phone2: {11}; Fax: {12}; eMail: {13}; TaxNo: {14}; " +
                    "Bulstat: {15}; BankName: {16}; BankCode: {17}; BankAcct: {18}; BankVATAcct: {19}; PriceGroup: {20}; " +
                    "Type: {21}; CardNumber: {22}; Discount: {23}; PaymentDays: {24}; Note1: {25}; Note2: {26}; IsVeryUsed: {27}; "
                    + "Deleted: {28}; UserID: {29}; GroupID: {30};",
                    ID, Code, Company, Company2, MOL, MOL2, City, City2, Address, Address2, Phone, Phone2, Fax, eMail, TaxNo, Bulstat,
                    BankName, BankCode, BankAcct, BankVATAcct, PriceGroup.ToString(), Type.ToString(), CardNumber, Discount.ToString(),
                    PaymentDays.ToString(), Note1, Note2, IsVeryUsed.ToString(), Deleted.ToString(), UserID.ToString(), GroupID.ToString());
            }
        }

        public float GetBalance(IDbConnection connection)
        {
            try
            {
                if (connection.State == ConnectionState.Open)
                {
                    string query = String.Format("SELECT ISNULL(SUM(Qtty*Mode),0) FROM payments, Partners WHERE Payments.PartnerID = Partners.ID AND [OperType]=36	AND Partners.ID = '{0}'", ID);
                    return float.Parse(DbWorks.ExecuteScalar(connection, query).ToString());
                }
                else
                    return 0;
            }
            catch
            {
                return 0;
            }
        }

        /// <summary>
        /// Возвращает отметку о том был ли в этот день окончательный расчет клиента. Т.е. пополнялся ли баланс на отрицательную величину.
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <returns></returns>
        public bool IsFinishedToday(IDbConnection connection)
        {
            string query = String.Format("SELECT COUNT(*) FROM Payments WHERE PartnerID = {0} AND OperType = 36 AND Qtty < 0 AND Date = CONVERT(date, GETDATE())", ID);
            int result = 0;
            try
            {
                result = (int)DbWorks.ExecuteScalar(connection, query);
                if (result == 0)
                    return false;
                else
                    return true;
            }
            catch
            {
                return true;
            }
        }

        /// <summary>
        /// Возвращает отметку были ли у партнера продажи в текущий день
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <returns></returns>
        public bool HasSales(IDbConnection connection)
        {
            string query = String.Format("SELECT COUNT(*) FROM Payments WHERE PartnerID = {0} AND OperType = 2 AND Date = CONVERT(date, GETDATE())", ID);
            int result = 0;
            try
            {
                result = (int)DbWorks.ExecuteScalar(connection, query);
                if (result == 0)
                    return false;
                else
                    return true;
            }
            catch
            {
                return false;
            }
        }
    }

}

using System;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{


    /// <summary>
    /// Информация об объекте регистрации (собственной фирмы)
    /// </summary>
    public class Organisation
    {
        /// <summary>
        /// Идентификатор организации
        /// </summary>
        public int ID;

        /// <summary>
        /// Код организации
        /// </summary>
        public string Code;

        /// <summary>
        /// Наименование организации
        /// </summary>
        public string Company;

        /// <summary>
        /// Материально ответственное лицо организации
        /// </summary>
        public string MOL;

        /// <summary>
        /// Город организации
        /// </summary>
        public string City;

        /// <summary>
        /// Адрес организации
        /// </summary>
        public string Address;

        /// <summary>
        /// Телефон организации
        /// </summary>
        public string Phone;

        /// <summary>
        /// Факс организации
        /// </summary>
        public string Fax;

        /// <summary>
        /// Электронный почтовый адрес организации
        /// </summary>
        public string eMail;

        /// <summary>
        /// КПП организации
        /// </summary>
        public string TaxNo;

        /// <summary>
        /// ИНН организации
        /// </summary>
        public string Bulstat;

        /// <summary>
        /// Наименование банка организации
        /// </summary>
        public string BankName;

        /// <summary>
        /// БИК банка организации
        /// </summary>
        public string BankCode;

        /// <summary>
        /// Расчетный счет организации
        /// </summary>
        public string BankAcct;

        /// <summary>
        /// Корреспондентский счет банка организации
        /// </summary>
        public string BankVATAcct;

        /// <summary>
        /// ОКПД организации
        /// </summary>
        public string Note1;

        /// <summary>
        /// ОКПО организации
        /// </summary>
        public string Note2;

        /// <summary>
        /// Флаг использования по умолчанию
        /// </summary>
        public int IsDefault;

        /// <summary>
        /// Флаг логического удаления
        /// </summary>
        public int Deleted;

        /// <summary>
        /// Идентификатор пользователя создавшего организацию
        /// </summary>
        public int UserID;

        /// <summary>
        /// Дата создания (изменение) организации в БД
        /// </summary>
        public DateTime UserRealTime;

        /// <summary>
        /// Суммарная информация об организации
        /// </summary>
        public string OrganisationInfo
        {
            get
            {
                return String.Format("ID: {0}; Code: {1}; Company: {2}; MOL: {3}; City: {4}, Address: {5}; Phone: {6}; " +
                    "Fax: {7}; eMail: {8}; TaxNo: {9}; Bulstat: {10}; BankName: {11}; BankCode: {12}; BankAcct: {13}; " +
                    "BankVATAcct: {14}; Note1: {15}; Note2: {16}; IsDefault: {17}; Deleted: {18}; UserID: {19}; UserRealTime: {20}",
                    ID, Code, Company, MOL, City, Address, Phone, Fax, eMail, TaxNo, Bulstat, BankName, BankCode, BankAcct, BankVATAcct,
                    Note1, Note2, IsDefault, Deleted, UserID, UserRealTime);
            }
        }
    }

}

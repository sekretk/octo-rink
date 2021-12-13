using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Data.SqlClient;
using System.Data.OleDb;
using System.Data;
using System.Threading;
using MILibrary.MainObjects;

namespace MILibrary
{

    /// <summary>
    /// Класс для работы с базой данных.
    /// </summary>
    static public class DbWorks
    {
        /// <summary>
        /// Метод выполняет команду по заданному соединению, возвращает набор данных. Если соединение закрыто то метод пытается его открыть.
        /// </summary>
        /// <param name="connection">подключение к БД типа SQLConnection или OleDBConnection</param>
        /// <param name="strQuery">Текст запроса к БД</param>
        /// <returns>Вернет результат запроса или в противном случе пустой набор данных</returns>
        static public DataSet GetDataSet(IDbConnection connection, string strQuery)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    if (((SqlConnection)connection).State == ConnectionState.Closed) ((SqlConnection)connection).Open();
                    SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                    cmd.CommandText = strQuery;
                    DataSet ds = new DataSet();
                    SqlDataAdapter da = new SqlDataAdapter(cmd);
                    da.Fill(ds);
                    return ds;
                }
                catch
                {
                    return new DataSet();
                }
            }
            if (connection is OleDbConnection)
            {
                try
                {
                    if (((OleDbConnection)connection).State == ConnectionState.Closed) ((OleDbConnection)connection).Open();
                    OleDbCommand cmd = ((OleDbConnection)connection).CreateCommand();
                    cmd.CommandText = strQuery;
                    DataSet ds = new DataSet();
                    OleDbDataAdapter da = new OleDbDataAdapter(cmd);
                    da.Fill(ds);
                    return ds;
                }
                catch
                {
                    return new DataSet();
                }
            }
            return new DataSet();
        }

        ///<summary>
        ///</summary>
        ///<param name="connection"></param>
        ///<param name="cmd"></param>
        ///<returns></returns>
        ///<exception cref="ArgumentException"></exception>
        public static DataSet GetDataSet(IDbConnection connection, IDbCommand cmd)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    if (((SqlConnection)connection).State == ConnectionState.Closed)
                        ((SqlConnection)connection).Open();
                    DataSet ds = new DataSet();
                    SqlDataAdapter da = new SqlDataAdapter(cmd as SqlCommand);
                    da.Fill(ds);
                    return ds;
                }
                catch
                {
                    return new DataSet();
                }
            }
            if (connection is OleDbConnection)
            {
                try
                {
                    if (((OleDbConnection)connection).State == ConnectionState.Closed) ((OleDbConnection)connection).Open();
                    DataSet ds = new DataSet();
                    OleDbDataAdapter da = new OleDbDataAdapter(cmd as OleDbCommand);
                    da.Fill(ds);
                    return ds;
                }
                catch
                {
                    return new DataSet();
                }
            }

            throw new ArgumentException("Not supported DB type", connection.GetType().Name);
        }

        /// <summary>
        /// Выполняет без проверки заданную строку запроса
        /// </summary>
        /// <param name="connection">подключение к БД типа SQLConnection или OleDBConnection</param>
        /// <param name="strQuery">Строка запроса</param>
        static public void ExecuteQuery(IDbConnection connection, string strQuery)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    if (((SqlConnection)connection).State == ConnectionState.Closed) ((SqlConnection)connection).Open();
                    SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                    cmd.CommandText = strQuery;
                    cmd.ExecuteNonQuery();
                }
                catch { }
            }
            if (connection is OleDbConnection)
            {
                try
                {
                    if (((OleDbConnection)connection).State == ConnectionState.Closed) ((OleDbConnection)connection).Open();
                    OleDbCommand cmd = ((OleDbConnection)connection).CreateCommand();
                    cmd.CommandText = strQuery;
                    cmd.ExecuteNonQuery();
                }
                catch
                { }
            }
        }

        /// <summary>
        /// Выполняет запрос и возвращает скалярный результат
        /// </summary>
        /// <param name="connection">подключение к БД типа SQLConnection или OleDBConnection</param>
        /// <param name="strQuery">Строка запроса</param>
        /// <returns>Скалярный результат выполнения запроса</returns>
        static public object ExecuteScalar(IDbConnection connection, string strQuery)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    if (((SqlConnection)connection).State == ConnectionState.Closed) ((SqlConnection)connection).Open();
                    SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                    cmd.CommandText = strQuery;
                    return cmd.ExecuteScalar();
                }
                catch { return null; }
            }
            if (connection is OleDbConnection)
            {
                try
                {
                    if (((OleDbConnection)connection).State == ConnectionState.Closed) ((OleDbConnection)connection).Open();
                    OleDbCommand cmd = ((OleDbConnection)connection).CreateCommand();
                    cmd.CommandText = strQuery;
                    return cmd.ExecuteScalar();
                }
                catch
                { return null; }
            }
            return null;
        }

        /// <summary>
        /// Выполняет запрос и возвращает скалярный результат
        /// </summary>
        /// <param name="connection">подключение к БД типа SQLConnection или OleDBConnection</param>
        /// <param name="cmd">Комманда </param>
        /// <returns>Скалярный результат выполнения запроса</returns>
        static public object ExecuteScalar(IDbConnection connection, IDbCommand cmd)
        {
            try
            {
                cmd.Connection = connection;
                return cmd.ExecuteScalar();
            }
            catch { return null; }
        }

        /// <summary>
        /// Проверяет корректная ли база данных.
        /// </summary>
        /// <param name="connection">объект реализующий интерфейс IDbConnection</param>
        /// <returns>true если база данных имеет необходимые таблицы, false в противном случае</returns>
        static public bool CheckDB(IDbConnection connection)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    int result = 0;
                    SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('System', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Goods', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('GoodsGroups', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Partners', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('PartnersGroups', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Store', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Documents', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Registration', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Payments', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Objects', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('ObjectsGroups', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('Operations', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('VATGroups', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    cmd.CommandText = "SELECT CASE WHEN OBJECT_ID('OperationType', 'U') IS NULL THEN 1 ELSE 0 END";
                    result += Int32.Parse(cmd.ExecuteScalar().ToString());

                    if (result == 0) return true;
                }
                catch
                {
                    return false;
                }
            }
            if (connection is OleDbConnection)
            {
                try
                {
                    OleDbCommand cmd = ((OleDbConnection)connection).CreateCommand();
                    cmd.CommandText = "Select top 1 * from Goods"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from Currencies"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from Objects"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from ObjectsGroups"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from OperationType"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from Partners"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from PartnersGroups"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from Store"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from System"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from Users"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from UsersGroups"; cmd.ExecuteNonQuery();
                    cmd.CommandText = "Select top 1 * from VATGroups"; cmd.ExecuteNonQuery();
                    return true;
                }
                catch
                {
                    return false;

                }
            }
            return false;
        }

        /// <summary>
        /// Возвращает версию базы данных. В случае ошибки возвращает 0;
        /// </summary>
        /// <param name="connection">Открытое подключение к базе данных</param>
        /// <returns>Версия БД.</returns>
        static public float DBVersion(IDbConnection connection)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                    cmd.CommandText = "SELECT Version FROM System WHERE ProductID = 1;";
                    string version = cmd.ExecuteScalar().ToString();
                    return Single.Parse(version);
                }
                catch
                {
                    return 0;
                }
            }

            if (connection is OleDbConnection)
            {
                try
                {
                    OleDbCommand cmd = ((OleDbConnection)connection).CreateCommand();
                    cmd.CommandText = "SELECT Version FROM System WHERE ProductID = 1;";
                    return Single.Parse(cmd.ExecuteScalar().ToString());
                }
                catch
                {
                    return 0;
                }
            }
            return 0;

        }

        /// <summary>
        /// Запрашивает таблицу партнеров БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица партнеров</returns>
        static public DataTable PartnersTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM Partners").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу пользователей БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица пользователей</returns>
        static public DataTable UsersTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM Users").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу групп пользователей БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица групп пользователей</returns>
        static public DataTable UsersGroupsTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM UsersGroups").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу настроек БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица настроек</returns>
        static public DataTable ConfigurationTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM Configuration").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу групп товаров БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица групп товаров</returns>
        static public DataTable GoodsGroupsTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM GoodsGroups").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу групп партнеров БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица групп партнеров</returns>
        static public DataTable PartnersGroupsTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM PartnersGroups").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу налогов НДС БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица налогов НДС</returns>
        static public DataTable VATGroupsTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM VATGroups").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу валют БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица валют</returns>
        static public DataTable CurrenciesTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM Currencies").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу объектов БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица объектов</returns>
        static public DataTable ObjectsTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM Objects").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу групп объектов БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица групп объектов</returns>
        static public DataTable ObjectsGroupsTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM ObjectsGroups").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// Запрашивает таблицу регистрационной информации БД Microinvest, в случае ошибки возвращает пустой DataTable
        /// </summary>
        /// <param name="connection">Открытое соединение с корректной БД.</param>
        /// <returns>Таблица регистрационной информации</returns>
        static public DataTable RegistationTbl(IDbConnection connection)
        {
            try
            {
                DataTable table = DbWorks.GetDataSet(connection, "SELECT * FROM Registration").Tables[0];
                return table;
            }
            catch
            {
                return new DataTable();
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="connection"></param>
        /// <returns></returns>
        public static int ChoosePartner(IDbConnection connection)
        {
            return 1;
        }

        /// <summary>
        /// Показывает пользователю оконную форму выбора товаров.
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <returns></returns>
        public static int[] ChooseGood(IDbConnection connection)
        {
            return ChooseGood(connection, 1);
        }

        /// <summary>
        /// Показывает пользователю оконную форму выбора товаров.
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="good">Объект товара выбранный в форме при отображении</param>
        /// <returns></returns>
        public static int[] ChooseGood(IDbConnection connection, Good good)
        {
            GoodSelectFrm gSelectFrm = new GoodSelectFrm(connection, good);
            if (gSelectFrm.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                return gSelectFrm.SelectedID;
            else
                return null;
        }

        /// <summary>
        /// Показывает пользователю оконную форму выбора товаров.
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="goodID">Идентификатор товара выбранный в форме при отображении</param>
        /// <returns></returns>
        public static int[] ChooseGood(IDbConnection connection, int goodID)
        {
            return ChooseGood(connection, GetGood(connection, goodID));
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу партнеров
        /// </summary>
        /// <param name="connection">Активное и открытое подключение к источнику данных</param>
        /// <param name="partnerGroup">Объект ранее выбранной группы</param>
        /// <returns></returns>
        public static Group ChoosePartnersGroup(IDbConnection connection, Group partnerGroup)
        {
            GroupsSelectFrm grpFrm = new GroupsSelectFrm(connection, MIObjectType.Partner, partnerGroup);
            grpFrm.ShowDialog();
            return grpFrm.choosenGrp;
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу партнеров
        /// </summary>
        /// <param name="connection">Активное и открытое подключение к источнику данных</param>
        /// <returns></returns>
        public static Group ChoosePartnersGroup(IDbConnection connection)
        {
            return ChoosePartnersGroup(connection, new Group());
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу партнеров
        /// </summary>
        /// <param name="connection">Активное и открытое подключение к источнику данных</param>
        /// <param name="pgID">Идентификатор ранее выбранной группу партнеров</param>
        /// <returns></returns>
        public static Group ChoosePartnersGroup(IDbConnection connection, int pgID)
        {
            return ChoosePartnersGroup(connection, GetGroup(connection, pgID, MIObjectType.Partner));
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу партнеров
        /// </summary>
        /// <param name="connection">Активное и открытое подключение к источнику данных</param>
        /// <param name="pgCode">Код ранее выбранной группы парнеров</param>
        /// <returns></returns>
        public static Group ChoosePartnersGroup(IDbConnection connection, string pgCode)
        {
            return ChoosePartnersGroup(connection, GetGroup(connection, pgCode, MIObjectType.Partner));
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу товаров. 
        /// </summary>
        /// <param name="connection">Активное и открытое подключение к источнику данных</param>
        /// <param name="goodGroup">Объект ранее выбранной группы</param>
        /// <returns></returns>
        public static Group ChooseGoodsGroups(IDbConnection connection, Group goodGroup)
        {
            GroupsSelectFrm grpFrm = new GroupsSelectFrm(connection, MIObjectType.Good, goodGroup);
            if (grpFrm.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                return grpFrm.choosenGrp;
            else
                return goodGroup;
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу. 
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <returns></returns>
        public static Group ChooseGoodsGroups(IDbConnection connection)
        {
            return ChooseGoodsGroups(connection, new Group());
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу. 
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="ggID">Идентификатор ранее выбранной группу</param>
        /// <returns></returns>
        public static Group ChooseGoodsGroups(IDbConnection connection, int ggID)
        {
            return ChooseGoodsGroups(connection, GetGroup(connection, ggID, MIObjectType.Good));
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу товаров. 
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="ggCode">Код ранее выбранной группы</param>
        /// <returns></returns>
        public static Group ChooseGoodsGroups(IDbConnection connection, string ggCode)
        {
            return ChooseGoodsGroups(connection, GetGroup(connection, ggCode, MIObjectType.Good));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="connection"></param>
        /// <returns></returns>
        public static int ChooseObject(IDbConnection connection)
        {
            return 1;
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу объектов
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="objGrp">Объект ранее выбранной группы</param>
        /// <returns></returns>
        public static Group ChooseObjectsGroup(IDbConnection connection, Group objGrp)
        {
            GroupsSelectFrm grpFrm = new GroupsSelectFrm(connection, MIObjectType.Object, objGrp);
            if (grpFrm.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                return grpFrm.choosenGrp;
            else
                return objGrp;
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу объектов
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <returns></returns>
        public static Group ChooseObjectsGroup(IDbConnection connection)
        {
            return ChooseObjectsGroup(connection, new Group());
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу объектов
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="ogID">Идентификатор ранее выбранной группы объектов</param>
        /// <returns></returns>
        public static Group ChooseObjectsGroup(IDbConnection connection, int ogID)
        {
            return ChooseObjectsGroup(connection, GetGroup(connection, ogID, MIObjectType.Object));
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу объектов
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="ogCode">Код ранее выбранной группы объектов</param>
        /// <returns></returns>
        public static Group ChooseObjectsGroup(IDbConnection connection, string ogCode)
        {
            return ChooseObjectsGroup(connection, GetGroup(connection, ogCode, MIObjectType.Object));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="connection"></param>
        /// <returns></returns>
        public static int ChooseUser(IDbConnection connection)
        {
            return 1;
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу пользователей
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="uGroup">Объект ранее выбранной группы</param>
        /// <returns></returns>
        public static Group ChooseUsersGroup(IDbConnection connection, Group uGroup)
        {
            GroupsSelectFrm grpFrm = new GroupsSelectFrm(connection, MIObjectType.User, uGroup);
            if (grpFrm.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                return grpFrm.choosenGrp;
            else
                return uGroup;
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу пользователей
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <returns></returns>
        public static Group ChooseUsersGroup(IDbConnection connection)
        {
            return ChooseUsersGroup(connection, new Group());
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу пользователей
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="ugID">Идентификатор ранее выбранной группы пользователей</param>
        /// <returns></returns>
        public static Group ChooseUsersGroup(IDbConnection connection, int ugID)
        {
            return ChooseUsersGroup(connection, GetGroup(connection, ugID, MIObjectType.User));
        }

        /// <summary>
        /// Возвращает выбранную пользователем группу пользователей
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="ugCode">Код ранее выбранной группы пользователей</param>
        /// <returns></returns>
        public static Group ChooseUsersGroup(IDbConnection connection, string ugCode)
        {
            return ChooseUsersGroup(connection, GetGroup(connection, ugCode, MIObjectType.User));
        }

        /// <summary>
        /// Возвращает информацию о партнере по его идентификатору
        /// </summary>
        /// <param name="connection">Подключение к источнику даннных</param>
        /// <param name="ID">Идентификатор партнера</param>
        /// <returns>Информация о партнере</returns>
        public static Partner GetPartner(IDbConnection connection, int ID)
        {
            Partner partner = new Partner();
            DataSet ds = DbWorks.GetDataSet(connection, "SELECT * FROM Partners WHERE ID = " + ID.ToString());
            if (ds == null) return null;
            if (ds.Tables.Count == 0) return null;
            if (ds.Tables[0].Rows.Count == 0) return null;
            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                partner.ID = Int32.Parse(row["ID"].ToString());
                partner.Company = (string)row["Company"];
                partner.Company2 = (string)row["Company2"];
                partner.Code = row["Code"].ToString();
                partner.MOL = row["MOL"].ToString();
                partner.MOL2 = row["MOL2"].ToString();
                partner.City = row["City"].ToString();
                partner.City2 = row["City2"].ToString();
                partner.Address = row["Address"].ToString();
                partner.Address2 = row["Address2"].ToString();
                partner.Phone = row["Phone"].ToString();
                partner.Phone2 = row["Phone2"].ToString();
                partner.Fax = row["Fax"].ToString();
                partner.eMail = row["eMail"].ToString();
                partner.TaxNo = row["TaxNo"].ToString();
                partner.Bulstat = row["Bulstat"].ToString();
                partner.BankName = row["BankName"].ToString();
                partner.BankCode = row["BankCode"].ToString();
                partner.BankAcct = row["BankAcct"].ToString();
                partner.BankVATAcct = row["BankVATAcct"].ToString();
                partner.PriceGroup = Int32.Parse(row["PriceGroup"].ToString());
                partner.Type = Int32.Parse(row["Type"].ToString());
                partner.CardNumber = row["CardNumber"].ToString();
                partner.Discount = Double.Parse(row["Discount"].ToString());
                partner.PaymentDays = Int32.Parse(row["PaymentDays"].ToString());
                partner.Note1 = row["Note1"].ToString();
                partner.Note2 = row["Note2"].ToString();
                partner.IsVeryUsed = Int32.Parse(row["IsVeryUsed"].ToString());
                partner.Deleted = Int32.Parse(row["Deleted"].ToString());
                partner.UserID = Int32.Parse(row["UserID"].ToString());
                partner.GroupID = Int32.Parse(row["GroupID"].ToString());
                return partner;
            }
            catch
            {
                return null;
            }
        }

        /// <summary>
        /// Возвращает информацию о партнере по его номеру карты
        /// </summary>
        /// <param name="connection">Подключение к источнику даннных</param>
        /// <param name="cardNumber">Номер карты партнера</param>
        /// <param name="code">Код партнера</param>
        /// <param name="name">Имя партнера</param>
        /// <param name="findExact">Искать по точному совпадению</param>
        /// <param name="searchCondition">8 - по номеру карты, 4 - по коду, 2 - по названию, 6 - код и название, 10 - номер карты и название, 12 - код и номер карты, 14 - все три</param>
        /// <returns>Информация о партнере</returns>
        public static Partner GetPartner(IDbConnection connection, string cardNumber, string code, string name, bool findExact, short searchCondition)
        {
            Partner partner = new Partner();
            DataSet ds = new DataSet();
            switch (searchCondition)
            {
                case 8:
                    ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Partners WHERE CardNumber LIKE '{0}'", findExact ? cardNumber : cardNumber + "%"));
                    break;
                case 2:
                    ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Partners WHERE Company LIKE '{0}'", findExact ? name : name + "%"));
                    break;
                case 4:
                    ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Partners WHERE Code LIKE '{0}'", findExact ? code : code + "%"));
                    break;
                case 10:
                    ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Partners WHERE CardNumber LIKE '{0}' OR Company LIKE '{1}'", findExact ? cardNumber : cardNumber + "%", findExact ? name : name + "%"));
                    break;
                case 12:
                    ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Partners WHERE CardNumber LIKE '{0}' OR Code LIKE '{1}'", findExact ? cardNumber : cardNumber + "%", findExact ? code : code + "%"));
                    break;
                case 6:
                    ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Partners WHERE Company LIKE '{0}' OR Code LIKE '{1}'", findExact ? name : name + "%", findExact ? code : code + "%"));
                    break;
                case 14:
                    ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Partners WHERE CardNumber LIKE '{0}' OR Company LIKE '{0}' OR Code LIKE '{1}'", findExact ? cardNumber : cardNumber + "%", findExact ? name : name + "%", findExact ? code : code + "%"));
                    break;
                default: return null;
            }
            if (ds == null) return null;
            if (ds.Tables.Count == 0) return null;
            if (ds.Tables[0].Rows.Count == 0) return null;
            if (ds.Tables[0].Rows.Count > 1) return null;
            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                partner.ID = Int32.Parse(row["ID"].ToString());
                partner.Company = row["Company"].ToString();
                partner.Company2 = row["Company2"].ToString();
                partner.Code = row["Code"].ToString();
                partner.MOL = row["MOL"].ToString();
                partner.MOL2 = row["MOL2"].ToString();
                partner.City = row["City"].ToString();
                partner.City2 = row["City2"].ToString();
                partner.Address = row["Address"].ToString();
                partner.Address2 = row["Address2"].ToString();
                partner.Phone = row["Phone"].ToString();
                partner.Phone2 = row["Phone2"].ToString();
                partner.Fax = row["Fax"].ToString();
                partner.eMail = row["eMail"].ToString();
                partner.TaxNo = row["TaxNo"].ToString();
                partner.Bulstat = row["Bulstat"].ToString();
                partner.BankName = row["BankName"].ToString();
                partner.BankCode = row["BankCode"].ToString();
                partner.BankAcct = row["BankAcct"].ToString();
                partner.BankVATAcct = row["BankVATAcct"].ToString();
                partner.PriceGroup = Int32.Parse(row["PriceGroup"].ToString());
                partner.Type = Int32.Parse(row["Type"].ToString());
                partner.CardNumber = row["CardNumber"].ToString();
                partner.Discount = Double.Parse(row["Discount"].ToString());
                partner.PaymentDays = Int32.Parse(row["PaymentDays"].ToString());
                partner.Note1 = row["Note1"].ToString();
                partner.Note2 = row["Note2"].ToString();
                partner.IsVeryUsed = Int32.Parse(row["IsVeryUsed"].ToString());
                partner.Deleted = Int32.Parse(row["Deleted"].ToString());
                partner.UserID = Int32.Parse(row["UserID"].ToString());
                partner.GroupID = Int32.Parse(row["GroupID"].ToString());
                return partner;
            }
            catch
            {
                return null;
            }
        }

        /// <summary>
        /// Возвращает информацию о партнере по его номеру карты
        /// </summary>
        /// <param name="connection">Подключение к источнику даннных</param>
        /// <param name="cardNumber">Номер карты партнера</param>
        /// <returns>Информация о партнере</returns>
        public static Partner GetPartner(IDbConnection connection, string cardNumber)
        {
            return GetPartner(connection, cardNumber, "", "", true, 8);
        }

        /// <summary>
        /// Возвращает информацию о пользователе по его идентификатору
        /// </summary>
        /// <param name="connection">Подключение к источнику даннных</param>
        /// <param name="ID">Идентификатор пользователя</param>
        /// <returns>Информация о пользователе</returns>
        public static User GetUser(IDbConnection connection, int ID)
        {
            User user = new User();
            DataSet ds = DbWorks.GetDataSet(connection, "SELECT * FROM Users WHERE ID = " + ID.ToString());
            if (ds == null) return null;
            if (ds.Tables.Count == 0) return null;
            if (ds.Tables[0].Rows.Count == 0) return null;
            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                user.ID = Int32.Parse(row["ID"].ToString());
                user.GroupID = Int32.Parse(row["GroupID"].ToString());
                user.Code = row["Code"].ToString();
                user.Name = row["Name"].ToString();
                user.Name2 = row["Name2"].ToString();
                user.Password = row["Password"].ToString();
                user.CardNumber = row["CardNumber"].ToString();
                user.UserLevel = Int32.Parse(row["UserLevel"].ToString());
                user.IsVeryUsed = Int32.Parse(row["IsVeryUsed"].ToString());
                user.Deleted = Int32.Parse(row["Deleted"].ToString());
                return user;
            }
            catch
            {
                return new User() { ID = 1 };
            }
        }

        /// <summary>
        /// Возвращает информацию о пользователе по его номеру карты
        /// </summary>
        /// <param name="connection">Подключение к источнику даннных</param>
        /// <param name="cardNumber">Номер карты пользователя</param>
        /// <returns>Информация о пользователе</returns>
        public static User GetUser(IDbConnection connection, string cardNumber)
        {
            User user = new User();
            DataSet ds = DbWorks.GetDataSet(connection, String.Format("SELECT * FROM Users WHERE CardNumber = '{0}'", cardNumber));
            if (ds == null) return null;
            if (ds.Tables.Count == 0) return null;
            if (ds.Tables[0].Rows.Count == 0) return null;
            if (ds.Tables[0].Rows.Count > 1) return null;
            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                user.ID = Int32.Parse(row["ID"].ToString());
                user.GroupID = Int32.Parse(row["GroupID"].ToString());
                user.Code = row["Code"].ToString();
                user.Name = row["Name"].ToString();
                user.Name2 = row["Name2"].ToString();
                user.Password = row["Passwords"].ToString();
                user.CardNumber = row["CardNumber"].ToString();
                user.UserLevel = Int32.Parse(row["UserLevel"].ToString());
                user.IsVeryUsed = Int32.Parse(row["IsVeryUsed"].ToString());
                user.Deleted = Int32.Parse(row["Deleted"].ToString());
                return user;
            }
            catch
            {
                return null;
            }
        }

        /// <summary>
        /// Возвращает колекцию товаров заданной группы
        /// </summary>
        /// <param name="cnc">Активное соединени</param>
        /// <param name="groupId">Идентификатор группы</param>
        /// <returns></returns>
        public static IEnumerable<Good> GetGoodByGroup(IDbConnection cnc, int groupId)
        {

            var result = new List<Good>();

            DataSet ds = GetDataSet(cnc, "SELECT * FROM Goods WHERE GroupID = " + groupId);
            if (ds == null) return result;
            if (ds.Tables.Count == 0) return result;
            if (ds.Tables[0].Rows.Count == 0) return result;
            try
            {
                foreach (DataRow row in ds.Tables[0].Rows)
                {
                    var good = new Good();

                    good.ID = Int32.Parse(row["ID"].ToString());
                    good.GroupID = Int32.Parse(row["GroupID"].ToString());
                    good.Code = row["Code"].ToString();
                    good.BarCode1 = row["BarCode1"].ToString();
                    good.BarCode2 = row["BarCode2"].ToString();
                    good.BarCode3 = row["BarCode3"].ToString();
                    good.Catalog1 = row["Catalog1"].ToString();
                    good.Catalog2 = row["Catalog2"].ToString();
                    good.Catalog3 = row["Catalog3"].ToString();
                    good.Name = row["Name"].ToString();
                    good.Name2 = row["Name2"].ToString();
                    good.Measure = row["Measure1"].ToString();
                    good.Measure2 = row["Measure2"].ToString();
                    good.PriceIn = Double.Parse(row["PriceIn"].ToString());
                    good.PriceOut1 = Double.Parse(row["PriceOut1"].ToString());
                    good.PriceOut2 = Double.Parse(row["PriceOut2"].ToString());
                    good.PriceOut3 = Double.Parse(row["PriceOut3"].ToString());
                    good.PriceOut4 = Double.Parse(row["PriceOut4"].ToString());
                    good.PriceOut5 = Double.Parse(row["PriceOut5"].ToString());
                    good.PriceOut6 = Double.Parse(row["PriceOut6"].ToString());
                    good.PriceOut7 = Double.Parse(row["PriceOut7"].ToString());
                    good.PriceOut8 = Double.Parse(row["PriceOut8"].ToString());
                    good.PriceOut9 = Double.Parse(row["PriceOut9"].ToString());
                    good.PriceOut10 = Double.Parse(row["PriceOut10"].ToString());
                    good.MinQtty = Double.Parse(row["MinQtty"].ToString());
                    good.NormalQtty = Double.Parse(row["NormalQtty"].ToString());
                    good.Description = row["Description"].ToString();
                    good.Type = Int32.Parse(row["Type"].ToString());
                    good.IsRecipe = Int32.Parse(row["IsRecipe"].ToString());
                    good.TaxGroup = Int32.Parse(row["TaxGroup"].ToString());
                    good.IsVeryUsed = Int32.Parse(row["IsVeryUsed"].ToString());
                    good.Deleted = Int32.Parse(row["Deleted"].ToString());
                    result.Add(good);
                }
            }
            catch
            {
                return result;
            }

            return result;
        }

        /// <summary>
        /// Возвращает полную информацию о товаре, в случае ошибке вернет пустую структуру
        /// </summary>
        /// <param name="connection">Соединение с исчтоником данных</param>
        /// <param name="ID">Идентификатор товара</param>
        /// <returns>Полную информацию о товаре</returns>
        public static Good GetGood(IDbConnection connection, int ID)
        {
            Good good = new Good();
            DataSet ds = DbWorks.GetDataSet(connection, "SELECT * FROM Goods WHERE ID = " + ID.ToString());
            if (ds == null) return good;
            if (ds.Tables.Count == 0) return good;
            if (ds.Tables[0].Rows.Count == 0) return good;
            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                good.ID = Int32.Parse(row["ID"].ToString());
                good.GroupID = Int32.Parse(row["GroupID"].ToString());
                good.Code = row["Code"].ToString();
                good.BarCode1 = row["BarCode1"].ToString();
                good.BarCode2 = row["BarCode2"].ToString();
                good.BarCode3 = row["BarCode3"].ToString();
                good.Catalog1 = row["Catalog1"].ToString();
                good.Catalog2 = row["Catalog2"].ToString();
                good.Catalog3 = row["Catalog3"].ToString();
                good.Name = row["Name"].ToString();
                good.Name2 = row["Name2"].ToString();
                good.Measure = row["Measure1"].ToString();
                good.Measure2 = row["Measure2"].ToString();
                good.PriceIn = Double.Parse(row["PriceIn"].ToString());
                good.PriceOut1 = Double.Parse(row["PriceOut1"].ToString());
                good.PriceOut2 = Double.Parse(row["PriceOut2"].ToString());
                good.PriceOut3 = Double.Parse(row["PriceOut3"].ToString());
                good.PriceOut4 = Double.Parse(row["PriceOut4"].ToString());
                good.PriceOut5 = Double.Parse(row["PriceOut5"].ToString());
                good.PriceOut6 = Double.Parse(row["PriceOut6"].ToString());
                good.PriceOut7 = Double.Parse(row["PriceOut7"].ToString());
                good.PriceOut8 = Double.Parse(row["PriceOut8"].ToString());
                good.PriceOut9 = Double.Parse(row["PriceOut9"].ToString());
                good.PriceOut10 = Double.Parse(row["PriceOut10"].ToString());
                good.MinQtty = Double.Parse(row["MinQtty"].ToString());
                good.NormalQtty = Double.Parse(row["NormalQtty"].ToString());
                good.Description = row["Description"].ToString();
                good.Type = Int32.Parse(row["Type"].ToString());
                good.IsRecipe = Int32.Parse(row["IsRecipe"].ToString());
                good.TaxGroup = Int32.Parse(row["TaxGroup"].ToString());
                good.IsVeryUsed = Int32.Parse(row["IsVeryUsed"].ToString());
                good.Deleted = Int32.Parse(row["Deleted"].ToString());
                return good;
            }
            catch
            {
                return new Good();
            }
        }

        /// <summary>
        /// Возвращает информацию об объекте организации по его идентификатору
        /// </summary>
        /// <param name="connection">Соединение с источником данных</param>
        /// <param name="ID">Идентификатор объекта</param>
        /// <returns>Полную информацию об объекте</returns>
        public static Warehouse GetObject(IDbConnection connection, int ID)
        {
            Warehouse obj = new Warehouse();
            DataSet ds = DbWorks.GetDataSet(connection, "SELECT * FROM Objects WHERE ID = " + ID);
            if (ds == null) return obj;
            if (ds.Tables.Count == 0) return obj;
            if (ds.Tables[0].Rows.Count == 0) return obj;
            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                obj.ID = Int32.Parse(row["ID"].ToString());
                obj.GroupID = Int32.Parse(row["GroupID"].ToString());
                obj.Code = row["Code"].ToString();
                obj.Name = row["Name"].ToString();
                obj.Name2 = row["Name2"].ToString();
                obj.PriceGroup = Int32.Parse(row["PriceGroup"].ToString());
                obj.IsVeryUsed = Int32.Parse(row["IsVeryUsed"].ToString());
                obj.Deleted = Int32.Parse(row["Deleted"].ToString());
                return obj;
            }
            catch
            {
                return new Warehouse() { ID = 1 };
            }
        }

        /// <summary>
        /// Возвращает полную информацию об организации указанной по умолчанию, в случае ошибки возвращает пустой объект
        /// </summary>
        /// <param name="connection">Соединение с источником данных</param>
        /// <returns>Информация об организации</returns>
        public static Organisation GetRegOrganisation(IDbConnection connection)
        {
            Organisation organisation = new Organisation();
            DataSet ds = DbWorks.GetDataSet(connection, "SELECT * FROM Registration WHERE IsDefault = -1");
            if (ds == null) return organisation;
            if (ds.Tables.Count == 0) return organisation;
            if (ds.Tables[0].Rows.Count == 0) return organisation;
            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                organisation.ID = Int32.Parse(row["ID"].ToString());
                organisation.Code = row["Code"].ToString();
                organisation.Company = row["Company"].ToString();
                organisation.MOL = row["MOL"].ToString();
                organisation.City = row["City"].ToString();
                organisation.Address = row["Address"].ToString();
                organisation.Phone = row["Phone"].ToString();
                organisation.Fax = row["Fax"].ToString();
                organisation.eMail = row["eMail"].ToString();
                organisation.TaxNo = row["TaxNo"].ToString();
                organisation.Bulstat = row["Bulstat"].ToString();
                organisation.BankName = row["BankName"].ToString();
                organisation.BankCode = row["BankCode"].ToString();
                organisation.BankAcct = row["BankAcct"].ToString();
                organisation.BankVATAcct = row["BankVATAcct"].ToString();
                organisation.Note1 = row["Note1"].ToString();
                organisation.Note2 = row["Note2"].ToString();
                organisation.IsDefault = Int32.Parse(row["IsDefault"].ToString());
                organisation.Deleted = Int32.Parse(row["Deleted"].ToString());
                organisation.UserID = Int32.Parse(row["UserID"].ToString());
                organisation.UserRealTime = DateTime.Parse(row["UserRealTime"].ToString());
                return organisation;
            }
            catch
            {
                return new Organisation();
            }
        }

        /// <summary>
        /// Получает баланс клиента по номеру карты
        /// </summary>
        /// <param name="connection">Активное подключение к БД</param>
        /// <param name="number">Номер клиентской карты партнера</param>
        /// <returns>Баланс клиента</returns>
        public static float GetBalance(IDbConnection connection, string number)
        {
            return GetBalance(connection, GetPartner(connection, number));
        }

        /// <summary>
        /// Получает баланс клиента
        /// </summary>
        /// <param name="connection">Активное подключение к БД</param>
        /// <param name="partner">Партнер</param>
        /// <returns></returns>
        public static float GetBalance(IDbConnection connection, Partner partner)
        {
            try
            {
                if (connection.State == ConnectionState.Open)
                {
                    if (partner == null) return 0;
                    string query = String.Format(
                                @"SELECT ISNULL(SUM(Qtty*Mode),0) 
                                  FROM payments, Partners 
                                  WHERE 
                                        Payments.PartnerID = Partners.ID 
                                        AND [OperType]=36
                                        AND Partners.ID = '{0}'", partner.ID);
                    return float.Parse(ExecuteScalar(connection, query).ToString());
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
        /// Возвращает округленную до 2 знаков после запятой внесенную предоплатой сумму
        /// </summary>
        /// <param name="connection">Активное подключение к БД. Работает только с MS SQL Server!!!</param>
        /// <param name="partner">Партнер</param>
        /// <param name="isToday">true если брать данные только за сегодняшний день, false вынудит собрать данные за всю историю</param>
        /// <returns>Внесенная предоплатой сумма</returns>
        public static float PartnerAdvanceSum(IDbConnection connection, Partner partner, bool isToday)
        {
            if (connection is SqlConnection)
            {
                string query = String.Format(
                                    @"SELECT ROUND(ISNULL(SUM(Qtty),0),2) 
                                    FROM Payments 
                                    WHERE PartnerID = {0} AND OperType = 36 AND Mode = 1 {1}", partner.ID,
                                                                                             isToday ? "AND Date = Convert(date, GETDATE())" :
                                                                                                String.Format(@"AND UserRealTime > (SELECT ISNULL(MAX(UserRealTime), 0) 
                                                                                                    FROM Operations 
                                                                                                    WHERE OperType = 98 AND PartnerID = {0})", partner.ID));
                return float.Parse(ExecuteScalar(connection, query).ToString());
            }
            else return 0;
        }

        /// <summary>
        /// Возвращает округленную до 2 знаков после запятой сумму покупок клиента
        /// </summary>
        /// <param name="connection">Активное подключение к БД</param>
        /// <param name="partner">Партнер, по которому бедется отчет</param>
        /// <param name="isToday">true если брать данные только за сегодняшний день, false вынудит собрать данные за всю историю</param>
        /// <returns></returns>
        public static float PartnerSalesSum(IDbConnection connection, Partner partner, bool isToday)
        {
            if (connection is SqlConnection)
            {
                string query = String.Format("SELECT ROUND(ISNULL(SUM(Qtty),0),2) FROM Payments WHERE PartnerID = {0} AND OperType = 2 AND Mode = 1 {1}",
                    partner.ID,
                    isToday ?
                        "AND Date = Convert(date, GETDATE())" :
                        string.Format(@"AND UserRealTime > (SELECT ISNULL(MAX(UserRealTime), 0) 
                                                                                                        FROM Operations 
                                                                                                        WHERE OperType = 98 AND PartnerID = {0})", partner.ID));
                return float.Parse(ExecuteScalar(connection, query).ToString());
            }
            else return 0;
        }

        /// <summary>
        /// Возвращает текущее время на сервере БД
        /// </summary>
        /// <param name="connection">Активное подключение к БД</param>
        /// <returns>Текущее время на сервер БД</returns>
        public static DateTime GetDBDate(IDbConnection connection)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    string date = ExecuteScalar(connection, "SELECT GETDATE()").ToString();
                    return DateTime.Parse(date);
                }
                catch
                {
                    return DateTime.Today;
                }
            }
            else
                return DateTime.Today;
        }

        /// <summary>
        /// Возвращает полные данные о продажах за текущий день
        /// </summary>
        /// <param name="connection">Активное соединение</param>
        /// <param name="partner">Партнер по которому требуется получить данные</param>
        /// <returns></returns>
        public static DataSet GetFullDaySales(IDbConnection connection, Partner partner)
        {
            if (connection is SqlConnection && partner != null)
            {
                string query = String.Format(@"SELECT 
                                        	G.Name + ' (' + Obj.Name + ')' AS Name, SUM(O.Qtty) AS Qtty, SUM(O.PriceOut*O.Qtty) AS Price
                                            FROM 
	                                            Operations AS O 
	                                            JOIN Goods AS G ON G.ID = O.GoodID 
	                                            JOIN Objects AS Obj ON Obj.ID = O.ObjectID 
                                            WHERE 
	                                            PartnerID = {0} 
	                                            AND OperType = 2
	                                            AND O.Date = Convert(date, GETDATE())
                                            GROUP BY 
                                                G.Name,
                                                Obj.Name", partner.ID);
                return GetDataSet(connection, query);
            }
            else
                return null;
        }

        /// <summary>
        /// Возвращает полные данные о продажах за последнюю активную сессию партнера
        /// </summary>
        /// <param name="connection">Активное соединение</param>
        /// <param name="partner">Партнер по которому требуется получить данные</param>
        /// <returns></returns>
        public static DataSet GetFullSales(IDbConnection connection, Partner partner)
        {
            if (connection is SqlConnection && partner != null)
            {
                string query = String.Format(@"SELECT 
                                        	G.Name + ' (' + Obj.Name + ')' AS Name, SUM(O.Qtty) AS Qtty, SUM(O.PriceOut*O.Qtty) AS Price
                                            FROM 
	                                            Operations AS O 
	                                            JOIN Goods AS G ON G.ID = O.GoodID 
	                                            JOIN Objects AS Obj ON Obj.ID = O.ObjectID 
                                            WHERE 
	                                            PartnerID = {0} 
	                                            AND OperType = 2
	                                            AND O.UserRealTime > (
                                                                        SELECT ISNULL(MAX(UserRealTime), 0) 
                                                                        FROM Operations 
                                                                        WHERE OperType = 98 AND PartnerID = {0}
                                                                      )
                                            GROUP BY 
                                                G.Name,
                                                Obj.Name", partner.ID);
                return GetDataSet(connection, query);
            }
            else
                return null;
        }

        /// <summary>
        /// Возвращает краткие данные о продажах за текущий день
        /// </summary>
        /// <param name="connection">Активное соединение с источником данных</param>
        /// <param name="partner">Партнетр по которому нужна статистика</param>
        /// <returns></returns>
        public static DataSet GetShortDaySales(IDbConnection connection, Partner partner)
        {
            if (connection is SqlConnection && partner != null)
            {
                string query = String.Format(@"SELECT 
                                        	Obj.Name AS Name, SUM(O.Qtty) AS Qtty, SUM(O.Qtty*O.PriceOut) AS Price
                                            FROM 
	                                            Operations AS O 
	                                            JOIN Goods AS G ON G.ID = O.GoodID 
	                                            JOIN Objects AS Obj ON Obj.ID = O.ObjectID 
                                            WHERE 
	                                            PartnerID = {0} 
	                                            AND OperType = 2
	                                            AND O.Date = Convert(date, GETDATE())
                                            GROUP BY
                                                Obj.Name", partner.ID);
                return GetDataSet(connection, query);
            }
            else
                return null;
        }

        /// <summary>
        /// Возвращает краткие данные о продажах за период активации партнера
        /// </summary>
        /// <param name="connection">Активное соединение с источником данных</param>
        /// <param name="partner">Партнетр по которому нужна статистика</param>
        /// <returns></returns>
        public static DataSet GetShortSales(IDbConnection connection, Partner partner)
        {
            if (connection is SqlConnection && partner != null)
            {
                string query = String.Format(@"SELECT 
                                        	Obj.Name AS Name, SUM(O.Qtty) AS Qtty, SUM(O.Qtty*O.PriceOut) AS Price
                                            FROM 
	                                            Operations AS O 
	                                            JOIN Goods AS G ON G.ID = O.GoodID 
	                                            JOIN Objects AS Obj ON Obj.ID = O.ObjectID 
                                            WHERE 
	                                            PartnerID = {0} 
	                                            AND OperType = 2
	                                            AND O.UserRealTime > (
                                                                        SELECT ISNULL(MAX(UserRealTime), 0) 
                                                                        FROM Operations 
                                                                        WHERE OperType = 98 AND PartnerID = {0}
                                                                      )                                                                     
                                            GROUP BY
                                                Obj.Name", partner.ID);
                return GetDataSet(connection, query);
            }
            else
                return null;
        }

        /// <summary>
        /// Возвращает номер следующей операции заданного типа. Если операций такого типа не было, то возвращает 1
        /// </summary>
        /// <param name="connection">Активное соединение с БД</param>
        /// <param name="operType">Тип операции. Подробнее в таблице OperationType</param>
        /// <returns>Номер следующей операции</returns>
        public static Int64 GetNextPaymentAcct(IDbConnection connection, int operType)
        {
            Int64 result;
            try
            {
                result = Int64.Parse(ExecuteScalar(connection, String.Format("SELECT ISNULL(MAX(Acct),0) + 1 FROM payments WHERE OperType = {0}", operType)).ToString());
            }
            catch
            {
                result = 1;
            }

            return result;
        }

        /// <summary>
        /// Пополняет баланс клиента на заданную сумму.
        /// </summary>
        /// <param name="connection">Активное соединения с БД. Работает только с MS SQL Server</param>
        /// <param name="partner">Партнер на чей баланс будет начислена сумма</param>
        /// <param name="money">Сумма для пополнения</param>
        /// <param name="nextAcct">Номер документа</param>
        /// <param name="userId">Идентификатор пользователя от имени которого проводится операция</param>
        /// <param name="objectId">Идентификатор объекта</param>
        /// <returns></returns>
        public static bool AddBalance(IDbConnection connection, Partner partner, float money, Int64 nextAcct, int userId = 1, int objectId = 1)
        {
            if (connection is SqlConnection && partner != null)
            {
                SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                cmd.CommandText = @"INSERT INTO 
                                        payments ( Acct, OperType, PartnerID, 
                                                   [Date], EndDate, [Mode], 
                                                   [Type],  Qtty, ObjectID, 
                                                   UserID, UserRealTime, [Sign])
                                    VALUES 
                                            (@nextAcct, 36, @pID, Convert(date, GETDATE()), 
                                            Convert(date, GETDATE()), 1, 1, @money, @objectId, @userID, GETDATE(), 1)";
                cmd.Parameters.Add("@nextAcct", SqlDbType.BigInt);
                cmd.Parameters["@nextAcct"].Value = nextAcct;

                cmd.Parameters.Add("@pID", SqlDbType.Int);
                cmd.Parameters["@pID"].Value = partner.ID;

                cmd.Parameters.Add("@money", SqlDbType.Float);
                cmd.Parameters["@money"].Value = money;

                cmd.Parameters.Add("@objectId", SqlDbType.Int);
                cmd.Parameters["@objectId"].Value = objectId;

                cmd.Parameters.Add("@userID", SqlDbType.Int);
                cmd.Parameters["@userID"].Value = userId;

                try
                {
                    cmd.ExecuteNonQuery();
                }
                catch
                {
                    return false;
                }

                cmd.Parameters.Clear();

                cmd.CommandText = @"INSERT INTO cashbook
                                        ([Date], [Desc], OperType, 
                                        Sign, Profit, ObjectID,
                                        UserID, UserRealTime)
                                    VALUES 
                                        (Convert(date, GETDATE()), 'Предоплата No. ' + @nextAcct + ', ' + @partner, 10, 1, @money, @objectId, @userID, GETDATE())";
                cmd.Parameters.Add("@nextAcct", SqlDbType.NVarChar);
                cmd.Parameters["@nextAcct"].Value = nextAcct.ToString().PadLeft(10, '0');

                cmd.Parameters.Add("@partner", SqlDbType.NVarChar);
                cmd.Parameters["@partner"].Value = partner.Company;

                cmd.Parameters.Add("@money", SqlDbType.Float);
                cmd.Parameters["@money"].Value = money;

                cmd.Parameters.Add("@objectId", SqlDbType.Int);
                cmd.Parameters["@objectId"].Value = objectId;

                cmd.Parameters.Add("@userID", SqlDbType.Int);
                cmd.Parameters["@userID"].Value = userId;

                try
                {
                    cmd.ExecuteNonQuery();
                }
                catch
                {
                    return false;
                }

                return true;

            }
            else
                return false;
        }

        /// <summary>
        /// Получает следущий номер чека
        /// </summary>
        /// <param name="connection">Активное соединение с БД</param>
        /// <returns>Номер следующего чека</returns>
        public static Int64 GetNextReceiptID(IDbConnection connection)
        {
            Int64 result;
            try
            {
                result = Int64.Parse(ExecuteScalar(connection, "SELECT ISNULL(MAX(ReceiptID),0) + 1 FROM ecrreceipts").ToString());
            }
            catch
            {
                result = 1;
            }

            return result;
        }

        /// <summary>
        /// Записывает в БД о выданном чеке
        /// </summary>
        /// <param name="connection">Активное соединение. Работает только с MS SQL SERVER!!!</param>
        /// <param name="operType">Тип операции. Подробнее в таблице OperationType</param>
        /// <param name="nextAcct">Номер операции</param>
        /// <param name="nextReceipt">Номер чека</param>
        /// <param name="receiptType">Тип чека. 1 - фискальный, 2 - нефискальный, 3 - чек возврата ...</param>
        /// <param name="ecrID">Номер кассового аппарата</param>
        /// <param name="desc">Описание кассового аппарата</param>
        /// <param name="money">Сумма</param>
        /// <param name="userID">Пользователь</param>
        /// <returns>Отметку об удаче выполнения операции</returns>
        public static bool AddECRReceipt(IDbConnection connection, int operType, Int64 nextAcct, Int64 nextReceipt, int receiptType, string ecrID, string desc, float money, int userID)
        {
            if (connection is SqlConnection)
            {
                SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                cmd.CommandText = @"INSERT INTO ecrreceipts
                                        (OperType, Acct, ReceiptID, 
                                        ReceiptDate, ReceiptType, ECRID, 
                                        [Description], [Total], UserID, UserRealTime)
                                    VALUES
                                        (@operType, @nextAcct, @nextReceipt, GETDATE(), @receiptType, @ecrID, @desc, @money, @userID, GETDATE())";

                cmd.Parameters.Add("@operType", SqlDbType.Int);
                cmd.Parameters["@operType"].Value = operType;

                cmd.Parameters.Add("@nextAcct", SqlDbType.BigInt);
                cmd.Parameters["@nextAcct"].Value = nextAcct;

                cmd.Parameters.Add("@nextReceipt", SqlDbType.BigInt);
                cmd.Parameters["@nextReceipt"].Value = nextReceipt;

                cmd.Parameters.Add("@receiptType", SqlDbType.Int);
                cmd.Parameters["@receiptType"].Value = receiptType;

                cmd.Parameters.Add("@ecrID", SqlDbType.NVarChar);
                cmd.Parameters["@ecrID"].Value = ecrID;

                cmd.Parameters.Add("@desc", SqlDbType.NVarChar);
                cmd.Parameters["@desc"].Value = desc;

                cmd.Parameters.Add("@money", SqlDbType.Float);
                cmd.Parameters["@money"].Value = money;

                cmd.Parameters.Add("@userID", SqlDbType.Int);
                cmd.Parameters["@userID"].Value = userID;

                try
                {
                    cmd.ExecuteNonQuery();
                }
                catch
                {
                    return false;
                }

                return true;
            }
            else
            {
                return false;
            }
        }

        /// <summary>
        /// Возвращает DataReader по заказанному запросу
        /// </summary>
        /// <param name="connection">Активное открытое подключение к БД</param>
        /// <param name="query">Строка запроса</param>
        /// <returns></returns>
        public static SqlDataReader GetDataReader(IDbConnection connection, string query)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                    cmd.CommandText = query;
                    return cmd.ExecuteReader();
                }
                catch
                {
                    return null;
                }
            }
            return null;
        }

        /// <summary>
        /// Возвращает массив пользователей
        /// </summary>
        /// <param name="connection">Активное открытое соединение с БД</param>
        /// <returns>Массив пользователей</returns>
        public static ArrayList GetUsers(IDbConnection connection)
        {
            ArrayList users = new ArrayList();
            try
            {
                DataTable dt = UsersTbl(connection);
                foreach (DataRow row in dt.Rows)
                {
                    User user = new User();
                    user.ID = (int)row["ID"];
                    user.Name = (string)row["Name"];
                    user.Name2 = (string)row["Name2"];
                    user.CardNumber = (string)row["CardNumber"];
                    user.Code = (string)row["Code"];
                    user.Deleted = (int)row["Deleted"];
                    user.GroupID = (int)row["GroupID"];
                    user.IsVeryUsed = (int)row["IsVeryUsed"];
                    user.Password = (string)row["Password"];
                    user.UserLevel = (int)row["UserLevel"];
                    users.Add(user);
                }
            }
            catch { }
            return users;
        }

        /// <summary>
        /// Возвращает массив объектов предприятия
        /// </summary>
        /// <param name="connection">Активное открытое подключение</param>
        /// <returns>Массив объектов Warehouse</returns>
        public static ArrayList GetObjects(IDbConnection connection)
        {
            ArrayList objs = new ArrayList();
            try
            {
                DataTable dt = ObjectsTbl(connection);
                foreach (DataRow row in dt.Rows)
                {
                    Warehouse obj = new Warehouse();
                    obj.ID = (int)row["ID"];
                    obj.Name = (string)row["Name"];
                    obj.Name2 = (string)row["Name2"];
                    obj.Code = (string)row["Code"];
                    obj.Deleted = (int)row["Deleted"];
                    obj.GroupID = (int)row["GroupID"];
                    obj.IsVeryUsed = (int)row["IsVeryUsed"];
                    obj.PriceGroup = (int)row["PriceGroup"];
                    objs.Add(obj);
                }
            }
            catch { }
            return objs;
        }

        /// <summary>
        /// Возвращает суммарную предоплату по всем партнерам
        /// </summary>
        /// <param name="connection">Активное и открытое соединение с БД</param>
        /// <returns></returns>
        public static float GetBalance(IDbConnection connection)
        {
            try
            {
                if (connection.State == ConnectionState.Open)
                    return float.Parse(ExecuteScalar(connection, "SELECT ISNULL(ABS(SUM(Qtty * Mode)),0) FROM Payments WHERE OperType = 36 and PartnerID <> 1").ToString());
                else
                    return 0F;
            }
            catch
            {
                return 0F;
            }
        }

        /// <summary>
        /// Возвращает суммарный баланс всех партнеров
        /// </summary>
        /// <param name="connection"></param>
        /// <returns></returns>
        public static float GetActiveBalance(IDbConnection connection)
        {
            try
            {
                if (connection.State == ConnectionState.Open)
                    return float.Parse(ExecuteScalar(connection, "SELECT ISNULL(ABS(SUM(Qtty * Mode)),0) FROM Payments WHERE OperType = 36 and PartnerID <> 1").ToString());
                else
                    return 0F;
            }
            catch
            {
                return 0F;
            }
        }

        /// <summary>
        /// Возвращает массив партнеров с не нулевым балансом, кроме служебного
        /// </summary>
        /// <param name="connection">Активное соединение с БД</param>
        /// <returns>массив партнеров</returns>
        public static ArrayList GetPartnersWithBalance(IDbConnection connection)
        {
            ArrayList result = new ArrayList();
            if (connection is SqlConnection)
            {
                string query = @"SELECT 
	                                P.ID,
	                                ISNULL(ABS(SUM(Pay.Qtty * Pay.Mode)),0) AS Balance
                                FROM 
	                                Payments AS Pay
	                                JOIN Partners AS P ON P.ID= Pay.PartnerID
                                WHERE 
	                                Pay.PartnerID <> 1
                                GROUP BY 
	                                P.ID
                                HAVING 
                                ISNULL(ABS(SUM(Pay.Qtty * Pay.Mode)),0) > 0";
                DataTable dt = GetDataSet(connection, query).Tables[0];

                foreach (DataRow row in dt.Rows)
                {
                    result.Add(GetPartner(connection, (int)row["ID"]));
                }
            }

            return result;
        }

        /// <summary>
        /// Возвращает массив недоступных партнеров
        /// </summary>
        /// <param name="connection">Активное открытое подключение к БД</param>
        /// <returns></returns>
        public static ArrayList GetAllPartners(IDbConnection connection)
        {
            ArrayList result = new ArrayList();
            if (connection is SqlConnection)
            {
                string query = @"SELECT ID FROM Partners";
                DataTable dt = GetDataSet(connection, query).Tables[0];

                foreach (DataRow row in dt.Rows)
                {
                    result.Add(GetPartner(connection, (int)row["ID"]));
                }
            }

            return result;
        }

        /// <summary>
        /// Возвращает массив недоступных партнеров
        /// </summary>
        /// <param name="connection">Активное открытое подключение к БД</param>
        /// <returns></returns>
        public static ArrayList GetNonDeletedPartners(IDbConnection connection)
        {
            ArrayList result = new ArrayList();
            if (connection is SqlConnection)
            {
                string query = @"SELECT ID FROM Partners WHERE Deleted <> -1";
                DataTable dt = GetDataSet(connection, query).Tables[0];

                foreach (DataRow row in dt.Rows)
                {
                    result.Add(GetPartner(connection, (int)row["ID"]));
                }
            }

            return result;
        }

        /// <summary>
        /// Возвращает массив партнеров требующих финализацию. Это все партнеры (с типом Клиент) отметившие платежи в этот день и не имеющие отрицательных платежей
        /// </summary>
        /// <param name="connection"></param>
        /// <returns></returns>
        public static ArrayList GetNeedFinishPartners(IDbConnection connection)
        {
            ArrayList result = new ArrayList();
            if (connection is SqlConnection)
            {
                string query = @"SELECT 
	                                PartnerID AS ID, MIN(Qtty)
                                FROM 
	                                Payments
	                                JOIN Partners ON Partners.ID = Payments.PartnerID
                                WHERE 
	                                Date = CONVERT(date, GETDATE())
	                                AND Partners.Type = 2
                                GROUP BY 
	                                PartnerID
                                HAVING 
	                                MIN(Qtty) >= 0";
                DataTable dt = GetDataSet(connection, query).Tables[0];

                foreach (DataRow row in dt.Rows)
                {
                    result.Add(GetPartner(connection, (int)row["ID"]));
                }
            }

            return result;
        }

        /// <summary>
        /// Возвращает список активных клиентов без окончательного расчета в этот день.
        /// </summary>
        /// <param name="connection">Активное подключение к источнику данных</param>
        /// <returns></returns>
        public static ArrayList GetActiveNonFinishedPartners(IDbConnection connection)
        {
            ArrayList result = new ArrayList();
            if (connection is SqlConnection)
            {
                string query = @"WITH Part AS 
                                (
                                SELECT 
                                    P.ID,
                                    ISNULL(ABS(SUM(Pay.Qtty * Pay.Mode)),0) AS Balance,
                                    (
		                                SELECT 
			                                COUNT(*) 
		                                FROM 
			                                Payments 
		                                WHERE 
			                                PartnerID = P.ID 
			                                AND OperType = 36 
			                                AND Qtty < 0 
			                                AND Date = CONVERT(date, GETDATE())
                                    ) AS isFinished
                                FROM 
                                    Payments AS Pay
                                    JOIN Partners AS P ON P.ID = Pay.PartnerID
                                WHERE 
                                    Pay.PartnerID <> 1 AND P.Deleted = 0
                                GROUP BY 
                                    P.ID
                                )
                                SELECT 
	                                * 
                                FROM 
	                                Part
                                WHERE 
	                                Part.isFinished <> 0";
                DataTable dt = GetDataSet(connection, query).Tables[0];

                foreach (DataRow row in dt.Rows)
                {
                    result.Add(GetPartner(connection, (int)row["ID"]));
                }
            }

            return result;
        }

        ///<summary>
        /// Get list of active partner in specified shift
        ///</summary>
        ///<param name="cnc">Alive connection</param>
        ///<param name="startDate">Start date</param>
        ///<returns></returns>
        ///<exception cref="NotSupportedException"></exception>
        public static IEnumerable<Partner> ActivePartnersInCurrentShift(IDbConnection cnc, DateTime startDate)
        {
            if (!(cnc is SqlConnection))
                throw new NotSupportedException("Not supported DB type");

            var result = new List<Partner>();

            string query = @"SELECT P.ID
                            FROM Partners AS P
                            JOIN Operations as O1 ON O1.OperType IN (98, 99) AND O1.PartnerID = P.ID AND O1.UserRealTime = (SELECT MAX(O2.UserRealTime) FROM Operations as O2 WHERE O2.PartnerID = P.ID AND O2.OperType IN (98, 99))
                            WHERE O1.UserRealTime >= @startDate AND O1.OperType = 99";

            SqlCommand cmd = ((SqlConnection)cnc).CreateCommand();
            cmd.CommandText = query;

            cmd.Parameters.AddWithValue("@startDate", startDate);

            DataTable dt = GetDataSet(cnc, cmd).Tables[0];

            foreach (DataRow row in dt.Rows)
                result.Add(GetPartner(cnc, (int)row["ID"]));

            return result;
        }

        /// <summary>
        /// Возвращает статус существования таблицы в БД
        /// </summary>
        /// <param name="connection">Активное и открытое соединение с БД. Предусмотрена работа только с MS SQL Server</param>
        /// <param name="tableName">Имя таблицы с учетом схемы, существование которой требуется проверить</param>
        /// <returns></returns>
        /// <example>if (TableExists(activeConnection, "[dbo].[Users]")) ... </example>
        public static bool TableExists(IDbConnection connection, string tableName)
        {
            if (connection is SqlConnection)
            {
                try
                {
                    int count =
                        (int)ExecuteScalar(connection,
                            String.Format("SELECT COUNT(*) FROM sys.objects WHERE object_id = OBJECT_ID(N'{0}') AND type in (N'U')",
                                tableName));
                    return count >= 1;

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
        /// Возвращает массив подгрупп заданной группы включая данную группу
        /// </summary>
        /// <param name="connection">Активное открытое подключение к БД</param>
        /// <param name="code">Код группы</param>
        /// <param name="type">Тип объекта</param>
        /// <returns></returns>
        public static ArrayList GetSubgroups(IDbConnection connection, string code, MIObjectType type)
        {
            if (connection.State != ConnectionState.Open)
                connection.Open();

            ArrayList groups = new ArrayList();

            string query = code != "-1" ?
                String.Format("SELECT * FROM {0}sGroups WHERE Code LIKE '{1}%'", type.ToString("G"), code) :
                String.Format("SELECT * FROM {0}sGroups", type.ToString("G"));

            DataSet ds = GetDataSet(connection, query);

            if (ds.Tables.Count == 0) return null;

            try
            {
                foreach (DataRow row in ds.Tables[0].Rows)
                {
                    Group grp = new Group();
                    grp.ID = (int)row["ID"];
                    grp.Code = (string)row["Code"];
                    grp.Name = (string)row["Name"];
                    grp.Type = type;
                    groups.Add(grp);
                }
                return groups;
            }
            catch
            {
                return null;
            }

        }

        /// <summary>
        /// Возвращает объект группа по идентификатору
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="groupID">Идентификатор группы</param>
        /// <param name="type">Тип обьъекта</param>
        /// <returns></returns>
        public static Group GetGroup(IDbConnection connection, int groupID, MIObjectType type)
        {
            string query = String.Format("SELECT * FROM {0}sGroups WHERE ID = {1}", type.ToString("G"), groupID);

            DataSet ds = GetDataSet(connection, query);

            Group group = new Group();

            group.Type = type;

            if (ds.Tables.Count == 0) return null;
            if (ds.Tables[0].Rows.Count == 0) return null;

            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                group.ID = (int)row["ID"];
                group.Code = (string)row["Code"];
                group.Name = (string)row["Name"];
                return group;
            }
            catch
            {
                return null;
            }
        }

        /// <summary>
        /// Возвращает объект группу по коду
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="groupCode">Код группы</param>
        /// <param name="type">Тип объекта</param>
        /// <returns></returns>
        public static Group GetGroup(IDbConnection connection, string groupCode, MIObjectType type)
        {
            string query = String.Format("SELECT * FROM {0}sGroups WHERE Code = {1}", type.ToString("G"), "'" + groupCode + "'");

            DataSet ds = GetDataSet(connection, query);

            Group group = new Group();

            group.Type = type;

            if (ds.Tables.Count == 0) return null;
            if (ds.Tables[0].Rows.Count == 0) return null;

            try
            {
                DataRow row = ds.Tables[0].Rows[0];
                group.ID = (int)row["ID"];
                group.Code = (string)row["Code"];
                group.Name = (string)row["Name"];
                return group;
            }
            catch
            {
                return null;
            }
        }

        ///<summary>
        /// Возвращает отметку об активности партнера
        ///</summary>
        ///<param name="connection">Активное соединение</param>
        ///<param name="partner">Проверяемый партнер</param>
        ///<returns></returns>
        public static bool IsActive(IDbConnection connection, Partner partner)
        {
            string query = String.Format(
                   @"SELECT COUNT(*) 
                    FROM Operations
                    WHERE 
                        PartnerID = {0} 
                        AND OperType = 99 
                        AND UserRealTime > (
                                    SELECT ISNULL(MAX(UserRealTime), 0) 
                                    FROM Operations 
                                    WHERE OperType = 98 AND PartnerID = {0})", partner.ID);

            //Результат: количество активаций с момента последней деактивации
            int result = 0;
            try
            {
                result = (int)ExecuteScalar(connection, query);
                return result != 0; // == 0 не активный, != 0 активный
            }
            catch
            {
                return false;
            }
        }

        /// <summary>
        /// Возвращает отметку о том был ли в этот день окончательный расчет клиента. Т.е. поплнялся ли баланс на отрицательную величину.
        /// </summary>
        /// <param name="connection">Активное открытое подключение к источнику данных</param>
        /// <param name="partner">Клиент, данные по которому нужно получить</param>
        /// <returns></returns>
        public static bool HasFinishedToday(IDbConnection connection, Partner partner)
        {
            string query = String.Format(
                    @"SELECT COUNT(*) 
                    FROM Payments 
                    WHERE 
                        PartnerID = {0} 
                        AND OperType = 36 
                        AND Qtty < 0 
                        AND Date = CONVERT(date, GETDATE())", partner.ID);
            int result = 0;
            try
            {
                result = (int)ExecuteScalar(connection, query);
                return result != 0;
            }
            catch
            {
                return true;
            }
        }

        /// <summary>
        /// Возвращает количество проданного товара с заданным названием на заданную дату
        /// </summary>
        /// <param name="connection">Активное открытое подключение к БД</param>
        /// <param name="date">Дата актуальности запроса</param>
        /// <param name="goodName">Название товара</param>
        /// <returns></returns>
        public static int SalesCount(IDbConnection connection, DateTime date, string goodName)
        {
            if (connection is SqlConnection)
            {
                SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                cmd.CommandText = @"SELECT 
                                        SUM(Operations.Qtty) 
                                    FROM
                                        Operations, Goods
                                    WHERE 
	                                    Operations.GoodID = Goods.ID
	                                    AND Operations.OperType = 2
	                                    AND Goods.Name = @gName
	                                    AND Operations.Date = @oDate";
                cmd.Parameters.Add("gName", SqlDbType.VarChar);
                cmd.Parameters.Add("oDate", SqlDbType.Date);
                cmd.Parameters["gName"].Value = goodName;
                cmd.Parameters["oDate"].Value = date.Date;
                int result = 0;
                try
                {
                    result = int.Parse(ExecuteScalar(connection, cmd).ToString());
                }
                catch (Exception ex)
                { }

                return result;
            }

            if (connection is OleDbConnection)
            {
                OleDbCommand cmd = ((OleDbConnection)connection).CreateCommand();
                cmd.CommandText = @"SELECT 
                                        COUNT(Operations.Qtty) 
                                    FROM
                                        Operations, Goods
                                    WHERE 
	                                    Operations.GoodID = Goods.ID
	                                    AND Operations.OperType = 2
	                                    AND Goods.Name = @gName
	                                    AND Operations.Date = @oDate";
                cmd.Parameters.Add("gName", OleDbType.VarChar);
                cmd.Parameters.Add("oDate", OleDbType.Date);
                cmd.Parameters["gName"].Value = goodName;
                cmd.Parameters["oDate"].Value = date;
                int result = 0;
                try
                {
                    result = (int)ExecuteScalar(connection, cmd);
                }
                catch { }

                return result;
            }

            return 0;
        }

        /// <summary>
        /// Возвращает общую сумму внесенного аванса за текущий день
        /// </summary>
        /// <param name="connection">Открытое подключение к БД. Работает только с MS SQL Server соединением</param>
        /// <param name="objectId">Идентификатор объекта</param>
        /// <param name="startDate">Дата с которой подсчитывается баланс</param>
        /// <returns></returns>
        public static float TotalAdvance(IDbConnection connection, int objectId, DateTime startDate)
        {
            if (connection is SqlConnection)
            {
                SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                cmd.CommandText = String.Format(@"SELECT 
	                                                SUM(P.Qtty) 
                                                FROM 
	                                                Payments AS P
                                                WHERE
	                                                P.UserRealTime > @sDate
	                                                AND P.OperType = 36 AND P.Mode = 1
	                                                AND P.Qtty > 0 AND ObjectID = {0}", objectId);



                cmd.Parameters.Add("sDate", SqlDbType.DateTime);
                cmd.Parameters["sDate"].Value = startDate;


                float result = 0;
                try
                {
                    result = float.Parse(ExecuteScalar(connection, cmd).ToString());
                }
                catch (Exception ex)
                { }

                return result;
            }
            else
                return 0F;
        }

        ///<summary>
        /// Возвращает сумму внесенных на депозит денег (операция #198)
        ///</summary>
        ///<param name="connection"></param>
        ///<param name="objectId"></param>
        ///<returns></returns>
        ///<exception cref="NotImplementedException"></exception>
        public static float TotalDeposit(IDbConnection connection, int objectId)
        {
            if (!(connection is SqlConnection))
                throw new NotSupportedException("Supported only SQL server DB type");


            SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
            cmd.CommandText = @"SELECT ISNULL(SUM(O.Sign*O.Qtty),0) as Qty FROM Operations as O WHERE o.OperType in (198, 199) AND O.ObjectID = @objectId";

            cmd.Parameters.AddWithValue("@objectId", objectId);

            try
            {
                float result = float.Parse(ExecuteScalar(connection, cmd).ToString());

                return result;
            }
            catch (Exception ex)
            {
                return 0F;
            }



        }

        /// <summary>
        /// Возвращает общую сумму баланса по всем клиентам
        /// </summary>
        /// <param name="connection">Открытое подключение к БД. Работает только с MS SQL Server соединением</param>
        /// <returns></returns>
        public static float TotalBalance(IDbConnection connection)
        {
            if (!(connection is SqlConnection)) throw new NotSupportedException("Supported only SQL Server DB type");

            SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
            cmd.CommandText =
                            @"SELECT ISNULL(SUM(Qtty*Mode),0) 
                            FROM payments, Partners 
                            WHERE 
	                            Payments.PartnerID = Partners.ID 
	                            AND [OperType]=36";

            try
            {
                return float.Parse(ExecuteScalar(connection, cmd).ToString());
            }
            catch (Exception ex)
            {
                return 0f;
            }
        }

        ///<summary>
        /// Внесение денег в кассу
        ///</summary>
        ///<param name="connection">Активное соединение</param>
        ///<param name="money">Сумма</param>
        ///<param name="userId">Идентификатор пользователя отимени которого выполняется операция</param>
        ///<param name="objectId">Идентификатор объекта на котором выполняется операция</param>
        ///<returns>Отметка об удаче операции</returns>
        ///<exception cref="NotImplementedException"></exception>
        public static bool Deposit(IDbConnection connection, float money, int userId = 1, int objectId = 1)
        {
            if (!(connection is SqlConnection))
                throw new NotSupportedException("Only supported DB type is SQL");


            SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
            cmd.CommandText =
                @"INSERT INTO [dbo].[Operations]
                                   ([OperType]
                                   ,[Acct]
                                   ,[GoodID]
                                   ,[PartnerID]
                                   ,[ObjectID]
                                   ,[OperatorID]
                                   ,[Qtty]
                                   ,[Sign]
                                   ,[PriceIn]
                                   ,[PriceOut]
                                   ,[VATIn]
                                   ,[VATOut]
                                   ,[Discount]
                                   ,[CurrencyID]
                                   ,[CurrencyRate]
                                   ,[Date]
                                   ,[Lot]
                                   ,[LotID]
                                   ,[Note]
                                   ,[SrcDocID]
                                   ,[UserID]
                                   ,[UserRealTime])
                             VALUES
                                   (@operType
                                   ,0
                                   ,0
                                   ,0
                                   ,@objectId
                                   ,@userId
                                   ,@qty
                                   ,1
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,GETDATE()
                                   ,0
                                   ,0
                                   ,''
                                   ,0
                                   ,@userId
                                   ,GETDATE())";

            cmd.Parameters.Add("@operType", SqlDbType.BigInt);
            cmd.Parameters["@operType"].Value = 198;

            cmd.Parameters.AddWithValue("@objectId", objectId);
            cmd.Parameters.AddWithValue("@userId", userId);
            cmd.Parameters.AddWithValue("@qty", money);

            try
            {
                cmd.ExecuteNonQuery();

                return true;
            }
            catch
            {
                return false;
            }
        }

        ///<summary>
        /// Внесение денег в кассу
        ///</summary>
        ///<param name="connection">Активное соединение</param>
        ///<param name="qtty"></param>
        ///<param name="userId">Идентификатор пользователя отимени которого выполняется операция</param>
        ///<param name="partnerId"></param>
        ///<param name="objectId">Идентификатор объекта на котором выполняется операция</param>
        ///<param name="price"></param>
        ///<param name="goodId"></param>
        ///<returns>Отметка об удаче операции</returns>
        ///<exception cref="NotImplementedException"></exception>
        public static bool AddBuyOperation(IDbConnection connection, int goodId, int partnerId, int objectId, float qtty, int userId)
        {
            LogWriter log = new LogWriter("Выполнение операций над данными");

            if (!(connection is SqlConnection))
                throw new NotSupportedException("Only supported DB type is SQL");

            var transaction = ((SqlConnection)connection).BeginTransaction();

            try
            {

                SqlCommand cmd = ((SqlConnection)connection).CreateCommand();

                cmd.Transaction = transaction;

                cmd.CommandText =
                    @"INSERT INTO [dbo].[Operations]
                                   ([OperType]
                                   ,[Acct]
                                   ,[GoodID]
                                   ,[PartnerID]
                                   ,[ObjectID]
                                   ,[OperatorID]
                                   ,[Qtty]
                                   ,[Sign]
                                   ,[PriceIn]
                                   ,[PriceOut]
                                   ,[VATIn]
                                   ,[VATOut]
                                   ,[Discount]
                                   ,[CurrencyID]
                                   ,[CurrencyRate]
                                   ,[Date]
                                   ,[Lot]
                                   ,[LotID]
                                   ,[Note]
                                   ,[SrcDocID]
                                   ,[UserID]
                                   ,[UserRealTime])
                             VALUES
                                   (@operType
                                   ,(SELECT ISNULL(MAX(O2.Acct),0) + 1 FROM [dbo].[Operations] as O2 WHERE O2.OperType = @operType)
                                   ,@goodId
                                   ,@partnerId
                                   ,@objectId
                                   ,@userId
                                   ,@qty
                                   ,-1
                                   ,0
                                   ,(SELECT PriceOut2 FROM Goods WHERE ID = @goodId)
                                   ,0
                                   ,0
                                   ,0
                                   ,1
                                   ,1
                                   ,DATEADD(dd, 0, DATEDIFF(dd, 0, GETDATE()))
                                   ,0
                                   ,0
                                   ,''
                                   ,0
                                   ,@userId
                                   ,GETDATE())";

                cmd.Parameters.AddWithValue("@operType", 2);
                cmd.Parameters.AddWithValue("@goodId", goodId);
                cmd.Parameters.AddWithValue("@partnerId", partnerId);
                cmd.Parameters.AddWithValue("@objectId", objectId);
                cmd.Parameters.AddWithValue("@userId", userId);
                cmd.Parameters.AddWithValue("@qty", qtty);

                cmd.ExecuteNonQuery();

                cmd.CommandText =
                    @"INSERT INTO [Payments]
                               ([Acct]
                               ,[OperType]
                               ,[PartnerID]
                               ,[Qtty]
                               ,[Mode]
                               ,[Sign]
                               ,[Date]
                               ,[UserID]
                               ,[ObjectID]
                               ,[UserRealTime]
                               ,[Type]
                               ,[TransactionNumber]
                               ,[EndDate])
                         VALUES
                               (
                                (SELECT ISNULL(MAX(Acct)+1, 1) FROM [Payments] WHERE OperType = @operType)
                               ,@operType
                               ,@partnerId
                               ,(SELECT PriceOut2 FROM Goods WHERE ID = @goodId)*@qty
                               ,@mode
                               ,1
                               ,DATEADD(dd, 0, DATEDIFF(dd, 0, GETDATE()))
                               ,@userId
                               ,@objectId
                               ,GETDATE()
                               ,1
                               ,''
                               ,DATEADD(dd, 0, DATEDIFF(dd, 0, GETDATE()))
                                )";

                cmd.Parameters.AddWithValue("@mode", -1);

                cmd.ExecuteNonQuery();

                cmd.Parameters.RemoveAt("@mode");
                cmd.Parameters.AddWithValue("@mode", 1);

                cmd.ExecuteNonQuery();

                cmd.Parameters.RemoveAt("@operType");
                cmd.Parameters.AddWithValue("@operType", 36);
                cmd.Parameters.RemoveAt("@mode");
                cmd.Parameters.AddWithValue("@mode", -1);

                cmd.ExecuteNonQuery();

                cmd.CommandText = @"
                                INSERT INTO [CashBook]
                                           ([Date]
                                           ,[Desc]
                                           ,[OperType]
                                           ,[Sign]
                                           ,[Profit]
                                           ,[UserID]
                                           ,[UserRealtime]
                                           ,[ObjectID])
                                VALUES
                                    (DATEADD(dd, 0, DATEDIFF(dd, 0, GETDATE()))
                                    ,'Продажа #0000000'
                                    ,@cashBookType
                                    ,1
                                    ,(SELECT PriceOut2 FROM Goods WHERE ID = @goodId)*@qty
                                    ,@userId
                                    ,GETDATE()
                                    ,@objectId)";

                cmd.Parameters.AddWithValue("@cashBookType", 8);

                cmd.ExecuteNonQuery();

                cmd.Parameters.RemoveAt("@cashBookType");
                cmd.Parameters.AddWithValue("@cashBookType", 10);

                cmd.Parameters.RemoveAt("@qty");
                cmd.Parameters.AddWithValue("@qty", -qtty);

                cmd.ExecuteNonQuery();

                transaction.Commit();

                return true;
            }
            catch (Exception ex)
            {
                log.Write(String.Format("Произошла ошибка при продажи товара клиенту. \r\n {0}", ex.Message), EventType.FatalError);

                transaction.Rollback();
                return false;
            }
        }

        /// <summary>
        /// Инкассация денег
        /// </summary>
        /// <param name="connection">Активное соединение</param>
        ///<param name="money">Сумма</param>
        ///<param name="userId">Идентификатор пользователя отимени которого выполняется операция</param>
        ///<param name="objectId">Идентификатор объекта на котором выполняется операция</param>
        /// <returns>Отметка об удаче операции</returns>
        public static bool Encashment(IDbConnection connection, float money, int userId = 1, int objectId = 1)
        {
            if (!(connection is SqlConnection))
                throw new NotSupportedException("Only supported DB type is SQL");


            SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
            cmd.CommandText =
                @"INSERT INTO [dbo].[Operations]
                                   ([OperType]
                                   ,[Acct]
                                   ,[GoodID]
                                   ,[PartnerID]
                                   ,[ObjectID]
                                   ,[OperatorID]
                                   ,[Qtty]
                                   ,[Sign]
                                   ,[PriceIn]
                                   ,[PriceOut]
                                   ,[VATIn]
                                   ,[VATOut]
                                   ,[Discount]
                                   ,[CurrencyID]
                                   ,[CurrencyRate]
                                   ,[Date]
                                   ,[Lot]
                                   ,[LotID]
                                   ,[Note]
                                   ,[SrcDocID]
                                   ,[UserID]
                                   ,[UserRealTime])
                             VALUES
                                   (@operType
                                   ,0
                                   ,0
                                   ,0
                                   ,@objectId
                                   ,@userId
                                   ,@qty
                                   ,-1
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,GETDATE()
                                   ,0
                                   ,0
                                   ,''
                                   ,0
                                   ,@userId
                                   ,GETDATE())";

            cmd.Parameters.Add("@operType", SqlDbType.BigInt);
            cmd.Parameters["@operType"].Value = 199;

            cmd.Parameters.AddWithValue("@objectId", objectId);
            cmd.Parameters.AddWithValue("@userId", userId);
            cmd.Parameters.AddWithValue("@qty", money);

            try
            {
                cmd.ExecuteNonQuery();

                return true;
            }
            catch
            {
                return false;
            }
        }

        /// <summary>
        /// Устанавливает флаг активности для заданного партнера
        /// </summary>
        /// <param name="connection">активное соединение с БД</param>
        /// <param name="partner">партнер</param>
        /// <param name="flag">true- активироваться, false - деактивировать</param>
        /// <param name="objectId"></param>
        /// <param name="userId"></param>
        /// <returns></returns>
        public static bool SetActivityFlag(IDbConnection connection, Partner partner, bool flag, int objectId = 1, int userId = 1)
        {
            if (connection is SqlConnection && partner != null)
            {
                SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                cmd.CommandText =
                    @"INSERT INTO [dbo].[Operations]
                                   ([OperType]
                                   ,[Acct]
                                   ,[GoodID]
                                   ,[PartnerID]
                                   ,[ObjectID]
                                   ,[OperatorID]
                                   ,[Qtty]
                                   ,[Sign]
                                   ,[PriceIn]
                                   ,[PriceOut]
                                   ,[VATIn]
                                   ,[VATOut]
                                   ,[Discount]
                                   ,[CurrencyID]
                                   ,[CurrencyRate]
                                   ,[Date]
                                   ,[Lot]
                                   ,[LotID]
                                   ,[Note]
                                   ,[SrcDocID]
                                   ,[UserID]
                                   ,[UserRealTime])
                             VALUES
                                   (@operType
                                   ,0
                                   ,0
                                   ,@partner
                                   ,@objectId
                                   ,@userId
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,0
                                   ,GETDATE()
                                   ,0
                                   ,0
                                   ,''
                                   ,0
                                   ,@userId
                                   ,GETDATE())";

                cmd.Parameters.Add("@operType", SqlDbType.BigInt);
                cmd.Parameters["@operType"].Value = flag ? 99 : 98;

                cmd.Parameters.Add("@partner", SqlDbType.BigInt);
                cmd.Parameters["@partner"].Value = partner.ID;

                cmd.Parameters.AddWithValue("@objectId", objectId);
                cmd.Parameters.AddWithValue("@userId", userId);

                try
                {
                    cmd.ExecuteNonQuery();

                    return true;
                }
                catch
                {
                    return false;
                }
            }

            return false;
        }

        /// <summary>
        /// Возвращает общую сумму вохвращенных денег
        /// </summary>
        /// <param name="connection">Открытое подключение к БД. Работает только с MS SQL Server соединением</param>
        /// <param name="objectId">Идентификатор объекта</param>
        /// <param name="startDate">Время начала подсчета баланса</param>
        /// <returns></returns>
        public static float TotalReturns(IDbConnection connection, int objectId, DateTime startDate)
        {
            if (connection is SqlConnection)
            {
                SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
                cmd.CommandText = String.Format(@"SELECT 
	                                                ABS(ISNULL(SUM(P.Qtty),0))
                                                FROM 
	                                                Payments AS P
                                                WHERE
	                                                P.UserRealTime > @sDate
	                                                AND P.OperType = 36 AND P.Mode = 1
	                                                AND P.Qtty < 0 AND ObjectID = {0}", objectId);

                cmd.Parameters.Add("sDate", SqlDbType.DateTime);
                cmd.Parameters["sDate"].Value = startDate;

                float result = 0;
                try
                {
                    result = float.Parse(ExecuteScalar(connection, cmd).ToString());
                }
                catch (Exception ex)
                { }

                return result;
            }
            else
                return 0F;
        }

        ///<summary>
        /// Write to ApplicationLog
        ///</summary>
        ///<param name="cnc">Alive connection</param>
        ///<param name="message">Log message</param>
        ///<param name="userId">User id</param>
        ///<param name="source">Log source</param>
        ///<returns>true on success operation, else false</returns>
        ///<exception cref="NotSupportedException"></exception>
        public static bool Log(IDbConnection cnc, string message, int userId, string source)
        {
            if (!(cnc is SqlConnection))
                throw new NotSupportedException("Not supported DB type, only SQl server");

            SqlCommand cmd = ((SqlConnection)cnc).CreateCommand();
            cmd.CommandText = @"INSERT INTO 
                                        ApplicationLog (Message, UserID, UserRealtime, MessageSource)
                                    VALUES 
                                            (@msg, @usrId, GETDATE(), @source)";

            cmd.Parameters.AddWithValue("@msg", message);
            cmd.Parameters.AddWithValue("@usrId", userId);
            cmd.Parameters.AddWithValue("@source", source);

            try
            {
                return Convert.ToBoolean(cmd.ExecuteNonQuery());
            }
            catch
            {
                return false;
            }

        }

        ///<summary>
        ///</summary>
        ///<param name="cnc">Активное подключение</param>
        ///<param name="cardNumber">Номер карты партнера</param>
        ///<param name="days">Количество дней, за которые требуется статистика</param>
        ///<returns></returns>
        public static IEnumerable<Purchase> GetPartnerPurchases(IDbConnection cnc, string cardNumber)
        {
            if (!(cnc is SqlConnection))
                throw new NotSupportedException("Only supported conncetion type is SQL Server");


            List<Purchase> result = new List<Purchase>();

            SqlCommand cmd = ((SqlConnection)cnc).CreateCommand();

            cmd.CommandText = @"SELECT ID FROM Partners WHERE CardNumber = @cardNumber";

            cmd.Parameters.AddWithValue("@cardNumber", cardNumber ?? String.Empty);

            object execResult = cmd.ExecuteScalar();

            if (!(execResult is int))
                return result;

            int partnerId = (int) execResult;

            cmd.CommandText = @"SELECT 
	                            ID = O.ID,
	                            Acct = O.Acct,
	                            Good = G.Name,
                                GoodID = G.ID,
                                CardNumber = P.CardNumber,
	                            Object = Obj.Name,
                                ObjectID = Obj.ID,
	                            Operator = U.Name,
	                            Qtty = O.Qtty,
	                            Price = O.PriceOut,
	                            Time = O.UserRealTime
                            FROM 
	                            Operations as O
	                            JOIN Goods as G on G.ID = O.GoodID
	                            JOIN Objects as Obj on Obj.ID = O.ObjectID
	                            JOIN Users as U on U.ID = O.OperatorID
                                JOIN Partners as P on P.ID = O.PartnerID
                              WHERE 
	                            O.OperType = 2 AND
	                            O.PartnerID = @partnerId AND
                                O.UserRealTime > (SELECT ISNULL(MAX(UserRealTime), 0) FROM Operations WHERE OperType = 98 AND PartnerID = @partnerId)";

            cmd.Parameters.AddWithValue("@partnerId", partnerId);


            DataTable dt = GetDataSet(cnc, cmd).Tables[0];

            foreach (DataRow row in dt.Rows)
            {
                Purchase purchase = new Purchase
                                        {
                                            ID = (int)row["ID"],
                                            Acct = (int)row["Acct"],
                                            Good = (string)row["Good"],
                                            GoodID = (int)row["GoodID"],
                                            Object = (string)row["Object"],
                                            ObjectID = (int)row["ObjectID"],
                                            Operator = (string)row["Operator"],
                                            Qtty = (double)row["Qtty"],
                                            CardNumber = (string)row["CardNumber"],
                                            Price = (double)row["Price"],
                                            Time = (DateTime)row["Time"]
                                        };
                result.Add(purchase);
            }


            return result;
        }

        ///<summary>
        /// Отменяет покупку заданного товара, фактически делает возврат
        ///</summary>
        ///<param name="cnc">Активное подключение</param>
        ///<param name="acct">Номер документа</param>
        ///<param name="qty">Возвращаемое количество</param>
        ///<param name="goodId">Идентификатор купленного товара</param>
        ///<param name="objectId">Идентификатор объекта на котором совершена покупка</param>
        ///<returns></returns>
        public static bool CancelPurchase(SqlConnection cnc, string cardNumber, int acct, double qty, int goodId, int objectId)
        {
            LogWriter log = new LogWriter("Операция возврата");

            SqlTransaction cancelPurchaseTransaction = cnc.BeginTransaction();

            try
            {
                SqlCommand cmd = cnc.CreateCommand();
                cmd.Transaction = cancelPurchaseTransaction;

                cmd.CommandText = @"UPDATE Operations SET Qtty = Qtty - @qty WHERE OperType = 2 AND Acct = @acct AND GoodID = @good AND ObjectID = @objectID";

                cmd.Parameters.AddWithValue("@qty", qty);
                cmd.Parameters.AddWithValue("@acct", acct);
                cmd.Parameters.AddWithValue("@good", goodId);
                cmd.Parameters.AddWithValue("@objectID", objectId);

                int affectedOperationsRows = cmd.ExecuteNonQuery();

                if (affectedOperationsRows == 0)
                {
                    string msg =
                        String.Format("Операция возврата отменена! Для номера документа {0} идентификатора склада {1} и товара {2} не найдена операция в БД.", acct, objectId, goodId);
                    log.Write(msg, EventType.Caution);
                    throw new OperationCanceledException(msg);
                }

                cmd.CommandText =
                    @"SELECT PriceOut FROM Operations WHERE OperType = 2 AND Acct = @acct AND GoodID = @good AND ObjectID = @objectID";

                double price = (double) cmd.ExecuteScalar();

                cmd.CommandText = @"SELECT Qtty FROM Operations WHERE OperType = 2 AND Acct = @acct AND GoodID = @good AND ObjectID = @objectID";

                double operationQtty = (double)cmd.ExecuteScalar();

                if (operationQtty == 0)
                {
                    cmd.CommandText = @"DELETE FROM Operations WHERE OperType = 2 AND Acct = @acct AND GoodID = @good AND ObjectID = @objectID;";

                    int deletedOperationsCount = cmd.ExecuteNonQuery();

                    if (deletedOperationsCount > 1)
                    {
                        string msg =
                            String.Format(
                                "Операция возврата отменена! Найдено несколько строк для номера документа {0} идентификатора склада {1} и товара {2} в таблице операций",
                                acct, objectId, goodId);
                        log.Write(msg, EventType.Caution);
                        throw new OperationCanceledException(msg);
                    }
                }

                cmd.CommandText =
                    @"UPDATE Payments SET Qtty = Qtty - @qty*@price WHERE OperType IN (2, 36) AND Acct = @acct AND ObjectID = @objectID;";

                cmd.Parameters.AddWithValue("@price", price);

                int paymentAffectedRows = cmd.ExecuteNonQuery();

                if (paymentAffectedRows != 3)
                {
                    string msg = String.Format("Операция возврата отменена! Количество строк в таблице платежей для номера документа {0} идентификатора склада {1} и товара {2} не равно 3.", acct, objectId, goodId);
                    log.Write(msg, EventType.Caution);
                    throw new OperationCanceledException(msg);
                }

                cmd.CommandText = @"UPDATE Store SET Qtty = Qtty + @qty WHERE ObjectID = @objectID AND GoodID = @good";

                int storeAffectedRows = cmd.ExecuteNonQuery();

                if (storeAffectedRows != 1)
                {
                    string msg = String.Format("Операция возврата отменена! Количество строк в таблице Store для идентификатора склада {0} и товара {1} не равно 1.", objectId, goodId);
                    log.Write(msg, EventType.Caution);
                    throw new OperationCanceledException(msg); 
                }

                cmd.CommandText = @"INSERT INTO CashBook
                                        ([Date]
                                       ,[Desc]
                                       ,[OperType]
                                       ,[Sign]
                                       ,[Profit]
                                       ,[UserID]
                                       ,[UserRealtime]
                                       ,[ObjectID])
                                 VALUES
                                       (DATEADD(dd, 0, DATEDIFF(dd, 0, GETDATE()))
                                       ,@desc
                                       ,1
                                       ,-1
                                       ,@qty*@price
                                       ,1
                                       ,GETDATE()
                                       ,@objectID)";

                cmd.Parameters.AddWithValue("@desc", String.Format("Возврат  от покупателя с номером карты {0} товара {1}", cardNumber, goodId));

                cmd.ExecuteNonQuery();

                cancelPurchaseTransaction.Commit();
                return true;
            }
            catch (Exception ex)
            {
                log.Write(String.Format("При выполнении возврата товара {0} партнеру {1} на складе {2} возникла ошибка {3}", goodId, cardNumber, objectId, ex.Message));
                cancelPurchaseTransaction.Rollback();
                return false;
            }

        }

        /// <summary>
        /// Возвращает количество товара на заданном складе
        /// </summary>
        /// <param name="cnc">Активное подключение</param>
        /// <param name="goodId">Идентификатор товара</param>
        /// <param name="objectID">Идентификатор объекта</param>
        /// <returns></returns>
        public static double GetGoodQty(IDbConnection cnc, int goodId, int objectID)
        {
            if (!(cnc is SqlConnection))
                throw new NotSupportedException("Only supported conncetion type is SQL Server");

            var cmd = new SqlCommand
                {
                    CommandText = @"select top 1 Qtty from store where GoodID = @goodId and ObjectID = @objectId"
                };

            cmd.Parameters.AddWithValue("@goodId", goodId);

            cmd.Parameters.AddWithValue("@objectId", objectID);

            var result = ExecuteScalar(cnc, cmd);

            return (double)(result ?? 0m);
        }

        /// <summary>
        /// Возвращает список всех единиц измерения
        /// </summary>
        /// <param name="cnc">Активное соединени с БД</param>
        /// <returns></returns>
        public static IEnumerable<string> GetMeasures(IDbConnection cnc)
        {
            string query = @"with cte as 
                            (Select Measure1, COUNT(*) as cnt  From Goods Group by Measure1)
                            select * from cte order by cnt desc
	                            ";

            var ds = GetDataSet(cnc, query);

            var result = new List<string>();

            if (ds.Tables.Count == 0)
                return result;

            foreach (DataRow row in ds.Tables[0].Rows)
                result.Add(row["Measure1"].ToString());

            return result;
        }


        /// <summary>
        /// Создает заказ поставщику
        /// </summary>
        /// <param name="connection"></param>
        /// <param name="operationItems"></param>
        /// <returns></returns>
//        public static bool CreateSupplierOrder(IDbConnection connection, IEnumerable<OperationItem> operationItems)
//        {
//            SqlCommand cmd = ((SqlConnection)connection).CreateCommand();
//            cmd.CommandText =
//                @"INSERT INTO [dbo].[Operations]
//                                   ([OperType]
//                                   ,[Acct]
//                                   ,[GoodID]
//                                   ,[PartnerID]
//                                   ,[ObjectID]
//                                   ,[OperatorID]
//                                   ,[Qtty]
//                                   ,[Sign]
//                                   ,[PriceIn]
//                                   ,[PriceOut]
//                                   ,[VATIn]
//                                   ,[VATOut]
//                                   ,[Discount]
//                                   ,[CurrencyID]
//                                   ,[CurrencyRate]
//                                   ,[Date]
//                                   ,[Lot]
//                                   ,[LotID]
//                                   ,[Note]
//                                   ,[SrcDocID]
//                                   ,[UserID]
//                                   ,[UserRealTime])
//                             VALUES
//                                   (@operType
//                                   ,0
//                                   ,0
//                                   ,0
//                                   ,@objectId
//                                   ,@userId
//                                   ,@qty
//                                   ,1
//                                   ,0
//                                   ,0
//                                   ,0
//                                   ,0
//                                   ,0
//                                   ,0
//                                   ,0
//                                   ,GETDATE()
//                                   ,0
//                                   ,0
//                                   ,''
//                                   ,0
//                                   ,@userId
//                                   ,GETDATE())";

//            cmd.Parameters.AddWithValue("@operType", 12);

//            cmd.Parameters.AddWithValue("@objectId", objectId);
//            cmd.Parameters.AddWithValue("@userId", userId);
//            cmd.Parameters.AddWithValue("@qty", money);
//        }

    }
}

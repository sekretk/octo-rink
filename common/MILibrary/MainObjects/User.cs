using System;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{

    /// <summary>
    /// Информация о пользователе БД.
    /// </summary>
    public class User
    {
        /// <summary>
        /// Идентификатор пользователя
        /// </summary>
        public int ID;

        /// <summary>
        /// Идентификатор группы пользователя
        /// </summary>
        public int GroupID;

        /// <summary>
        /// Код пользователя
        /// </summary>
        public string Code;

        /// <summary>
        /// Наименование пользователя
        /// </summary>
        public string Name;

        /// <summary>
        /// Наименование пользователя для печати
        /// </summary>
        public string Name2;

        /// <summary>
        /// Зашифрованный пароль, для дешифрации примените класс Crypto
        /// </summary>
        public string Password;

        /// <summary>
        /// Номер карты пользователя
        /// </summary>
        public string CardNumber;

        /// <summary>
        /// Уровень доступа пользователя
        /// </summary>
        public int UserLevel;

        /// <summary>
        /// Флаг частого использования
        /// </summary>
        public int IsVeryUsed;

        /// <summary>
        /// Флаг логического удаления
        /// </summary>
        public int Deleted;

        /// <summary>
        /// Суммарная информация о пользователе
        /// </summary>
        public string UserInfo
        {
            get
            {
                return String.Format("ID: {0}; GroupID: {1}; Code: {2}; Name: {3}; Name2: {4}; Password: {5}; CardNumber: {6}; UserLevel: {7}; " +
                    "IsVeryUsed: {8}; Deleted: {9}", ID, GroupID, Code, Name, Name2, Password, CardNumber, UserLevel, IsVeryUsed, Deleted);
            }
        }

        /// <summary>
        /// Возвращает имя пользователя. Поле Name.
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return Name;
        }

        /// <summary>
        /// Сравнивает объекты. Допустимо равентсво только в случае сравнения с дргуим пользвателем. Равенство достигается при совпадении ID и Name
        /// </summary>
        /// <param name="obj">Объект, с которым нужно сравнить</param>
        public override bool Equals(object obj)
        {
            if (obj is User)
                return (ID == ((User)obj).ID) && (Name == ((User)obj).Name);
            else
                return false;
        }

        public override int GetHashCode()
        {
            return ID;
        }
    }

}

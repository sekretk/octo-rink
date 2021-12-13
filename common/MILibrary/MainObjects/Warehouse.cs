using System;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{

    /// <summary>
    /// Информация об объекте
    /// </summary>
    public class Warehouse
    {
        /// <summary>
        /// Идентификатор объекта
        /// </summary>
        public int ID;

        /// <summary>
        /// Идентификатор группы пользователей
        /// </summary>
        public int GroupID;

        /// <summary>
        /// Код объекта
        /// </summary>
        public string Code;

        /// <summary>
        /// Наименование объекта
        /// </summary>
        public string Name;

        /// <summary>
        /// Наименование объекта для печати
        /// </summary>
        public string Name2;

        /// <summary>
        /// Ценования группа объекта
        /// </summary>
        public int PriceGroup;

        /// <summary>
        /// Флаг для частого использования
        /// </summary>
        public int IsVeryUsed;

        /// <summary>
        /// Флаг логического удаления
        /// </summary>
        public int Deleted;

        /// <summary>
        /// Возвращает суммарную информацию об объекте
        /// </summary>
        public string ObjectInfo
        {
            get
            {
                return String.Format("ID: {0}; GroupID: {1}; Code: {2}; Name: {3}; Name2: {4}; PriceGroup: {5}; IsVeryUsed: {6}; Deleted: {7}", ID.ToString(),
                                      GroupID.ToString(), Code, Name, Name2, PriceGroup.ToString(), IsVeryUsed.ToString(), Deleted.ToString());
            }
        }

        /// <summary>
        /// Возвращает наименование объекта. Поле Name.
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return Name;
        }

        public override int GetHashCode()
        {
            return ID;
        }

        /// <summary>
        /// Сравнивает текущий экземпляр с параметром. Равентсво может достигаться только с аргуметом того же типа и равными с исходным полями ID и Name.
        /// </summary>
        /// <param name="obj">Объект с которым будет производиться сравнение</param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            if (obj is Warehouse)
                return (ID == ((Warehouse)obj).ID) && (Name == ((Warehouse)obj).Name);
            else
                return false;
        }
    }

}

using System;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{
    /// <summary>
    /// Объекты БД: Товар (Good), Пользователь (User), Партнер (Partner), Объект (Object)
    /// </summary>
    public enum MIObjectType : byte
    {
        /// <summary>
        /// Товар
        /// </summary>
        Good = 0,

        /// <summary>
        /// Пользователь
        /// </summary>
        User = 1,

        /// <summary>
        /// Партнер
        /// </summary>
        Partner = 2,

        /// <summary>
        /// Объект
        /// </summary>
        Object = 3
    }

    /// <summary>
    /// Экземпляр класса представляет собой полную информацию о группе разных типов (Пользователи, Партнеры, Товары, Объекты)
    /// </summary>
    public class Group
    {
        /// <summary>
        /// Идентификатор группы
        /// </summary>
        public int ID;

        /// <summary>
        /// Код группы
        /// </summary>
        public string Code;

        /// <summary>
        /// Наименование группы
        /// </summary>
        public string Name;

        /// <summary>
        /// Тип группы
        /// </summary>
        public MIObjectType Type;

        /// <summary>
        /// Имя группы
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return Name;
        }

        /// <summary>
        /// Сравнивает объекты. Равенство достигается только на объектах одного типа с равным ID
        /// </summary>
        /// <param name="obj">Объект для сравнения</param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            if (!(obj is Group))
                return false;
            return (obj as Group).ID == ID && (obj as Group).Type == Type;
        }

        /// <summary>
        /// Код экземпляра класса
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            return ID;
        }
    }
}

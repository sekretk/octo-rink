using System;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{

    /// <summary>
    /// Информация о товаре или услуге
    /// </summary>
    public class Good
    {
        /// <summary>
        /// Идентификатор товара
        /// </summary>
        public int ID;

        /// <summary>
        /// Идентификатор группы товара
        /// </summary>
        public int GroupID;

        /// <summary>
        /// Код товара
        /// </summary>
        public string Code;

        /// <summary>
        /// Штрих-код 1 товара
        /// </summary>
        public string BarCode1;

        /// <summary>
        /// Штрих-код 2 товара
        /// </summary>
        public string BarCode2;

        /// <summary>
        /// Штрих-код 3 товара
        /// </summary>
        public string BarCode3;

        /// <summary>
        /// Каталожный номер 1 товара
        /// </summary>
        public string Catalog1;

        /// <summary>
        /// Каталожный номер 2 товара
        /// </summary>
        public string Catalog2;

        /// <summary>
        /// Каталожный номер 3 товара
        /// </summary>
        public string Catalog3;

        /// <summary>
        /// Наименование товара
        /// </summary>
        public string Name;

        /// <summary>
        /// Наименование товара для печати
        /// </summary>
        public string Name2;

        /// <summary>
        /// Единица измерения товара
        /// </summary>
        public string Measure;

        /// <summary>
        /// Дополнительная единица измерения товара
        /// </summary>
        public string Measure2;

        /// <summary>
        /// Отношение дополнительной единицы к основной
        /// </summary>
        public double Ratio;

        /// <summary>
        /// Закупочная цена товара
        /// </summary>
        public double PriceIn;

        /// <summary>
        /// Оптовая цена товара
        /// </summary>
        public double PriceOut1;

        /// <summary>
        /// Розничная цена товара
        /// </summary>
        public double PriceOut2;

        /// <summary>
        /// Цена товара для ценовой группы 1
        /// </summary>
        public double PriceOut3;

        /// <summary>
        /// Цена товара для ценовой группы 2
        /// </summary>
        public double PriceOut4;

        /// <summary>
        /// Цена товара для ценовой группы 3
        /// </summary>
        public double PriceOut5;

        /// <summary>
        /// Цена товара для ценовой группы 4
        /// </summary>
        public double PriceOut6;

        /// <summary>
        /// Цена товара для ценовой группы 5
        /// </summary>
        public double PriceOut7;

        /// <summary>
        /// Цена товара для ценовой группы 6
        /// </summary>
        public double PriceOut8;

        /// <summary>
        /// Цена товара для ценовой группы 7
        /// </summary>
        public double PriceOut9;

        /// <summary>
        /// Цена товара для ценовой группы 8
        /// </summary>
        public double PriceOut10;

        /// <summary>
        /// Минимальное количество товара
        /// </summary>
        public double MinQtty;

        /// <summary>
        /// Номинальное количество товара
        /// </summary>
        public double NormalQtty;

        /// <summary>
        /// Описание товара
        /// </summary>
        public string Description;

        /// <summary>
        /// Тип товара (0 - стандарт, 1 - с фиксированной ценой)
        /// </summary>
        public int Type;

        /// <summary>
        /// Флаг рецептуры
        /// </summary>
        public int IsRecipe;

        /// <summary>
        /// Налоговая группа
        /// </summary>
        public int TaxGroup;

        /// <summary>
        /// Флаг частого использования
        /// </summary>
        public int IsVeryUsed;

        /// <summary>
        /// Флаг логического удаления
        /// </summary>
        public int Deleted;

        /// <summary>
        /// Возвращает полную инфрмацию о товаре одной строкой
        /// </summary>
        public string GoodInfo
        {
            get
            {
                return String.Format("ID: {0}; GroupID: {1}; Code: {2}; BarCode1: {3}; BarCode2: {4}; BarCode3: {5}; Catalog1: {6}; " +
                        "Catalog2: {7}; Catalog3: {8}; Name: {9}; Name2: {10}; Measure1: {11}; Measure2: {12}; Ratio: {13}; PriceIn: {14}; " +
                        "PriceOut1: {15}; PriceOut2: {16}; PriceOut3: {17}; PriceOut4: {18}; PriceOut5: {19}; PriceOut6: {20}; PriceOut7: {21}; " +
                        "PriceOut8: {22}; PriceOut9: {23}; PriceOut10: {24}; MinQtty: {25}; NormalQtty: {26}; Description: {27}; Type: {28}; " +
                        "IsRecipe: {29}; TaxGroup: {30}; IsVeryUsed: {31}; Deleted: {32}", ID, GroupID, Code, BarCode1, BarCode2, BarCode3, Catalog1, Catalog2,
                        Catalog3, Name, Name2, Measure, Measure2, Ratio, PriceIn, PriceOut1, PriceOut2, PriceOut3, PriceOut4, PriceOut5, PriceOut6, PriceOut7, PriceOut8,
                        PriceOut9, PriceOut10, MinQtty, NormalQtty, Description, Type, IsRecipe, TaxGroup, IsVeryUsed, Deleted);
            }
        }
    }

   
}

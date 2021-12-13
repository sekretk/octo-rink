using System;
using System.Collections.Generic;
using System.Text;

namespace MILibrary.MainObjects
{
    ///<summary>
    /// Операция продажи
    ///</summary>
    public class Purchase
    {
        public int ID { get; set; }

        public int Acct { get; set; }

        public string Good { get; set; }

        public int GoodID { get; set; }

        public string Object { get; set; }

        public int ObjectID { get; set; }

        public string Operator { get; set; }

        public double Qtty { get; set; }

        public double Price { get; set; }

        public DateTime Time { get; set; }

        public string CardNumber { get; set; }
    }
}

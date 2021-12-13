using System;

namespace dbfirstplay
{
    public partial class Transformations
    {
        public int ID { get; set; }

        public int RootOperType { get; set; }

        public int RootAcct { get; set; }

        public int FromOperType { get; set; }

        public int FromAcct { get; set; }

        public int ToOperType { get; set; }

        public int ToAcct { get; set; }

        public int UserID { get; set; }

        public DateTime? UserRealTime { get; set; }
    }
}

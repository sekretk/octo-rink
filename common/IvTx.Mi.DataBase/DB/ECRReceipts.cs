using System;
using System.ComponentModel.DataAnnotations;

namespace dbfirstplay
{
    public partial class ECRReceipts
    {
        public int ID { get; set; }

        public int OperType { get; set; }

        public int Acct { get; set; }

        public int ReceiptID { get; set; }

        public DateTime? ReceiptDate { get; set; }

        public int ReceiptType { get; set; }

        [StringLength(255)]
        public string ECRID { get; set; }

        [StringLength(255)]
        public string Description { get; set; }

        public double? Total { get; set; }

        public int UserID { get; set; }

        public DateTime? UserRealTime { get; set; }
    }
}

using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    public partial class Payments
    {
        public int ID { get; set; }

        public int? Acct { get; set; }

        public int? OperType { get; set; }

        public int? PartnerID { get; set; }

        public double? Qtty { get; set; }

        public int? Mode { get; set; }

        public int? Sign { get; set; }

        [Column(TypeName = "smalldatetime")]
        public DateTime? Date { get; set; }

        public int? UserID { get; set; }

        public int? ObjectID { get; set; }

        public DateTime? UserRealTime { get; set; }

        public int? Type { get; set; }

        [StringLength(255)]
        public string TransactionNumber { get; set; }

        [Column(TypeName = "smalldatetime")]
        public DateTime? EndDate { get; set; }
    }
}

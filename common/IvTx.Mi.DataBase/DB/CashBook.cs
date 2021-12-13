using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    [Table("CashBook")]
    public partial class CashBook
    {
        public int ID { get; set; }

        [Column(TypeName = "smalldatetime")]
        public DateTime? Date { get; set; }

        [StringLength(255)]
        public string Desc { get; set; }

        public int? OperType { get; set; }

        public int? Sign { get; set; }

        public double? Profit { get; set; }

        public int? UserID { get; set; }

        public DateTime? UserRealtime { get; set; }

        public int? ObjectID { get; set; }
    }
}

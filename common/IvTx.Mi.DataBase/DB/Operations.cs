using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    public partial class Operations
    {
        public int ID { get; set; }

        public int? OperType { get; set; }

        public int? Acct { get; set; }

        public int? GoodID { get; set; }

        public int? PartnerID { get; set; }

        public int? ObjectID { get; set; }

        public int? OperatorID { get; set; }

        public double? Qtty { get; set; }

        public int? Sign { get; set; }

        public double? PriceIn { get; set; }

        public double? PriceOut { get; set; }

        public double? VATIn { get; set; }

        public double? VATOut { get; set; }

        public double? Discount { get; set; }

        public int? CurrencyID { get; set; }

        public double? CurrencyRate { get; set; }

        [Column(TypeName = "smalldatetime")]
        public DateTime? Date { get; set; }

        [StringLength(50)]
        public string Lot { get; set; }

        public int? LotID { get; set; }

        [StringLength(255)]
        public string Note { get; set; }

        public int? SrcDocID { get; set; }

        public int? UserID { get; set; }

        public DateTime? UserRealTime { get; set; }
    }
}

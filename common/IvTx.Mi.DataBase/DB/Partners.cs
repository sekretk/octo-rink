using System;
using System.ComponentModel.DataAnnotations;
using IvTx.Mi.DataBase.Enums;

namespace IvTx.Mi.DataBase.DB
{
    public partial class Partners
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Code { get; set; }

        [StringLength(255)]
        public string Company { get; set; }

        [StringLength(255)]
        public string Company2 { get; set; }

        [StringLength(255)]
        public string MOL { get; set; }

        [StringLength(255)]
        public string MOL2 { get; set; }

        [StringLength(255)]
        public string City { get; set; }

        [StringLength(255)]
        public string City2 { get; set; }

        [StringLength(255)]
        public string Address { get; set; }

        [StringLength(255)]
        public string Address2 { get; set; }

        [StringLength(255)]
        public string Phone { get; set; }

        [StringLength(255)]
        public string Phone2 { get; set; }

        [StringLength(255)]
        public string Fax { get; set; }

        [StringLength(255)]
        public string eMail { get; set; }

        [StringLength(255)]
        public string TaxNo { get; set; }

        [StringLength(255)]
        public string Bulstat { get; set; }

        [StringLength(255)]
        public string BankName { get; set; }

        [StringLength(255)]
        public string BankCode { get; set; }

        [StringLength(255)]
        public string BankAcct { get; set; }

        [StringLength(255)]
        public string BankVATName { get; set; }

        [StringLength(255)]
        public string BankVATCode { get; set; }

        [StringLength(255)]
        public string BankVATAcct { get; set; }

        public int? PriceGroup { get; set; }

        public double? Discount { get; set; }

        public short? Type { get; set; }

        public short? IsVeryUsed { get; set; }

        public int? UserID { get; set; }

        public int? GroupID { get; set; }

        public DateTime? UserRealTime { get; set; }

        public int? Deleted { get; set; }

        [StringLength(255)]
        public string CardNumber { get; set; }

        [StringLength(255)]
        public string Note1 { get; set; }

        [StringLength(255)]
        public string Note2 { get; set; }

        public int? PaymentDays { get; set; }

        public DateTime? GoodTill { get; set; }

        public SecurityType? SecurityType { get; set; }
    }
}

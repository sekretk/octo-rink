using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    [Table("Registration")]
    public partial class Registration
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Code { get; set; }

        [StringLength(255)]
        public string Company { get; set; }

        [StringLength(255)]
        public string MOL { get; set; }

        [StringLength(255)]
        public string City { get; set; }

        [StringLength(255)]
        public string Address { get; set; }

        [StringLength(255)]
        public string Phone { get; set; }

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
        public string BankVATAcct { get; set; }

        public int? UserID { get; set; }

        public DateTime? UserRealTime { get; set; }

        public int? IsDefault { get; set; }

        [StringLength(255)]
        public string Note1 { get; set; }

        [StringLength(255)]
        public string Note2 { get; set; }

        public int? Deleted { get; set; }
    }
}

using System.ComponentModel.DataAnnotations;

namespace IvTx.Mi.DataBase
{
    public partial class Goods
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Code { get; set; }

        [StringLength(255)]
        public string BarCode1 { get; set; }

        [StringLength(255)]
        public string BarCode2 { get; set; }

        [StringLength(255)]
        public string BarCode3 { get; set; }

        [StringLength(255)]
        public string Catalog1 { get; set; }

        [StringLength(255)]
        public string Catalog2 { get; set; }

        [StringLength(255)]
        public string Catalog3 { get; set; }

        [StringLength(255)]
        public string Name { get; set; }

        [StringLength(255)]
        public string Name2 { get; set; }

        [StringLength(255)]
        public string Measure1 { get; set; }

        [StringLength(255)]
        public string Measure2 { get; set; }

        public double? Ratio { get; set; }

        public double? PriceIn { get; set; }

        public double? PriceOut1 { get; set; }

        public double? PriceOut2 { get; set; }

        public double? PriceOut3 { get; set; }

        public double? PriceOut4 { get; set; }

        public double? PriceOut5 { get; set; }

        public double? PriceOut6 { get; set; }

        public double? PriceOut7 { get; set; }

        public double? PriceOut8 { get; set; }

        public double? PriceOut9 { get; set; }

        public double? PriceOut10 { get; set; }

        public double? MinQtty { get; set; }

        public double? NormalQtty { get; set; }

        [StringLength(255)]
        public string Description { get; set; }

        public int? Type { get; set; }

        public int? IsRecipe { get; set; }

        public int? TaxGroup { get; set; }

        public int? IsVeryUsed { get; set; }

        public int? GroupID { get; set; }

        public int? Deleted { get; set; }
    }
}

using System.ComponentModel.DataAnnotations;

namespace dbfirstplay
{
    public partial class Currencies
    {
        public int ID { get; set; }

        [StringLength(3)]
        public string Currency { get; set; }

        [StringLength(255)]
        public string Description { get; set; }

        public double? ExchangeRate { get; set; }

        public int? Deleted { get; set; }
    }
}

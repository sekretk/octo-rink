using System.ComponentModel.DataAnnotations;

namespace dbfirstplay
{
    public partial class PriceRules
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Name { get; set; }

        [StringLength(1000)]
        public string Formula { get; set; }

        public int? Enabled { get; set; }

        public int? Priority { get; set; }
    }
}

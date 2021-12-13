using System.ComponentModel.DataAnnotations;

namespace dbfirstplay
{
    public partial class Objects
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Code { get; set; }

        [StringLength(255)]
        public string Name { get; set; }

        [StringLength(255)]
        public string Name2 { get; set; }

        public int? PriceGroup { get; set; }

        public int? IsVeryUsed { get; set; }

        public int? GroupID { get; set; }

        public int? Deleted { get; set; }
    }
}

using System.ComponentModel.DataAnnotations;

namespace dbfirstplay
{
    public partial class GoodsGroups
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Name { get; set; }

        [StringLength(255)]
        public string Code { get; set; }
    }
}

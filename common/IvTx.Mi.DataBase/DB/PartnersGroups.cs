using System.ComponentModel.DataAnnotations;

namespace dbfirstplay
{
    public partial class PartnersGroups
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Code { get; set; }

        [StringLength(255)]
        public string Name { get; set; }
    }
}

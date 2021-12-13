using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    [Table("UsersSecurity")]
    public partial class UsersSecurity
    {
        public int ID { get; set; }

        public int? UserID { get; set; }

        [StringLength(100)]
        public string ControlName { get; set; }

        public int? State { get; set; }
    }
}

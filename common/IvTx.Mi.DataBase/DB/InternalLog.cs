using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    [Table("InternalLog")]
    public partial class InternalLog
    {
        public int ID { get; set; }

        [StringLength(3000)]
        public string Message { get; set; }
    }
}

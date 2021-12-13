using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace IvTx.Mi.DataBase.DB
{
    [Table("NextAcct")]
    public partial class NextAcct
    {
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public int ID { get; set; }

        [Key]
        [Column("NextAcct")]
        [StringLength(50)]
        public string NextAcct1 { get; set; }
    }
}

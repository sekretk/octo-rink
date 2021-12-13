using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    [Table("System")]
    public partial class System
    {
        [Key]
        [StringLength(20)]
        public string Version { get; set; }

        public short? ProductID { get; set; }

        [Column(TypeName = "smalldatetime")]
        public DateTime? LastBackup { get; set; }
    }
}

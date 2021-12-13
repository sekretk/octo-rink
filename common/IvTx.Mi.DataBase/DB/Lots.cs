using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace dbfirstplay
{
    public partial class Lots
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string SerialNo { get; set; }

        [Column(TypeName = "smalldatetime")]
        public DateTime? EndDate { get; set; }

        [Column(TypeName = "smalldatetime")]
        public DateTime? ProductionDate { get; set; }

        [StringLength(255)]
        public string Location { get; set; }
    }
}

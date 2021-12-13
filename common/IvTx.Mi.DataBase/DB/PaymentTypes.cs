using System.ComponentModel.DataAnnotations;

namespace dbfirstplay
{
    public partial class PaymentTypes
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Name { get; set; }

        public int? PaymentMethod { get; set; }
    }
}

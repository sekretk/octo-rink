using System.ComponentModel.DataAnnotations;

namespace IvTx.Mi.DataBase.DB
{
    public partial class Users
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string Code { get; set; }

        [StringLength(255)]
        public string Name { get; set; }

        [StringLength(255)]
        public string Name2 { get; set; }

        public int? IsVeryUsed { get; set; }

        public int? GroupID { get; set; }

        [StringLength(50)]
        public string Password { get; set; }

        public int? UserLevel { get; set; }

        public int? Deleted { get; set; }

        [StringLength(255)]
        public string CardNumber { get; set; }
    }
}

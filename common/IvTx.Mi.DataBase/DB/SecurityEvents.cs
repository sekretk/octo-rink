using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IvTx.Mi.DataBase.DB
{
    public enum SecurityEventType
    {
        [Description("Инициация")]
        Init = 0,
        [Description("Карта прочтена")]
        Read = 1,
        [Description("Проход")]
        Entrance = 2,
        [Description("Выход")]
        Exit = 3,
        [Description("Заблокирована")]
        Blocked = 4,
    }

    public partial class SecurityEvents
    {
        public int ID { get; set; }

        [StringLength(255)]
        public string CardNumber { get; set; }
        
        public SecurityEventType Event { get; set; }

        [StringLength(255)]
        public string Comment { get; set; }

        [StringLength(255)]
        public string ControllerID { get; set; }

        public DateTime Time { get; set; }
    }
}

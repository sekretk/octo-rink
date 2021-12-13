using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using IvTx.Mi.DataBase.Enums;

namespace IvTx.Mi.DataBase
{
    public class PayItem
    {
        public PaymentEnum Type { get; set; }

        public double Amount { get; set; }
    }
}

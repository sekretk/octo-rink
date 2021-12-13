using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using IvTx.IronLogic.Facade;
using IvTx.IronLogic.Facade.Enums;

namespace Octo.Rnd.GateControl.Settings
{
    public class GateControlConfig
    {
        public ConnectionType Type { get; set; }
        public string Port { get; set; }
        public Controller[] Controllers { get; set; }
        public int ServiceGroup { get; set; }
        public string CardPrefix { get; set; }
        public int? Display { get; set; }
        public bool NoStateControl { get; set; }
    }
}

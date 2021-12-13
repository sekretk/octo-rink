using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using IvTx.IronLogic.Facade.Enums;

namespace IvTx.IronLogic.Facade
{
    public class Controller
    {
        public string Name { get; set; }

        public string Comment { get; set; }

        public byte Address { get; set; }

        public ControllerType Type { get; set; }

        public ushort SerialNumber { get; set; }

        public string FirmwareVersion { get; set; }

        public int MaxEvent { get; set; }
    }
}

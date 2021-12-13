using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using IvTx.IronLogic.Facade.Enums;

namespace IvTx.IronLogic.Facade
{
    public struct CtrlEvent
    {
        public EventType EventType { get; set; }

        public Controller Controller { get; set; }

        public Direction Direction { get; set; }

        /// <summary>
        /// card accepted to enter or not
        /// </summary>
        public bool AccessGranted { get; set; }

        public string Key { get; set; }
    }
}

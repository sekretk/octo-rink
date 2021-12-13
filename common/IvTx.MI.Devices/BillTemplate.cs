using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

namespace IvTx.MI.Devices
{
    public class BillTemplate
    {
        [XmlElement]
        public string[] Header { get; set; }

        [XmlElement]
        public string[] Details { get; set; }

        [XmlElement]
        public string[] Footer { get; set; }

        [XmlElement]
        public string[] Total { get; set; }
    }
}

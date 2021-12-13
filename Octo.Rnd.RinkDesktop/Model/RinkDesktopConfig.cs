using System.Xml.Serialization;
using IvTx.MI.Devices;

namespace Octo.Rnd.RinkDesktop.Model
{
    public class RinkDesktopConfig
    {
        [XmlElement]
        public int[] Groups { get; set; }

        [XmlElement]
        public int[] SubscriptionGoods { get; set; }

        public int PassGroup { get; set; }

        public int SubscriptionPartnerGroup { get; set; }

        public int PartnerID { get; set; }

        public int ObjectID { get; set; }

        public int UserID { get; set; }
        
        public int ToleranceInMin { get; set; } = 15;

        public BillTemplate Template { get; set; }
    }
}

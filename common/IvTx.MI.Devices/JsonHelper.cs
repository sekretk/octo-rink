using IvTx.Core.Abstract;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IvTx.MI.Devices
{


    public static class JsonHelper
    {     

        public static object NonFiscalObject(IEnumerable<IPosition> data, string auxilaryInfo = null)
        {
            //_fp.NonFiscalReceiptPrintItem("1", position.Name, position.Price, position.Quantity, position.VATCode, position.VATPercent, "", FixedVAT: position.FixedVAT);
            return new
            {
                uuid = Guid.NewGuid().ToString(),
                requests = new[]
                {
                    new {
                    type = "sell",
                    taxationType = "osn",
                    ignoreNonFiscalPrintErrors = false,
                        items = data.Select(d =>
                         new {
                            type = "text",
                            text = $"{d.Name}x{d.Quantity} -- {d.Price}",
                            alignment = "left",
                            font = 0,
                            doubleWidth = false,
                            doubleHeight = false
                        }
                        ).Union(new[] {
                        new {
                            type = "text",
                            text = auxilaryInfo??"-------------------",
                            alignment = "left",
                            font = 0,
                            doubleWidth = false,
                            doubleHeight = false
                        }}).ToArray()
                        }
                }
            };
        }

        public static object XReportObject()
        {
            return new
            {
                uuid = Guid.NewGuid().ToString(),
                requests = new[]
                {
                    new {
                        type = "reportX"
                    }
                }

            };
        }

        public static object CloseShiftObject()
        {
            return new
            {
                uuid = Guid.NewGuid().ToString(),
                requests = new[]
                {
                    new {
                        type = "closeShift"
                    }
                }

            };
        }

        public static object OpenShiftObject()
        {
            return new
            {
                uuid = Guid.NewGuid().ToString(),
                requests = new[]
                {
                    new {
                        type = "openShift"
                    }
                }

            };
        }
    }
}

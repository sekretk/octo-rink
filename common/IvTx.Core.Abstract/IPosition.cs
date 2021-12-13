namespace IvTx.Core.Abstract
{
    public interface IPosition
    {
        int GoodID { get; }

        string Name { get; set; }

        double Price { get; set; }

        double Quantity { get; set; }

        double TotalPrice { get; }

        int VATId { get; }

        short VATCode { get;  }

        double VATPercent { get;  }

        double FixedVAT { get; }
    }
}

using System;
using dbfirstplay;
using IvTx.Core.Abstract;
using IvTx.Mi.DataBase;
using Prism.Mvvm;

namespace Octo.Rnd.RinkDesktop.Model
{
    public class Position : BindableBase, IPosition
    {
        public Goods Good { get; }

        public Position()
        {
            
        }

        public Position(Goods good, VATGroups vatgroup)
        {
            if (good.TaxGroup != vatgroup.ID)
                throw new ArgumentException("не совпадает группа налогообложения");

            Good = good;
            GoodID = good.ID;
            Name = good.Name;
            Price = good.PriceOut2.GetValueOrDefault();
            Quantity = 1;
            VATCode = short.Parse(vatgroup.Code);
            VATPercent = vatgroup.VATValue.GetValueOrDefault();
            VATId = good.TaxGroup.GetValueOrDefault();
        }

        public int GoodID { get; private set; }

        public string Name { get; set; }

        private double _price;
        public double Price
        {
            get { return _price; }
            set { _price = value; OnPropertyChanged(); }
        }

        private double _quantity;
        public double Quantity
        {
            get { return _quantity; }
            set { _quantity = value; OnPropertyChanged(); }
        }

        public int VATId { get; }

        public double TotalPrice => Price*Quantity;
        public short VATCode { get; }
        public double VATPercent { get; }

        public double FixedVAT => TotalPrice * VATPercent / 100;
    }
}

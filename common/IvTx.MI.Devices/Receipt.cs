using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using IvTx.MI.Devices;

namespace Microinvest.RemotePrintServices
{

    /// <remarks/>
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
    [System.Xml.Serialization.XmlRootAttribute(Namespace = "", IsNullable = false)]
    [Serializable]
    public class Receipt
    {
        private int countryField;

        private int receiptTypeField;

        private int acctField;

        private int opertypeField;

        private double totalField;

        private ReceiptLocation locationField;

        private ReceiptOperator operatorField;

        private ReceiptPartner partnerField;

        private ReceiptOwner ownerField;

        private ReceiptItem[] itemsField;

        private ReceiptPayment paymentField;

        /// <remarks/>
        public int Country
        {
            get
            {
                return this.countryField;
            }
            set
            {
                this.countryField = value;
            }
        }

        /// <remarks/>
        public int ReceiptType
        {
            get
            {
                return this.receiptTypeField;
            }
            set
            {
                this.receiptTypeField = value;
            }
        }

        /// <remarks/>
        public int Acct
        {
            get
            {
                return this.acctField;
            }
            set
            {
                this.acctField = value;
            }
        }

        /// <remarks/>
        public int Opertype
        {
            get
            {
                return this.opertypeField;
            }
            set
            {
                this.opertypeField = value;
            }
        }

        public CommonEnums.OperType OperationType
        {
            get
            {                
                switch (opertypeField)
                {
                    case 34:
                        return CommonEnums.OperType.Refund;
                       
                    case 36:
                        return CommonEnums.OperType.AdvancePayment;

                    default:
                        return CommonEnums.OperType.Sale;
                }
            }
        }

        /// <remarks/>
        public double Total
        {
            get
            {
                return this.totalField;
            }
            set
            {
                this.totalField = value;
            }
        }

        /// <remarks/>
        public ReceiptLocation Location
        {
            get
            {
                return this.locationField;
            }
            set
            {
                this.locationField = value;
            }
        }

        /// <remarks/>
        public ReceiptOperator Operator
        {
            get
            {
                return this.operatorField;
            }
            set
            {
                this.operatorField = value;
            }
        }

        /// <remarks/>
        public ReceiptPartner Partner
        {
            get
            {
                return this.partnerField;
            }
            set
            {
                this.partnerField = value;
            }
        }

        /// <remarks/>
        public ReceiptOwner Owner
        {
            get
            {
                return this.ownerField;
            }
            set
            {
                this.ownerField = value;
            }
        }

        /// <remarks/>
        [System.Xml.Serialization.XmlArrayItemAttribute("Item", IsNullable = false)]
        public ReceiptItem[] Items
        {
            get
            {
                return this.itemsField;
            }
            set
            {
                this.itemsField = value;
            }
        }

        /// <remarks/>
        public ReceiptPayment Payment
        {
            get
            {
                return this.paymentField;
            }
            set
            {
                this.paymentField = value;
            }
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
    [Serializable]
    public class ReceiptLocation
    {

        private int codeField;

        private string nameField;

        private string groupField;

        /// <remarks/>
        public int Code
        {
            get
            {
                return this.codeField;
            }
            set
            {
                this.codeField = value;
            }
        }

        /// <remarks/>
        public string Name
        {
            get
            {
                return this.nameField;
            }
            set
            {
                this.nameField = value;
            }
        }

        /// <remarks/>
        public string Group
        {
            get
            {
                return this.groupField;
            }
            set
            {
                this.groupField = value;
            }
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
    [Serializable]
    public class ReceiptOperator
    {

        private int codeField;

        private string nameField;

        private string groupField;

        /// <remarks/>
        public int Code
        {
            get
            {
                return this.codeField;
            }
            set
            {
                this.codeField = value;
            }
        }

        /// <remarks/>
        public string Name
        {
            get
            {
                return this.nameField;
            }
            set
            {
                this.nameField = value;
            }
        }

        /// <remarks/>
        public string Group
        {
            get
            {
                return this.groupField;
            }
            set
            {
                this.groupField = value;
            }
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
    [Serializable]
    public class ReceiptPartner
    {

        private int codeField;

        private string nameField;

        private string groupField;

        private uint vATIDField;

        private string tAXIDField;

        private string mOLField;

        private string cityField;

        private string addressField;

        private uint phoneField;

        private string eMailField;

        private uint cardNumberField;

        private int paymentDaysField;

        private int dutyField;

        private double balanceField;

        private object note1Field;

        private object note2Field;

        /// <remarks/>
        public int Code
        {
            get
            {
                return this.codeField;
            }
            set
            {
                this.codeField = value;
            }
        }

        /// <remarks/>
        public string Name
        {
            get
            {
                return this.nameField;
            }
            set
            {
                this.nameField = value;
            }
        }

        /// <remarks/>
        public string Group
        {
            get
            {
                return this.groupField;
            }
            set
            {
                this.groupField = value;
            }
        }

        /// <remarks/>
        public uint VATID
        {
            get
            {
                return this.vATIDField;
            }
            set
            {
                this.vATIDField = value;
            }
        }

        /// <remarks/>
        public string TAXID
        {
            get
            {
                return this.tAXIDField;
            }
            set
            {
                this.tAXIDField = value;
            }
        }

        /// <remarks/>
        public string MOL
        {
            get
            {
                return this.mOLField;
            }
            set
            {
                this.mOLField = value;
            }
        }

        /// <remarks/>
        public string City
        {
            get
            {
                return this.cityField;
            }
            set
            {
                this.cityField = value;
            }
        }

        /// <remarks/>
        public string Address
        {
            get
            {
                return this.addressField;
            }
            set
            {
                this.addressField = value;
            }
        }

        /// <remarks/>
        public uint Phone
        {
            get
            {
                return this.phoneField;
            }
            set
            {
                this.phoneField = value;
            }
        }

        /// <remarks/>
        public string eMail
        {
            get
            {
                return this.eMailField;
            }
            set
            {
                this.eMailField = value;
            }
        }

        /// <remarks/>
        public uint CardNumber
        {
            get
            {
                return this.cardNumberField;
            }
            set
            {
                this.cardNumberField = value;
            }
        }

        /// <remarks/>
        public int PaymentDays
        {
            get
            {
                return this.paymentDaysField;
            }
            set
            {
                this.paymentDaysField = value;
            }
        }

        /// <remarks/>
        public int Duty
        {
            get
            {
                return this.dutyField;
            }
            set
            {
                this.dutyField = value;
            }
        }

        /// <remarks/>
        public double Balance
        {
            get
            {
                return this.balanceField;
            }
            set
            {
                this.balanceField = value;
            }
        }

        /// <remarks/>
        public object Note1
        {
            get
            {
                return this.note1Field;
            }
            set
            {
                this.note1Field = value;
            }
        }

        /// <remarks/>
        public object Note2
        {
            get
            {
                return this.note2Field;
            }
            set
            {
                this.note2Field = value;
            }
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
    [Serializable]
    public class ReceiptOwner
    {

        private int codeField;

        private string nameField;

        private uint vATIDField;

        private string tAXIDField;

        private string mOLField;

        private string cityField;

        private string addressField;

        private uint phoneField;

        private string eMailField;

        /// <remarks/>
        public int Code
        {
            get
            {
                return this.codeField;
            }
            set
            {
                this.codeField = value;
            }
        }

        /// <remarks/>
        public string Name
        {
            get
            {
                return this.nameField;
            }
            set
            {
                this.nameField = value;
            }
        }

        /// <remarks/>
        public uint VATID
        {
            get
            {
                return this.vATIDField;
            }
            set
            {
                this.vATIDField = value;
            }
        }

        /// <remarks/>
        public string TAXID
        {
            get
            {
                return this.tAXIDField;
            }
            set
            {
                this.tAXIDField = value;
            }
        }

        /// <remarks/>
        public string MOL
        {
            get
            {
                return this.mOLField;
            }
            set
            {
                this.mOLField = value;
            }
        }

        /// <remarks/>
        public string City
        {
            get
            {
                return this.cityField;
            }
            set
            {
                this.cityField = value;
            }
        }

        /// <remarks/>
        public string Address
        {
            get
            {
                return this.addressField;
            }
            set
            {
                this.addressField = value;
            }
        }

        /// <remarks/>
        public uint Phone
        {
            get
            {
                return this.phoneField;
            }
            set
            {
                this.phoneField = value;
            }
        }

        /// <remarks/>
        public string eMail
        {
            get
            {
                return this.eMailField;
            }
            set
            {
                this.eMailField = value;
            }
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
    [Serializable]
    public class ReceiptItem
    {

        private ushort codeField;

        private string nameField;

        private double priceField;

        private double quantityField;

        private string taxGroupField;

        private double taxPercentField;

        private object descriptionField;

        private double discountField;

        /// <remarks/>
        public ushort Code
        {
            get
            {
                return this.codeField;
            }
            set
            {
                this.codeField = value;
            }
        }

        /// <remarks/>
        public string Name
        {
            get
            {
                return this.nameField;
            }
            set
            {
                this.nameField = value;
            }
        }

        /// <remarks/>
        public double Price
        {
            get
            {
                return this.priceField;
            }
            set
            {
                this.priceField = value;
            }
        }

        /// <remarks/>
        public double Quantity
        {
            get
            {
                return this.quantityField;
            }
            set
            {
                this.quantityField = value;
            }
        }

        /// <remarks/>
        public string TaxGroup
        {
            get
            {
                return this.taxGroupField;
            }
            set
            {
                this.taxGroupField = value;
            }
        }

        /// <remarks/>
        public double TaxPercent
        {
            get
            {
                return this.taxPercentField;
            }
            set
            {
                this.taxPercentField = value;
            }
        }

        /// <remarks/>
        public object Description
        {
            get
            {
                return this.descriptionField;
            }
            set
            {
                this.descriptionField = value;
            }
        }

        /// <remarks/>
        public double Discount
        {
            get
            {
                return this.discountField;
            }
            set
            {
                this.discountField = value;
            }
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
    [Serializable]
    public class ReceiptPayment
    {

        private double cashField;

        private double cardField;

        private double accountField;

        private double voucherField;

        private double advancedField;

        /// <remarks/>
        public double Cash
        {
            get
            {
                return this.cashField;
            }
            set
            {
                this.cashField = value;
            }
        }

        /// <remarks/>
        public double Card
        {
            get
            {
                return this.cardField;
            }
            set
            {
                this.cardField = value;
            }
        }

        /// <remarks/>
        public double Account
        {
            get
            {
                return this.accountField;
            }
            set
            {
                this.accountField = value;
            }
        }

        /// <remarks/>
        public double Voucher
        {
            get
            {
                return this.voucherField;
            }
            set
            {
                this.voucherField = value;
            }
        }

        /// <remarks/>
        public double Advanced
        {
            get
            {
                return this.advancedField;
            }
            set
            {
                this.advancedField = value;
            }
        }
    }
}

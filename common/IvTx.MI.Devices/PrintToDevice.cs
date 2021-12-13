using System;
using System.IO;
using Microinvest.RemotePrintServices;
using Microsoft.VisualBasic;

namespace IvTx.MI.Devices
{
    /// <summary>
    /// Клас за принтиране на устройство
    /// </summary>
    public class PrintToDevice
    {
        #region Declarations

        /// <summary>
        /// Supported version of DeviceManager.dll
        /// </summary>
        private const string DeviceManagerVersion = "1.02";

        /// <summary>
        /// DeviceTestTool Config file
        /// </summary>
        private const string DeviceManagerConfig = "DeviceTestTool.config";

        /// <summary>
        /// DeviceTestTool resources file
        /// </summary>
        private const string DeviceManagerDictionary = "Dictionary_DTT.lng";

        /// <summary>
        /// DeviceDescriptor
        /// </summary>
        private const string DeviceDescriptor = "Fiscal_Printer_00";

        /// <summary>
        /// FP_Driver
        /// </summary>
        private dynamic fpDriver;

        /// <summary>
        /// dataPath
        /// </summary>
        private string dataPath;

        /// <summary>
        /// Път на конфигурационен файл
        /// </summary>
        private string cfgFile;

        /// <summary>
        /// Път на конфигурационен файл за държава
        /// </summary>
        private CommonEnums.County country;

        /// <summary>
        /// Път на конфигурационен файл за ресурси
        /// </summary>
        private string resFile;

        #endregion

        #region Constructors

        /// <summary>
        /// Конструктор за принтиране към устройство
        /// </summary>
        /// <param name="configCountry"></param>
        /// <param name="configFile"></param>
        /// <param name="dictionaryFile"></param>
        public PrintToDevice(CommonEnums.County country, string configFile, string dictionaryFile)
        {            
            this.cfgFile = configFile;
            this.resFile = dictionaryFile;            
            this.Country = country;
            InitFPDriver();
        }

        /// <summary>
        /// Конструктор за принтиране към устройство
        /// </summary>
        /// <param name="configCoutry"></param>
        public PrintToDevice(CommonEnums.County country)
        {
            this.cfgFile = Path.Combine(GetDeviceTestToolPath(), DeviceManagerConfig);
            this.resFile = Path.Combine(GetDeviceTestToolPath(), DeviceManagerDictionary);
            this.Country = country;
            InitFPDriver();
        }

        #endregion

        #region Properties

        public CommonEnums.County Country 
        { 
            get
            {
                return this.country;
            }
            private set
            {
                if (value >= CommonEnums.County.Bulgaria && value<= CommonEnums.County.Georgia)
                {
                    this.country = value;
                }
                else
                {
                    this.country = CommonEnums.County.Bulgaria;
                }
            }
        }

        /// <summary>
        /// ActiveProperties
        /// </summary>
        public string ActiveProperties
        {
            get
            {
                return fpDriver.ActiveProperties;
            }
        }

        #endregion

        #region Print Methods

        /// <summary>
        /// Отпечатване на касова бележка през Device manager
        /// </summary>
        /// <param name="receipt">Обект с данни за касовата бележка</param>
        /// <param name="opType"></param>
        /// <returns></returns>
        /// <remarks>Светослав Кадиев</remarks>        
        public bool PrintReceipt(Receipt receipt)
        {
            int i = 0;
            double tempSum = 0;
            double lastVatValue = 0;
            object lastVatCode = null;
            bool refusePrint = false;
            CommonEnums.OperType singleOperType;

            try
            {
                if (fpDriver == null)
                {
                    return false;
                }

                singleOperType = receipt.OperationType;
                fpDriver.ReceiptNew();

                foreach (ReceiptItem tmpLine in receipt.Items)
                {
                    tmpLine.Quantity = MIRound(Convert.ToDouble(tmpLine.Quantity), 3);

                    if (tmpLine.TaxGroup == null)
                    {
                        tmpLine.TaxGroup = string.Empty;
                    }

                    fpDriver.ReceiptAddItem(
                        Convert.ToString(tmpLine.Code),
                        Convert.ToString(tmpLine.Name),
                        Convert.ToDouble(tmpLine.Price),
                        Convert.ToDouble(tmpLine.Quantity),
                        Convert.ToString(tmpLine.TaxGroup),
                        Convert.ToDouble(tmpLine.TaxPercent),
                        Convert.ToString(tmpLine.Description),
                        0);

                    tempSum = tempSum + MIRound(Convert.ToDouble(Convert.ToDouble(tmpLine.Price) * Convert.ToDouble(tmpLine.Quantity)), 2);
                    lastVatValue = tmpLine.TaxPercent / 100;
                    lastVatCode = tmpLine.TaxGroup;
                    i += 1;
                }

                //// Изравняване
                //// if (singleOperType != CommonEnums.OperType.AdvancePayment &&
                //// (tempSum < receipt.Total & Math.Abs(MIRound(receipt.Total - tempSum, 2)) > 0))
                if (tempSum < receipt.Total & Math.Abs(MIRound(receipt.Total - tempSum, 2)) > 0)
                {
                    tempSum = Math.Abs(MIRound(receipt.Total - tempSum, 2));
                    fpDriver.ReceiptAddItem(string.Empty, "Addition", tempSum, 1, lastVatCode, lastVatValue * 100, string.Empty, 0);
                }

                i = 0;
                fpDriver.ReceiptAddPayment(receipt.Payment.Cash, i, false);
                if (receipt.Payment.Advanced > 0)
                {
                    fpDriver.ReceiptAddPayment(receipt.Payment.Advanced, i, false);
                }

                i += 1;
                fpDriver.ReceiptAddPayment(receipt.Payment.Card, i, false);
                i += 1;
                fpDriver.ReceiptAddPayment(receipt.Payment.Account, i, false);
                i += 1;
                fpDriver.ReceiptAddPayment(receipt.Payment.Voucher, i, false);
                i += 1;

                //// if (singleOperType != CommonEnums.OperType.AdvancePayment &&
                ////   receipt.PaidAdvanced > 0)
                //// {
                ////    fpDriver.ReceiptAddPayment(receipt.PaidAdvanced, i, true);
                //// }

                //// tempSum = fpDriver.ReceiptPrint(
                //// receipt.PaidAdvanced >= receipt.Total, singleOperType == CommonEnums.OperType.Refund || receipt.Total < 0, false, 0, refusePrint);

                tempSum = fpDriver.ReceiptPrint(false, singleOperType == CommonEnums.OperType.Refund || receipt.Total < 0, false, 0, refusePrint);

                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        /// <summary>
        /// Метод за съкращаване на децимал число чрз премахване на края на числото
        /// </summary>
        /// <param name="value"></param>
        /// <param name="numDigitsAfterDecimal"></param>
        /// <returns></returns>
        private decimal Truncate(double value, int numDigitsAfterDecimal)
        {
            int coef = 10 ^ numDigitsAfterDecimal;
            return Math.Truncate(Convert.ToDecimal(value * coef)) / coef;
        }

        /// <summary>
        /// Метод за закръгление на число
        /// </summary>
        /// <param name="number"></param>
        /// <param name="numDigitsAfterDecimal"></param>
        /// <returns></returns>
        private double MIRound(double number, int numDigitsAfterDecimal)
        {
            return (int)((number * (10 ^ numDigitsAfterDecimal)) + (0.5 * Math.Sign(number))) / (10 ^ numDigitsAfterDecimal);
        }

        #endregion

        #region Methods

        /// <summary>
        /// Метод за запазване на настройки
        /// </summary>
        public void SaveSettings()
        {
            fpDriver.SaveSettings(cfgFile, DeviceDescriptor);
        }

        /// <summary>
        /// Метод за инициализция на настройките при зареждане на програмата
        /// </summary>
        public void InitFPDriver()
        {
            try
            {
                fpDriver = Interaction.CreateObject("DeviceManager.FPMain");

                ////Проверка на версията
                if (fpDriver.Version.ToString().Substring(0, 4) != DeviceManagerVersion)
                    return;

                fpDriver.LoadSettings(cfgFile, DeviceDescriptor, (short)country);
                fpDriver.ResourceManager.DictionaryFile = resFile;
                fpDriver.ParentAppPath = new System.IO.DirectoryInfo(cfgFile).Parent.FullName;
                fpDriver.PortMonitor = false;
                fpDriver.NoErrorsHandling = true;
            }
            catch (IOException)
            {
                return;
            }
        }

        /// <summary>
        /// Метод за пътя на директорията, от която се взима DeviceTestTool.dll
        /// </summary>
        /// <returns></returns>
        public string GetDeviceTestToolPath()
        {
            if (string.IsNullOrEmpty(dataPath))
            {
                dataPath = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), @"Microinvest\Device Test Tool");
            }
            else if (!Directory.Exists(dataPath))
            {
                Directory.CreateDirectory(dataPath);
            }

            return dataPath;
        }

        /// <summary>
        /// Метод, който връща описание на операцията
        /// </summary>
        /// <param name="operType"></param>
        /// <returns></returns>
        private string GetOperatonDescription(CommonEnums.OperType operType)
        {
            switch (operType)
            {
                case CommonEnums.OperType.Sale:                         ////TOperType.ConsigAccounting
                    return "Sale";                                                      ////Dictionary.GetResource("strSaleOp")

                case CommonEnums.OperType.Refund:                       ////Dictionary.GetResource("strRefund")
                    return "Refund";

                case CommonEnums.OperType.AdvancePayment:
                    return "Advance Payment";

                default:
                    return string.Empty;
            }
        }
      
        #endregion
    }

    ///// <summary>
    ///// Клас за касова бележка
    ///// </summary>
    //public class CashReceipt
    //{
    //    #region Declarations

    //    /// <summary>
    //    /// Плащане по банков път
    //    /// </summary>
    //    private double paidBank;

    //    /// <summary>
    //    /// Плащане в брой
    //    /// </summary>
    //    private double paidCash;

    //    /// <summary>
    //    /// Друг вид плащане
    //    /// </summary>
    //    private double paidOther;

    //    /// <summary>
    //    /// Авансово плащане
    //    /// </summary>
    //    private double paidAdvanced;

    //    /// <summary>
    //    /// Общо
    //    /// </summary>
    //    private double total;

    //    /// <summary>
    //    /// Номер на устройството
    //    /// </summary>
    //    private string deviceNo;

    //    /// <summary>
    //    /// Списък с редовете на касовата блежка
    //    /// </summary>
    //    private List<ReceiptLine> receiptLine;

    //    /// <summary>
    //    /// Единично плащане
    //    /// </summary>
    //    private bool isSinglePayment;

    //    /// <summary>
    //    /// Команди
    //    /// </summary>
    //    private CommonEnums.FPCommands command;

    //    #endregion

    //    #region Constructor

    //    /// <summary>
    //    /// Конструктор за касова бележка
    //    /// </summary>
    //    public CashReceipt()
    //    {
    //        this.receiptLine = new List<ReceiptLine>();
    //        deviceNo = "0";
    //    }

    //    #endregion Constructor

    //    #region Properties

    //    /// <summary>
    //    /// Плащане в брой
    //    /// </summary>
    //    public double PaidCash
    //    {
    //        get
    //        {
    //            return paidCash;
    //        }

    //        set
    //        {
    //            paidCash = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Плащане по банков път
    //    /// </summary>
    //    public double PaidBank
    //    {
    //        get
    //        {
    //            return paidBank;
    //        }

    //        set
    //        {
    //            paidBank = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Плащане чрез карта
    //    /// </summary>
    //    public double PaidCard
    //    {
    //        get
    //        {
    //            return paidCash;
    //        }

    //        set
    //        {
    //            paidCash = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Друг вид плащане
    //    /// </summary>
    //    public double PaidOther
    //    {
    //        get
    //        {
    //            return paidOther;
    //        }

    //        set
    //        {
    //            paidOther = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Авансово плащане
    //    /// </summary>
    //    public double PaidAdvanced
    //    {
    //        get
    //        {
    //            return paidAdvanced;
    //        }

    //        set
    //        {
    //            paidAdvanced = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Общо
    //    /// </summary>
    //    public double Total
    //    {
    //        get
    //        {
    //            return total;
    //        }

    //        set
    //        {
    //            total = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Номер на устройство
    //    /// </summary>
    //    public string DeviceNo
    //    {
    //        get
    //        {
    //            return deviceNo;
    //        }

    //        set
    //        {
    //            deviceNo = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Команди
    //    /// </summary>
    //    public CommonEnums.FPCommands Command
    //    {
    //        get
    //        {
    //            return command;
    //        }

    //        set
    //        {
    //            command = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Единично или неединично плащане
    //    /// </summary>
    //    public bool IsSinglePayment
    //    {
    //        get
    //        {
    //            return isSinglePayment;
    //        }

    //        set
    //        {
    //            isSinglePayment = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Редове на касовата бележка
    //    /// </summary>
    //    public List<ReceiptLine> ReceiptLines
    //    {
    //        get
    //        {
    //            return receiptLine;
    //        }

    //        set
    //        {
    //            receiptLine = value;
    //        }
    //    }

    //    #endregion

    //    #region Methods

    //    /// <summary>
    //    /// Изчистване на редовете на бележка
    //    /// </summary>
    //    public void Clear()
    //    {
    //        this.receiptLine.Clear();
    //    }

    //    #endregion Methods
    //}

    ///// <summary>
    ///// Редеве на касова бележка
    ///// </summary>
    //public class ReceiptLine
    //{
    //    #region Declarations

    //    /// <summary>
    //    /// Код
    //    /// </summary>
    //    private string code;

    //    /// <summary>
    //    /// Име
    //    /// </summary>
    //    private string name;

    //    /// <summary>
    //    /// Количество
    //    /// </summary>
    //    private double quantity;

    //    /// <summary>
    //    /// Цена
    //    /// </summary>
    //    private double price;

    //    /// <summary>
    //    /// ДДС код
    //    /// </summary>
    //    private string vatCode;

    //    /// <summary>
    //    /// ДДС стойност
    //    /// </summary>
    //    private double vatValue;

    //    /// <summary>
    //    /// Описание на артикул
    //    /// </summary>
    //    private string itemDescription;

    //    #endregion

    //    #region Constructors

    //    /// <summary>
    //    /// Конструктор за данните на касовата бележка
    //    /// </summary>
    //    /// <param name="code"></param>
    //    /// <param name="name"></param>
    //    /// <param name="qtty"></param>
    //    /// <param name="price"></param>
    //    /// <param name="vatCode"></param>
    //    /// <param name="vatValue"></param>
    //    /// <param name="description"></param>
    //    public ReceiptLine(string code, string name, double qtty, double price, string vatCode, double vatValue, string description)
    //    {
    //        this.code = code;
    //        this.name = name;
    //        this.quantity = qtty;
    //        this.price = price;
    //        this.vatCode = vatCode;
    //        this.vatValue = vatValue;
    //        this.itemDescription = description;
    //    }

    //    #endregion

    //    #region Properties

    //    /// <summary>
    //    /// Код
    //    /// </summary>
    //    public string Code
    //    {
    //        get
    //        {
    //            return code;
    //        }

    //        set
    //        {
    //            code = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Име
    //    /// </summary>
    //    public string Name
    //    {
    //        get
    //        {
    //            return name;
    //        }

    //        set
    //        {
    //            name = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Количество
    //    /// </summary>
    //    public double Quantity
    //    {
    //        get
    //        {
    //            return quantity;
    //        }

    //        set
    //        {
    //            quantity = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Цена
    //    /// </summary>
    //    public double Price
    //    {
    //        get
    //        {
    //            return price;
    //        }

    //        set
    //        {
    //            price = value;
    //        }
    //    }

    //    /// <summary>
    //    /// ДДС код
    //    /// </summary>
    //    public string VatCode
    //    {
    //        get
    //        {
    //            return vatCode;
    //        }

    //        set
    //        {
    //            vatCode = value;
    //        }
    //    }

    //    /// <summary>
    //    /// ДДС стойност
    //    /// </summary>
    //    public double VatValue
    //    {
    //        get
    //        {
    //            return vatValue;
    //        }

    //        set
    //        {
    //            vatValue = value;
    //        }
    //    }

    //    /// <summary>
    //    /// Описание на артикул
    //    /// </summary>
    //    public string Description
    //    {
    //        get
    //        {
    //            return itemDescription;
    //        }

    //        set
    //        {
    //            itemDescription = value;
    //        }
    //    }

    //    #endregion
    //}
}

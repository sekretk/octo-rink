namespace IvTx.MI.Devices
{
    /// <summary>
    /// Общ модул 
    /// </summary>
    public static class CommonEnums
    {
        #region Enums

        /// <summary>
        /// Начални езици
        /// </summary>
        public enum TLanguages : int
        {
            Bulgarian = 0,
            English = 1,
            German = 2,
            Russian = 3,
            Turkish = 4,
            Albanian = 5,
            Serbian = 6,
            Romanian = 7,
            Greek = 8,
            EnglishUS = 9,
            Armenian = 10,
            Georgian = 11,
            Spanish = 12,
            Ukrainian = 13,
            Polish = 14,
        }

        /// <summary>
        /// Налични държави 
        /// </summary>

        public enum County:int
        {
            Bulgaria = 0,
            Russia = 1,
            Romania = 2,
            Kenya = 3,
            Malta = 4,
            Greece = 5,
            Cyprus = 6,
            Molodova = 7,
            Kazakhstan = 8,
            USA = 9,
            Canada = 10,
            Belarus = 11,
            Armenia = 12,
            Germany = 13,
            Spain = 14,
            Mexico = 15,
            Turkey = 16,
            Austria = 17,
            Ukraine = 18,
            Tanzania = 19,
            SouthAfrica = 20,
            Azerbaijan = 21,
            Poland = 22,
            Rwanda = 23,
            UAE = 24,
            Cuba = 25,
            Zambia = 26,
            UnitedKingdom = 27,
            Latvia = 28,
            Oman = 29,
            Kyrgyzstan = 30,
            Finland = 31,
            SaudiArabia = 32,
            Turkmenistan = 33,
            Georgia = 34
        }            

        /// <summary>
        /// Вид на операция
        /// </summary>
        public enum OperType : int
        {
            Sale = 2,                    ////Sale 
            Refund = 34,                 ////Refund
            AdvancePayment = 36          ////Advance Payment
        }

        #endregion

        #region Enumerators and Structures

        /// <summary>
        /// ФПС команди
        /// </summary>
        public enum FPCommands : int
        {
            Init = 1,
            Print = 2,
            Done = 3,
            PosPayment = 201
        }

        /// <summary>
        /// Видове касови бележки
        /// </summary>
        public enum ReceiptTypes : int
        {
            FiscalReceipt = 1,
            NonFiscalReceipt = 2,
            RefundReceipt = 3,
            PayIn = 4,
            Payout = 5,
            ZReport = 6,
            XReport = 7
        }

        /// <summary>
        /// Енкодинг
        /// </summary>
        public enum TEncoding : int
        {
            Default = 0,
            Mik = 1,
            CP866 = 2,
            Combined = 3,
            CP737 = 4,
            CyrToLat = 5,
            CP855 = 6,
            Unknown
        }

        /// <summary>
        /// Съобщения
        /// </summary>
        public enum TReports
        {
            ZReport = 0,
            XReport = 1,
            EJReport = 2,
            PeriodReportShort = 3,
            PeriodReportLong = 4,
            NumberReportShort = 5,
            NumberReportLong = 6,
            Unknown
        }

        #endregion
    }
}

using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using dbfirstplay;
using IvTx.Core;
using IvTx.Core.Abstract;
using IvTx.Core.Helpers;
using IvTx.Mi.DataBase.DB;
using IvTx.Mi.DataBase.Enums;

namespace IvTx.Mi.DataBase
{
    public class MIOperationFacade
    {
        private Model _dbModel;

        public Model Model { get { return _dbModel; } }

        public MIOperationFacade()
        {
            _dbModel = new IvTx.Mi.DataBase.Model();
            Partner = _dbModel.Partners.FirstOrDefault(p => p.ID == 1);
            Object = _dbModel.Objects.FirstOrDefault(o => o.ID == 1);
            Operator = _dbModel.Users.FirstOrDefault(u => u.ID == 0);
            Currency = _dbModel.Currencies.FirstOrDefault();
        }

        public Partners Partner { get; set; }

        public Objects Object { get; set; }

        public Users Operator { get; set; }

        public Currencies Currency { get; set; }

        public void Sale(IEnumerable<IPosition> positions, IEnumerable<PayItem> payItems)
        {
            ExecuteInTransaction(() =>
            {
                //todo: check amounts equality

                //var na = _dbModel.NextAcct.OrderByDescending(_ => _.ID).FirstOrDefault(n => n.NextAcct1.StartsWith("S"));

                //int nextAcct = 1;

                //int ina = 0;
                //if (na != null && Int32.TryParse(na.NextAcct1.Substring(1), NumberStyles.Any, CultureInfo.InvariantCulture, out ina))
                //        nextAcct += ina;

                int nextAcct = 1 + (_dbModel.Operations.Where(o => o.OperType == 2/*sale*/ && o.ObjectID == Object.ID).Max(o => o.Acct) ?? 0);

                //try
                //{
                //    _dbModel.NextAcct.Add(new NextAcct() {NextAcct1 = $"S{nextAcct}"});
                //}
                //finally
                //{
                //    _dbModel.SaveChanges();
                //}

                foreach (var position in positions)
                {
                    _dbModel.Database.ExecuteSqlCommand("UPDATE store SET  Qtty = Qtty - @qty WHERE ObjectID = @object AND GoodID = @good", new SqlParameter("qty", position.Quantity),
                        new SqlParameter("object", Object?.ID ?? 0), new SqlParameter("good", position.GoodID));

                    var vat = _dbModel.VATGroups.FirstOrDefault(v => v.ID == position.VATId) ?? new VATGroups();

                    var operation = new Operations()
                    {
                        OperType = (int)OperationsEnum.Sale,
                        Acct = nextAcct,
                        GoodID = position.GoodID,
                        PartnerID = Partner?.ID ?? 0,
                        ObjectID = Object?.ID ?? 0,
                        OperatorID = Operator?.ID ?? 0,
                        Qtty = position.Quantity,
                        Sign = -1,
                        PriceIn = 0, //todo: remove hardcode
                        PriceOut = position.Price,
                        VATIn = 0, //todo: remove hardcode
                        VATOut = position.Price * vat.VATValue / 100,
                        Discount = 0, //todo: remove hardcode
                        CurrencyID = Currency?.ID ?? 0,
                        CurrencyRate = Currency?.ExchangeRate,
                        Lot = String.Empty,
                        Note = String.Empty,
                        SrcDocID = 0, //todo: what is SrcDocID?
                        Date = DateTime.Today,
                        LotID = 1, //todo: remove hardcode
                        UserID = Operator?.ID ?? 0,
                        UserRealTime = DateTime.Now,
                    };

                    _dbModel.Operations.Add(operation);
                }

                if (payItems.Any())
                    _dbModel.Payments.Add(new Payments()
                    {
                        Acct = nextAcct,
                        OperType = (int)OperationsEnum.Sale,
                        PartnerID = Partner?.ID ?? 0,
                        Qtty = payItems.Sum(_ => _.Amount),
                        Mode = -1,
                        Sign = 1,
                        Date = DateTime.Today,
                        UserID = Operator?.ID ?? 0,
                        ObjectID = Object?.ID ?? 0,
                        UserRealTime = DateTime.Now,
                        Type = (int)payItems.FirstOrDefault()?.Type,
                        TransactionNumber = payItems.FirstOrDefault()?.Type.ToString(),
                        EndDate = DateTime.Today,
                    });

                foreach (var payItem in payItems)
                {
                    var payment = new Payments()
                    {
                        Acct = nextAcct,
                        OperType = (int)OperationsEnum.Sale,
                        PartnerID = Partner?.ID ?? 0,
                        Qtty = payItem.Amount,
                        Mode = 1,
                        Sign = 1,
                        Date = DateTime.Today,
                        UserID = Operator?.ID ?? 0,
                        ObjectID = Object?.ID ?? 0,
                        UserRealTime = DateTime.Now,
                        Type = (int)payItem.Type,
                        TransactionNumber = payItem.Type.ToString(),
                        EndDate = DateTime.Today,
                    };

                    _dbModel.Payments.Add(payment);

                    if (payItem.Type == PaymentEnum.Cash || payItem.Type == PaymentEnum.Voucher)
                        _dbModel.CashBook.Add(new CashBook()
                        {
                            Date = DateTime.Today,
                            Desc = $"Продажа #{nextAcct:D10}" + (payItem.Type == PaymentEnum.Voucher ? " ВАУЧЕРОМ" : String.Empty),
                            OperType = 8, //todo: WTF?
                            Sign = 1,
                            Profit = payItem.Amount,
                            UserID = Operator?.ID ?? 0,
                            UserRealtime = DateTime.Now,
                            ObjectID = Object?.ID ?? 0,
                        });
                }

                _dbModel.SaveChanges();

            });

        }

        public void Return(IEnumerable<IPosition> positions, PaymentEnum paymentType)
        {
            ExecuteInTransaction(() =>
            {
                //todo: check amounts equality

                //var na = _dbModel.NextAcct.OrderByDescending(_ => _.ID).FirstOrDefault(n => n.NextAcct1.StartsWith("S"));

                //int nextAcct = 1;

                //int ina = 0;
                //if (na != null && Int32.TryParse(na.NextAcct1.Substring(1), NumberStyles.Any, CultureInfo.InvariantCulture, out ina))
                //        nextAcct += ina;

                int nextAcct = 1 + (_dbModel.Operations.Where(o => (o.OperType == (int)OperationsEnum.Refund) && o.ObjectID == Object.ID).Max(o => o.Acct) ?? 0);

                //try
                //{
                //    _dbModel.NextAcct.Add(new NextAcct() {NextAcct1 = $"S{nextAcct}"});
                //}
                //finally
                //{
                //    _dbModel.SaveChanges();
                //}

                foreach (var position in positions)
                {
                    _dbModel.Database.ExecuteSqlCommand("UPDATE store SET  Qtty = Qtty - @qty WHERE ObjectID = @object AND GoodID = @good", new SqlParameter("qty", position.Quantity),
                        new SqlParameter("object", Object?.ID ?? 0), new SqlParameter("good", position.GoodID));

                    var vat = _dbModel.VATGroups.FirstOrDefault(v => v.ID == position.VATId) ?? new VATGroups();

                    var operation = new Operations()
                    {
                        OperType = (int)OperationsEnum.Refund,
                        Acct = nextAcct,
                        GoodID = position.GoodID,
                        PartnerID = Partner?.ID ?? 0,
                        ObjectID = Object?.ID ?? 0,
                        OperatorID = Operator?.ID ?? 0,
                        Qtty = position.Quantity,
                        Sign = 1,
                        PriceIn = 0, //todo: remove hardcode
                        PriceOut = position.Price,
                        VATIn = 0, //todo: remove hardcode
                        VATOut = position.Price * vat.VATValue / 100,
                        Discount = 0, //todo: remove hardcode
                        CurrencyID = Currency?.ID ?? 0,
                        CurrencyRate = Currency?.ExchangeRate,
                        Lot = String.Empty,
                        Note = String.Empty,
                        SrcDocID = 0, //todo: what is SrcDocID?
                        Date = DateTime.Today,
                        LotID = 1, //todo: remove hardcode
                        UserID = Operator?.ID ?? 0,
                        UserRealTime = DateTime.Now,
                    };

                    _dbModel.Operations.Add(operation);
                }


                _dbModel.Payments.Add(new Payments()
                {
                    Acct = nextAcct,
                    OperType = (int)OperationsEnum.Refund,
                    PartnerID = Partner?.ID ?? 0,
                    Qtty = positions.Sum(_ => _.TotalPrice),
                    Mode = -1,
                    Sign = -1,
                    Date = DateTime.Today,
                    UserID = Operator?.ID ?? 0,
                    ObjectID = Object?.ID ?? 0,
                    UserRealTime = DateTime.Now,
                    Type = (int)paymentType,
                    TransactionNumber = paymentType.ToString(),
                    EndDate = DateTime.Today,
                });

                _dbModel.Payments.Add(new Payments()
                {
                    Acct = nextAcct,
                    OperType = (int)OperationsEnum.Refund,
                    PartnerID = Partner?.ID ?? 0,
                    Qtty = positions.Sum(_ => _.TotalPrice),
                    Mode = 1,
                    Sign = -1,
                    Date = DateTime.Today,
                    UserID = Operator?.ID ?? 0,
                    ObjectID = Object?.ID ?? 0,
                    UserRealTime = DateTime.Now,
                    Type = (int)paymentType,
                    TransactionNumber = paymentType.ToString(),
                    EndDate = DateTime.Today,
                });


                _dbModel.CashBook.Add(new CashBook()
                {
                    Date = DateTime.Today,
                    Desc = $"Возврат #{nextAcct:D10} {paymentType}",
                    OperType = 8, //todo: WTF?
                    Sign = 1,
                    Profit = positions.Sum(_ => _.TotalPrice),
                    UserID = Operator?.ID ?? 0,
                    UserRealtime = DateTime.Now,
                    ObjectID = Object?.ID ?? 0,
                });


                _dbModel.SaveChanges();

            });

        }

        private void ExecuteInTransaction(Action action)
        {
            var transaction = _dbModel.Database.BeginTransaction(IsolationLevel.Serializable);
            Failover.Execute(action, exception => { transaction.Rollback(); }, true, () => transaction.Commit());
        }

        public void SetPartnersValidity(IEnumerable<Partners> partner, DateTime time, string comment = null)
        {
            partner.ForEach(p =>
            {
                if (String.IsNullOrEmpty(p.CardNumber))
                    return;

                p.GoodTill = time;
                p.SecurityType = SecurityType.Inited;
                _dbModel.SecurityEvents.Add(new SecurityEvents()
                {
                    CardNumber = p.CardNumber,
                    Event = SecurityEventType.Init,
                    Time = DateTime.Now,
                    Comment = comment
                });
            });

            _dbModel.SaveChanges();
        }

        public void SetPartnerValidity(Partners partner, DateTime time, string comment = null)
        {
            if (String.IsNullOrEmpty(partner.CardNumber))
                return;

            partner.GoodTill = time;
            partner.SecurityType = SecurityType.Inited;
            _dbModel.SecurityEvents.Add(new SecurityEvents()
            {
                CardNumber = partner.CardNumber,
                Event = SecurityEventType.Init,
                Time = DateTime.Now,
                Comment = comment
            });

            _dbModel.SaveChanges();
        }

        public bool IsCardActive(string card)
        {
            var evnt =
                _dbModel.SecurityEvents.Where(c => c.CardNumber == card).OrderByDescending(c => c.Time).FirstOrDefault(c => c.Event == SecurityEventType.Blocked || c.Event == SecurityEventType.Init);

            return evnt == null || evnt.Event != SecurityEventType.Blocked;
        }

        public IEnumerable<SecurityEvents> LastCardSecurityEvents(string card, int countOfEvents = 10)
        {
            return _dbModel.SecurityEvents.Where(e => e.CardNumber == card).OrderByDescending(e => e.Time).Take(countOfEvents);
        }

        public void BlockCard(Partners partner)
        {
            partner.SecurityType = SecurityType.Blocked;

            _dbModel.SecurityEvents.Add(new SecurityEvents()
            {
                CardNumber = partner.CardNumber,
                Event = SecurityEventType.Blocked,
                Time = DateTime.Now,
            });

            _dbModel.SaveChanges();
        }

        public decimal PartnerBalance(int partnerId)
        {
            return
                (decimal)(_dbModel.Payments.Where(
                    p => p.PartnerID == partnerId && p.OperType == (int)OperationsEnum.AdvancePayment)
                    .Sum(p => p.Qtty * p.Mode) ?? 0);
        }

        public bool UsePrepaiedAmount(IEnumerable<IPosition> positions)
        {
            var posCount = positions.Sum(x => x.Quantity);

            if (PartnerBalance(Partner?.ID ?? 0) < (decimal)posCount)
                return false;

            ExecuteInTransaction(() =>
            {
                int nextAcct = 1 +
                               _dbModel.Operations.Where(
                                   o =>
                                       o.OperType == (int)OperationsEnum.Sale ||
                                       o.OperType == (int)OperationsEnum.SalesOnConsignment).Max(o => o.Acct) ?? 0;

                _dbModel.NextAcct.Add(new NextAcct() { NextAcct1 = $"S{nextAcct}" });

                foreach (var position in positions)
                {
                    _dbModel.Database.ExecuteSqlCommand("UPDATE store SET  Qtty = Qtty - @qty WHERE ObjectID = @object AND GoodID = @good", new SqlParameter("qty", position.Quantity),
                        new SqlParameter("object", Object?.ID ?? 0), new SqlParameter("good", position.GoodID));

                    var vat = _dbModel.VATGroups.FirstOrDefault(v => v.ID == position.VATId) ?? new VATGroups();

                    var operation = new Operations()
                    {
                        OperType = (int)OperationsEnum.Sale,
                        Acct = nextAcct,
                        GoodID = position.GoodID,
                        PartnerID = Partner?.ID ?? 0,
                        ObjectID = Object?.ID ?? 0,
                        OperatorID = Operator?.ID ?? 0,
                        Qtty = position.Quantity,
                        Sign = -1,
                        PriceIn = 0, //todo: remove hardcode
                        PriceOut = 1,
                        VATIn = 0, //todo: remove hardcode
                        VATOut = position.Price * vat.VATValue / 100,
                        Discount = 0, //todo: remove hardcode
                        CurrencyID = Currency?.ID ?? 0,
                        CurrencyRate = Currency?.ExchangeRate,
                        Lot = String.Empty,
                        Note = String.Empty,
                        SrcDocID = 0, //todo: what is SrcDocID?
                        Date = DateTime.Today,
                        LotID = 1, //todo: remove hardcode
                        UserID = Operator?.ID ?? 0,
                        UserRealTime = DateTime.Now,
                    };

                    _dbModel.Operations.Add(operation);
                }


                _dbModel.Payments.Add(new Payments()
                {
                    Acct = nextAcct,
                    OperType = (int)OperationsEnum.Sale,
                    PartnerID = Partner?.ID ?? 0,
                    Qtty = posCount,
                    Mode = -1,
                    Sign = 1,
                    Date = DateTime.Today,
                    UserID = Operator?.ID ?? 0,
                    ObjectID = Object?.ID ?? 0,
                    UserRealTime = DateTime.Now,
                    Type = 1,
                    TransactionNumber = String.Empty,
                    EndDate = DateTime.Today,
                });

                _dbModel.Payments.Add(new Payments()
                {
                    Acct = nextAcct,
                    OperType = (int)OperationsEnum.Sale,
                    PartnerID = Partner?.ID ?? 0,
                    Qtty = posCount,
                    Mode = 1,
                    Sign = 1,
                    Date = DateTime.Today,
                    UserID = Operator?.ID ?? 0,
                    ObjectID = Object?.ID ?? 0,
                    UserRealTime = DateTime.Now,
                    Type = 1,
                    TransactionNumber = "Предоплата",
                    EndDate = DateTime.Today,
                });

                _dbModel.Payments.Add(new Payments()
                {
                    Acct = nextAcct,
                    OperType = (int)OperationsEnum.AdvancePayment,
                    PartnerID = Partner?.ID ?? 0,
                    Qtty = posCount,
                    Mode = -1,
                    Sign = 1,
                    Date = DateTime.Today,
                    UserID = Operator?.ID ?? 0,
                    ObjectID = Object?.ID ?? 0,
                    UserRealTime = DateTime.Now,
                    Type = 1,
                    TransactionNumber = String.Empty,
                    EndDate = DateTime.Today,
                });

                _dbModel.CashBook.Add(new CashBook()
                {
                    Date = DateTime.Today,
                    Desc = $"Предоплата - Продажа #{nextAcct:D10}",
                    OperType = 10, //todo: WTF?
                    Sign = 1,
                    Profit = -1,
                    UserID = Operator?.ID ?? 0,
                    UserRealtime = DateTime.Now,
                    ObjectID = Object?.ID ?? 0,
                });
            });

            _dbModel.SaveChanges();

            return true;

            //ID Acct    OperType PartnerID   Qtty Mode    Sign Date    UserID ObjectID    UserRealTime Type    TransactionNumber EndDate
            //123 38  2   12857   100 - 1  1   2016 - 11 - 13 00:00:00 2   1   2016 - 11 - 13 19:39:19.373 1       2016 - 11 - 13 00:00:00
            //124 38  2   12857   100 1   1   2016 - 11 - 13 00:00:00 2   1   2016 - 11 - 13 19:39:19.377 1   Предоплата  2016 - 11 - 13 00:00:00
            //125 38  36  12857   100 - 1  1   2016 - 11 - 13 00:00:00 2   1   2016 - 11 - 13 19:39:19.380 1       2016 - 11 - 13 00:00:00
        }

        public void BlockAllCards()
        {
            _dbModel.Database.ExecuteSqlCommand("UPDATE dbo.Partners SET GoodTill = NULL, SecurityType = 2 ");
            
            //_dbModel.Partners.ForEach(p => { p.SecurityType = SecurityType.Blocked;
            //                                   p.GoodTill = null;});
            //partner.SecurityType = SecurityType.Blocked;

            _dbModel.SecurityEvents.Add(new SecurityEvents()
            {
                CardNumber = "",
                Comment = "Общая блокировка",
                Event = SecurityEventType.Blocked,
                Time = DateTime.Now,
            });

            _dbModel.SaveChanges();
        }
    }
}

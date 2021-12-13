using System;
using System.Collections.Generic;
using IvTx.Mi.DataBase.Enums;
using Octo.Rnd.RinkDesktop.Views;
using Prism.Commands;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public enum PaymentResultEnum { Cash, Card, Cancel, CancelAndClear }

    public class PaymentResult
    {
        public PaymentResult(PaymentResultEnum result)
        {
            Result = result;
        }

        public IDictionary<PaymentEnum, double> Payment { get; set; }

        public PaymentResultEnum Result { get; set; }
    }

    public class PayViewModel : ResultDialogViewModel<PayConfirmWindow, PaymentResult>
    {
        private readonly double _total;

        public PayViewModel(double total)
        {
            _total = total;
        }

        private DelegateCommand<PaymentEnum?> _acceptCommand;
        public DelegateCommand<PaymentEnum?> AcceptCommand => _acceptCommand ?? (_acceptCommand = new DelegateCommand<PaymentEnum?>(Accept, @enum => true));

        private void Accept(PaymentEnum? @enum)
        {
            Close();

            if (!@enum.HasValue)
                OnResult?.Invoke(new PaymentResult(PaymentResultEnum.Cancel));

            OnResult?.Invoke(new PaymentResult(@enum.GetValueOrDefault() == PaymentEnum.Cash ? PaymentResultEnum.Cash : PaymentResultEnum.Card)
            {
                Payment = new Dictionary<PaymentEnum, double>()
            {
                {@enum.Value, _total}
            }
            });
        }

        private DelegateCommand _cancelCommand;
        public DelegateCommand CancelCommand
        {
            get
            {
                return _cancelCommand ?? (_cancelCommand = new DelegateCommand(() => { Close(); OnResult?.Invoke(new PaymentResult(PaymentResultEnum.Cancel)); }));
            }
        }
        private DelegateCommand _cancelAndClearCommand;
        public DelegateCommand CancelAndClearCommand
        {
            get
            {
                return _cancelAndClearCommand ?? (_cancelAndClearCommand = new DelegateCommand(() => { Close(); OnResult?.Invoke(new PaymentResult(PaymentResultEnum.CancelAndClear)); }));
            }
        }
    }
}

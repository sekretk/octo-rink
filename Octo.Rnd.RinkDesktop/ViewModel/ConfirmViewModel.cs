using System;
using System.Collections.Generic;
using IvTx.Mi.DataBase.Enums;
using Octo.Rnd.RinkDesktop.Views;
using Prism.Commands;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public enum ConfirmResultEnum { Confirm, Cancel }

    public class ConfirmViewModel : ResultDialogViewModel<ConfirmWindow, ConfirmResultEnum>
    {
        private DelegateCommand<ConfirmResultEnum?> _confirmCommand;
        public DelegateCommand<ConfirmResultEnum?> ConfirmCommand => _confirmCommand ?? (_confirmCommand = new DelegateCommand<ConfirmResultEnum?>(Confirm, @enum => true));

        private void Confirm(ConfirmResultEnum? @enum)
        {
            Close();

            if (!@enum.HasValue)
            {
                OnResult?.Invoke(ConfirmResultEnum.Cancel);
                return;
            }

            OnResult?.Invoke(ConfirmResultEnum.Confirm);
        }

        private DelegateCommand _cancelCommand;
        public DelegateCommand CancelCommand
        {
            get
            {
                return _cancelCommand ?? (_cancelCommand = new DelegateCommand(() => { Close(); OnResult?.Invoke(ConfirmResultEnum.Cancel); }));
            }
        }
    }
}

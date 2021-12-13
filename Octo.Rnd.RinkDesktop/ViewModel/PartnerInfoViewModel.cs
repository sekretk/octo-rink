using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Threading;
using Framework.ComponentModel;
using Framework.UI;
using IvTx.Mi.DataBase;
using IvTx.Mi.DataBase.DB;
using Octo.Rnd.RinkDesktop.Views;
using Prism.Commands;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public enum ActivePartnerAction { Cancel, Block } 

    public class PartnerInfoViewModel : ResultDialogViewModel<ActivePartnerInfoView, ActivePartnerAction>
    {
        private readonly Partners _partner;
        private readonly DispatcherTimer _timer = new DispatcherTimer();
        public PartnerInfoViewModel(MIOperationFacade miOp, Partners partner)
        {
            miOp.Model.Entry(partner).Reload();
            _partner = partner;
            _timer.Interval = TimeSpan.FromMilliseconds(500);
            _timer.Tick += TimerOnTick;
            _timer.IsEnabled = true;
             
            Log.AddRange(miOp.LastCardSecurityEvents(partner.CardNumber).Where(c => c.Event != SecurityEventType.Read));
        }

        public ObservableCollection<SecurityEvents> Log { get; private set; } = new ObservableCollection<SecurityEvents>();

        private void TimerOnTick(object sender, EventArgs eventArgs)
        {
            OnPropertyChanged(() => LastMinutes);
        }

        public string Number => _partner?.CardNumber;

        public int LastMinutes => (int) _partner.GoodTill.GetValueOrDefault().Subtract(DateTime.Now).TotalMinutes;

        public DateTime? ExpireTime => _partner.GoodTill;

        public override void Close()
        {
            _timer.Tick -= TimerOnTick;

            _timer.Stop();

            OnResult?.Invoke(ActivePartnerAction.Cancel);

            base.Close();
        }

        private DelegateCommand _blockCardCommand;
        public DelegateCommand BlockCardCommand => _blockCardCommand ?? (_blockCardCommand = new DelegateCommand(BlockCard));

        private void BlockCard()
        {
            OnEnd(ActivePartnerAction.Block);
        }
    }
}

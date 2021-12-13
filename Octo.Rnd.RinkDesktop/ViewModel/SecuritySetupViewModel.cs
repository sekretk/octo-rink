using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Framework;
using Framework.UI.Input;
using IvTx.Core.Helpers;
using IvTx.Core.Log;
using IvTx.Mi.DataBase;
using IvTx.Mi.DataBase.Enums;
using Octo.Rnd.RinkDesktop.Model;
using Octo.Rnd.RinkDesktop.Views;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public class SecuritySetupViewModel : ResultDialogViewModel<SecuritySetupView, bool>
    {
        private readonly MIOperationFacade _miOperation;
        private readonly RinkDesktopConfig _config;

        public ObservableCollection<Goods> ObservableEntranceItems { get; set; }

        public Stack<Goods> EntranceItems { get; set; }

        public Goods CurrentGood { get; set; }

        private string _number;
        public string Number
        {
            get { return _number; }
            set { _number = value; OnPropertyChanged(); }
        }

        private string _status;
        public string Status
        {
            get { return _status; }
            set { _status = value; OnPropertyChanged(); }
        }

        public SecuritySetupViewModel(IEnumerable<Position> positions, MIOperationFacade _miOperation, RinkDesktopConfig _config)
        {
            this._miOperation = _miOperation;
            this._config = _config;
            var result = new List<Goods>();
            ObservableEntranceItems = new ObservableCollection<Goods>();

            EnumerableHelper.ForEach(positions, position =>
            {
            if (position.Quantity> 0 && position.Good.MinQtty > 0)
                    result.AddRange(Enumerable.Repeat(position.Good, (int) position.Quantity));
            });

            EntranceItems = new Stack<Goods>(result);

            ObservableEntranceItems.AddRange(EntranceItems);

            CurrentGood = EntranceItems.Peek();
        }

        private DelegateCommand _findCommand;
        public DelegateCommand FindCommand => _findCommand ?? (_findCommand = new DelegateCommand(FindCard));

        private DelegateCommand _cancelCommand;
        public DelegateCommand CancelCommand => _cancelCommand ?? (_cancelCommand = new DelegateCommand(Cancel));

        private void Cancel()
        {
            Close();
        }

        private void FindCard()
        {
            if (CurrentGood == null)
            {
                return;
            }

            if (CurrentGood.MinQtty == 0)
            {
                return;
            }

            var number = Number;

            Number = String.Empty;

            var foundPartner = _miOperation.Model.Partners.FirstOrDefault(p => p.CardNumber == number);

            if (foundPartner == null)
            {
                Status = $"Не найден партнер с номером карты {number}";
                Logger.Warning(Status);

                return;
            }

            _miOperation.Model.Entry(foundPartner).Reload();

            if (foundPartner.GroupID != _config.PassGroup)
            {
                Status = $"Партнер из другой группы.";
                Logger.Warning(Status);
                return;
            }

            if (foundPartner.SecurityType == SecurityType.Inited && foundPartner.GoodTill > DateTime.Now)
            {
                Status = $"Партнер активен.";
                Logger.Warning(Status);
                return;
            }

            var entranceTill = DateTime.Now.AddMinutes((CurrentGood.MinQtty + _config.ToleranceInMin) ?? 0);

            //if (foundPartner.GoodTill > DateTime.Today && foundPartner.GoodTill<DateTime.Now)
            //            CurrentStatus = $"Окончание предыдущего периода было {foundPartner.GoodTill:HH:mm} - {DateTime.Now.Subtract(foundPartner.GoodTill.GetValueOrDefault()).TotalMinutes:F0} минут назад";

            _miOperation.SetPartnerValidity(foundPartner, entranceTill, $"Проход разрешен по билету: {CurrentGood.Name}. Проход разрешен до {entranceTill:HH:mm}");

            Status = $"Проход разрешен по билету: {CurrentGood.Name}. Проход разрешен до {entranceTill:HH:mm}";

            var handledPartner = EntranceItems.Pop();

            if (EntranceItems.Any())
            {
                CurrentGood = EntranceItems.Peek();
                ObservableEntranceItems.Remove(handledPartner);
            }
            else
                Close();
        }

        //if (foundPartner.GoodTill > DateTime.Today && foundPartner.GoodTill<DateTime.Now)
        //            CurrentStatus = $"Окончание предыдущего периода было {foundPartner.GoodTill:HH:mm} - {DateTime.Now.Subtract(foundPartner.GoodTill.GetValueOrDefault()).TotalMinutes:F0} минут назад";


        //_dbOperations.SetPartnersValidity(CardPartners, entranceTill, $"Проход разрешен по билету: {EntranceAmountGood?.Good?.Name}. Проход разрешен до {entranceTill:HH:mm}");

        //CardPartners.ForEach(cp =>
        //                    Logger.Info($"Партнеру {cp.ID} карта #{cp.CardNumber} Проход разрешен до {cp.GoodTill:t}"));

        //var entranceTill = DateTime.Now.AddMinutes((EntranceAmountGood?.Good?.MinQtty + _config.ToleranceInMin) ?? 0);
        //entranceTill = entranceTill >= DateTime.Today.AddDays(1) ? DateTime.Today.AddDays(1).AddMinutes(-1) : entranceTill;
    }
}

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Threading;
using IvTx.Core;
using IvTx.Core.Config;
using IvTx.Core.Log;
using IvTx.Mi.DataBase;
using IvTx.Mi.DataBase.DB;
using Prism.Commands;
using Prism.Mvvm;

namespace Octo.Rnd.CheckCard
{
    public class MainWindowViewModel : BindableBase
    {
        private const int EVENTS_COUNT = 50;

        private readonly DispatcherTimer _timer = new DispatcherTimer();

        public MainWindowViewModel()
        {
            _connection = new SqlConnection(System.Configuration.ConfigurationManager.ConnectionStrings["IceRinkModel"].ConnectionString);

            _connection.Open();

            _timer.Interval = TimeSpan.FromMilliseconds(500);
            _timer.Tick += TimerOnTick;
            _timer.IsEnabled = true;
        }

        private void TimerOnTick(object sender, EventArgs eventArgs)
        {
            OnPropertyChanged(() => LastMinutes);
        }

        private Partners _foundPartner;
        public Partners FoundPartner
        {
            get { return _foundPartner; }
            set
            {
                _foundPartner = value;
                OnPropertyChanged();
                OnPropertyChanged(() => LastMinutes);
                OnPropertyChanged(() => ExpireTime);
            }
        }

        private string _number;

        public string Number
        {
            get { return _number; }
            set { _number = value; OnPropertyChanged(); }
        }

        private DelegateCommand _clearCommand;
        public DelegateCommand ClearCommand => _clearCommand ?? (_clearCommand = new DelegateCommand(Clear));

        public ObservableCollection<SecurityEvents> Log { get; private set; } = new ObservableCollection<SecurityEvents>();

        private DelegateCommand _findNumberCommand;
        public DelegateCommand FindNumberCommand => _findNumberCommand ?? (_findNumberCommand = new DelegateCommand(Find));

        private void Find()
        {
            Failover.Execute(() =>
            {
                Log.Clear();

                Message = String.Empty;

                using (var dbModel = new Model(_connection, false))
                {
                    FoundPartner = dbModel.Partners.FirstOrDefault(p => p.CardNumber == Number);
                    if (FoundPartner == null)
                        Message = $"Партнер с картой {Number} не найден";

                    bool stop = false;

                    Log.AddRange(
                        dbModel.SecurityEvents.Where(e => e.CardNumber == Number && e.Event != SecurityEventType.Read).OrderByDescending(e => e.Time).Take(EVENTS_COUNT).ToList().TakeWhile(x =>
                        {
                            var result = stop;
                            stop = x.Event == SecurityEventType.Init;
                            return !result;
                        })
                        );

                    Number = String.Empty;
                }
            }, exception =>
            {
                Clear();
                Message = "Ошибка выполнения";
                Logger.Error(exception, "Ошибка при заполнении истории карты");
            });
        }

        private string _message;
        private SqlConnection _connection;

        public string Message
        {
            get { return _message; }
            set { _message = value; OnPropertyChanged(); }
        }

        public int LastMinutes => _foundPartner == null ? 0 : (int)_foundPartner.GoodTill.GetValueOrDefault().Subtract(DateTime.Now).TotalMinutes;

        public DateTime? ExpireTime => _foundPartner?.GoodTill;

        private void Clear()
        {
            Failover.Execute(() =>
            {
                FoundPartner = null;
                Log.Clear();
                Message = "Очищено";
            }, exception =>
            {
                Message = "Ошибка очистки";
                Logger.Error(exception, "Ошибка при очистке");
            });
        }
    }
}

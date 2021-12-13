using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Threading;
using Framework.UI.Controls;
using IvTx.Core;
using IvTx.Core.Config;
using IvTx.Core.Helpers;
using IvTx.Core.Log;
using IvTx.Desktop;
using IvTx.IronLogic.Facade;
using IvTx.IronLogic.Facade.Enums;
using IvTx.Mi.DataBase;
using IvTx.Mi.DataBase.DB;
using IvTx.Mi.DataBase.Enums;
using Octo.Rnd.GateControl.Settings;
using Prism.Commands;
using Prism.Mvvm;
using Window = System.Windows.Window;

namespace Octo.Rnd.GateControl.ViewModel
{
    public class MainViewModel : BindableBase
    {
        private const int UI_LOG_SIZE = 30;

        private readonly Window _wnd;
        //private Model _dbModel;
        private GateControlConfig _config;
        private GateControlSettings _settings;
        private readonly FixedSizeObservableCollection<string> _mainLogBuilder = new FixedSizeObservableCollection<string>(UI_LOG_SIZE);
        private ICvrtHandler _cvrtHandler;

        public MainViewModel(Window wnd)
        {
            _wnd = wnd;

            Add2Log("Работа с приложением начата");

            Logger.Logging += (sender, info) => Application.Current.Dispatcher.Invoke(() => Add2Log(info.Message));

            DispatcherTimer t = new DispatcherTimer();
            t.Interval = TimeSpan.FromMinutes(10);
            t.Tick += (sender, args) => ClearLogs();
            t.Start();

            Failover.Execute(() =>
            {
                _connection = new SqlConnection(System.Configuration.ConfigurationManager.ConnectionStrings["IceRinkModel"].ConnectionString);

                _connection.Open();

                using (var dbModel = new Model(_connection, false))
                {
                    var v = dbModel.Partners.FirstOrDefault()?.ID;

                    Logger.Info($"Иницирована работа с базой {v}");

                    _config = AppConfig.Get<GateControlConfig>();

                    _settings = XMLHelper.Read<GateControlSettings>();

                    Add2Log($"Инициализированно следующими настройками: База: {_connection.ConnectionString}; Конфиг: {AppConfig.SerializeToString(_config)}");
                } 

                _cvrtHandler = new CvrtHandler(_config.Port, _config.Type, _config.Controllers, GateAcceptor, GateListener);
                //_cvrtHandler = new DummyCvrtHandler(GateAcceptor, GateListener, new []{ "09047735" });

                _cvrtHandler.StartMonitoringEvents();

                Application.Current.Exit += (sender, args) =>
                {
                    Logger.Info("Иницирована отписка от мониторинга");
                    _cvrtHandler.Dispose();
                    Logger.Info("Приложение закрыто");
                };


            }, exception =>
            {
                Logger.Error("Ошибка при инициализации", exception);
            });
        }

        private readonly object _lockObject = new object();

        private bool GateAcceptor(CtrlEvent @event)
        {
            var crd = string.Concat(_config.CardPrefix, @event.Key);

            SqlCommand cmd = _connection.CreateCommand();
            cmd.CommandText = "select SecurityType, GoodTill, GroupID From Partners Where CardNumber = @cardNumber";

            cmd.Parameters.AddWithValue("cardNumber", crd);

            using (SqlDataReader reader = cmd.ExecuteReader())
            {
                if (reader.Read())
                {
                    var sectype = reader["SecurityType"] == DBNull.Value ? SecurityType.Blocked : (SecurityType)reader["SecurityType"];
                    var goodTill = reader["GoodTill"] == DBNull.Value ? DateTime.MinValue : (DateTime?)reader["GoodTill"];

                    var groupId = reader["GroupID"] == DBNull.Value ? -1 : (int)reader["GroupID"];

                    if (groupId == _config.ServiceGroup)
                    {
                        Logger.Info($"Сервисная карта обнаружена {crd}");
                        return true;
                    }

                    if (goodTill < DateTime.Now)
                    {
                        Logger.Info($"Время прохода карты {crd} закончилось");
                        return false;
                    }

                    //https://ivtexx.atlassian.net/browse/IC-1
                    if (_config.NoStateControl)
                        return true;

                    if (sectype == SecurityType.Blocked) //Allow anlimited usage of Gates it available time
                    {
                        Logger.Info($"Карта {crd} заблокирована");
                        return false;
                    }

                    if (@event.Direction == Direction.In)
                        return sectype != SecurityType.In;

                    if (@event.Direction == Direction.Out)
                        return sectype == SecurityType.In;
                }

                Logger.Info($"Не найден партнер с картой {crd}");
                return false;
            }
        }

        private void GateListener(CtrlEvent @event)
        {
            var cNumber = string.Concat(_config.CardPrefix, @event.Key);

            using (var dbModel = new Model())
            {
                EventType type = @event.EventType;

                if (@event.Controller.Type == ControllerType.Door && @event.AccessGranted && (type == EventType.KeyNotFoundInStorage || type == EventType.UnknownKey))
                    type = EventType.EntranceSuccess;

                switch (type)
                {
                    case EventType.EntranceSuccess:
                        dbModel.SecurityEvents.Add(new SecurityEvents()
                                                    {
                                                        CardNumber = cNumber,
                                                        ControllerID = @event.Controller.SerialNumber.ToString(),
                                                        Event =
                                                            @event.Direction == Direction.In
                                                                ? SecurityEventType.Entrance
                                                                : SecurityEventType.Exit,
                                                        Time = DateTime.Now,
                                                    });

                        var partner =
                            dbModel.Partners.FirstOrDefault(p => p.CardNumber == cNumber);

                        if (partner == null)
                        {
                            Logger.Warning($"Карта #{cNumber} не найдена");
                            break;
                        }

                        if (partner.GroupID != _config.ServiceGroup && @event.Direction == Direction.Out)
                        {
                            dbModel.SecurityEvents.Add(new SecurityEvents()
                            {
                                CardNumber = cNumber,
                                ControllerID = @event.Controller.SerialNumber.ToString(),
                                Event = SecurityEventType.Blocked,
                                Time = DateTime.Now,
                            });

                            partner.SecurityType = SecurityType.Blocked; //blocking on out

                            break;
                        }

                        if (@event.Direction == Direction.In)
                            partner.SecurityType = SecurityType.In;

                        if (@event.Direction == Direction.Out)
                            partner.SecurityType = SecurityType.Out;

                        break;
                    case EventType.KeyNotFoundInStorage:
                    case EventType.UnknownKey:
                        dbModel.SecurityEvents.Add(new SecurityEvents()
                                                    {
                                                        CardNumber = string.Concat(_config.CardPrefix, @event.Key),
                                                        Event = SecurityEventType.Read,
                                                        ControllerID = @event.Controller.SerialNumber.ToString(),
                                                        //todo: change to int
                                                        Time = DateTime.Now,
                                                    });
                        break;
                }

                dbModel.SaveChanges();
            }
        }
    

        public IEnumerable<string> MainLog
        {
            get { return _mainLogBuilder; }
        }

        private DelegateCommand _openTurnstileCommand;
        public DelegateCommand OpenTurnstileCommand => _openTurnstileCommand ?? (_openTurnstileCommand = new DelegateCommand(OpenGates));

        private DelegateCommand _сloseCommand;
        public DelegateCommand CloseCommand => _сloseCommand ?? (_сloseCommand = new DelegateCommand(Close));

        private void Close()
        {
            _connection.Close();
            Confirmation("Закрыть приложение?", () => Application.Current.Shutdown(0));
        }

        private bool _fireModeOn;
        public bool FireModeOn
        {
            get { return _fireModeOn; }
            set { _fireModeOn = value; OnPropertyChanged(); }
        }

        private DelegateCommand<bool?> _fireModeCommand;
        public DelegateCommand<bool?> FireModeCommand => _fireModeCommand ?? (_fireModeCommand = new DelegateCommand<bool?>(FireModeChange));

        private void FireModeChange(bool? obj)
        {
            if (!obj.GetValueOrDefault())
            {
                FireModeOn = false;
                _cvrtHandler.TurnFireMode(false); return;
            }

            Confirmation("Включить пожарный режим?", () => { _cvrtHandler.TurnFireMode(true); FireModeOn = true; }, () => FireModeOn = false);
        }

        private DelegateCommand _clearLogCommand;
        private SqlConnection _connection;
        public DelegateCommand ClearLogCommand => _clearLogCommand ?? (_clearLogCommand = new DelegateCommand(ClearLogs));

        private void ClearLogs()
        {
            _mainLogBuilder.Clear();
            OnPropertyChanged(() => MainLog);
        }

        private void OpenGates()
        {
            Confirmation("Открыть турникет?", _cvrtHandler.OpenGates);
        }

        private void Add2Log(string txt)
        {
            _mainLogBuilder.Add($"{DateTime.Now:G} - {txt}. {Environment.NewLine}");
            OnPropertyChanged(() => MainLog);
        }

        private async void Confirmation(string txt, Action action, Action negativeAction = null, string header = "Подтверждение")
        {
            MessageBoxResult result = await MessageDialog.ShowAsync(
                header,
                txt,
                MessageBoxButton.YesNo, MessageDialogType.Accent, _wnd);

            if (result == MessageBoxResult.Yes)
                action();
            else
            {
                negativeAction?.Invoke();
            }
        }


    }
}

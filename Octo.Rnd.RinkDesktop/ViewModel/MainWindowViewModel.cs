using System;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using dbfirstplay;
//using Framework;
using Framework.UI.Controls;
//using Framework.UI.Input;
using IvTx.Core;
using IvTx.Core.Config;
using IvTx.Core.Helpers;
using IvTx.Desktop;
using IvTx.Desktop.Helpers;
using IvTx.Desktop.ViewModel;
using IvTx.Mi.DataBase;
using IvTx.Mi.DataBase.DB;
using IvTx.Mi.DataBase.Enums;
using IvTx.MI.Devices;
using NLog;
using Octo.Rnd.RinkDesktop.Model;
using Prism.Commands;
using Logger = IvTx.Core.Log.Logger;
using Position = Octo.Rnd.RinkDesktop.Model.Position;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public enum MainState
    {
        [Description("Готов")]
        Ready,
        [Description("Чек без прохода")]
        BillMode,
        [Description("Использование абонемента")]
        Subscription,
        [Description("Ошибка")]
        Failed
    }

    public class MainWindowViewModel : WindowViewModelBase
    {
        private RinkARMSettings _settings;

        private Partners _defaultPartner;

        private Partners _currentPartner;
        public Partners CurrentPartner
        {
            get { return _currentPartner; }
            set
            {
                _currentPartner = value;
                RecalculatePrice();
                _dbOperations.Partner = value;
                OnPropertyChanged();
            }
        }

        private Users _defaultOperator;

        private Users _currentOperator;
        public Users Operator
        {
            get
            { return _currentOperator; }
            set
            {
                _currentOperator = value; OnPropertyChanged();
                _dbOperations.Operator = value;
            }
        }

        private MIOperationFacade _dbOperations;
        private RinkDesktopConfig _config;

        private MainState _mainState;

        public MainState MainState
        {
            get
            {
                return _mainState;
            }
            set
            {
                _mainState = value;
                OnPropertyChanged();
                if (value == MainState.Failed)
                    new LogViewModel(Log, MainState).Show();
            }
        }

        private string _number;
        public string Number { get { return _number; } set { _number = value; OnPropertyChanged(); } }

        private string _currentStatus;
        public string CurrentStatus
        {
            get { return _currentStatus; }
            set { _currentStatus = value; OnPropertyChanged(); }
        }

        private string _validationMsg;
        public string ValidationMsg
        {
            get { return _validationMsg; }
            set { _validationMsg = value; OnPropertyChanged(); }
        }

        public ObservableCollection<Goods> ActiveItems { get; } = new ObservableCollection<Goods>();

        private DelegateCommand<Goods> _addItemCommand;
        public DelegateCommand<Goods> AddItemCommand => _addItemCommand ?? (_addItemCommand = new DelegateCommand<Goods>(AddItem));

        private void AddItem(Goods item)
        {
            //todo: check item qty
            var strItem = _dbOperations.Model.Store.FirstOrDefault(x => x.GoodID == item.ID && x.ObjectID == _config.ObjectID);

            if (strItem == null)
            {
                Logger.Warning($"Нет данных о количестве товара '{item.ID} - {item.Name}' на складе {_config.ObjectID}");
                return;
            }

            if (MainState == MainState.Subscription)
            {

                if (!_config.SubscriptionGoods.Contains(item.ID))
                {
                    Logger.Warning($"Запрешенный для абонемента товар '{item.Name}'");
                }

                if (Positions.Sum(p => p.Quantity) >= (double) _subscribrionBalance)
                {
                    Logger.Warning($"Абонемент ограничен {_subscribrionBalance:N0} билетами");
                    return;
                }
            }

            var bi = Positions.FirstOrDefault(_ => _.GoodID == item.ID);

            if (bi != null)
            {
                if (strItem.Qtty < bi.Quantity + 1)
                {
                    Logger.Warning($"Для товара '{item.Name}' нет достаточного количества на складе {_config.ObjectID}");
                    return;
                }

                bi.Quantity += 1; //todo: check item qty
                DeviceManagerFacade.ShowOnVFD(bi.Name, $"{bi.Price} x {bi.Quantity:N0}");
            }
            else
            {
                if (strItem.Qtty < 1)
                {
                    Logger.Warning($"Для товара '{item.Name}' нет достаточного количества на складе {_config.ObjectID}");
                    return;
                }

                var pos = new Position(item, _dbOperations.Model.VATGroups.First(_ => _.ID == item.TaxGroup));
                SetPrice(pos);
                Positions.Add(pos);
                DeviceManagerFacade.ShowOnVFD(pos.Name, $"{pos.Price} x {pos.Quantity:N0}");
            }

            UpdateState();

            OnPropertyChanged(() => Total);
            ProceedBillCommand.RaiseCanExecuteChanged();
        }

        public ObservableCollection<Position> _positions;
        public ObservableCollection<Position> Positions => _positions ?? (_positions = new ObservableCollection<Position>());


        public FixedSizeObservableCollection<LogEventInfo> Log { get; } = new FixedSizeObservableCollection<LogEventInfo>(100);

        public MainWindowViewModel()
        {
            Logger.Logging += (sender, info) =>
            {
                Application.Current.Dispatcher.Invoke(() =>
                                                      {
                                                          Log.Add(info);

                                                          ValidationMsg = info.FormattedMessage;

                                                          if (info.Level >= LogLevel.Warn)
                                                              ShowNotification(info.FormattedMessage, true);
                                                      });
            };

            Logger.Info("Старт приложения");

            Init();
        }

        private async void Init()
        {
            Loading = true;

            await Task.Run(() =>
            {
                Failover.Execute(() =>
                                 {
                                     Logger.Info("Инициация настроек и подключения к базе данных");

                                     _dbOperations = new MIOperationFacade();

                                     _config = AppConfig.Get<RinkDesktopConfig>();

                                     _settings = XMLHelper.Read<RinkARMSettings>();

                                     _dbOperations.Currency = _dbOperations.Model.Currencies.FirstOrDefault();//todo take from settings

                                     CurrentPartner = _defaultPartner = _dbOperations.Model.Partners.FirstOrDefault(p => p.ID == _config.PartnerID);

                                     Operator = _defaultOperator = _dbOperations.Model.Users.FirstOrDefault(p => p.ID == _config.UserID);

                                     _dbOperations.Partner = _defaultPartner;

                                     _dbOperations.Object = _dbOperations.Model.Objects.FirstOrDefault(p => p.ID == _config.ObjectID);

                                     Application.Current.Dispatcher.Invoke(() =>
                                                                         {
                                                                             Groups.AddRange(_config.Groups.Select(g => _dbOperations.Model.GoodsGroups.FirstOrDefault(x => x.ID == g)));

                                                                             SelectedGroup = Groups.FirstOrDefault();

                                                                             LogOut();
                                                                         });
                                 }, exception =>
                                 {
                                     Logger.Error(exception, "Ошибка инициализации приложения");
                                     Application.Current.Dispatcher.Invoke(() => MainState = MainState.Failed);
                                 });
            });

            Loading = false;
        }

        private DelegateCommand _findNumberCommand;
        public DelegateCommand FindNumberCommand => _findNumberCommand ?? (_findNumberCommand = new DelegateCommand(Find));

        private DelegateCommand<Position> _decreaseCountCommand;
        public DelegateCommand<Position> DecreaseCountCommand { get { return _decreaseCountCommand ?? (_decreaseCountCommand = new DelegateCommand<Position>((i) => ChangeCount(i, false))); } }

        private DelegateCommand<Position> _increaseCountCommand;
        public DelegateCommand<Position> IncreaseCountCommand { get { return _increaseCountCommand ?? (_increaseCountCommand = new DelegateCommand<Position>((i) => ChangeCount(i, true))); } }

        private DelegateCommand<Position> _removeBillItemCountCommand;
        public DelegateCommand<Position> RemoveBillItemCountCommand => _removeBillItemCountCommand ?? (_removeBillItemCountCommand = new DelegateCommand<Position>(RemoveBillItem));

        private DelegateCommand _proceedBillCommand;
        public DelegateCommand ProceedBillCommand => _proceedBillCommand ?? (_proceedBillCommand = new DelegateCommand(ProceedBill, () => Positions.Any()));

        private DelegateCommand _сlearCommand;
        public DelegateCommand ClearCommand => _сlearCommand ?? (_сlearCommand = new DelegateCommand(Clear));

        private DelegateCommand _logoutCommand;
        public DelegateCommand LogoutCommand => _logoutCommand ?? (_logoutCommand = new DelegateCommand(LogOut));

        private void LogOut()
        {
            var vm = new LogonViewModel(_dbOperations.Model);
            vm.Show(u =>
            {
                Operator = u;
            });
        }

        private void ProceedBill()
        {
            if (!Positions.Any())
                return;

            _dbOperations.Partner = _currentPartner;

            if (MainState == MainState.Subscription)
            {
                if (Positions.Sum(x => x.Quantity) > (double)_dbOperations.PartnerBalance(_currentPartner?.ID ?? 0))
                {
                    Logger.Warning("На абонементе недостаточно баланса!");
                    Clear();
                    return;
                }

                Failover.Execute(() =>
                {
                    DeviceManagerFacade.PrintNonFiscalT(_currentOperator?.Name ?? string.Empty, Positions);

                    if (!_dbOperations.UsePrepaiedAmount(Positions))
                        return;

                    if (Positions.Any(x => x.Good.MinQtty > 0))
                    {
                        SecuritySetupViewModel securitySetVM = new SecuritySetupViewModel(Positions, _dbOperations, _config);
                        securitySetVM.Show(b =>
                        {

                        });
                    }

                    Clear();
                }, exception => Logger.Error(exception, "Ошибка при продаже абонемента"));
                return;
            }

            DeviceManagerFacade.ShowOnVFD("Оплата", $"{Total:N0}");

            //normal sale
            Failover.Execute(() =>
            {
                PayViewModel vm = new PayViewModel(Total);

                vm.Show(result =>
                {
                    switch (result.Result)
                    {
                        case PaymentResultEnum.CancelAndClear:
                            Logger.Info("Оплата чека отменена. Очистка");
                            Clear();
                            break;
                        case PaymentResultEnum.Cancel:
                            Logger.Info("Оплата чека отменена.");
                            break;
                        case PaymentResultEnum.Cash:

                            Logger.Info("Оплата наличными.");

                            Logger.Info("Отправлено на печать в ККМ.");
                            var v = DeviceManagerFacade.PrintFiscalT(_currentOperator?.Name ?? string.Empty, Positions, _config.Template, PaymentType.Cash);

                            if (!v) return;

                            _dbOperations.Sale(Positions, result.Payment.Select(p => new PayItem() { Amount = p.Value, Type = p.Key }));

                            if (Positions.Any(x => x.Good.MinQtty > 0))
                            {
                                SecuritySetupViewModel securitySetVM = new SecuritySetupViewModel(Positions, _dbOperations, _config);
                                securitySetVM.Show(b =>
                                {
                                    
                                });
                            }

                            Clear();

                            break;

                        case PaymentResultEnum.Card:

                            Logger.Info("Оплата безналичной оплатой.");

                            Logger.Info("Отправлено на печать в фискальный ККМ.");
                            var r = DeviceManagerFacade.PrintFiscalT(_currentOperator?.Name ?? string.Empty, Positions, _config.Template, PaymentType.Card);

                            if (!r) return;

                            _dbOperations.Sale(Positions, result.Payment.Select(p => new PayItem() { Amount = p.Value, Type = p.Key }));

                            Logger.Info("Отправлено на печать в не фискальный ККМ.");
                            DeviceManagerFacade.PrintNonFiscalT(_currentOperator?.Name ?? string.Empty, Positions);
                            
                            if (Positions.Any(x => x.Good.MinQtty > 0))
                            {
                                SecuritySetupViewModel securitySetVM = new SecuritySetupViewModel(Positions, _dbOperations, _config);
                                securitySetVM.Show(b =>
                                {

                                });
                            }

                            Clear();

                            break;
                    }
                });
            }, exception => Logger.Error(exception, "Ошибка при продаже товара"));
        }

        private void Clear()
        {
            DeviceManagerFacade.ClearVFD();

            _currentPartner = _defaultPartner;
            
            OnPropertyChanged(() => CurrentPartner);

            Positions.Clear();

            CurrentStatus = _number = String.Empty;

            OnPropertyChanged(() => Number);

            _subscribrionBalance = 0;

            UpdateState();
        }

        private void RemoveBillItem(Position item)
        {
            Positions.Remove(item);

            OnPropertyChanged(() => Total);
            ProceedBillCommand.RaiseCanExecuteChanged();

            UpdateState();
        }

        private void ChangeCount(Position item, bool b)
        {
            if (MainState == MainState.Subscription)
                return;

            if (b)
                item.Quantity += 1;
            else if (item.Quantity > 1)
                item.Quantity -= 1;

            DeviceManagerFacade.ShowOnVFD(item.Name, $"{item.Price} x {item.Quantity:N0}");

            UpdateState();
        }

        public ObservableCollection<GoodsGroups> Groups { get; } = new ObservableCollection<GoodsGroups>();

        private GoodsGroups _selectedGroup;
        public GoodsGroups SelectedGroup
        {
            get { return _selectedGroup; }
            set
            {
                _selectedGroup = value; OnPropertyChanged(); ItemsPopulation();
            }
        }

        private void ItemsPopulation()
        {
            ActiveItems.Clear();
            ActiveItems.AddRange(SelectedGroup != null
                ? _dbOperations.Model.Goods.Where(_ => _.GroupID == SelectedGroup.ID).ToList()
                : Enumerable.Empty<Goods>());
        }

        public double Total
        {
            get { return Positions.Sum(x => x.TotalPrice); }
        }

        private DelegateCommand _logCommand;
        public DelegateCommand LogCommand => _logCommand ?? (_logCommand = new DelegateCommand(ShowLog));

        private DelegateCommand _returnCommand;
        public DelegateCommand ReturnCommand => _returnCommand ?? (_returnCommand = new DelegateCommand(Return));

        private DelegateCommand _blockAllCommand;
        public DelegateCommand BlockAllCommand => _blockAllCommand ?? (_blockAllCommand = new DelegateCommand(BlockAll));
        

        private DelegateCommand _settingsCommand;
        private decimal _subscribrionBalance;
        public DelegateCommand SettingsCommand => _settingsCommand ?? (_settingsCommand = new DelegateCommand(ShowSettings));

        private void ShowSettings()
        {
            new SettingsViewModel(Operator).Show();
        }

        private void ShowLog()
        {
            new LogViewModel(Log, MainState).Show();
        }

        private void Find()
        {
            var number = Number;

            Logger.Info($"Введен номер {number}");

            Number = String.Empty;

            var good = _dbOperations.Model.Goods.FirstOrDefault(x => x.BarCode1 == number && x.Deleted != -1);

            if (good != null)
            {
                Logger.Info($"Найден товар по коду: {number}");

                _dbOperations.Model.Entry(good).Reload();

                AddItem(good);

                return;
            }

            var foundPartner = _dbOperations.Model.Partners.FirstOrDefault(p => p.CardNumber == number);

            if (foundPartner == null)
            {
                Logger.Warning($"Не найден партнер с номером карты {number}");

                return;
            }

            _dbOperations.Model.Entry(foundPartner).Reload();

            if (foundPartner.GroupID == _config.PassGroup)
            {
                var vm = new PartnerInfoViewModel(_dbOperations, foundPartner);

                vm.Show(action =>
                {
                    switch (action)
                    {
                        case ActivePartnerAction.Block:
                            _dbOperations.BlockCard(foundPartner);
                            break;
                    }
                });

                return;
            }

            if (foundPartner.GroupID == _config.SubscriptionPartnerGroup)
            {
                Clear();

                _subscribrionBalance = _dbOperations.PartnerBalance(foundPartner.ID);

                CurrentStatus = $"По абонементу доступно {_subscribrionBalance:F0} билетов";
            }

            CurrentPartner = foundPartner;

            UpdateState();
        }

        private void RecalculatePrice()
        {
            foreach (var position in Positions)
                SetPrice(position);

            OnPropertyChanged(() => Total);
        }

        private void ShowNotification(string message, bool isError, string caption = null)
        {
            NotifyBox.Show(
                  (DrawingImage)Application.Current.Resources.FindResource(isError ? "DeleteDrawingImage" : "SearchDrawingImage"),
                  isError ? "Ошибка" : caption,
                  message, TimeSpan.FromSeconds(1));
        }


        private void UpdateState()
        {
            if (MainState == MainState.Failed) //terminated state
                return;
            
            var pos = Positions.FirstOrDefault(x => x.Good.MinQtty > 0);
            
            if (Positions.Count == 0 && _currentPartner.GroupID != _config.SubscriptionPartnerGroup)
                MainState = MainState.Ready;
            
            if (_currentPartner.GroupID == _config.SubscriptionPartnerGroup)
                MainState = MainState.Subscription;

            if (_currentPartner.GroupID != _config.SubscriptionPartnerGroup)
                MainState = MainState.BillMode;

            OnPropertyChanged(() => Total);
        }

        private void SetPrice(Position position)
        {
            position.Price = position.Good?.PriceOut2 ?? 0;

            switch (CurrentPartner.PriceGroup)
            {
                case 1: position.Price = position.Good?.PriceOut2 ?? 0; break;
                case 2: position.Price = position.Good?.PriceOut3 ?? 0; break;
                case 3: position.Price = position.Good?.PriceOut4 ?? 0; break;
                default: position.Price = position.Good?.PriceOut2 ?? 0; break;
            }
        }

        private void BlockAll()
        {
            Failover.Execute(() =>
            {
                ConfirmViewModel vm = new ConfirmViewModel();
                vm.Show(result =>
                {
                    if (result == ConfirmResultEnum.Confirm)
                        _dbOperations.BlockAllCards();
                });
            });
        }

        private void Return()
        {
            //normal return
            Failover.Execute(() =>
            {
                PayViewModel vm = new PayViewModel(Total);

                vm.Show(result =>
                {
                    switch (result.Result)
                    {
                        case PaymentResultEnum.CancelAndClear:
                            Logger.Info("Возврат отменен. Очистка");
                            Clear();
                            break;
                        case PaymentResultEnum.Cancel:
                            Logger.Info("Возврат отменен.");
                            break;
                        case PaymentResultEnum.Cash:

                            Logger.Info("Возврат наличных.");

                            Logger.Info("Отправлено на печать в ККМ.");
                            var v = DeviceManagerFacade.PrintReturnT(_currentOperator?.Name ?? string.Empty, Positions, _config.Template, true, PaymentType.Cash);

                            if (!v) return;

                            _dbOperations.Return(Positions, PaymentEnum.Cash);

                            Clear();

                            break;

                        case PaymentResultEnum.Card:

                            Logger.Info("Возврат безналичной оплаты.");

                            Logger.Info("Отправлено на печать в фискальный ККМ.");
                            var r = DeviceManagerFacade.PrintReturnT(_currentOperator?.Name ?? string.Empty, Positions, _config.Template, false, PaymentType.Card);

                            if (!r) return;

                            //Logger.Info("Отправлено на печать в не фискальный ККМ.");
                            //DeviceManagerFacade.PrintReturnT(_currentOperator?.Name ?? string.Empty, Positions,);

                            _dbOperations.Return(Positions, PaymentEnum.Card);

                            Clear();

                            break;
                    }
                });
            }, exception => Logger.Error(exception, "Ошибка при продаже товара"));
        }
    }
}

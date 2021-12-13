using System.Linq;
using IvTx.Core;
using IvTx.Core.Config;
using IvTx.Core.Log;
using IvTx.Mi.DataBase;
using IvTx.Mi.DataBase.DB;
using IvTx.MI.Devices;
using Octo.Rnd.RinkDesktop.Model;
using Octo.Rnd.RinkDesktop.Views;
using Prism.Commands;
using Position = Octo.Rnd.RinkDesktop.Model.Position;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public class SettingsViewModel : DialogViewModel<SettingsWindow>
    {
        private readonly Users _operator;
        private readonly RinkDesktopConfig _config;

        public SettingsViewModel(Users @operator)
        {
            _operator = @operator;
            _config = AppConfig.Get<RinkDesktopConfig>();
        }

        private DelegateCommand _checkPrinterCommand;
        public DelegateCommand CheckPrinterCommand => _checkPrinterCommand ?? (_checkPrinterCommand = new DelegateCommand(CheckPrinter));

        private DelegateCommand _zReportCommand;
        public DelegateCommand ZReportCommand => _zReportCommand ?? (_zReportCommand = new DelegateCommand(ZReport));

        private DelegateCommand _xReportCommand;
        public DelegateCommand XReportCommand => _xReportCommand ?? (_xReportCommand = new DelegateCommand(XReport, () => _operator.UserLevel > 0));

        private void ZReport()
        {
            Failover.Execute(() => {

                DeviceManagerFacade.zReport();

            }, exception => Logger.Error("Ошибка z отчета", exception));
        }

        private void XReport()
        {
            Failover.Execute(() => {

                DeviceManagerFacade.xReport();

            }, exception => Logger.Error("Ошибка x отчета", exception));
        }

        private void CheckPrinter()
        {

            Failover.Execute(() => {

            DeviceManagerFacade.PrintFiscalT("Оператор", new[]
            {
                new Position() {Name = "Apple", Price = 10, Quantity = 2}, 
                new Position() {Name = "Pear", Price = 15, Quantity = 3}, 
            }, _config.Template);
            }, exception => Logger.Error("Ошибка печати", exception));
        }
    }
}

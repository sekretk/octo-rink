using System.ComponentModel;
using System.Windows.Data;
using IvTx.Desktop;
using NLog;
using Octo.Rnd.RinkDesktop.Views;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public enum LogLevelEnum
    {
        Debug,
        Error,
        Fatal,
        Info,
        Off,
        Trace,
        Warn
    }

    public class LogViewModel : DialogViewModel<LogWindow>
    {
        public MainState State { get; private set; }

        public ICollectionView Log {get { return _logCvs.View; } }

        readonly CollectionViewSource _logCvs = new CollectionViewSource();
        
        private LogLevelEnum _filterLevel;

        public LogLevelEnum FilterLevel
        {
            get { return _filterLevel; }
            set { _filterLevel = value; OnPropertyChanged(); Log.Refresh(); }
        }

        public LogViewModel(FixedSizeObservableCollection<LogEventInfo> log, MainState state)
        {
            _logCvs.Source = log;
            State = state;

            Log.Filter = Filter;
        }

        private bool Filter(object o)
        {
            return o is LogEventInfo;
        }
    }
}

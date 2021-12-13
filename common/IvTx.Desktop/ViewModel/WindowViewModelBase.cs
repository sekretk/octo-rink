using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prism.Commands;
using Prism.Mvvm;

namespace IvTx.Desktop.ViewModel
{
    public class WindowViewModelBase : BindableBase
    {
        private DelegateCommand _loadCommand;
        public DelegateCommand LoadCommand => _loadCommand ?? (_loadCommand = new DelegateCommand(Load));

        protected virtual void Load() { }

        private DelegateCommand _unloadCommand;
        public DelegateCommand UnloadCommand => _unloadCommand ?? (_unloadCommand = new DelegateCommand(UnLoad));

        protected virtual void UnLoad() { }

        private bool _loading;
        public bool Loading
        {
            get { return _loading; }
            set { _loading = value; OnPropertyChanged();}
        }
    }
}

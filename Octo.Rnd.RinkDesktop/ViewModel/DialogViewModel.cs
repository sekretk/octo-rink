using System.Windows;
using Framework.UI.Controls;
using Prism.Commands;
using Prism.Mvvm;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public abstract class DialogViewModel<T> : BindableBase where T: OverlayWindow, new()
    {
        public bool IsShown { get; private set; }

        internal T _window;
        protected DialogViewModel()
        {
            _window = new T();
            _window.Owner = Application.Current.MainWindow;
            _window.DataContext = this;
        }

        public virtual void Show()
        {
            _window.Show();
            IsShown = true;
        }

        public virtual void Close()
        {
            _window.Close();
            IsShown = false;
        }

        private DelegateCommand _closeCommand;
        public DelegateCommand CloseCommand => _closeCommand ?? (_closeCommand = new DelegateCommand(Close));
    }
}

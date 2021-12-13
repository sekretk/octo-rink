using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using dbfirstplay;
using IvTx.Core.Log;
using IvTx.Mi.DataBase;
using IvTx.Mi.DataBase.DB;
using MILibrary;
using Octo.Rnd.RinkDesktop.Views;
using Prism.Commands;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public class LogonViewModel : ResultDialogViewModel<LogonView, Users>
    {
        private readonly IvTx.Mi.DataBase.Model _model;

        public LogonViewModel(IvTx.Mi.DataBase.Model model)
        {
            _model = model;
        }

        private string _password;
        public string Password
        {
            get { return _password; }
            set { _password = value; OnPropertyChanged(); }
        }

        private DelegateCommand _findPasswordCommand;
        public DelegateCommand FindPasswordCommand => _findPasswordCommand ?? (_findPasswordCommand = new DelegateCommand(FindPassword));

        private DelegateCommand _closeAppCommand;
        public DelegateCommand CloseAppCommand => _closeAppCommand ?? (_closeAppCommand = new DelegateCommand(CloseApp));

        private void CloseApp()
        {
            Application.Current.Shutdown(0);
        }

        private string _errorMessage;
        public string ErrorMessage
        {
            get { return _errorMessage; }
            set { _errorMessage = value; OnPropertyChanged(); }
        }

        private void FindPassword()
        {
            var user = _model.Users.FirstOrDefault(f => f.CardNumber == Password);

            if (user == null)
            {
                var pwd = Crypto.EncryptText(Password);
                user = _model.Users.FirstOrDefault(f => f.Password == pwd);
            }

            if (user != null)
               OnEnd(user);
            else
            {
                Password = String.Empty;
                ErrorMessage = "Пользователь не найден";
            }
        }

        public override void Show()
        {
            base.Show();

            Password = string.Empty;
        }
    }
}

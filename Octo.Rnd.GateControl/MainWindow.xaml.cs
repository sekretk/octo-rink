using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using IvTx.Desktop.Helpers;
using Octo.Rnd.GateControl.ViewModel;

namespace Octo.Rnd.GateControl
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow
    {
        public MainWindow()
        {
            DataContext = new MainViewModel(this);

            MinimizeToTray.Enable(this);

            InitializeComponent();

            this.Title = "Управление турникетом" + " - " + Assembly.GetExecutingAssembly().GetName().Version;

            this.Closing += (sender, args) =>
            {
                args.Cancel = true;

                this.WindowState = WindowState.Minimized;
            };
        }

        private void ButtonBase_OnClick(object sender, RoutedEventArgs e)
        {
            this.WindowState = WindowState.Minimized;
        }
    }
}

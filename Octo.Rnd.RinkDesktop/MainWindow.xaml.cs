using System;
using System.Reflection;
using System.Windows.Input;
using Octo.Rnd.RinkDesktop.ViewModel;

namespace Octo.Rnd.RinkDesktop
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow 
    {
        public MainWindow()
        {
            InitializeComponent();

            this.Title = "АРМ кассира" + " - " + Assembly.GetExecutingAssembly().GetName().Version ;

            DataContext = new MainWindowViewModel();
        }

        private void MainWindow_OnKeyDown(object sender, KeyEventArgs e)
        {
            if (MainTextBox.IsFocused)
                return;

            MainTextBox.Focus();

            MainTextBox.Text = String.Empty;
        }
    }
}

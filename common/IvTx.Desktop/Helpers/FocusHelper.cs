using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Threading;

namespace IvTx.Desktop.Helpers
{
    public static class FocusHelper
    {
        public const int DELAY_IN_MILLISECONDS = 500;

        static readonly DispatcherTimer _timer = new DispatcherTimer() {Interval = TimeSpan.FromMilliseconds(DELAY_IN_MILLISECONDS) };
        private static UIElement _focusElement;

        static FocusHelper()
        {
            _timer.Start();

            _timer.Tick += (sender, args) =>
            {
                if (GetReturn(_focusElement) && !_focusElement.IsFocused)
                    _focusElement.Focus();
            };
        }

        public static readonly DependencyProperty ReturnProperty = DependencyProperty.RegisterAttached(
            "Return", typeof (bool), typeof (FocusHelper), new PropertyMetadata(default(bool), OnReturnFocusChanged));

        private static void OnReturnFocusChanged(DependencyObject dependencyObject, DependencyPropertyChangedEventArgs dependencyPropertyChangedEventArgs)
        {
            _focusElement = dependencyObject as UIElement;
        }

        public static void SetReturn(DependencyObject element, bool value)
        {
            element.SetValue(ReturnProperty, value);
        }

        public static bool GetReturn(DependencyObject element)
        {
            return (bool) element.GetValue(ReturnProperty);
        }
    }
}

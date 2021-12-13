using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;

namespace IvTx.Desktop.Extentions
{
    public static class InputForward
    {
        public static readonly DependencyProperty InputTextBoxProperty = DependencyProperty.RegisterAttached(
            "InputTextBox", typeof (TextBox), typeof (InputForward), new PropertyMetadata(default(TextBox), OnInputTextBoxChanged));

        private static void OnInputTextBoxChanged(DependencyObject dependencyObject, DependencyPropertyChangedEventArgs dependencyPropertyChangedEventArgs)
        {
            FrameworkElement fe = dependencyObject as FrameworkElement;

            if (fe == null)
                return;

            Action<TextBox> handler = box =>
            {
                if (box == null)
                    return;

                if (box.IsFocused)
                    return;

                box.Focus();

                box.Text = String.Empty;
            };

            fe.Loaded += (sender, args) => handler(dependencyPropertyChangedEventArgs.NewValue as TextBox);

            fe.KeyDown += (sender, args) => handler(dependencyPropertyChangedEventArgs.NewValue as TextBox);
        }

        public static void SetInputTextBox(DependencyObject element, TextBoxBase value)
        {
            element.SetValue(InputTextBoxProperty, value);
        }

        public static TextBox GetInputTextBox(DependencyObject element)
        {
            return (TextBox) element.GetValue(InputTextBoxProperty);
        }
    }
}

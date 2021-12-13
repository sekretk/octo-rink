using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.TextFormatting;

namespace IvTx.Desktop.Helpers
{
    public static class ScrollDown
    {
        public static readonly DependencyProperty ScrollDownProperty = DependencyProperty.RegisterAttached(
            "ScrollDown", typeof (bool), typeof (ScrollDown), new PropertyMetadata(default(bool), OnScrollDownChanged));

        private static void OnScrollDownChanged(DependencyObject dependencyObject, DependencyPropertyChangedEventArgs dependencyPropertyChangedEventArgs)
        {
            var tb = dependencyObject as TextBox;

            if (tb != null)
            {
                tb.TextChanged += (sender, args) =>
                {
                    tb.CaretIndex = tb.Text.Length;
                    tb.ScrollToEnd();
                };
            }
        }

        public static void SetScrollDown(DependencyObject element, bool value)
        {
            element.SetValue(ScrollDownProperty, value);
        }

        public static bool GetScrollDown(DependencyObject element)
        {
            return (bool) element.GetValue(ScrollDownProperty);
        }

        public static readonly DependencyProperty CollectionProperty = DependencyProperty.RegisterAttached(
            "Collection", typeof (INotifyCollectionChanged), typeof (ScrollDown), new PropertyMetadata(default(INotifyCollectionChanged), OnCollectionChanged));

        private static void OnCollectionChanged(DependencyObject dependencyObject, DependencyPropertyChangedEventArgs dependencyPropertyChangedEventArgs)
        {
            var sw = dependencyObject as ScrollViewer;

            var col = dependencyPropertyChangedEventArgs.NewValue as INotifyCollectionChanged;

            if (col != null)
                col.CollectionChanged += (sender, args) =>
                {
                    if (sw != null)
                        sw.ScrollToEnd();
                };
        }

        public static void SetCollection(DependencyObject element, INotifyCollectionChanged value)
        {
            element.SetValue(CollectionProperty, value);
        }

        public static INotifyCollectionChanged GetCollection(DependencyObject element)
        {
            return (INotifyCollectionChanged) element.GetValue(CollectionProperty);
        }
    }
}

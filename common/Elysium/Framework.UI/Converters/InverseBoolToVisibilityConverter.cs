namespace Framework.UI.Converters
{
    using System;
    using System.Globalization;
    using System.Windows;
    using System.Windows.Data;

    /// <summary>
    /// The inverse Boolean to visibility converter.
    /// </summary>
    public sealed class InverseBoolToVisibilityConverter : IValueConverter
    {
        #region Public Methods

        /// <summary>
        /// The convert.
        /// </summary>
        /// <param name="value"> The value. </param>
        /// <param name="targetType"> The target type. </param>
        /// <param name="parameter"> The parameter. </param>
        /// <param name="culture"> The culture. </param>
        /// <returns> The <see cref="object"/>. </returns>
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if ((bool)value)
            {
                return Visibility.Collapsed;
            }

            return Visibility.Visible;
        }

        /// <summary>
        /// The convert back.
        /// </summary>
        /// <param name="value"> The value. </param>
        /// <param name="targetType"> The target type. </param>
        /// <param name="parameter"> The parameter. </param>
        /// <param name="culture"> The culture. </param>
        /// <returns> The <see cref="object"/>. </returns>
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            Visibility visibility = (Visibility)value;

            if (visibility == Visibility.Collapsed)
            {
                return true;
            }

            return false;
        }

        #endregion
    }
}

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Data;

namespace IvTx.Desktop.Converters
{
    public class DictionaryConverter : Dictionary<string, object>, IValueConverter
    {
        public object DefaultValue { get; set; }

        public bool UseBackConverter { get; set; }

        public object TargetNullValue { get; set; }

        #region Implementation of IValueConverter

        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            object result;

            if (value == null) return TargetNullValue;

            if (TryGetValue(value.ToString(), out result))
            {
                return result;
            }

            return DefaultValue;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var item = this.FirstOrDefault(kv => kv.Value.Equals(value));
            return item.Key;
        }

        #endregion
    }
}

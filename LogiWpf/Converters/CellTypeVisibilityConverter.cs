using LogiWpf.Models;
using System;
using System.Globalization;
using System.Windows;

namespace LogiWpf.Converters
{
    public class CellTypeVisibilityConverter : ConverterBase<CellTypeVisibilityConverter>
    {
        public override object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var param = parameter as string;
            if (param != null && value is CellTypes)
            {
                var val = ((CellTypes)value).ToString();
                if (val == param)
                    return Visibility.Visible;
            }
            return Visibility.Collapsed;
        }
    }
}

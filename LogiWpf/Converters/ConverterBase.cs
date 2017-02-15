using System;
using System.Globalization;
using System.Windows.Data;

namespace LogiWpf.Converters
{
    public abstract class SingletonBase<T> where T : new()
    {
        private static readonly Lazy<T> Instance =
            new Lazy<T>(() => new T());

        public static T Current => Instance.Value;
    }

    public abstract class ConverterBase<T> : SingletonBase<T>, IValueConverter
        where T : new()
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

    public abstract class MultiConverterBase<T> : SingletonBase<T>, IMultiValueConverter
        where T : new()
    {
        public virtual object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }

        public virtual object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}

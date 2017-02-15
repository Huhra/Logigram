using LogiWpf.Parser;
using LogiWpf.ViewModels;
using System;
using System.Windows;

namespace LogiWpf
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class LogiWindow : Window
    {
        public LogiWindow()
        {
            InitializeComponent();
            try
            {
                string[] args = Environment.GetCommandLineArgs();
                if (args.Length > 1)
                {
                    var parser = new LogiParser(args[1]);
                    DataContext = new LogiWindowViewModel(parser.Lines);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString());
            }
        }
    }
}

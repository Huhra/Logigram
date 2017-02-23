using LogiWpf.Models;
using Prism.Mvvm;
using System.Collections.Generic;

namespace LogiWpf.ViewModels
{
    public class LogiWindowViewModel : BindableBase
    {
        public List<CellRow> Rows { get; }
        public int Count => Rows.Count;

        public LogiWindowViewModel(List<List<CellModel>> cellLists)
        {
            var rows = new List<CellRow>();
            foreach (var cellList in cellLists)
                rows.Add(new CellRow(cellList));
            Rows = rows;
        }
    }
}

using System.Collections.Generic;

namespace LogiWpf.Models
{
    public class CellRow
    {
        public List<CellModel> Cells { get; }
        public int Count { get { return Cells.Count; } }

        public CellRow(List<CellModel> cells)
        {
            Cells = cells;
        }
    }
}

using System.Windows;

namespace LogiWpf.Models
{
    public class CellModel
    {
        public CellTypes CellType { get; }
        public int Digit { get; }
        public int FileLine { get; }
        public CellModel(CellTypes cellType, int digit, int fileLine)
        {
            FileLine = fileLine;
            CellType = cellType;
            Digit = digit;
        }

        public override string ToString()
        {
            switch (CellType)
            {
                case CellTypes.Nothing:
                    return " ";
                case CellTypes.Filled:
                    return "#";
                case CellTypes.Empty:
                    return "X";
                case CellTypes.Digit:
                    return $"{Digit}";
                default:
                    break;
            }
            return "@";
        }
    }
}

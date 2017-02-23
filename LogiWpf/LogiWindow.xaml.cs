using LogiWpf.Parser;
using LogiWpf.ViewModels;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using Logi.Common;
using LogiWpf.Models;

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
                else
                {
                    var converter = new LogiConverter();
                    var parser = new LogiParser(converter.Horizontals, converter.Verticals);
                    var lines = converter.FillResult(parser.Lines);
                    DataContext = new LogiWindowViewModel(lines);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString());
            }
        }
    }

    public class LogiConverter
    {
        private Constraints _puzzle;
        public List<CellModel> Horizontals { get; } = new List<CellModel>();
        public List<CellModel> Verticals { get; } = new List<CellModel>();

        public LogiConverter()
        {
            var puzzles = new Logi.Common.Puzzles();
            _puzzle = puzzles.Puzzle2;

            int line = 0;
            foreach (var horizontals in _puzzle.Horizontal)
            {
                foreach (var horizontal in horizontals)
                    Horizontals.Add(new CellModel(CellTypes.Digit, horizontal, line));
                line += 1;
            }

            foreach (var verticals in _puzzle.Vertical)
            {
                foreach (var vertical in verticals)
                    Verticals.Add(new CellModel(CellTypes.Digit, vertical, line));
                line += 1;
            }

        }
        
        public List<List<CellModel>> FillResult(List<List<CellModel>> parserLines)
        {
            var xDec = _puzzle.Horizontal.Select(x => x.Length).Max();
            var yDec = _puzzle.Vertical.Select(x => x.Length).Max();
            var gridWidth = _puzzle.Vertical.Length;
            var gridHeight = _puzzle.Horizontal.Length;

            var solver = new Logi.Algo.Solver();
            var resolved = solver.SolvePuzzleForCSharp(_puzzle);
            for (int y = 0; y < gridHeight; y++)
            {
                for (int x = 0; x < gridWidth; x++)
                {
                    CellModel model;
                    var value = resolved[y][x];
                    switch (value)
                    {
                        case 0:
                            model = new CellModel(CellTypes.Unknown, 0, 0);
                            break;
                        case 1:
                            model = new CellModel(CellTypes.Empty, 0, 0);
                            break;
                        default:
                            model = new CellModel(CellTypes.Filled, 0, 0);
                            break;
                    }
                    parserLines[y + yDec][x + xDec] = model;
                }
            }
            return parserLines;
        }
    }
}

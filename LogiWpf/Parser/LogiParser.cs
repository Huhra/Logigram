using System;
using System.Linq;
using System.Collections.Generic;
using System.IO;
using LogiWpf.Models;

namespace LogiWpf.Parser
{
    public class LogiParser
    {
        private static readonly char[] _separators = new [] { ';', ':', ',' };
        private List<CellModel> _horizontalLines = new List<CellModel>();
        private List<CellModel> _verticalLines = new List<CellModel>();
        private List<CellModel> _contents = new List<CellModel>();

        //Processing cache
        private List<IGrouping<int, CellModel>> _groupedContents;
        private List<IGrouping<int, CellModel>> _groupedVertical;
        private int _maxHorizontalHeaders;
        private int _maxVerticalHeaders;
        private int _numberVertical;
        //Processing cache

        public List<List<CellModel>> Lines { get; } = new List<List<CellModel>>();

        public LogiParser(string path)
        {
            var lines = File.ReadAllLines(path);
            ParseLines(lines);
        }

        private void ParseLines(string[] lines)
        {
            int lineNumber = 0;
            foreach (var line in lines)
            {
                if (string.IsNullOrEmpty(line))
                    continue;
                if (line[0].ToString().ToLower() == "h")
                    ParseHorizontal(line.Substring(1, line.Length - 1), lineNumber);
                else if (line[0].ToString().ToLower() == "v")
                    ParseVertical(line.Substring(1, line.Length - 1), lineNumber);
                else
                    ParseContent(line, lineNumber);
                lineNumber += 1;
            }
            FillContent();
        }

        private int GetMaxLineLength(List<CellModel> cells)
        {
            return cells.GroupBy(c => c.FileLine).Max(group => group.Count());
        }

        private void FillContent()
        {
            _maxHorizontalHeaders = GetMaxLineLength(_horizontalLines);
            _maxVerticalHeaders = GetMaxLineLength(_verticalLines);
            _numberVertical = _verticalLines.GroupBy(c => c.FileLine).ToList().Count;
            var numberLines = _maxVerticalHeaders + _horizontalLines.GroupBy(c => c.FileLine).ToList().Count;
            for (int line = 0; line < numberLines; line++)
            {
                var lineCells = new List<CellModel>();
                if (line < _maxVerticalHeaders)
                    FillHeaderLine(line, lineCells);
                else
                    FillBottomLines(line, lineCells);
                Lines.Add(lineCells);
            }
            foreach (var line in Lines)
            {
                var lineStr = line.Select(c => c.ToString()).Aggregate((c1, c2) => $"{c1}|{c2}");
                lineStr += "|";
                Console.WriteLine(lineStr);
            }
        }

        private void FillHeaderLine(int line, List<CellModel> lineCells)
        {
            for (int i = 0; i < _maxHorizontalHeaders; i++)
                lineCells.Add(new CellModel(CellTypes.Nothing, 0, -1));
            if (_groupedVertical == null)
                _groupedVertical = _verticalLines.GroupBy(c => c.FileLine).ToList();
            var mini = _groupedVertical.Min(g => g.Key);
            for (int i = 0; i < _groupedVertical.Count; i++)
            {
                var group = _groupedVertical.FirstOrDefault(g => (g.Key - mini) == i);
                var groupCells = group.ToList();

                var index = groupCells.Count - _maxVerticalHeaders + line;
                if (index < 0)
                    lineCells.Add(new CellModel(CellTypes.Nothing, 0, -1));
                else
                    lineCells.Add(groupCells[index]);
            }
        }

        private CellModel GetContent(int line, int index)
        {
            if (_contents.Count > 0)
            {
                if (_groupedContents == null)
                    _groupedContents = _contents.GroupBy(c => c.FileLine).ToList();
                var min = _groupedContents.Select(g => g.Key).Min();
                int indexGroup = min + line;
                foreach (var group in _groupedContents)
                {
                    if (group.Key == indexGroup)
                    {
                        var list = group.ToList();
                        if (index < list.Count)
                            return list[index];
                    }
                }
            }
            return new CellModel(CellTypes.Empty, 0, -1);
        }

        private void FillBottomLines(int line, List<CellModel> lineCells)
        {
            line -= _maxVerticalHeaders;
            var horizontalCells = _horizontalLines.Where(c => c.FileLine == line).ToList();

            for (int i = 0; i < _maxHorizontalHeaders - horizontalCells.Count; i++)
                lineCells.Add(new CellModel(CellTypes.Nothing, 0, -1));
            foreach (var hCell in horizontalCells)
                lineCells.Add(hCell);
            for (int i = 0; i < _numberVertical; i++)
            {
                lineCells.Add(GetContent(line, i));
            }
        }

        private void ParseContent(string line, int lineNumber)
        {
            Console.WriteLine($"ParseContent: {line}");
            foreach (var character in line)
            {
                if (character == 'X')
                    _contents.Add(new CellModel(CellTypes.Filled, 0, lineNumber));
                else
                    _contents.Add(new CellModel(CellTypes.Empty, 0, lineNumber));
            }
        }

        private void ParseHorizontal(string line, int lineNumber)
        {
            Console.WriteLine($"ParseHorizontal: {line}");
            var splitted = line.Split(_separators, StringSplitOptions.RemoveEmptyEntries);
            foreach (var split in splitted)
            {
                var cell = new CellModel(CellTypes.Digit, int.Parse(split), lineNumber);
                _horizontalLines.Add(cell);
            }
        }

        private void ParseVertical(string line, int lineNumber)
        {
            Console.WriteLine($"ParseVertical: {line}");
            var splitted = line.Split(_separators, StringSplitOptions.RemoveEmptyEntries);
            foreach (var split in splitted)
            {
                var cell = new CellModel(CellTypes.Digit, int.Parse(split), lineNumber);
                _verticalLines.Add(cell);
            }
        }
    }
}

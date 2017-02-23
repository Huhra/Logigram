namespace Logi.Algo
open System
open Logi.Common

type Solver() =
    member x.generateAllPossibleLine (constr : Blocks) (len : int) (first : bool) : Color list list =
            let minSpace = if first then 0 else 1
            match constr with
            | [] -> [List.init len (fun _ -> White)]
            | curConstr::nextConstr ->
                let result =
                    [minSpace..(len - curConstr)]
                    |> List.map (fun emptyOnLeft ->
                                    let empties = List.init emptyOnLeft (fun _ -> White)
                                    let filled = List.init curConstr (fun _ -> Black)
                                    let cells : Color list = List.append empties filled
                                    let remainLen = len - (emptyOnLeft + curConstr)
                                    let recursiveResults : Color list list = x.generateAllPossibleLine nextConstr remainLen false
                                    recursiveResults
                                    |> List.map (fun (recResult : Color list) -> List.append cells recResult)
                               )
                List.concat result
    
    member x.isLineValid (toValidate : Color seq) (line : Cell seq) : bool =
        Seq.zip toValidate line
        |> Seq.forall(fun (color, cell) ->
            match (color, cell) with
            | (_, None) -> true
            | (White, Some White) -> true
            | (Black, Some Black) -> true
            | _ -> false)
     
    member x.filterPossibilities (possibilities : Color list list) (line : Cell seq) : Color list list =
        possibilities
        |> Seq.filter(fun possibility -> x.isLineValid possibility line)
        |> Seq.map(fun possibility -> possibility |> Seq.toList)
        |> Seq.toList
    
    member x.fusePossiblities (possibilities : Color list list) (length : int) : Cell list =
        let fuseCellPossibilities (cellPossibilities : Color seq) : Cell =
            match (Seq.forall ((=) White) cellPossibilities), (Seq.forall ((=) Black) cellPossibilities) with
            | true, _ -> Some White
            | _, true -> Some Black
            | _ -> None
        [
            for i in 0..(length - 1) do
                let cells = possibilities |> Seq.map(fun possibility -> possibility.[i]) |> Seq.toList
                yield cells |> fuseCellPossibilities
        ] |> Seq.toList
    
    member x.getEmptyLine (length : int) =
        Seq.init length (fun _ -> None)
    
    member x.getLinePossibilities blocks length =
        let emptyline = x.getEmptyLine length
        let possibilities = x.generateAllPossibleLine blocks length true
        let filtered = x.filterPossibilities possibilities emptyline
        filtered
    
    member x.solvePuzzle(puzzle : Constraints) : Grid =
        let gridHeight = puzzle.Horizontal.Length
        let gridWidth = puzzle.Vertical.Length
        let emptyCells : Grid = Array2D.create gridHeight gridWidth None
        printf "GHeight:%d GWidth:%d" gridHeight gridWidth
        printf "Generating possibilities for every line"
        let horizontalPossibilities =
            puzzle.Horizontal
            |> Seq.map(fun horizontal -> x.getLinePossibilities horizontal gridWidth)
            |> Seq.toList
        let verticalPossibilities =
            puzzle.Vertical
            |> Seq.map(fun vertical -> x.getLinePossibilities vertical gridHeight)
            |> Seq.toList
        Console.WriteLine("")
    
        let gridIsValid(cells : Grid) : bool =
            [ 0..(puzzle.Horizontal.Length - 1) ]
            |> List.forall(fun h ->
                [ 0..(puzzle.Vertical.Length - 1) ]
                |> List.forall(fun v ->
                    match cells.[h, v] with
                    | None -> false
                    | _ -> true))
    
    
        let gridAreEqual(c1 : Grid, c2 : Grid) : bool =
            [ 0..(puzzle.Horizontal.Length - 1) ]
            |> List.forall(fun h ->
                [ 0..(puzzle.Vertical.Length - 1) ]
                |> List.forall(fun v ->
                    c1.[h,v] = c2.[h,v]))
    
        let printGrid(cells : Grid) =
            let printCell(cell : Cell) =
                let str =
                    match cell with
                    | Some(White) -> "[ ]"
                    | Some(Black) -> "[@]"
                    | _ -> "[?]"
                str |> Console.Write
            Console.WriteLine("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            for x in 0..(puzzle.Horizontal.Length - 1) do
                for y in 0..(puzzle.Vertical.Length - 1) do
                    cells.[x,y] |> printCell
                Console.WriteLine("")
            Console.WriteLine("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    
        let rec fillCells (cells : Grid) =
            if gridIsValid cells then
                "Cells are all valid" |> Console.WriteLine
                cells
            else
                cells |> printGrid
                let getHorizontal(x : int) : Cell list = [
                        for y in 0..(gridWidth - 1) do
                            yield cells.[x, y]
                    ]
    
                let hPossibilities = [
                    for h in 0..(gridHeight - 1) do
                        let possibleLines = horizontalPossibilities.[h]
                        let lineCells = getHorizontal(h)
                        let filtered = x.filterPossibilities possibleLines lineCells
                        let fuzed = x.fusePossiblities filtered gridWidth
                        yield fuzed
                    ]
                
                let getVertical(y : int) : Cell list = [
                        for x in 0..(gridHeight - 1) do
                            yield hPossibilities.[x].[y]
                    ]
    
                let vPossibilities = [
                    for v in 0..(gridWidth - 1) do
                        let possibleLines = verticalPossibilities.[v]
                        let lineCells = getVertical(v)
                        let filtered = x.filterPossibilities possibleLines lineCells
                        let fuzed = x.fusePossiblities filtered gridHeight
                        yield fuzed
                    ]
                
                let newCells : Grid =
                    Array2D.init
                        gridHeight
                        gridWidth
                        (fun x y -> vPossibilities.[y].[x])
    
                if gridAreEqual(cells, newCells) then
                    "Cells are NOT all valid" |> Console.WriteLine
                    newCells
                else
                    newCells |> fillCells
                
        printf "Trying to solve puzzle"
        let result = emptyCells |> fillCells
        result |> printGrid
        result
    
    member x.SolvePuzzleForCSharp(puzzle : Constraints) : System.Collections.Generic.List<System.Collections.Generic.List<int>> =
        let solved = x.solvePuzzle(puzzle)
        let gridWidth = puzzle.Vertical.Length
        let gridHeight = puzzle.Horizontal.Length
        
        let result = System.Collections.Generic.List<System.Collections.Generic.List<int>>()
        for y in 0..(gridHeight - 1) do
            let line = System.Collections.Generic.List<int>()
            for x in 0..(gridWidth - 1) do
                let converted =
                    match solved.[y, x] with
                    | None -> 0
                    | Some White -> 1
                    | Some Black -> 2
                line.Add(converted)
            result.Add(line)
        result
//    solvePuzzle(Puzzle2) |> ignore
//    
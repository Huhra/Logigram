open System
// Here is a proposed model
// You can remove it and use whatever you find more convenient!
type Color = | White | Black
type Cell = Color option
type Block = int
type Blocks = Block list

type Constraints = {
    Horizontal: Blocks list
    Vertical: Blocks list
}

type Grid = Cell[,]

// Here you should write your dojo/exercise logic

let isThisReallyTrue x = true


let MostSimpleCase = {
    Horizontal =
        [
            [4]
            [2]
        ]
    Vertical =
        [
            [1]
            [1]
            [1]
            [2]
            [1]
        ] }

let TutorialFromOldUkrainianLeaflet = {
    Horizontal =
        [
            [2;1;2]
            [1;3;1]
            [3;1;3]
        ]
    Vertical =
        [
            [1;1]
            [3]
            [1]
            [1]
            [3]
            [1]
            [1;1]
            [3]
            [1]
        ] }

let Puzzle1 = {
    Horizontal =
        [
            [5]
            [1;3]
            [1;4]
            [1;1;1]
            [3;1;1;1;1]
            [1;1;2;2;1]
            [1;1;3;1]
            [1;1;1;1;1]
            [2;4;1;1;1]
            [1;1;1;1]
            [1;2;2;1]
            [2;2;1;1;1]
            [3;4;1]
            [5;5;1]
            [4;4]
        ]
    Vertical =
        [
            [4]
            [7]
            [2;3]
            [3;2]
            [1;2]
            [9;2]
            [1;1;3]
            [1;1;1;1;2]
            [1;1;1;2;1]
            [2;3;1]
            [1;3;2;1]
            [2;3;2;2]
            [1;1;2;1;1]
            [1;1]
            [13]
        ] }

let Puzzle2 = {
    Horizontal =
        [
            [1;1]
            [2;2]
            [3;2]
            [4;3]
            [6;5]
            [2;3;2;2]
            [2;4;3;2]
            [2;4;3;3]
            [2;3;2;3]
            [2;3;2;2]
            [2;3;2;2]
            [3;2;2;2]
            [3;3;3;3]
            [14;3]
            [16]
            [15]
            [17]
            [19]
            [2;4;9]
            [1;2;2;8]
            [1;2;1;8]
            [2;1;2;1;7]
            [2;1;3;1;7]
            [2;1;5;6]
            [2;1;5;6]
            [1;1;3;6]
            [1;1;5]
            [7;5]
            [2;2;5]
            [2;2;6]
            [2;1;2;4]
            [1;1;2;4]
            [1;2;3;4]
            [1;6;2;3]
            [1;6;2;3]
            [1;2;4;2;3]
            [1;2;3;5]
            [1;2;3;4]
            [2;2;3;3]
            [5;3;2]
        ]
    Vertical =
        [
            [9]
            [2;2]
            [2;1]
            [2;1]
            [8;10;1]
            [11;2;4;1;2]
            [4;4;2;1;2]
            [8;6;1;2]
            [8;5;1;2;1]
            [7;6;9;2;2]
            [12;2;2;2;3]
            [10;4;10]
            [6;5;6]
            [6;5;4]
            [7;2;3]
            [5;6;2]
            [13;1]
            [17;3]
            [8;11;2;2]
            [4;12;2;2]
            [6;18;2]
            [29;2]
            [7;23]
            [23]
            [22]
        ] }

let Puzzle3 = {
    Horizontal =
        [
            [1;1]
            [2;2]
            [3;3]
            [4;4]
            [5;5]
            [6;6]
            [7;7]
            [7;7]
            [7;1;7]
            [7;2;7]
            [7;3;7]
            [7;4;7]
            [7;3;7]
            [7;2;7]
            [7;1;7]
            [7;7]
            [7;7]
            [6;6]
            [5;5]
            [4;4]
            [3;3]
            [2;2]
            [1;1]
        ]
    Vertical =
        [
            [1]
            [3]
            [5]
            [7]
            [9]
            [11]
            [13]
            [7;7]
            [7;1;7]
            [7;3;7]
            [7;5;7]
            [7;7;7]
            []
            [7;7]
            [7;7]
            [7;7]
            [7;7]
            [7;7]
            [13]
            [11]
            [9]
            [7]
            [5]
            [3]
            [1]
        ] }

let rec generateAllPossibleLine (constr : Blocks) (len : int) (first : bool) : Color list list =
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
                                let recursiveResults : Color list list = generateAllPossibleLine nextConstr remainLen false
                                recursiveResults
                                |> List.map (fun (recResult : Color list) -> List.append cells recResult)
                           )
            List.concat result

let isLineValid (toValidate : Color seq) (line : Cell seq) : bool =
    Seq.zip toValidate line
    |> Seq.forall(fun (color, cell) ->
        match (color, cell) with
        | (_, None) -> true
        | (White, Some White) -> true
        | (Black, Some Black) -> true
        | _ -> false)
 
let filterPossibilities (possibilities : Color list list) (line : Cell seq) : Color list list =
    possibilities
    |> Seq.filter(fun possibility -> isLineValid possibility line)
    |> Seq.map(fun possibility -> possibility |> Seq.toList)
    |> Seq.toList

let fusePossiblities (possibilities : Color list list) (length : int) : Cell list =
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

let getEmptyLine (length : int) =
    Seq.init length (fun _ -> None)

//let printBlocks(blocks : Blocks) =
//    Console.WriteLine()
//    Console.WriteLine()
//    for block in blocks do
//        Console.Write("{0},", block)

let printCell(cell : Cell) =
    let str =
        match cell with
        | Some(White) -> "[ ]"
        | Some(Black) -> "[@]"
        | _ -> "[?]"
    str |> Console.Write

//let printPossibilities(possibilities : Color list list) =
//    for possibility in possibilities do
//        Console.WriteLine()
//        for color in possibility do
//            match color with
//            | White -> Console.Write("[W]")
//            | Black -> Console.Write("[B]")

let getLinePossibilities blocks length =
    let emptyline = getEmptyLine length
    let possibilities = generateAllPossibleLine blocks length true
    let filtered = filterPossibilities possibilities emptyline
    //printBlocks blocks
    //printPossibilities filtered
    filtered

let solvePuzzle(puzzle : Constraints) : Grid =
    let gridHeight = puzzle.Horizontal.Length
    let gridWidth = puzzle.Vertical.Length
    let emptyCells : Grid = Array2D.create gridHeight gridWidth None
    printf "GHeight:%d GWidth:%d" gridHeight gridWidth
    let horizontalPossibilities =
        puzzle.Horizontal
        |> Seq.map(fun horizontal -> getLinePossibilities horizontal gridWidth)
        |> Seq.toList
    let verticalPossibilities =
        puzzle.Vertical
        |> Seq.map(fun vertical -> getLinePossibilities vertical gridHeight)
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
                    let filtered = filterPossibilities possibleLines lineCells
                    let fuzed = fusePossiblities filtered gridWidth
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
                    let filtered = filterPossibilities possibleLines lineCells
                    let fuzed = fusePossiblities filtered gridHeight
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
            

    emptyCells |> fillCells |> printGrid
    emptyCells

solvePuzzle(Puzzle2) |> ignore


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

let puzzle = MostSimpleCase
let h = puzzle.Horizontal
let v = puzzle.Vertical
let gridHeight = puzzle.Horizontal.Length
let gridWidth = puzzle.Vertical.Length

printf "GHeight:%d GWidth:%d" gridHeight gridWidth
let cells : Grid = Array2D.create gridHeight gridWidth None

// let test = [0; 1; 2]
// let test2 = [3]
// let test3 = Seq.

let rec generateAllPossibleLine (constr : Blocks) (len : int) (first : bool) : Color seq seq =
        let minSpace = if first then 0 else 1
        match constr with
        | [] -> [Seq.init len (fun _ -> White)] |> List.toSeq
        | curConstr::nextConstr ->
            let result =
                seq { minSpace..(len - curConstr) }
                |> Seq.map (fun emptyOnLeft ->
                                let empties = Seq.init emptyOnLeft (fun _ -> White)
                                let filled = Seq.init curConstr (fun _ -> Black)
                                let cells : Color seq = Seq.append empties filled
                                let remainLen = len - (emptyOnLeft + curConstr)
                                let recursiveResults : Color seq seq = generateAllPossibleLine nextConstr remainLen false
                                recursiveResults
                                |> Seq.map (fun (recResult : Color seq) -> Seq.append cells recResult)
                           )
            Seq.concat result

let isLineValid (toValidate : Color seq) (line : Cell seq) : bool =
    Seq.zip toValidate line
    |> Seq.forall(fun (color, cell) ->
        match (color, cell) with
        | (_, None) -> true
        | (White, Some White) -> true
        | (Black, Some Black) -> true
        | _ -> false)
 
let filterPossibilities (possibilities : Color seq seq) (line : Cell seq) : Color seq seq =
    possibilities
    |> Seq.filter(fun possibility -> isLineValid possibility line)



let fusePossiblities (possibilities : Color seq seq) : Cell seq =
    let fuseCellPossibilities (cellPossibilities : Color seq) : Cell =
        match (Seq.forall ((=) White) cellPossibilities), (Seq.forall ((=) Black) cellPossibilities) with
        | true, _ -> Some White
        | _, true -> Some Black
        | _ -> None
    possibilities
    |> Seq.map(fun possibility -> possibility |> fuseCellPossibilities) 



let test = generateAllPossibleLine [1; 1] 3 true
let test2 = filterPossibilities test [None; None; None]
let ptest2 = test2 |> Seq.map Seq.toList |> Seq.toList

let test3 = fusePossiblities test2
            //|> Seq.map Seq.toList
            |> Seq.toList
let fillLine(constr : Blocks, cells : Cell[]) =
    cells
    
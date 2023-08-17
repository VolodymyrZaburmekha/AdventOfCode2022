open System

// type Board = Map<int, int * int>

let inputLines = (System.IO.File.ReadAllText "Data/Day4_2021.txt").Split "\n"

let numbers = (inputLines |> Seq.head).Split(",") |> Seq.map Int32.Parse |> Seq.toList

let boards =
    let createBoard (lines: string list) =
        lines
        |> Seq.rev
        |> Seq.mapi (fun i l ->
            l.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.mapi (fun ni n -> Int32.Parse n, (i, ni)))
        |> Seq.collect id
        |> Map
    inputLines
    |> Seq.skip 2
    |> Seq.fold
        (fun (boards, linesInBoard: string list) line ->
            if String.IsNullOrWhiteSpace line then
                createBoard linesInBoard :: boards, []
            else
                boards, line :: linesInBoard)
        ([], [])
    |> fun (boards, rest) -> boards |> Seq.append (seq { createBoard rest }) |> Seq.rev |> Seq.toList


// where -> filter
// Seq.choose -> List.choose
// lambda -> fst, snd

let winPlays =
    numbers
    |> Seq.scan
        (fun (playedNumbers, previousWinners) number ->
            let currentlyPlayedNumbers = number :: playedNumbers
            let winners =
                boards
                |> List.filter (fun b ->
                    if Seq.contains b previousWinners then
                        false
                    else
                        let hits = List.choose (fun n -> Map.tryFind n b) currentlyPlayedNumbers
                        [ fst; snd ] |> Seq.exists (fun f -> hits |> Seq.countBy f |> Seq.exists (snd >> (=) 5)))
            //|> Seq.exists (fun f -> hits |> Seq.groupBy f |> Seq.exists (snd >> Seq.length >> (=) 5)))
            (currentlyPlayedNumbers, winners @ previousWinners))
        ([], [])
    |> Seq.choose (fun (playedNumbers, winners) ->
        if Seq.isEmpty winners then None else Some(playedNumbers, winners |> Seq.toList))

let getBoardScore playedNumbers board =
    let notPlayedSum =
        board |> Map.keys |> Seq.filter (fun k -> not (playedNumbers |> Seq.exists (fun pn -> pn = k))) |> Seq.sum
    notPlayedSum * (playedNumbers |> Seq.head)

let scoreWinnerAt index =
    winPlays
    |> Seq.find (fun (_, boards) -> boards |> Seq.length = index)
    |> fun (numbers, boards) -> getBoardScore numbers (boards |> Seq.head)

let part1 = scoreWinnerAt 1
let part2 = scoreWinnerAt (boards |> Seq.length)




// ----------------------------------------------------------------------

let rec checkBoards boardPairs winners number =
    match boardPairs with
    | [] -> [], []
    | (board: Map<int, int * int>, checkedBoard) as pair :: rest ->
        let boards', winners' = checkBoards rest winners number
        match Map.tryFind number board with
        | None -> pair :: boards', winners'
        | Some pos ->
            let board', checkedBoard' as pair' = Map.remove number board, Map.add number pos checkedBoard
            let isWinningBoard =
                [ fst; snd ]
                |> Seq.exists (fun f -> checkedBoard' |> Map.values |> Seq.countBy f |> Seq.exists (snd >> (=) 5))
            if isWinningBoard then boards', pair' :: winners' else pair' :: boards', winners'


let winPlays' =
    let boardsPairs = List.map (fun b -> b, Map.empty<int, (int * int)>) boards
    numbers
    |> Seq.scan
        (fun (prevBoards, _, _) number ->
            let prevBoards', winners' = checkBoards prevBoards [] number
            (prevBoards', winners', number))
        (boardsPairs, [], 0)
    |> Seq.skip 1
    |> Seq.collect (fun (_, winners, number) -> Seq.map (fun w -> w, number) winners)


let getBoardScore' ((board, checkedBoard), number) =
    let notPlayedSum = board |> Map.keys |> Seq.sum
    notPlayedSum * number


let scoreWinnerAt' index = winPlays' |> Seq.indexed |> Seq.find (fst >> (=) index) |> snd |> getBoardScore'

let part1' = scoreWinnerAt' 0
let part2' = scoreWinnerAt' (List.length boards - 1)

part1 = part1'
part2 = part2'

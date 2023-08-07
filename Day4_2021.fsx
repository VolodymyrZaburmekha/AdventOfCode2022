open System

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

let winPlays =
    numbers
    |> Seq.scan
        (fun (playedNumbers, previousWinners) number ->
            let currentlyPlayedNumbers = number :: playedNumbers
            let winners =
                boards
                |> List.where (fun b ->
                    if previousWinners |> Seq.contains b then
                        false
                    else
                        let has5Elements (_, values) = values |> Seq.length = 5
                        let hits = currentlyPlayedNumbers |> Seq.choose (fun n -> b |> Map.tryFind n)
                        hits |> Seq.groupBy (fun (r, _) -> r) |> Seq.exists has5Elements
                        || hits |> Seq.groupBy (fun (_, c) -> c) |> Seq.exists has5Elements)
            (currentlyPlayedNumbers, winners @ previousWinners))
        ([], [])
    |> Seq.choose (fun (playedNumbers, winners) ->
        if winners |> Seq.isEmpty then None else Some(playedNumbers, winners |> Seq.toList))

let getBoardScore playedNumbers board =
    let notPlayedSum =
        board |> Map.keys |> Seq.where (fun k -> not (playedNumbers |> Seq.exists (fun pn -> pn = k))) |> Seq.sum
    notPlayedSum * (playedNumbers |> Seq.head)

let scoreWinnerAt index =
    winPlays
    |> Seq.find (fun (_, boards) -> boards |> Seq.length = index)
    |> fun (numbers, boards) -> getBoardScore numbers (boards |> Seq.head)

let part1 = scoreWinnerAt 1
let part2 = scoreWinnerAt (boards |> Seq.length)

open System.IO

type Coordinates = { X: int; Y: int (* ; ID: int *)  }

let move (state: Coordinates list) (command: string) =
    let moveStep state step =
        let moveHead headState step =
            match step with
            | "U" -> { headState with Y = headState.Y + 1 }
            | "D" -> { headState with Y = headState.Y - 1 }
            | "L" -> { headState with X = headState.X - 1 }
            | "R" -> { headState with X = headState.X + 1 }
            | _ -> failwith "incorrect step"

        let adjustTail headState tailState =
            let diffX, diffY =
                match headState.X - tailState.X, headState.Y - tailState.Y with
                | 1, 2
                | 2, 1
                | 2, 2 -> 1, 1
                | 2, 0 -> 1, 0
                | -2, 0 -> -1, 0
                | 0, 2 -> 0, 1
                | 0, -2 -> 0, -1
                | 2, -1
                | 1, -2
                | 2, -2 -> 1, -1
                | -1, -2
                | -2, -1
                | -2, -2 -> -1, -1
                | -2, 1
                | -1, 2
                | -2, 2 -> -1, 1
                | x, y when abs x < 2 && abs y < 2 -> 0, 0
                | incorrectDiff -> failwith $"not correct diff: {incorrectDiff}"

            { tailState with X = tailState.X + diffX; Y = tailState.Y + diffY }

        state |> Seq.skip 1 |> Seq.scan adjustTail (moveHead (List.head state) step) |> Seq.toList

    match command.Split(' ') with
    | [| direction; steps |] -> Seq.replicate (int steps) direction |> Seq.scan moveStep state
    | _ -> failwith "incorrect command"




let getState2 input initialState =
    input
    |> List.fold
        (fun bridgeStates command ->
            let lastState = List.head bridgeStates
            move lastState command |> Seq.fold (fun xs x -> x :: xs) bridgeStates)
        [ initialState ]


let getState input initialState =
    seq {
        let mutable lastState = initialState
        for command in input do
            for state in move lastState command do
                yield state
                lastState <- state
    }

// let getState input initialState =
//     input
//     |> List.fold
//         (fun bridgeStates command ->
//             let lastState = bridgeStates |> List.last // scanning whole list
//             List.append bridgeStates (move lastState command)) // rewriting first list many times
//         [ initialState ]


let getInitialState count = [ for i in 1..count -> { X = 0; Y = 0 (*; ID = i *)  } ]

let countTail input length =
    // getState input (getInitialState length) |> List.countBy (fun s -> List.last s) |> List.length // List.lasts -> scanning list
    // getInitialState length |> getState input |> Seq.map (List.last) |> set |> Set.count
    getInitialState length |> getState input |> Seq.distinctBy (List.last) |> Seq.length



let sw = System.Diagnostics.Stopwatch.StartNew()

let input = File.ReadAllLines "Data/Day9.txt" |> List.ofArray

let part1 = countTail input 2
let part2 = countTail input 10

let TIME = sw.ElapsedMilliseconds

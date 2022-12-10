open System.IO

type Coordinates = { X: int; Y: int; ID: int }

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

            { tailState with
                X = tailState.X + diffX
                Y = tailState.Y + diffY }
        let head = state |> List.head
        let tail = state |> List.tail

        let newHead = moveHead head step

        tail
        |> List.fold
            (fun previous current ->
                let prevHead = List.last previous
                let newCurrent = adjustTail prevHead current
                previous @ [ newCurrent ])
            [ newHead ]

    match command.Split(' ') |> List.ofArray with
    | [ direction; steps ] ->
        [ 1 .. (int steps) ]
        |> List.map (fun _ -> direction)
    | _ -> failwith "incorrect command"
    |> List.scan (fun s c -> moveStep s c) state


let getState input initialState =
    input
    |> List.fold
        (fun bridgeStates command ->
            let lastState = bridgeStates |> List.last

            (move lastState command)
            |> List.append bridgeStates)
        [ initialState ]

let getInitialState count =
    [ for i in 1..count do
          { X = 0; Y = 0; ID = i } ]

let countTail input length =
    getState input (getInitialState length)
    |> List.countBy (fun s -> List.last s)
    |> List.length

let input =
    File.ReadAllLines "Data/Day9.txt" |> List.ofArray
    
let part1 = countTail input 2
let part2 = countTail input 10

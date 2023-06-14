open System
open System.IO
open Microsoft.FSharp.Core

type Command =
    | Noop
    | Add of int


let input =
    File.ReadAllLines "Data/Day10.txt"
    |> Array.map (fun line ->
        match line.Split(" ") with
        | [| _; addVal |] -> Add(int addVal)
        | _ -> Noop)

// let getStatesForCommand command currentState =
//     match command with
//     | Noop -> [| currentState |]
//     | Add add -> [| currentState; currentState + add |]

// let value =
//     input
//     |> Array.fold
//         (fun state command ->
//             let prevState = Array.last state
//             let newStates = getStatesForCommand command prevState
//             newStates |> Array.append state)
//         [| 1 |]


let value =
    input
    |> Seq.scan
        (fun (current, _) command ->
            match command with
            | Add value -> let next = current + value in next, [ current; next ]
            | Noop -> current, [ current ])
        (1, [ 1 ])
    |> Seq.map snd
    |> Seq.concat
    |> Seq.toArray

let part1 =
    seq { for i in 1..5 -> i * 40 + 20 }
    |> Seq.fold (fun sum index -> sum + value.[index - 1] * index) (value.[19] * 20)


let part2 =
    let valueToPrint =
        value
        |> Seq.chunkBySize 40
        |> Seq.map (Array.mapi (fun index item -> if abs (item - index) <= 1 then '#' else '.'))
    for line in valueToPrint do
        Console.WriteLine(String line)

open System
open System.IO
open Microsoft.FSharp.Core

type Command =
    | Noop
    | Add of int

let getStatesForCommand command currentState =
    match command with
    | Noop -> [| currentState |]
    | Add add -> [| currentState; currentState + add |]

let input =
    File.ReadAllLines "Data/Day10.txt"
    |> Array.map (fun line ->
        match line.Split(" ") with
        | [| _; addVal |] -> Add(int addVal)
        | _ -> Noop)

let value =
    input
    |> Array.fold(fun state command ->
            let prevState = Array.last state
            let newStates = getStatesForCommand command prevState
            newStates |> Array.append state 
        )[|1|]
let part1 =
    [for i in 1 .. 5 -> i*40 + 20]
    |> List.fold(fun sum index ->
        sum + value.[index - 1 ] * index
        )(value.[19]*20)

let part2 =
    let valueToPrint = 
        value
        |> Array.chunkBySize 40
        |> Array.map(fun line ->
            line |> Array.mapi(fun index item -> if abs (item - index) <= 1 then '#' else '.' )
            )
    for line in valueToPrint do
        Console.WriteLine (String line)
    
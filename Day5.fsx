open System
open System.IO
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Command =
    { MoveFrom: int
      MoveTo: int
      Count: int }

let parseInputStacks (stackLines: string []) =
    stackLines
    |> Array.map (fun line ->
        line
        |> Seq.chunkBySize 4
        |> Seq.mapi (fun index text -> (index, text |> Seq.tryFind (fun s -> s |> Char.IsLetter)))
        |> Array.ofSeq)
    |> Array.collect (fun a -> a)
    |> Array.groupBy (fun (index, _) -> index)
    |> Array.map (fun (_, items) -> items |> Array.choose (fun (_, i) -> i))

let parseCommands (commands: string []) =
    commands
    |> Array.map (fun command ->
        let components = command.Split(' ')

        { Count = int components.[1]
          MoveFrom = int components.[3] - 1
          MoveTo = int components[5] - 1 })

let parseInput input =
    let stacks = input |> Array.take 8
    let commands = input |> Array.skip 10
    (parseInputStacks stacks, parseCommands commands)

let move multipleAtOnce (stacks: char [] []) (command: Command) =
    let moveElements =
        stacks.[command.MoveFrom]
        |> Array.take command.Count
        |> (fun elements ->
            if multipleAtOnce then
                elements
            else
                elements |> Array.rev)

    let movedTo =
        stacks.[command.MoveTo]
        |> Array.append moveElements

    let movedFrom =
        stacks.[command.MoveFrom]
        |> Array.skip command.Count

    let duplicate = Array.copy stacks
    duplicate[command.MoveFrom] <- movedFrom
    duplicate[command.MoveTo] <- movedTo
    duplicate

let solve multipleAtOnce stacks commands =
    commands
    |> Array.fold (fun state command -> move multipleAtOnce state command) stacks
    |> Array.choose (fun stack -> Array.tryHead stack)
    |> String

let input =
    File.ReadAllLines "Data/Day5.txt"

let stacks, commands = parseInput input

let partOneResult =
    solve false stacks commands

let partTwoResult =
    solve true stacks commands

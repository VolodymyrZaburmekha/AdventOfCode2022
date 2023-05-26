open System
open System.IO
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Command =
    { MoveFrom: int
      MoveTo: int
      Count: int }


// (1 |> inc) |> inc |> string

// inc >> inc >> string

// let func1 = string . inc . inc

let parseInputStacks (stackLines: string []) =
    stackLines
    |> Seq.map (fun line ->
        line
        |> Seq.chunkBySize 4
        |> Seq.map (Seq.tryFind Char.IsLetter)
        |> Seq.indexed)
    |> Seq.collect id
    |> Seq.groupBy fst
    |> Seq.map (fun (_, items) -> Seq.choose snd items |> Seq.toArray)
    // |> Seq.map (snd >> Seq.choose snd >> Seq.toArray)
    |> Seq.toArray

let parseCommands (commands: string seq) =
    commands
    |> Seq.map (fun command ->
        let components = command.Split(' ')

        { Count = int components.[1]
          MoveFrom = int components.[3] - 1
          MoveTo = int components[5] - 1 })
    |> Seq.toArray

let parseInput input =
    let indexOfBlankLine = Seq.findIndex String.IsNullOrEmpty input
    let stacks, commands = Array.splitAt indexOfBlankLine input
    (parseInputStacks stacks, parseCommands (Seq.skip 1 commands))

let move multipleAtOnce (stacks: char [] []) (command: Command) =
    let moveElements =
        stacks.[command.MoveFrom]
        |> Seq.take command.Count
        |> if multipleAtOnce then id else Seq.rev
        |> Seq.toArray


    let movedTo = Array.append moveElements stacks.[command.MoveTo]
    let movedFrom = Array.skip command.Count stacks.[command.MoveFrom]

    let duplicate = Array.copy stacks
    duplicate[command.MoveFrom] <- movedFrom
    duplicate[command.MoveTo] <- movedTo
    duplicate

let solve multipleAtOnce stacks commands =
    commands
    |> Array.fold (move multipleAtOnce) stacks
    |> Array.choose Array.tryHead
    |> String

let input = File.ReadAllLines "Data/Day5.txt"

let stacks, commands = parseInput input

let partOneResult = solve false stacks commands

let partTwoResult = solve true stacks commands

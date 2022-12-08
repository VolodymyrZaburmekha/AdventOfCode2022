open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Collections

type CommandType =
    | ChangeDirectory of string
    | ListFiles

type LineType =
    | Command of CommandType
    | Directory of string
    | File of int * string

let rec parseLine (line: string) =
    match line.Split(' ') with
    | [| "$"; "ls" |] -> Command ListFiles
    | [| "$"; "cd"; path |] -> Command(ChangeDirectory path)
    | [| "dir"; dirName |] -> Directory dirName
    | [| number; filename |] -> File(int number, filename)
    | _ -> failwith "bad line"

let getPath (path: string) command =
    match command with
    | "/" -> "root"
    | ".." ->
        let lastIndex = path.LastIndexOf('/')
        path.Substring(0, lastIndex)
    | p -> $"%s{path}/{p}"

let rec buildPossiblePaths path elements =
    let getNewPath str =
        match path with
        | Some some -> $"%s{some}/%s{str}"
        | None -> str

    match elements with
    | [] -> elements
    | [ last ] -> [ getNewPath last ]
    | head :: tail ->
        let newPath = getNewPath head

        newPath
        :: (buildPossiblePaths (Some newPath) tail)

let getFolderSizes lines =
    lines
    |> Array.map parseLine
    |> Array.mapFold
        (fun path line ->
            match line with
            | Command command ->
                match command with
                | ChangeDirectory cd -> (None, (getPath path cd))
                | _ -> (None, path)
            | Directory _ -> (None, path)
            | File (size, name) -> (Some($"%s{path}/%s{name}", size), path))
        ""
    |> (fun (data, _) -> data)
    |> Array.choose (fun f -> f)
    |> Array.distinct
    |> Array.fold
        (fun (state: Dictionary<string, int>) (path, size) ->
            let pathParts = path.Split('/')

            let possiblePaths =
                pathParts
                |> Array.take ((pathParts |> Array.length) - 1)
                |> List.ofArray
                |> buildPossiblePaths None

            for p in possiblePaths do
                let exists, value = state.TryGetValue(p)

                if exists then
                    state.[p] <- value + size
                else
                    state.[p] <- size

            state)
        (Dictionary<string, int>())

let input =
    File.ReadAllLines "Data/Day7.txt"

let part1 =
    getFolderSizes input
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.filter (fun v -> v <= 100000)
    |> Seq.sum

let part2 =
    getFolderSizes input
    |> (fun folders ->
        let taken =
            folders
            |> Seq.maxBy (fun f -> f.Value)
            |> (fun kv -> kv.Value)

        let totalSize = 70_000_000
        let needSize = 30_000_000
        let freeSize = totalSize - taken
        let needToFree = needSize - freeSize

        folders
        |> Seq.filter (fun f -> f.Value > needToFree)
        |> Seq.map (fun f -> f.Value, f.Value - needToFree)
        |> Seq.minBy (fun (_, diff) -> diff))
    |> (fun (v, _) -> v)

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
    |> Seq.map parseLine
    |> Seq.mapFold
        (fun path line ->
            match line with
            | Command (ChangeDirectory cd) -> (None, (getPath path cd))
            | File (size, name) -> (Some($"%s{path}/%s{name}", size), path)
            | _ -> (None, path))
        ""
    |> fst
    |> Seq.choose id
    |> Seq.distinct

    |> Seq.fold
        (fun (state: Map<string, int>) (path, size) ->
            let pathParts = path.Split('/')

            let possiblePaths =
                pathParts
                |> Seq.take ((Array.length pathParts) - 1)
                |> Seq.toList
                |> buildPossiblePaths None

            possiblePaths
            |> Seq.fold (fun m p -> Map.change p (Option.defaultValue 0 >> ((+) size) >> Some) m) state)
        (Map [])

// |> Seq.fold
//     (fun (state: Dictionary<string, int>) (path, size) ->
//         let pathParts = path.Split('/')

//         let possiblePaths =
//             pathParts
//             |> Seq.take ((Array.length pathParts) - 1)
//             |> Seq.toList
//             |> buildPossiblePaths None

//         for p in possiblePaths do
//             state.[p] <- state.GetValueOrDefault(p, 0) + size

//         state)
//     (Dictionary<string, int>())

let input = File.ReadAllLines "Data/Day7.txt"

let part1 =
    getFolderSizes input
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.filter (fun v -> v <= 100000)
    |> Seq.sum

let part2 =
    let folders = getFolderSizes input
    let taken = folders |> Seq.map (fun f -> f.Value) |> Seq.max
    let totalSize = 70_000_000
    let needSize = 30_000_000
    let freeSize = totalSize - taken
    let needToFree = needSize - freeSize

    folders
    |> Seq.filter (fun f -> f.Value > needToFree)
    |> Seq.map (fun f -> f.Value, f.Value - needToFree)
    |> Seq.minBy (fun (_, diff) -> diff)

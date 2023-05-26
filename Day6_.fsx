open System.IO

let findFirstUniqueCombination length str =
    Seq.toList str
    |> Seq.indexed
    |> Seq.windowed length
    |> Seq.tryFind (fun items ->
        let uniqueElements = items |> Seq.map snd |> set

        uniqueElements |> Set.count = Array.length items)
    |> (fun result ->
        match result with
        | Some r -> fst (Array.last r) + 1
        | None -> -1)

let input = File.ReadAllText "Data/Day6.txt"

let result1 = findFirstUniqueCombination 4 input

let result2 = findFirstUniqueCombination 14 input

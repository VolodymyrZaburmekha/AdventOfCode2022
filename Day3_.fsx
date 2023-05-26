open System
open System.IO

let getPriority item =
    int item - (if Char.IsLower item then 96 else 38)

let solvePart1 (rucksacks: string seq) =
    rucksacks
    |> Seq.collect (fun rucksack ->
        let halfOfLength = rucksack.Length / 2
        let firstPart = rucksack.Substring(0, halfOfLength)
        let secondPart = rucksack.Substring(halfOfLength)
        Seq.map getPriority (Set.intersect (set firstPart) (set secondPart)))
    |> Seq.sum

let solvePart2 rucksacks =
    rucksacks
    |> Seq.chunkBySize 3
    |> Seq.collect (fun group ->
        group
        |> Seq.map set
        |> Set.intersectMany
        |> Seq.map getPriority)
    |> Seq.sum

let input = File.ReadAllLines "Data/Day3.txt"

let part1Result = solvePart1 input

let part2Result = solvePart2 input

open System
open System.IO

let getPriority item =
    if Char.IsLower item then
        int item - 96
    else
        int item - 38

let solvePart1 rucksacks =
    rucksacks
    |> Seq.collect (fun rucksack ->
        rucksack
        |> (fun (rucksackContent: string) ->
            let itemsPerCompartment =
                rucksackContent.Length / 2

            let firstCompartmentContent =
                rucksackContent.Substring(0, itemsPerCompartment)

            let secondCompartmentContent =
                rucksackContent.Substring(itemsPerCompartment, rucksackContent.Length - itemsPerCompartment)

            (firstCompartmentContent, secondCompartmentContent))
        |> (fun (comp1, comp2) ->
            let comp1Items = Seq.toList comp1
            let comp2Items = Seq.toList comp2
            (set comp1Items, set comp2Items))
        |> (fun (comp1, comp2) -> Set.intersect comp1 comp2)
        |> Seq.map (fun s -> getPriority s))
    |> Seq.sum

let solvePart2 rucksacks =
    rucksacks
    |> Seq.chunkBySize 3
    |> Seq.collect (fun group ->
        group
        |> Seq.map (fun rucksack -> set rucksack)
        |> Set.intersectMany
        |> Seq.map (fun item -> getPriority item))
    |> Seq.sum

let input =
    File.ReadAllLines "Data/Day3.txt"

let part1Result = solvePart1 input

let part2Result = solvePart2 input

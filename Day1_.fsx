open System.IO

let parseInput (input: string) =
    input.Split("\n\n")
    |> Seq.map (fun sublist -> sublist.Split('\n'))
    |> Seq.map (Array.map int)

let sumCaloriesForDeer elements = Seq.map Array.sum elements

let solvePartOne elements =
    elements |> sumCaloriesForDeer |> Seq.max

let solvePartTwo elements =
    elements
    |> sumCaloriesForDeer
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

let input = parseInput (File.ReadAllText "Data/Day1.txt")

let partOneResult = solvePartOne input
let partTwoResult = solvePartTwo input

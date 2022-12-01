open System.IO

let parseInput (input: string) =
    input.Split("\n\n")
    |> Array.map (fun sublist ->
        sublist.Split('\n')
        |> Array.map (fun number -> number |> int))

let sumCaloriesForDeer (elements: int [] []) =
    elements
    |> Array.map (fun caloriesArray -> caloriesArray |> Array.sum)

let solvePartOne elements =
    elements |> sumCaloriesForDeer |> Array.max

let solvePartTwo elements =
    elements
    |> sumCaloriesForDeer
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum

let input =
    parseInput (File.ReadAllText "Data/Day1.txt")

let partOneResult = solvePartOne input
let partTwoResult = solvePartTwo input
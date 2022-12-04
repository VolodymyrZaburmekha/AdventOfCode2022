open System.IO

let parseInputLine (input: string) =
    input.Split(',')
    |> Array.map (fun pair ->
        match pair.Split('-') with
        | [| start; finish |] -> (int start, int finish))

let solvePart1 input =
    input
    |> Array.map parseInputLine
    |> Array.map (fun linePairs ->
        let from1, to1 = linePairs[0]
        let from2, to2 = linePairs[1]

        if (from1 <= from2 && to1 >= to2)
           || (from2 <= from1 && to2 >= to1) then
            1
        else
            0)
    |> Array.sum

let solvePart2 input =
    input
    |> Array.map parseInputLine
    |> Array.map (fun linePairs ->
        linePairs
        |> Array.map (fun (from, toEnd) -> set [ from..toEnd ])
        |> Set.intersectMany
        |> (fun seq -> if Set.count seq > 0 then 1 else 0))
    |> Array.sum

let input =
    File.ReadAllLines "Data/Day4.txt"

solvePart1 input
solvePart2 input

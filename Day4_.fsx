open System.IO

let parseInputLine (input: string) =
    let numbers =
        input.Split(',')
        |> Seq.collect (fun pair -> pair.Split('-'))
        |> Seq.map int
        |> Seq.toArray

    match numbers with
    | [| a; b; c; d |] -> (a, b), (c, d)
    | _ -> failwith "Incomplete pattern"

let isContained (a, b) (c, d) = a <= c && b >= d

isContained (6, 6) (4, 5)

let solvePart1 input =
    input
    |> Array.map parseInputLine
    |> Array.map (fun (left, right) ->
        if isContained left right || isContained right left then
            1
        else
            0)
    |> Array.sum

let solvePart2 input =
    input
    |> Array.map parseInputLine
    |> Array.map (fun ((from1, to1), (from2, to2)) ->
        let common = Set.intersect (set { from1..to1 }) (set { from2..to2 })
        if Seq.isEmpty common then 0 else 1)
    |> Array.sum

let input = File.ReadAllLines "Data/Day4.txt"

solvePart1 input
solvePart2 input

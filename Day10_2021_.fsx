open System.IO

let openCloseMap = [ ('[', ']'); ('(', ')'); ('{', '}'); ('<', '>') ] |> Map

let isOpening ch = openCloseMap |> Map.containsKey ch

type LineCheck =
    | Ok
    | Corrupted of char // | Corrupted of char * char
    | Incomplete of char list

let part1Prices = [ (')', 3); (']', 57); ('}', 1197); ('>', 25137) ] |> Map
let part1CharToScore ch = part1Prices[ch]

let part2Prices = [ (')', 1L); (']', 2L); ('}', 3L); ('>', 4L) ] |> Map
let part2CharToScore ch = part2Prices[ch]

let validate input =
    let rec loop remaining opened =
        match remaining, opened with
        | [], [] -> Ok
        | [], _ -> Incomplete opened
        | rh :: rt, opened' when isOpening rh -> loop rt (rh :: opened')
        | rh :: _, [] -> Corrupted(rh)
        | rh :: rt, oh :: ot -> if rh = openCloseMap[oh] then loop rt ot else Corrupted(rh)
    loop (Seq.toList input) []

let result1 =
    File.ReadAllLines "Data/Day10_2021.txt"
    |> Seq.choose (fun l ->
        match validate l with
        | Corrupted (actual) -> Some(part1CharToScore actual)
        | _ -> None)
    |> Seq.sum

let result2 =
    let elements =
        File.ReadAllLines "Data/Day10_2021.txt"
        |> Seq.choose (fun l ->
            match validate l with
            | Incomplete opened ->
                Some(Seq.fold (fun state bracket -> state * 5L + (part2CharToScore openCloseMap[bracket])) 0L opened)
            | _ -> None)
        |> Seq.sort
        |> Seq.toArray
    elements[elements.Length / 2]

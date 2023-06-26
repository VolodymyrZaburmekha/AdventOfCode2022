open System.IO

let openCloseMap = [ ('[', ']'); ('(', ')'); ('{', '}'); ('<', '>') ] |> Map

let isOpening ch = openCloseMap |> Map.containsKey ch

type LineCheck =
    | Ok
    | Corrupted of char * char
    | Incomplete of char list

let part1Prices = [ (')', 3); (']', 57); ('}', 1197); ('>', 25137) ] |> Map
let part1CharToScore ch = part1Prices[ch]

let part2Prices = [ (')', 1L); (']', 2L); ('}', 3L); ('>', 4L) ] |> Map

let part2CharToScore ch = part2Prices[ch]

let validate inp =
    let rec loop remaining opened =
        match remaining, opened with
        | [], [] -> Ok
        | [], _ ->
            let len = opened |> List.length
            Incomplete(opened |> List.take (len - 1))
        | rh :: rt, oh :: ot ->
            if isOpening rh then
                loop rt (rh :: opened)
            else
                let expectedClosing = openCloseMap[oh]
                if rh = expectedClosing then loop rt ot else Corrupted(expectedClosing, rh)
    loop (inp |> Seq.toList) (inp |> Seq.take 1 |> Seq.toList)

let result1 =
    File.ReadAllLines "Data/Day10_2021.txt"
    |> Seq.choose (fun l ->
        let validationResult = validate l
        match validationResult with
        | Corrupted(_, actual) -> Some(part1CharToScore actual)
        | _ -> None)
    |> Seq.sum

let result2 =
    let elements =
        File.ReadAllLines "Data/Day10_2021.txt"
        |> Seq.choose (fun l ->
            let validationResult = validate l
            match validationResult with
            | Incomplete opened ->
                Some(
                    opened
                    |> Seq.fold
                        (fun state bracket ->
                            let closeBracket = openCloseMap[bracket]
                            state * 5L + (part2CharToScore closeBracket))
                        0L
                )
            | _ -> None)
        |> Seq.sort
        |> Seq.toArray
    elements[elements.Length / 2]

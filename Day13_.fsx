open System
open Microsoft.FSharp.Core

type Packet =
    | ListContent of Packet list
    | Number of int



let parsePacket line =
    let getSubContent str =
        str
        |> Seq.skip 1
        |> Seq.take (Seq.length str - 2) // ignore first and last brackets
        |> Seq.fold
            (fun (collectedElements, currentContent, openedBracketsCount) element ->
                match element with
                | ',' ->
                    if openedBracketsCount > 0 then
                        collectedElements, currentContent + ",", openedBracketsCount
                    else
                        (collectedElements @ [ currentContent ]), "", openedBracketsCount
                | '[' -> collectedElements, currentContent + "[", openedBracketsCount + 1
                | ']' -> collectedElements, currentContent + "]", openedBracketsCount - 1
                | number -> collectedElements, currentContent + number.ToString(), openedBracketsCount)
            ([], "", 0)
        |> fun (elements, leftoverContent, _) -> elements @ [ leftoverContent ]

    let rec mapElement (content: string) =
        match Int32.TryParse content with
        | true, number -> Number number
        | _ -> ListContent(if content = "[]" then [] else content |> getSubContent |> List.map mapElement)

    mapElement line

let parseInput (lines: string) =
    // lines.Split("\n") |> Seq.filter (fun l -> not (String.IsNullOrEmpty l)) |> Seq.map parsePacket
    lines.Split("\n") |> Seq.choose (fun l -> if String.IsNullOrEmpty l then None else Some(parsePacket l))




let compare (leftPacket, rightPacket) =
    let rec loop prevResult (left, right) =
        let rec compareListElements prevResult l1 l2 =
            prevResult
            |> Option.orElseWith (fun () ->
                match l1, l2 with
                | [], [] -> None
                | [], _ :: _ -> Some true
                | _ :: _, [] -> Some false
                | h1 :: t1, h2 :: t2 ->
                    let current = (h1, h2) |> loop None
                    compareListElements current t1 t2)

        prevResult
        |> Option.orElseWith (fun () ->
            match left, right with
            | Number leftNumber, Number rightNumber ->
                if leftNumber < rightNumber then Some true
                else if leftNumber > rightNumber then Some false
                else None
            | ListContent leftList, Number numberRight ->
                loop prevResult (ListContent leftList, ListContent [ Number numberRight ])
            | Number leftNumber, ListContent rightList ->
                loop prevResult (ListContent [ Number leftNumber ], ListContent rightList)
            | ListContent leftList, ListContent rightList -> compareListElements None leftList rightList)

    loop None (leftPacket, rightPacket)

let result1 =
    System.IO.File.ReadAllText("Data/Day13.txt")
    |> parseInput
    |> Seq.chunkBySize 2
    // |> Seq.mapi (fun i packets -> i + 1, (compare (packets[0], packets[1])))
    // |> Seq.filter (fun (_, r) -> r = Some true)
    |> Seq.indexed
    |> Seq.filter (fun (_, packets) -> compare (packets[0], packets[1]) = Some true)
    |> Seq.sumBy fst //|> Seq.fold (fun acc (i, _) -> acc + i) 0

let result2 =
    let dividers = [ parsePacket "[[2]]"; parsePacket "[[6]]" ]
    let sortedList =
        System.IO.File.ReadAllText("Data/Day13.txt")
        |> parseInput
        |> Seq.append dividers
        |> Seq.sortWith (fun left right ->
            match compare (left, right) with
            | Some true -> -1
            | Some false -> 1
            | None -> 0)
        |> Seq.toList

    dividers |> Seq.map (fun d -> List.findIndex ((=) d) sortedList + 1) |> Seq.reduce (*)


// let findIndex searchedPacket l =
//     (l |> List.findIndex (fun p -> p = searchedPacket)) + 1
// (sortedList |> findIndex firstDivider) * (sortedList |> findIndex secondDivider)

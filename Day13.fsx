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
        let isNumber, number = Int32.TryParse content

        if isNumber then
            Number number
        else
            match content with
            | "[]" -> ListContent []
            | complexContent -> ListContent(complexContent |> getSubContent |> List.map mapElement)

    mapElement line

let parseInput (lines: string) =
    lines.Split("\n")
    |> Seq.filter (fun l -> not (String.IsNullOrEmpty l))
    |> Seq.map parsePacket

let zip placeEmpty leftPackets rightPackets =
    let length = List.max [ List.length leftPackets; List.length rightPackets ]

    let defaultEmpty index list =
        list |> List.tryItem index |> Option.defaultValue placeEmpty

    seq { for i in 0 .. length - 1 -> leftPackets |> defaultEmpty i, rightPackets |> defaultEmpty i }

let rec getFirstSome (values: seq<'a Option>) =
    match values |> Seq.tryHead with
    | None -> None
    | Some h ->
        match h with
        | Some sh -> Some sh
        | None -> getFirstSome (values |> Seq.skip 1)

let compare (leftPacket, rightPacket) =
    let rec loop prevResult (left, right) =
        match prevResult with
        | Some r -> Some r
        | None ->
            match left, right with
            | Number leftNumber, Number rightNumber ->
                if leftNumber < rightNumber then Some true
                else if leftNumber > rightNumber then Some false
                else None
            | ListContent leftList, Number numberRight ->
                let left = ListContent leftList
                let right = ListContent [ Number numberRight ]
                loop prevResult (left, right)
            | Number leftNumber, ListContent rightList ->
                let left = ListContent [ Number leftNumber ]
                let right = ListContent rightList
                loop prevResult (left, right)
            | ListContent leftList, ListContent rightList ->
                let leftLength = List.length leftList
                let rightLength = List.length rightList

                if leftLength > 0 && rightLength = 0 then
                    Some false
                else if leftLength = 0 && rightLength > 0 then
                    Some true
                else
                    let pairs = zip (Number -1) leftList rightList
                    pairs |> Seq.map (loop prevResult) |> getFirstSome

    loop None (leftPacket, rightPacket)

let result1 =
    System.IO.File.ReadAllText("Data/Day13.txt")
    |> parseInput
    |> Seq.chunkBySize 2
    |> Seq.mapi (fun i packets -> i + 1, (compare (packets[0], packets[1])))
    |> Seq.filter (fun (_, r) -> r = Some true)
    |> Seq.fold (fun acc (i, _) -> acc + i) 0

let result2 =
    let firstDivider = parsePacket "[[2]]"
    let secondDivider = parsePacket "[[6]]"

    let sortedList =
        System.IO.File.ReadAllText("Data/Day13.txt")
        |> parseInput
        |> Seq.append [ firstDivider; secondDivider ]
        |> Seq.sortWith (fun left right ->
            match compare (left, right) with
            | Some v -> if v = true then -1 else 1
            | None -> 0)
        |> Seq.toList

    let findIndex searchedPacket l =
        (l |> List.findIndex (fun p -> p = searchedPacket)) + 1

    (sortedList |> findIndex firstDivider) * (sortedList |> findIndex secondDivider)

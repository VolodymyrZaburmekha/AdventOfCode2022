open System
open System.IO
open Microsoft.FSharp.Collections

// no changes at all :) code formatting breaks this file :) there is a bug in F# code formatting

type RawScreen = { Encoding: string array; ValueToDecode: string list }

let input =
    File.ReadAllLines "Data/Day8_2021.txt"
    |> Seq.map (fun l ->
        let split (divider: char) (str: string) = str.Split(divider, StringSplitOptions.RemoveEmptyEntries)
        let parts = l |> split '|'
        { Encoding = parts[0] |> split ' ' |> Seq.sortBy (fun p -> p.Length) |> Seq.toArray
          ValueToDecode = parts[1] |> split ' ' |> Seq.toList })
    |> Seq.toList

let getDecodeFunc screen =
    let allChars = screen.Encoding |> Seq.reduce (fun prev next -> prev + next) |> Seq.countBy id |> Seq.toList
    let getCharByCount count =
        allChars |> Seq.where (fun (_, charCount) -> charCount = count) |> Seq.map fst |> Seq.toList
    let bottomLeft = getCharByCount 4 |> Seq.exactlyOne
    let topLeft = getCharByCount 6 |> Seq.exactlyOne
    let bottomRight = getCharByCount 9 |> Seq.exactlyOne
    let topRight = Set.difference (screen.Encoding[0] |> Set) (Set [ bottomRight ]) |> Seq.exactlyOne
    let top = Set.difference (Set screen.Encoding[1]) (Set screen.Encoding[0]) |> Seq.exactlyOne
    let middle = Set.difference (Set screen.Encoding[2]) (Set [ topLeft; topRight; bottomRight ]) |> Seq.exactlyOne
    let allCharsSet = allChars |> Seq.map fst |> Set
    let bottom =
        Set.difference
            allCharsSet
            (Set [ top
                   topLeft
                   topRight
                   middle
                   bottomLeft
                   bottomRight ])
        |> Seq.exactlyOne

    fun (str: string) ->
        let strSet = Set str

        match str.Length with
        | 2 -> 1
        | 3 -> 7
        | 4 -> 4
        | 5 -> // 2 or 3 or 5
            if strSet = Set[top
            topRight
            middle
            bottomLeft
            bottom] then
                2
            elif strSet = Set [ top
                                topRight
                                middle
                                bottomRight
                                bottom ] then
                3
            else
                5
        | 6 -> // 6 or 9 or 0
            if strSet = Set [ top
                              topLeft
                              middle
                              bottomLeft
                              bottomRight
                              bottom ] then
                6
            elif strSet = Set [ top
                                topLeft
                                topRight
                                bottomLeft
                                bottomRight
                                bottom ] then
                0
            else
                9
        | 7 -> 8
        | _ -> failwith $"can't decode '{str}'"

let solve (f: int seq -> int) =
    input
    |> Seq.map (fun screen ->
        let decode = getDecodeFunc screen
        screen.ValueToDecode |> Seq.map decode |> f)
    |> Seq.sum

let part1 = solve (fun s -> s |> Seq.where (fun n -> n = 1 || n = 4 || n = 7 || n = 8) |> Seq.length)
let part2 = solve (fun s -> s |> Seq.fold (fun acc n -> acc + n.ToString()) "" |> Int32.Parse)

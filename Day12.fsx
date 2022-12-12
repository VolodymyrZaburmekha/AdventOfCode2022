open System.Collections.Generic
open System.IO

let parseInput (lines: string []) =
    let findCharIndex (char: char) =
        lines
        |> Array.findIndex (fun l -> l.Contains(char))
        |> fun y -> (y, lines.[y].IndexOf(char))

    let start = findCharIndex 'S'
    let finish = findCharIndex 'E'

    let map =
        lines
        |> Array.map (fun line ->
            line
            |> Seq.map (fun c ->
                match c with
                | 'S' -> 'a'
                | 'E' -> 'z'
                | normalChar -> normalChar)
            |> Array.ofSeq)
        |> array2D

    (start, finish, map)


let findPaths map start finish =
    //Y*X, Distance
    let visitedPoints =
        Dictionary<int * int, int>()

    //Y,X
    let rec makeStep
        (map: char [,])
        (currentPosition: int * int)
        (currentStep: int)
        (previouslyVisited: Dictionary<int * int, int>)
        (finish: int * int)
        =
        let contains, value =
            previouslyVisited.TryGetValue currentPosition

        if contains && currentStep >= value then
            previouslyVisited
        else
            let mutable curretlyVisited =
                previouslyVisited

            let currentY, currentX = currentPosition

            curretlyVisited[currentPosition] <- currentStep

            if currentPosition = finish then
                curretlyVisited
            else
                let mapHeight = map |> Array2D.length1
                let mapWidth = map |> Array2D.length2

                let unreachable = int 'z' + 2

                let currentValue =
                    int map[currentY, currentX]

                let up =
                    if currentY - 1 >= 0 then
                        map[currentY - 1, currentX] |> int
                    else
                        unreachable

                let down =
                    if currentY + 1 < mapHeight then
                        map[currentY + 1, currentX] |> int
                    else
                        unreachable

                let right =
                    if currentX + 1 < mapWidth then
                        map[currentY, currentX + 1] |> int
                    else
                        unreachable

                let left =
                    if currentX - 1 >= 0 then
                        map[currentY, currentX - 1] |> int
                    else
                        unreachable

                if up - currentValue < 2 then
                    curretlyVisited <- makeStep map (currentY - 1, currentX) (currentStep + 1) curretlyVisited finish

                if down - currentValue < 2 then
                    curretlyVisited <- makeStep map (currentY + 1, currentX) (currentStep + 1) curretlyVisited finish

                if right - currentValue < 2 then
                    curretlyVisited <- makeStep map (currentY, currentX + 1) (currentStep + 1) curretlyVisited finish

                if left - currentValue < 2 then
                    curretlyVisited <- makeStep map (currentY, currentX - 1) (currentStep + 1) curretlyVisited finish
                else
                    ()

                curretlyVisited

    makeStep map start 0 visitedPoints finish

let input =
    parseInput (File.ReadAllLines "Data/Day12.txt")

let start, finish, arr = input

let result1 =
    findPaths arr start finish
    |> Seq.filter (fun kv -> kv.Key = finish)
    |> Seq.sortBy (fun kv -> kv.Value)
    |> Seq.head
    |> (fun kv -> kv.Value)
let result2 =
    let findCharIndexes (map: char [,]) (char: char) =
        let indexed =
            map |> Array2D.mapi (fun y x c -> (y, x, c))

        [ 0 .. (map |> Array2D.length1) - 1 ]
        |> List.collect (fun index -> List.ofArray indexed[index, *])
        |> List.filter (fun (_, _, c) -> c = char)
        |> List.map (fun (y, x, _) -> (y, x))

    let starts = findCharIndexes arr 'a'

    starts
    |> Seq.collect (fun s -> findPaths arr s finish)
    |> Seq.filter (fun kv -> kv.Key = finish)
    |> Seq.sortBy (fun kv -> kv.Value)
    |> Array.ofSeq
    |> Seq.head
    |> (fun h -> h.Value)

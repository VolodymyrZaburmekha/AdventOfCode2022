open System
open System.IO
open Microsoft.FSharp.Collections

let getRockStructure input =
    let getRockFootprint path =
        let getLineFootprint (xStart, yStart) (xEnd, yEnd) =
            let getPath p1 p2 =
                let min, max = if p1 > p2 then (p2, p1) else (p1, p2)
                [ for i in min..max -> i ]

            match xStart, yStart, xEnd, yEnd with
            | sameX1, diffY1, sameX2, diffY2 when sameX1 = sameX2 ->
                getPath diffY1 diffY2 |> List.map (fun p -> sameX1, p)
            | diffX1, sameY1, diffX2, sameY2 when sameY1 = sameY2 ->
                getPath diffX1 diffX2 |> List.map (fun p -> p, sameY1)
            | _, _, _, _ -> failwith $"bad input data (%i{xStart}, %i{yStart}) -> (%i{xEnd}, %i{yEnd})"
        path |> List.windowed 2 |> List.map (fun l -> getLineFootprint l[0] l[1]) |> List.collect (fun l -> l)

    input |> List.map (fun path -> getRockFootprint path) |> List.collect (fun l -> l) |> Set

let simulate canReachFloor map =
    let depth = map |> Seq.maxBy (fun (_, y) -> y) |> snd

    let simulateSandDrop (xFall, yFall) map =
        let rec sandDropLoop (x, y) =
            if canReachFloor && y = depth + 1 then
                Some(x, y)
            elif not (canReachFloor) && y > depth then
                None
            else
                let newY = y + 1
                let needLoop, newPoint =
                    if not (map |> Set.contains (x, newY)) then
                        true, (x, newY)
                    elif not (map |> Set.contains (x - 1, newY)) then
                        true, (x - 1, newY)
                    elif not (map |> Set.contains (x + 1, newY)) then
                        true, (x + 1, newY)
                    else
                        // break
                        false, (x, y)
                match needLoop, newPoint with
                | false, p -> Some p
                | true, p -> sandDropLoop p
        sandDropLoop (xFall, yFall)

    let rec loop count map =
        let sandPosition = simulateSandDrop (500, 0) map
        match sandPosition with
        | None -> count
        | Some(x, y) ->
            if y = 0 then
                count
            else
                let updatedMap = map |> Set.add (x, y)
                loop (count + 1) updatedMap
    loop 0 map

let map =
    File.ReadAllLines "Data/Day14.txt"
    |> Array.map (fun p ->
        p.Split(" -> ")
        |> Seq.map (fun point -> point.Split(",") |> fun xy -> Int32.Parse xy[0], Int32.Parse xy[1])
        |> Seq.toList)
    |> Seq.toList
    |> getRockStructure

let part1 = simulate false map
let part2 = simulate true map + 1
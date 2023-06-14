open System
open System.IO
open Microsoft.FSharp.Collections

let getRockStructure input =
    let getPath p1 p2 = { (min p1 p2) .. (max p1 p2) } // let min, max = if p1 > p2 then (p2, p1) else (p1, p2) [ for i in min..max -> i ]

    let getRockFootprint path =
        let getLineFootprint ((x1, y1), (x2, y2)) =
            if x1 = x2 then getPath y1 y2 |> Seq.map (fun y -> x1, y)
            elif y1 = y2 then getPath x1 x2 |> Seq.map (fun x -> x, y1)
            else failwith $"bad input data (%i{x1}, %i{y1}) -> (%i{x2}, %i{y2})"

        path |> Seq.pairwise |> Seq.collect getLineFootprint //path |> Seq.windowed 2 |> Seq.map (fun l -> getLineFootprint l[0] l[1]) |> Seq.collect (fun l -> l)

    input |> Seq.collect getRockFootprint |> set //input |> List.map (fun path -> getRockFootprint path) |> List.collect (fun l -> l) |> Set

let getPath p1 p2 = { (min p1 p2) .. (max p1 p2) }

let getRockStructure2 input =
    input
    |> Seq.collect (fun path ->
        path
        |> Seq.pairwise
        |> Seq.collect (fun ((x1, y1), (x2, y2)) ->
            if x1 = x2 then getPath y1 y2 |> Seq.map (fun y -> x1, y)
            elif y1 = y2 then getPath x1 x2 |> Seq.map (fun x -> x, y1)
            else failwith $"bad input data (%i{x1}, %i{y1}) -> (%i{x2}, %i{y2})"))
    |> Set


let simulate canReachFloor map =
    let depth = map |> Seq.map snd |> Seq.max //let depth = map |> Seq.maxBy (fun (_, y) -> y) |> snd

    let simulateSandDrop (xFall, yFall) map =
        let rec sandDropLoop (x, y) =
            if y = depth + 1 then
                if canReachFloor then Some(x, y) else None
            else
                let newY = y + 1
                let newP =
                    seq {
                        (x, newY)
                        (x - 1, newY)
                        (x + 1, newY)
                    }
                    |> Seq.tryFind (fun p -> not (Set.contains p map))
                match newP with
                | None -> Some(x, y)
                | Some p -> sandDropLoop p
        sandDropLoop (xFall, yFall)

    let rec loop count map =
        match simulateSandDrop (500, 0) map with
        | None
        | Some (_, 0) -> count
        | Some (x, y) -> loop (count + 1) (Set.add (x, y) map)
    loop 0 map


let sw = System.Diagnostics.Stopwatch.StartNew()

let map =
    File.ReadAllLines "Data/Day14.txt"
    |> Seq.map (fun p -> p.Split(" -> ") |> Seq.map (fun point -> let xy = point.Split(",") in int xy[0], int xy[1]))
    |> getRockStructure

let part1 = simulate false map
let part2 = simulate true map + 1

let TIME = sw.ElapsedMilliseconds

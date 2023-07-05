open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

// perfect :)

let xFrom, yFrom, xTo, yTo =
    "target area: x=230..283, y=-107..-57".Replace("target area: ", "").Split(", ")
    |> fun coordinates ->
        let xFromTo = coordinates[ 0 ].Replace("x=", "").Split("..")
        let yFromTo = coordinates[ 1 ].Replace("y=", "").Split("..")
        Int32.Parse(xFromTo[0]), Int32.Parse(yFromTo[0]), Int32.Parse(xFromTo[1]), Int32.Parse(yFromTo[1])

let isPointInTarget x y = (x >= min xFrom xTo) && (x <= max xFrom xTo) && (y >= min yFrom yTo) && (y <= max yFrom yTo)

let isPointCantReachTarget _ y = y < min yFrom yTo

type SimulationResult =
    | HitTarget of int // max Y
    | Missed

let simulate (velocityX, velocityY) =
    let rec loop maxY (positionX, positionY) (velocityX, velocityY) =
        let newX, newY = positionX + velocityX, positionY + velocityY
        let newMaxY = max maxY newY
        if isPointInTarget newX newY then
            HitTarget newMaxY
        elif isPointCantReachTarget newX newY then
            Missed
        else
            let newVelocityX =
                match velocityX with
                | moreThan0 when moreThan0 > 0 -> velocityX - 1
                | lessThan0 when lessThan0 < 0 -> velocityX + 1
                | _ -> 0
            let newVelocityY = velocityY - 1
            loop newMaxY (newX, newY) (newVelocityX, newVelocityY)
    loop 0 (0, 0) (velocityX, velocityY)

let sideMultiplier = if (min xFrom xTo) > 0 then 1 else -1

let xS = seq { for x in 1 .. abs (max xFrom xTo) -> x * sideMultiplier }
let yS = seq { for y in (min yFrom yTo) .. (max (abs yFrom) (abs yTo)) -> y }

#time "on"

let hits =
    Seq.allPairs xS yS
    |> Seq.choose (fun (x, y) ->
        match simulate (x, y) with
        | HitTarget h -> Some h
        | _ -> None)
    |> Seq.toList


let part1 = hits |> List.max
let part2 = hits |> List.length

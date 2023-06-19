open System
open System.IO
open Microsoft.FSharp.Collections

#time "on"

type Shape =
    | HorizontalLine
    | Plus
    | JShape
    | VerticalLine
    | Square

let createShapeBitmap shape =
    match shape with
    | HorizontalLine -> [ (0L, 0L); (1L, 0L); (2L, 0L); (3L, 0L) ]
    | Plus -> [ (1L, 0L); (0L, 1L); (1L, 1L); (2L, 1L); (1L, 2L) ]
    | JShape -> [ (0L, 0L); (1L, 0L); (2L, 0L); (2L, 1L); (2L, 2L) ]
    | VerticalLine -> [ (0L, 0L); (0L, 1L); (0L, 2L); (0L, 3L) ]
    | Square -> [ (0L, 0L); (1L, 0L); (0L, 1L); (1L, 1L) ]

type Move =
    | Left
    | Right
    | Down
    | Up

let moveShapeByPoints (points: int64) (direction: Move) (shape: seq<int64 * int64>) =
    let shiftX, shiftY =
        match direction with
        | Left -> -points, 0L
        | Right -> points, 0L
        | Down -> 0, -points
        | Up -> 0, points
    shape |> Seq.map (fun (x, y) -> x + shiftX, y + shiftY)

let moveShape direction = moveShapeByPoints 1L direction


// let a = 1000000000000L

let resolve =
    let shapes =
        (seq {
            while true do
                yield HorizontalLine
                yield Plus
                yield JShape
                yield VerticalLine
                yield Square
        })
            .GetEnumerator()

    let moves =
        (seq {
            while true do
                yield! File.ReadAllText("Data/Day17.txt") |> Seq.map (fun c -> if c = '>' then Right else Left)
        })
            .GetEnumerator()

    [ 1..2022 ]
    |> List.fold
        (fun mapState _ ->
            let rec moveShapeUntilEnd shape =
                let intersectsWithMap shapeProjection =
                    shapeProjection |> Set |> Set.intersect mapState |> Seq.isEmpty |> not


                let isStopped shape = shape |> moveShape Down |> intersectsWithMap

                let canMoveSidewaysToProjection projection =
                    if projection |> Seq.exists (fun (x, _) -> x < 0L || x > 6L) then
                        false
                    else
                        projection |> intersectsWithMap |> not

                let _ = moves.MoveNext()
                let currentMove = moves.Current
                let potentialSidewaysShiftedPosition = shape |> moveShape currentMove

                let newSidewaysPosition =
                    if canMoveSidewaysToProjection potentialSidewaysShiftedPosition then
                        potentialSidewaysShiftedPosition
                    else
                        shape
                if isStopped newSidewaysPosition then
                    newSidewaysPosition |> Set |> Set.union mapState
                else
                    moveShapeUntilEnd (newSidewaysPosition |> moveShape Down)

            let _ = shapes.MoveNext()

            let maxPosition = mapState |> Seq.maxBy (fun (_, y) -> y) |> snd
            let shape =
                shapes.Current
                |> createShapeBitmap
                |> moveShapeByPoints 2 Right
                |> moveShapeByPoints (maxPosition + 4L) Up

            let newMapState = moveShapeUntilEnd shape
            let maxY = newMapState |> Seq.maxBy (fun (_, y) -> y) |> snd
            let optimizedMap = newMapState |> Seq.where (fun (_, y) -> y >= maxY - 45L) // 45 is a minimum number which works
            optimizedMap |> Set)

        ([ for x in 0L .. 6L do
               (x, 0L) ]
         |> Set)
    |> Seq.maxBy (fun (_, y) -> y)

open System.IO
open System
open Microsoft.FSharp.Collections

type SensorReading =
    { sX: int
      sY: int
      bX: int
      bY: int
      distance: int
      leftBorder: int
      rightBorder: int
      topBorder: int
      bottomBorder: int }

let getDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let solvePart1 input line =
    let intersections =
        input
        |> Seq.filter (fun s -> s.topBorder <= line && s.bottomBorder >= line)
        |> Seq.map (fun sr ->
            let dy = abs (line - sr.sY)
            sr.leftBorder + dy, sr.rightBorder - dy)
        |> List.ofSeq
    let min = intersections |> List.minBy fst |> fst
    let max = intersections |> List.maxBy snd |> snd

    max - min

let input =
    "Data/Day15.txt"
    |> File.ReadAllLines
    |> Seq.map (fun line ->

        let parseXy (str: string) =
            let numbers = str.Replace("x=", "").Replace("y=", "").Split(",")
            Int32.Parse numbers[0], Int32.Parse numbers[1]
        let parts = line.Split(":")
        let sensorPosition, nearestBeacon =
            parts[0].Replace("Sensor at ", "") |> parseXy, parts[1].Replace(" closest beacon is at ", "") |> parseXy

        let sX, sY = sensorPosition
        let bX, bY = nearestBeacon

        let distance = getDistance sensorPosition nearestBeacon
        { sX = sX
          sY = sY
          bX = bX
          bY = bY
          leftBorder = sX - distance
          rightBorder = sX + distance
          topBorder = sY - distance
          bottomBorder = sY + distance
          distance = distance })
    |> List.ofSeq

let part1 = solvePart1 input 2000000
let part2 =
    let ascending, descending =
        input
        |> Seq.fold
            (fun (ascending, descending) sr ->
                (sr.sX - sr.sY - sr.distance) :: (sr.sX - sr.sY + sr.distance) :: ascending,
                (sr.sX + sr.sY - sr.distance) :: (sr.sX + sr.sY + sr.distance) :: descending)
            ([], [])

    let getSuitableLine equations =
        let pairEach s =
            seq {
                for i in s do
                    for i2 in s do
                        yield (i, i2)
            }
        equations
        |> pairEach
        |> Seq.find (fun (left, right) -> abs (left - right) = 2)
        |> fun (first, second) -> (if first < second then first else second) + 1

    let asc = getSuitableLine ascending
    let desc = getSuitableLine descending

    let x, y = (asc + desc) / 2, (desc - asc) / 2

    (uint64 x) * 4000000UL + (uint64 y)

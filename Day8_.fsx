open System.IO

let parseInput input = input |> Seq.map (Seq.map (fun c -> int $"%c{c}")) |> array2D

let getDimensions input = Array2D.length1 input, Array2D.length2 input // height, width

let getVisibleMap (input: int [,]) =
    let height, width = getDimensions input

    let isTheBiggestInSlice position value slice =
        let before, after = Array.splitAt position slice
        [ (Seq.skip 1 after); before ] |> Seq.exists (Seq.forall (fun p -> p < value))

    let isElementVisible (y, x) value =
        (x = 0 || y = 0 || x = (width - 1) || y = (height - 1))
        || isTheBiggestInSlice y value input[*, x]
        || isTheBiggestInSlice x value input[y, *]

    Array2D.mapi (fun y x value -> (value, isElementVisible (y, x) value)) input




let rec count maxValue (slicePart: int list) =
    match slicePart with
    | head :: tail -> if head >= maxValue then 1 else (1 + count maxValue tail)
    | _ -> 0

let getScenicScoreForSlice position value slice =
    let before, after = Array.splitAt position slice
    let beforeCount = before |> Seq.fold (fun lst item -> item :: lst) [] |> count value
    let afterCount = after |> Seq.skip 1 |> Seq.toList |> count value
    (beforeCount, afterCount)

let getScenicScoreMap (input: int [,]) =

    let getElementScore (y, x) value =
        seq {
            getScenicScoreForSlice y value input[*, x]
            getScenicScoreForSlice x value input[y, *]
        }
        |> Seq.fold (fun total (b, a) -> total * b * a) 1

    Array2D.mapi (fun y x value -> (value, getElementScore (y, x) value)) input


let sw = System.Diagnostics.Stopwatch.StartNew()

let input = File.ReadAllLines "Data/Day8.txt"

let parsed = parseInput input

let part1 = getVisibleMap parsed |> Seq.cast<int * bool> |> Seq.filter snd |> Seq.length

let part2 = getScenicScoreMap parsed |> Seq.cast<int * int> |> Seq.map snd |> Seq.max

let TIME = sw.ElapsedMilliseconds

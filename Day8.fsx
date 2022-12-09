open System.IO

let parseInput (input: string seq) =
    input
    |> Seq.toList
    |> List.map (fun l ->
        l
        |> Seq.toList
        |> List.map (fun c -> int $"%c{c}"))
    |> array2D

let getDimensions input =
    (input |> Array2D.length1, input |> Array2D.length2) // height, width

let getVisibleMap (input: int [,]) =
    let height, width = input |> getDimensions

    let isElementVisible (y, x) =
        let value = input[y, x]

        let isTheBiggestInSlice position slice =
            let isLess parameter = parameter < value
            let before = slice |> Array.take position

            let after =
                slice |> Array.skip (position + 1)

            (before |> Array.forall isLess)
            || (after |> Array.forall isLess)

        if x = 0
           || y = 0
           || x = (width - 1)
           || y = (height - 1) then
            true
        else if isTheBiggestInSlice y input[*, x] then
            true
        else if isTheBiggestInSlice x input[y, *] then
            true
        else
            false

    input
    |> Array2D.mapi (fun y x value -> (value, isElementVisible (y, x)))


let getScenicScoreMap (input: int [,]) =
    let getElementScore (y, x) =
        let value = input[y, x]

        let getScenicScoreForSlice position slice =
            let rec count maxValue (slicePart: int list) =
                match slicePart with
                | head :: tail ->
                    if head >= maxValue then
                        1
                    else
                        (1 + count maxValue tail)
                | [ oneValue ] when oneValue <= maxValue -> 1
                | _ -> 0

            let beforeCount =
                slice
                |> Array.take position
                |> Array.rev
                |> List.ofArray
                |> count value

            let afterCount =
                slice
                |> Array.skip (position + 1)
                |> List.ofArray
                |> count value

            (beforeCount, afterCount)


        let verticalBefore, verticalAfter =
            getScenicScoreForSlice y input[*, x]

        let horizontalBefore, horizontalAfter =
            getScenicScoreForSlice x input[y, *]

        verticalBefore
        * verticalAfter
        * horizontalBefore
        * horizontalAfter

    input
    |> Array2D.mapi (fun y x value -> (value, getElementScore (y, x)))


let input =
    File.ReadAllLines "Data/Day8.txt"

let parsed = parseInput input

let part1 =
    getVisibleMap parsed
    |> (fun visibleArr ->
        let mutable visibleCount = 0

        for y in 0 .. (Array2D.length1 visibleArr) - 1 do
            visibleCount <-
                visibleArr[y, *]
                |> Array.filter (fun (_, visible) -> visible = true)
                |> Array.length
                |> (+) visibleCount

        visibleCount)

let part2 =
    getScenicScoreMap parsed
    |> (fun scenicArr ->
        let mutable max = 0

        for y in 0 .. (Array2D.length1 scenicArr) - 1 do
            let _, lineMax =
                scenicArr[y, *]
                |> Array.maxBy (fun (_, score) -> score)

            max <- if max < lineMax then lineMax else max

        max)

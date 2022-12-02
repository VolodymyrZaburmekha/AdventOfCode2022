open System.IO

type Shape =
    | Rock
    | Paper
    | Scissors

type GameResult =
    { Player1Score: int
      Player2Score: int }

let playGame (player1Shape, player2Shape) =
    let getScoreForShape shape =
        match shape with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let getWinScore (shape1, shape2) =
        match (shape1, shape2) with
        | (Scissors, Paper)
        | (Paper, Rock)
        | (Rock, Scissors) -> 6
        | (s1, s2) when s1 = s2 -> 3
        | _ -> 0

    let getScoreForPlayer (interestedPlayerShape, otherPlayerShape) =
        getWinScore (interestedPlayerShape, otherPlayerShape)
        + getScoreForShape (interestedPlayerShape)

    { Player1Score = getScoreForPlayer (player1Shape, player2Shape)
      Player2Score = getScoreForPlayer (player2Shape, player1Shape) }

let part1ParseGame (player1ShapeStr, player2ShapeStr) =
    let getShapeFromStr shape =
        match shape with
        | "A"
        | "X" -> Rock
        | "B"
        | "Y" -> Paper
        | "C"
        | "Z" -> Scissors

    getShapeFromStr player1ShapeStr, getShapeFromStr player2ShapeStr

let part2ParseGame (player1ShapeStr, player2ShapeStr) =
    let player1Shape =
        match player1ShapeStr with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors

    let player2Shape =
        match (player2ShapeStr, player1Shape) with
        | "X", shape -> // if we need to loose
            match shape with
            | Rock -> Scissors
            | Paper -> Rock
            | Scissors -> Paper
        | "Y", shape -> shape // draw
        | "Z", shape -> //win
            match shape with
            | Rock -> Paper
            | Paper -> Scissors
            | Scissors -> Rock

    (player1Shape, player2Shape)

let solve parseGame (input: string) =
    let parsedInput =
        input.Split('\n')
        |> Array.map (fun line ->
            match line.Split(' ') with
            | [| a; b |] -> (a, b))
        |> Array.map parseGame

    parsedInput
    |> Array.map playGame
    |> Array.sumBy (fun r -> r.Player2Score)

let inputFromFile =
    File.ReadAllText "Data/Day2.txt"

solve part1ParseGame inputFromFile

solve part2ParseGame inputFromFile

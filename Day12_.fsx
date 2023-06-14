open System.Collections.Generic
open System.IO

let findCharIndex (lines: string []) (c: char) =
    // lines |> Array.findIndex (fun l -> l.Contains(char)) |> fun y -> (y, lines.[y].IndexOf(char))
    let y = lines |> Array.findIndex (fun l -> l.Contains(c)) in (y, lines.[y].IndexOf(c))
// let y = lines |> Array.findIndex (fun l -> l.Contains(c))
// (y, lines.[y].IndexOf(c))




let parseInput (lines: string []) =
    // let findCharIndex (char: char) =
    //     lines |> Array.findIndex (fun l -> l.Contains(char)) |> fun y -> (y, lines.[y].IndexOf(char))

    let start = findCharIndex lines 'S'
    let finish = findCharIndex lines 'E'

    let map =
        lines
        |> Seq.map (
            Seq.map (function
                | 'S' -> 'a'
                | 'E' -> 'z'
                | normalChar -> normalChar)
        )
        |> array2D

    (start, finish, map)


let findPaths map start finish =

    let rec makeStep (board: char [,]) position step (visited: Map<int * int, int>) finish =

        let contains, value = visited.TryGetValue position

        if contains && step >= value then
            visited
        else
            let visited' = Map.change position (fun _ -> Some step) visited

            if position = finish then
                visited'
            else
                let height = Array2D.length1 board
                let width = Array2D.length2 board
                let y, x = position
                let value = int board[y, x]

                seq {
                    if y - 1 >= 0 then (y - 1, x)
                    if y + 1 < height then (y + 1, x)
                    if x + 1 < width then (y, x + 1)
                    if x - 1 >= 0 then (y, x - 1)
                }
                |> Seq.filter (fun (y', x') -> int board[y', x'] - value < 2)
                |> Seq.fold (fun visited'' (y', x') -> makeStep board (y', x') (step + 1) visited'' finish) visited'

    makeStep map start 0 (Map []) finish



let input = parseInput (File.ReadAllLines "Data/Day12.txt")

let start, finish, arr = input

let result1 =
    findPaths arr start finish |> Seq.choose (fun kv -> if kv.Key = finish then Some kv.Value else None) |> Seq.min


let findCharIndexes (map: char [,]) (searchedChar: char) =
    let indexed = map |> Array2D.mapi (fun y x c -> (y, x, c))
    // [ 0 .. (map |> Array2D.length1) - 1 ]
    // |> List.collect (fun index -> List.ofArray indexed[index, *])
    // |> List.filter (fun (_, _, c) -> c = char)
    // |> List.map (fun (y, x, _) -> (y, x))
    indexed |> Seq.cast<int * int * char> |> Seq.choose (fun (y, x, c) -> if c = searchedChar then Some(y, x) else None)


let result2 =
    let starts = findCharIndexes arr 'a' |> Seq.toArray
    starts
    |> Seq.collect (fun s -> findPaths arr s finish)
    |> Seq.choose (fun kv -> if kv.Key = finish then Some kv.Value else None)
    |> Seq.min



// type List3<'t> =
//     | Empty
//     | Cons of Head: 't * Tail: List3<'t>


// Cons(1, Empty)

// let x = [1;2;3] |> Seq.map Some

// type List2<'T> =
//     | ([])  : List2<'T>
//     | ( :: )  : 'T * List2<'T>      -> 'List2<'T>


// let reverse (s: seq<_>) =
//     let list = s |> Seq.toList
//     let maxIndex = list.Length - 1
//     seq {
//         for i in 0..maxIndex do
//             yield list[maxIndex - i]
//     }

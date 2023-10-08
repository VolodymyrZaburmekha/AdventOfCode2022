open Microsoft.FSharp.Core

type Tree<'a when 'a: comparison> =
    | Empty
    | Node of value: 'a * left: Tree<'a> * right: Tree<'a>

type LevelsMap<'a when 'a: comparison> = Map<int, Map<int, 'a option>>
let parse elements =
    let rec buildLevels (level: int) (remaining: 'a option seq) (levelIndexes: int list option) (acc: LevelsMap<'a>) =
        if Seq.isEmpty remaining then
            acc
        else
            let levelItemsCount =
                match levelIndexes with
                | Some i -> i.Length
                | None -> 1
            let elementsInLevel = remaining |> Seq.take levelItemsCount

            let indexedLevelElements =
                match levelIndexes with
                | Some li -> Map(Seq.zip li elementsInLevel)
                | None -> elementsInLevel |> Seq.indexed |> Map
            let nextLevelIndexes =
                indexedLevelElements
                |> Seq.indexed
                |> Seq.choose (fun (_, kv) ->
                    let index = kv.Key
                    let value = kv.Value
                    if Option.isSome value then Some [ index * 2; index * 2 + 1 ] else None)
                |> Seq.collect id
                |> Seq.toList

            buildLevels
                (level + 1)
                (remaining |> Seq.skip levelItemsCount)
                (Some nextLevelIndexes)
                (acc |> Map.add level indexedLevelElements)

    let rec buildTree (levels: LevelsMap<'a>) =
        let rec loop (levelIndex: int, elementIndex: int) =
            let elementOption = levels |> Map.tryFind levelIndex |> Option.bind (Map.find elementIndex)
            match elementOption with
            | None -> Tree.Empty
            | Some element ->
                let leftIndex = elementIndex * 2
                let rightIndex = leftIndex + 1
                let nextLevel = levelIndex + 1
                Tree.Node(element, (loop (nextLevel, leftIndex)), (loop (nextLevel, rightIndex)))

        loop (0, 0)
    buildLevels 0 elements None Map.empty |> buildTree

let rec add element tree =
    match tree with
    | Empty -> Tree.Node(element, Empty, Empty)
    | Node(value, left, right) ->
        if value = element then
            tree
        elif element > value then
            let right' = add element right
            Node(value, left, right')
        else
            let left' = add element left
            Node(value, left', right)
let rec exist tree element =
    match tree with
    | Empty -> false
    | Node(value, left, right) ->
        if value = element then true
        elif value < element then exist right element
        else exist left element


let rec inorderTraversal tree =
    match tree with
    | Empty -> []
    | Node(value, left, right) -> (inorderTraversal left) @ [ value ] @ (inorderTraversal right)

let rec preorderTraversal tree =
    match tree with
    | Empty -> []
    | Node(value, left, right) -> value :: (preorderTraversal left) @ (preorderTraversal right)

let rec postorderTraversal tree =
    match tree with
    | Empty -> []
    | Node(value, left, right) -> (postorderTraversal left) @ (postorderTraversal right) @ [ value ]

let discoverTree f tree =
    let rec loop depth (treeNode: Tree<'a>) =
        match treeNode with
        | Empty -> depth
        | Node(_, left, right) ->
            let nextLvl = depth + 1
            match (left, right) with
            | Empty, Empty -> depth
            | l, Empty -> loop nextLvl l
            | Empty, r -> loop nextLvl r
            | l, r -> f (loop nextLvl l) (loop nextLvl r)
    loop 1 tree

let minDepth tree = discoverTree min tree
let maxDepth tree = discoverTree max tree

parse [ Some 3; Some 9; Some 20; None; None; Some 15; Some 7 ] |> minDepth = 2
parse [ Some 2; None; Some 3; None; Some 4; None; Some 5; None; Some 6 ] |> minDepth = 5

parse [ Some 3; Some 9; Some 20; None; None; Some 15; Some 7 ] |> maxDepth = 3
parse [ Some 1; None; Some 2 ] |> maxDepth = 2

let rec validate tree =
    match tree with
    | Empty -> true
    | Node(value, left, right) ->
        match (left, right) with
        | Empty, Empty -> true
        | Empty, _ -> false
        | _, Empty -> false
        | Node(v1, _, _), Node(v2, _, _) -> (value > v1) && (value < v2) && (validate left) && (validate right)

parse [ Some 5; Some 1; Some 4; None; None; Some 3; Some 6 ] |> validate = false
parse [ Some 2; Some 1; Some 3 ] |> validate = true

let rec compare tree1 tree2 =
    match tree1, tree2 with
    | Empty, Empty -> true
    | _, Empty -> false
    | Empty, _ -> false
    | Node(value1, left1, right1), Node(value2, left2, right2) ->
        (value1 = value2) && (compare left1 left2) && (compare right1 right2)

compare (parse [ Some 1; Some 2; Some 3 ]) (parse [ Some 1; Some 2; Some 3 ]) = true
compare (parse [ Some 1; Some 2; None ]) (parse [ Some 1; None; Some 2 ]) = false
compare (parse [ Some 1; Some 2; Some 1 ]) (parse [ Some 1; Some 1; Some 2 ]) = false

let rec inOrderEnumerator tree =
    seq {
        match tree with
        | Empty -> ()
        | Node(value, left, right) ->
            yield! inOrderEnumerator left
            yield value
            yield! inOrderEnumerator right
    }
parse [ Some 7; Some 3; Some 15; None; None; Some 9; Some 20 ] |> inOrderEnumerator |> Seq.toList = [ 3; 7; 9; 15; 20 ]

let levelOrderTraversal tree =
    let rec loop level branch acc =
        match branch with
        | Empty -> acc
        | Node(value, left, right) ->

            let nextLevel = level + 1
            (level, value) :: acc @ (loop nextLevel left []) @ (loop nextLevel right [])
    loop 0 tree [] |> Seq.groupBy fst |> Seq.map (fun (_, co) -> co |> Seq.map snd |> Seq.toList) |> Seq.toList

parse [ Some 3; Some 9; Some 20; None; None; Some 15; Some 7 ] |> levelOrderTraversal = [ [ 3 ]; [ 9; 20 ]; [ 15; 7 ] ]

let testTree =
    Node(
        'A',
        Node('B', Node('D', Node('H', Empty, Empty), Node('I', Empty, Empty)), Node('E', Empty, Empty)),
        Node(
            'C',
            Node('F', Node('J', Empty, Empty), Node('K', Empty, Empty)),
            Node('G', Empty, Node('L', Empty, Empty))
        )
    )


preorderTraversal testTree = ("ABDHIECFJKGL" |> Seq.toList)
postorderTraversal testTree = ("HIDEBJKFLGCA" |> Seq.toList)

let rec delete element tree =
    match tree with
    | Node(value, left, right) ->
        if element = value then
            match left, right with
            | Empty, Empty -> Empty
            | Empty, node -> node
            | node, Empty -> node
            | node1, node2 ->
                let rec getMin searchTree =
                    match searchTree with
                    | Empty ->
                        failwith "empty tree"
                    | Node(value, left, _) ->
                        match left with
                        | Empty -> value
                        | Node(leftValue, left, _) ->
                            match left with
                            | Empty -> leftValue
                            | notEmpty -> getMin notEmpty
                let min = getMin node2
                Node(min, node1, (delete min node2))
        else
            Node(value, (delete element left), (delete element right))
    | Empty -> Empty


let treeToDelete = [ Some 50; Some 30; Some 70; Some 20; Some 40; Some 60; Some 80 ] |> parse
treeToDelete |> delete 150

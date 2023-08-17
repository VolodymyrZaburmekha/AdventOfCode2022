// module TreeBfsDfs

#r "nuget: FSharpx.Collections"

open System
open FSharpx.Collections


let (===) actual expected = if actual = expected then () else failwithf "assertion failed: %A <> %A" actual expected

// Depth-First Search (DFS) - in/post/pre-order
// Breadth-First Search (BFS)
// https://www.redblobgames.com/pathfinding/a-star/introduction.html
// https://www.digitalocean.com/community/tutorials/breadth-first-search-depth-first-search-bfs-dfs


// **** DFS

type Node<'a> = { Value: 'a; Nodes: Node<'a> list }

let rec preorder node =
    seq {
        yield node
        yield! node.Nodes |> Seq.collect preorder
    }

let rec inorder node =
    seq {
        if node.Nodes.Length > 0 then yield! inorder node.Nodes.Head
        yield node
        if node.Nodes.Length > 1 then yield! inorder node.Nodes.Tail.Head
    }


let rec postorder node =
    seq {
        yield! node.Nodes |> Seq.collect postorder
        yield node
    }

// https://www.digitalocean.com/community/tutorials/breadth-first-search-depth-first-search-bfs-dfs
let tree =
    { Value = 0
      Nodes =
        [ { Value = 1; Nodes = [ { Value = 3; Nodes = [] }; { Value = 4; Nodes = [] } ] }; { Value = 2; Nodes = [] } ] }

[ preorder; inorder; postorder ]
|> Seq.map (fun f -> f tree |> Seq.map (fun n -> n.Value) |> Seq.toArray)
|> Seq.toArray
=== [| [| 0; 1; 3; 4; 2 |]; [| 3; 1; 4; 0; 2 |]; [| 3; 4; 1; 2; 0 |] |]



// **** BFS

let disc =
    { Value = "data"
      Nodes =
        [ { Value = "code"
            Nodes =
              [ { Value = "github"
                  Nodes = [ { Value = "powerseq"; Nodes = [] }; { Value = "powerfp"; Nodes = [] } ] }
                { Value = "gitlab"; Nodes = [] }
                { Value = "bitbucket"; Nodes = [] } ] }
          { Value = "photos"; Nodes = [ { Value = "cats"; Nodes = [] }; { Value = "dogs"; Nodes = [] } ] } ] }

// using queue

let rec bfs0 (nodes: seq<Node<_>>) =
    seq {
        let mutable todo = nodes |> Seq.fold (fun q n -> Queue.conj n q) Queue.empty
        while not (Queue.isEmpty todo) do
            let node = Queue.head todo
            yield node
            todo <- node.Nodes |> Seq.fold (fun q n -> Queue.conj n q) (Queue.tail todo)
    }

let rec bfs0' (nodes: seq<Node<_>>) =
    let rec loop todo =
        seq {
            match Queue.tryHead todo with
            | None -> ()
            | Some node ->
                yield node
                yield! loop (node.Nodes |> Seq.fold (fun q n -> Queue.conj n q) (Queue.tail todo))
        }
    loop (nodes |> Seq.fold (fun q n -> Queue.conj n q) Queue.empty)

(bfs0 [ disc ] |> Seq.map (fun n -> n.Value) |> Seq.toArray)
=== (bfs0' [ disc ] |> Seq.map (fun n -> n.Value) |> Seq.toArray)



// using sequence

// recusion forever during bfs execution, bfs function calls bfs function ...
let rec bfs1 (nodes: seq<Node<_>>) = Seq.append nodes (nodes |> Seq.map (fun n -> n.Nodes) |> Seq.concat |> bfs1)

// recusion forever during lazy iteraton, bfs called for empty seq calls bfs for empty seq ...
let rec bfs2 (nodes: seq<Node<_>>) =
    Seq.append nodes (Seq.delay (fun () -> (nodes |> Seq.collect (fun n -> n.Nodes) |> bfs2)))

// the same just written differently
let rec bfs2' (nodes: seq<Node<_>>) =
    seq {
        for x in nodes do
            yield x
        yield! nodes |> Seq.collect (fun n -> n.Nodes) |> bfs2'
    }

// correct solution
let rec bfs3 (nodes: seq<Node<_>>) =
    seq {
        let mutable hasAny = false
        for x in nodes do
            hasAny <- true
            yield x
        if hasAny then yield! nodes |> Seq.collect (fun n -> n.Nodes) |> bfs3 // check before recursive call
    }

bfs3 [ disc ] |> Seq.map (fun n -> n.Value) |> Seq.truncate 20 |> Seq.toArray |> ignore

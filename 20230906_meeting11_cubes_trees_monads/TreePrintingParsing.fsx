open System.Xml.Linq
open System.Xml
open System.Text
open System

let (===) actual expected = if actual = expected then () else failwithf "assertion failed: %A <> %A" actual expected

type BinaryTree<'a> =
    | Tip
    | Node of 'a * BinaryTree<'a> * BinaryTree<'a>


let rec insert value tree =
    match tree with
    | Tip -> Node(value, Tip, Tip)
    | Node (v, left, right) ->
        if value <= v then Node(v, insert value left, right) else Node(v, left, insert value right)

let rec exists value tree =
    match tree with
    | Tip -> false
    | Node (v, _, _) when v = value -> true
    | Node (v, left, right) -> if value < v then exists value left else exists value right



// pretty printing tree

let rec treeToString tree =
    match tree with
    | Tip -> "null"
    | Node (v, left, right) -> sprintf " %A (%s,%s)" v (treeToString left) (treeToString right)


let parseNodes (text: string) =
    text.Trim([| '['; ']' |]).Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun item -> if item = "null" then None else Some(int item))

parseNodes "[5,1,4,null,null,3,6]" |> Seq.toArray === [| Some 5; Some 1; Some 4; None; None; Some 3; Some 6 |]

"[7,3,15,null,null,9,20]"
|> parseNodes
|> Seq.choose id
|> Seq.fold (fun tree next -> insert next tree) Tip
|> treeToString
=== " 7 ( 3 (null,null), 15 ( 9 (null,null), 20 (null,null)))"


// printing tree to xml

let rec treeToXElement tree =
    match tree with
    | Tip -> new XElement("node")
    | Node (v, Tip, Tip) -> new XElement("node", new XAttribute("value", v))
    | Node (v, left, right) ->
        new XElement("node", new XAttribute("value", v), treeToXElement left, treeToXElement right)

let xElementToPrettyXml (xelement: XElement) =
    let stringBuilder = new StringBuilder()
    let settings = new XmlWriterSettings(OmitXmlDeclaration = true, Indent = true)
    // "use xmlWriter = ... " <- this does work correctly :) StringBuilder is empty because .Disponse() is not called on time
    using (XmlWriter.Create(stringBuilder, settings)) xelement.Save
    stringBuilder.ToString()

let treeToPrettyXml tree = tree |> treeToXElement |> xElementToPrettyXml

Tip |> insert 10 |> insert 5 |> insert 15 |> insert 2 |> insert 6 |> insert 12 |> insert 16 |> treeToPrettyXml
=== """<node value="10">
  <node value="5">
    <node value="2" />
    <node value="6" />
  </node>
  <node value="15">
    <node value="12" />
    <node value="16" />
  </node>
</node>"""















// creating Immutable tree from list of nodes for following levels

let rec splitAt items index =
    match items, index with
    | _, 0 -> [], 0, items
    | [], _ -> [], 0, []
    | head :: tail, _ ->
        let result, resultLength, tail' = splitAt tail (index - 1)
        head :: result, resultLength + 1, tail'

splitAt ([]: int list) 0 === ([], 0, [])
splitAt ([]: int list) 1 === ([], 0, [])
splitAt [ 1; 2; 3 ] 0 === ([], 0, [ 1; 2; 3 ])
splitAt [ 1; 2; 3 ] 1 === ([ 1 ], 1, [ 2; 3 ])
splitAt [ 1; 2; 3 ] 2 === ([ 1; 2 ], 2, [ 3 ])
splitAt [ 1; 2; 3 ] 3 === ([ 1; 2; 3 ], 3, [])
splitAt [ 1; 2; 3 ] 4 === ([ 1; 2; 3 ], 3, [])

let rec readLevel n items =
    let nItems, nItemsLength, restItems = splitAt items n
    if nItemsLength < n || List.isEmpty restItems then
        Seq.append nItems (Seq.replicate (n - nItemsLength) None)
        |> Seq.map (function
            | None -> Tip
            | Some v -> Node(v, Tip, Tip))
        |> Seq.toList
    else
        let nextN = 2 * (nItems |> Seq.filter Option.isSome |> Seq.length)
        if nextN = 0 then
            List.replicate n Tip
        else
            let nextNodes = readLevel nextN restItems
            let map =
                nItems
                |> Seq.mapi (fun index item -> Option.map (fun v -> index, v) item)
                |> Seq.choose id
                |> Seq.zip (Seq.chunkBySize 2 nextNodes)
                |> Seq.map (fun (chunk, (index, v)) -> index, Node(v, chunk.[0], chunk.[1]))
                |> Map
            nItems |> List.mapi (fun index item -> if Option.isNone item then Tip else map.[index])


"[]" |> parseNodes |> Seq.toList |> readLevel 1 === [ Tip ]
"[1]" |> parseNodes |> Seq.toList |> readLevel 1 === [ Node(1, Tip, Tip) ]
"[1, 2]" |> parseNodes |> Seq.toList |> readLevel 1 === [ Node(1, Node(2, Tip, Tip), Tip) ]

"[1,2,3,4,5,7,8]" |> parseNodes |> Seq.toList |> readLevel 1
=== [ Node(1, Node(2, Node(4, Tip, Tip), Node(5, Tip, Tip)), Node(3, Node(7, Tip, Tip), Node(8, Tip, Tip))) ]

"[1,null,null,4,5,7,8]" |> parseNodes |> Seq.toList |> readLevel 1 === [ Node(1, Tip, Tip) ]

"[1,null,3,null,5,null,8]" |> parseNodes |> Seq.toList |> readLevel 1
=== [ Node(1, Tip, Node(3, Tip, Node(5, Tip, Node(8, Tip, Tip)))) ]


let readTree items = items |> Seq.toList |> readLevel 1 |> List.head

let parseTree text = text |> parseNodes |> readTree




// creating Mmutable tree from list of nodes for following levels

type MNode<'a> = { mutable Value: 'a; mutable Left: MTree<'a>; mutable Right: MTree<'a> }
and MTree<'a> =
    | MTip
    | MNode of MNode<'a>

let readMTree items =
    match items with
    | []
    | None :: _ -> MTip
    | Some (value) :: rest ->
        let todo = ResizeArray()
        let root = { Value = value; Left = MTip; Right = MTip }
        todo.Add(root)
        use e = (List.toSeq rest).GetEnumerator()
        let mutable i = 0
        while todo.Count > 0 && e.MoveNext() do
            match e.Current with
            | None -> ()
            | Some (value) ->
                let node = { Value = value; Left = MTip; Right = MTip }
                todo.Add(node)
                if i = 0 then todo.[0].Left <- MNode node else todo.[0].Right <- MNode node
            i <- if i = 0 then 1 else 0
            if i = 0 then todo.RemoveAt(0)
        MNode root

let rec mapTree mtree =
    match mtree with
    | MTip -> Tip
    | MNode { Value = value; Left = left; Right = right } -> Node(value, mapTree left, mapTree right)



"[]" |> parseNodes |> Seq.toList |> readMTree === MTip
"[1]" |> parseNodes |> Seq.toList |> readMTree === MNode { Value = 1; Left = MTip; Right = MTip }

let compareTree text = text |> parseNodes |> Seq.toList |> readMTree |> mapTree === parseTree text

compareTree "[1,null,2,3]"
compareTree "[3,9,20,null,null,15,7]"

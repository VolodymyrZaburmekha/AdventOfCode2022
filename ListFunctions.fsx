let assertTrue b =
    if b = false then failwith "assert is wrong" else ()

// val length : xs:'a list -> int
let rec length l =
    match l with
    | [] -> 0
    | _ :: tail -> 1 + length tail

[ 1; 2; 3; 4; 5 ] |> length

// val map : f:('a -> 'b) -> xs:'a list -> 'b list
let rec map f l =
    match l with
    | [] -> []
    | head :: tail -> [ f head ] @ map f tail

[ 1; 2; 3; 4 ] |> map (fun n -> n + 1)

// val filter : f:('a -> bool) -> xs:'a list -> 'a list
let rec filter f l =
    match l with
    | [] -> []
    | head :: tail -> if f head then [ head ] @ filter f tail else filter f tail

[ 1; 2; 3; 4 ] |> filter (fun n -> n > 2)

// val fold : f:('a -> 'b -> 'a) -> state:'a -> xs:'b list -> 'a
let rec fold f state l =
    match l with
    | [] -> state
    | head :: tail -> fold f (f state head) tail

[ 1; 2; 3; 4; 5 ] |> fold (fun s c -> s + c.ToString()) ""

// val reduce : f:('a -> 'a -> 'a) -> xs:'a list -> 'a
let rec reduce f l =
    match l with
    | [] -> failwith "list must contain elements"
    | [ elem ] -> elem
    | first :: second :: tail -> [ f first second ] @ tail |> reduce f

[ 1; 2; 3; 4 ] |> reduce (fun a b -> a - b)


// val take : n:int -> xs:'a list -> 'a list
let rec take c l =
    let rec loop count taken remaining =
        if count = 0 then
            taken
        else
            match remaining with
            | [] -> taken
            | head :: tail -> loop (count - 1) (taken @ [ head ]) tail

    loop c [] l

// val skip : n:int -> xs:'a list -> 'a list
let rec skip c l =
    match c, l with
    | 0, _ -> l
    | _, [] -> []
    | _, _ :: tail -> skip (c - 1) tail

[ 1; 2; 3; 4 ] |> skip 2

// val concat : list1:'a list -> list2:'a list -> 'a list
let concat l1 l2 = l1 @ l2

[ 1; 2 ] |> concat [ 3; 4 ]

// val zip : f:('a -> 'b -> 'c) -> list1:'a list -> list2:'b list -> 'c list
let zip f l1 l2 =
    let listLength = List.min [ length l1; length l2 ]
    [ for i in 0 .. listLength - 1 -> f l1[i] l2[i] ]

zip (fun c n -> $"%s{c}%i{n}") [ "2"; "3"; "4"; "5" ] [ 1; 2; 3; 4 ]

// val collect : f:('a -> 'b list) -> xs:'a list -> 'b list
let rec collect f l =
    let rec loop acc remaining =
        match remaining with
        | [] -> acc
        | head :: tail -> loop (acc @ f head) tail

    loop [] l

[ [ 1; 2; 3 ]; [ 3; 4; 5 ] ] |> collect (fun l -> l)

// val reverse : xs:'a list -> 'a list
let reverse l =
    let rec loop r n =
        match n with
        | [] -> r
        | head :: tail -> loop (head :: r) tail

    loop [] l

[ 1; 2; 3; 4 ] |> reverse


// val forall : f:('a -> bool) -> xs:'a list -> bool
let rec forall f l =
    match l with
    | [] -> true
    | head :: tail -> if f head = false then false else forall f tail

[ 2; 3; -1 ] |> forall (fun i -> i > 0)

// val exists : f:('a -> bool) -> xs:'a list -> bool
let rec exists f l =
    match l with
    | [] -> false
    | head :: tail -> if f head then true else exists f tail

// val nth : n:int -> xs:'a list -> 'a
let rec nth n l =
    match n, l with
    | _, [] -> failwith $"list doesn't contain element with index %i{n}"
    | 0, head :: _ -> head
    | remaining, _ :: tail -> nth (remaining - 1) tail

[ 1; 2; 3; 4 ] |> nth 0


// val sequenceEqual : f:('a -> 'b -> bool) -> list1:'a list -> list2:'b list -> bool
let sequenceEqual f l1 l2 =
    if length l1 <> length l2 then
        false
    else
        zip f l1 l2 |> forall (fun e -> e = true)

let sequenceEqualAlternative f l1 l2 =
    let rec loop r1 r2 =
        match r1, r2 with // don't know why it complains that pattern is incomplete
        | [], [] -> true
        | [ h1 ], [ h2 ] -> f h1 h2
        | [], _ :: _
        | _ :: _, [] -> false
        | unequalHead1 :: _, unequalHead2 :: _ when f unequalHead1 unequalHead2 = false -> false
        | eqHead1 :: tail1, eqHead2 :: tail2 when f eqHead1 eqHead2 = true -> loop tail1 tail2

    loop l1 l2

sequenceEqual (fun a b -> a = b) [ 2; 2; 3; 4 ] [ 2; 2; 3; 4 ]
sequenceEqualAlternative (<) [ 1; 2; 3 ] [ 10; 20; 30 ]


length [] = 0 |> assertTrue
length [ 1 ] = 1 |> assertTrue
length [ 1; 2 ] = 2 |> assertTrue

map ((+) 1) [ 1; 2; 3 ] = [ 2; 3; 4 ] |> assertTrue
map ((+) 1) ([]: int list) = [] |> assertTrue

filter (fun x -> x > 5) [ 0; 5; 10; 20; 0 ] = [ 10; 20 ] |> assertTrue

fold (+) 0 [ 1; 2; 3 ] = 6 |> assertTrue
reduce (+) [ 1; 2; 3 ] = 6 |> assertTrue

take 2 ([]: int list) = [] |> assertTrue
take 0 [ 1; 2; 3 ] = [] |> assertTrue
take 1 [ 1; 2; 3 ] = [ 1 ] |> assertTrue
take 2 [ 1; 2; 3 ] = [ 1; 2 ] |> assertTrue
take 3 [ 1; 2; 3 ] = [ 1; 2; 3 ] |> assertTrue
take 4 [ 1; 2; 3 ] = [ 1; 2; 3 ] |> assertTrue

skip 2 ([]: int list) = [] |> assertTrue
skip 0 [ 1; 2; 3 ] = [ 1; 2; 3 ] |> assertTrue
skip 1 [ 1; 2; 3 ] = [ 2; 3 ] |> assertTrue
skip 2 [ 1; 2; 3 ] = [ 3 ] |> assertTrue
skip 3 [ 1; 2; 3 ] = [] |> assertTrue
skip 4 [ 1; 2; 3 ] = [] |> assertTrue


concat [ 1; 2 ] [] = [ 1; 2 ] |> assertTrue
concat [] [ 1; 2 ] = [ 1; 2 ] |> assertTrue
concat [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ] |> assertTrue

zip (+) [ 10; 100; 1000 ] [ 3; 7; 9 ] = [ 13; 107; 1009 ] |> assertTrue
zip (+) [ 10; 100 ] [ 3; 7; 9 ] = [ 13; 107 ] |> assertTrue
zip (+) [ 10; 100; 1000 ] [ 3; 7 ] = [ 13; 107 ] |> assertTrue

collect (fun x -> [ x; x ]) [ 1; 2 ] = [ 1; 1; 2; 2 ] |> assertTrue

reverse [ 1; 2; 3 ] = [ 3; 2; 1 ] |> assertTrue

forall (fun x -> x > 0) [ 5; 10; 15 ] = true |> assertTrue
forall (fun x -> x < 15) [ 5; 10; 15 ] = false |> assertTrue

exists (fun x -> x > 10) [ 5; 10; 15 ] = true |> assertTrue
exists (fun x -> x > 15) [ 5; 10; 15 ] = false |> assertTrue

nth 0 [ 11; 22; 33 ] = 11 |> assertTrue
nth 1 [ 11; 22; 33 ] = 22 |> assertTrue


sequenceEqual (=) [ 1; 2; 3 ] [ 1; 2; 3 ] = true |> assertTrue
sequenceEqual (<) [ 1; 2; 3 ] [ 10; 20; 30 ] = true |> assertTrue
sequenceEqual (<) [ 1; 2; 3 ] [ 10; 2; 30 ] = false |> assertTrue
sequenceEqual (=) [ 1; 2; 3 ] [ 1; 2 ] = false |> assertTrue

sequenceEqualAlternative (=) [ 1; 2; 3 ] [ 1; 2; 3 ] = true |> assertTrue
sequenceEqualAlternative (<) [ 1; 2; 3 ] [ 10; 20; 30 ] = true |> assertTrue
sequenceEqualAlternative (<) [ 1; 2; 3 ] [ 10; 2; 30 ] = false |> assertTrue
sequenceEqualAlternative (=) [ 1; 2; 3 ] [ 1; 2 ] = false |> assertTrue

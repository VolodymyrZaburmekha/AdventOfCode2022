// cr_mn: now a use custom '===' infix operator instead of 'assertTrue', '1 === 1' instead of '1 = 1 |> assertTrue' :)
let (===) actual expected = if actual = expected then () else failwithf "assertion failed: %A <> %A" actual expected

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
    | head :: tail -> (f head) :: map f tail
//Fixed:  cr_mn: we can use :: instead of @ while adding one element to the front of list -
// "f head :: map f tail" instead of "[ f head ] @ map f tail"


[ 1; 2; 3; 4 ] |> map (fun n -> n + 1)

// val filter : f:('a -> bool) -> xs:'a list -> 'a list
let rec filter f l =
    match l with
    | [] -> []
    | head :: tail -> if f head then head :: filter f tail else filter f tail

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
    match c, l with
    | 0, _ -> []
    | _, [] -> []
    | 1, head :: _ -> [ head ]
    | n, head :: tail -> head :: take (n - 1) tail

[ 1; 2; 3; 4 ] |> take 2
//Fixed: cr_mn: I am sure you will find a lot simpler implementation of 'take' :) (just recursion and pattern marching)


// val skip : n:int -> xs:'a list -> 'a list
let rec skip c l =
    match c, l with
    | 0, _ -> l
    | _, [] -> []
    | _, _ :: tail -> skip (c - 1) tail

[ 1; 2; 3; 4 ] |> skip 2


// val reverse : xs:'a list -> 'a list
let reverse l =
    let rec loop r n =
        match n with
        | [] -> r
        | head :: tail -> loop (head :: r) tail
    loop [] l
// cr_mn: just beautiful !

[ 1; 2; 3; 4 ] |> reverse


// val concat : list1:'a list -> list2:'a list -> 'a list
let concat list1 list2 =
    let rec loop l1 l2 =
        match l1 with
        | [] -> l2
        | head :: tail -> loop tail (head :: l2)
    loop (reverse list1) list2

concat [ 1; 2; 3 ] [ 4; 5; 6 ]
// Fixed: cr_mn: try to implement it yourself without using builtin @ operator

concat [ 3; 4 ] [ 5; 6 ]


// val zip : f:('a -> 'b -> 'c) -> list1:'a list -> list2:'b list -> 'c list
let rec zip f l1 l2 =
    match l1, l2 with
    | [], _ -> []
    | _, [] -> []
    | h1 :: t1, h2 :: t2 -> (f h1 h2) :: zip f t1 t2
//Fixed cr_mn: try to use recursion and pattern matching instead of list comprehension :)

zip (fun c n -> $"%s{c}%i{n}") [ "2"; "3"; "4"; "5" ] [ 1; 2; 3; 4 ]


// val collect : f:('a -> 'b list) -> xs:'a list -> 'b list
let rec collect f l =
    let rec loop acc remaining =
        match remaining with
        | [] -> acc
        | head :: tail -> loop (concat acc (f head)) tail
    loop [] l
//Maybe fixed? :) cr_mn: 'the problem' with this implementation is that many times @ operator is used, calling "a @ b" rewrites the whole "a" list
// maybe you could use 'concat' and recursion somehow :)

[ [ 1; 2; 3 ]; [ 3; 4; 5 ] ] |> collect (fun l -> l)



// val forall : f:('a -> bool) -> xs:'a list -> bool
let rec forall f l =
    match l with
    | [] -> true
    | head :: tail -> f head && forall f tail
//Fixed: cr_mn: we can use nice tick with && -> "| head :: tail -> f head && forall f tail"

[ 2; 3; 1 ] |> forall (fun i -> i > 0)

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
let rec sequenceEqual f l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | [], _ :: _ -> false
    | _ :: _, [] -> false
    | h1 :: t1, h2 :: t2 -> f h1 h2 && sequenceEqual f t1 t2
//Fixed: cr_mn: it's close but this function can be simplified a lot :)

//Fixed (removed function completely) cr_mn: ... pattern is incomplete maybe because the compiler is not able to handle 'when' clause
let test x =
    match x with
    | y when y = true -> true
    | y when y = false -> false

sequenceEqual (fun a b -> a = b) [ 2; 2; 3; 4 ] [ 2; 2; 3; 4 ]


length [] === 0
length [ 1 ] === 1
length [ 1; 2 ] === 2

map ((+) 1) [ 1; 2; 3 ] === [ 2; 3; 4 ]

map ((+) 1) ([]: int list) === []

filter (fun x -> x > 5) [ 0; 5; 10; 20; 0 ] === [ 10; 20 ]

fold (+) 0 [ 1; 2; 3 ] === 6
reduce (+) [ 1; 2; 3 ] === 6

take 2 ([]: int list) === []
take 0 [ 1; 2; 3 ] === []
take 1 [ 1; 2; 3 ] === [ 1 ]
take 2 [ 1; 2; 3 ] === [ 1; 2 ]
take 3 [ 1; 2; 3 ] === [ 1; 2; 3 ]
take 4 [ 1; 2; 3 ] === [ 1; 2; 3 ]

skip 2 ([]: int list) === []
skip 0 [ 1; 2; 3 ] === [ 1; 2; 3 ]
skip 1 [ 1; 2; 3 ] === [ 2; 3 ]
skip 2 [ 1; 2; 3 ] === [ 3 ]
skip 3 [ 1; 2; 3 ] === []
skip 4 [ 1; 2; 3 ] === []


concat [ 1; 2 ] [] === [ 1; 2 ]
concat [] [ 1; 2 ] === [ 1; 2 ]
concat [ 1; 2 ] [ 3; 4 ] === [ 1; 2; 3; 4 ]

zip (+) [ 10; 100; 1000 ] [ 3; 7; 9 ] === [ 13; 107; 1009 ]
zip (+) [ 10; 100 ] [ 3; 7; 9 ] === [ 13; 107 ]
zip (+) [ 10; 100; 1000 ] [ 3; 7 ] === [ 13; 107 ]

collect (fun x -> [ x; x ]) [ 1; 2 ] === [ 1; 1; 2; 2 ]

reverse [ 1; 2; 3 ] === [ 3; 2; 1 ]

forall (fun x -> x > 0) [ 5; 10; 15 ] === true
forall (fun x -> x < 15) [ 5; 10; 15 ] === false

exists (fun x -> x > 10) [ 5; 10; 15 ] === true
exists (fun x -> x > 15) [ 5; 10; 15 ] === false

nth 0 [ 11; 22; 33 ] === 11
nth 1 [ 11; 22; 33 ] === 22

sequenceEqual (=) [ 1; 2; 3 ] [ 1; 2; 3 ] === true
sequenceEqual (<) [ 1; 2; 3 ] [ 10; 20; 30 ] === true
sequenceEqual (<) [ 1; 2; 3 ] [ 10; 2; 30 ] === false
sequenceEqual (=) [ 1; 2; 3 ] [ 1; 2 ] === false

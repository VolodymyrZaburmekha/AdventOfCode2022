
### 2023.06.07 Meeting 3 immutable linked lists

#### Homework assignment  review

```fsharp
let rec length l =
    match l with
    | [] -> 0
    | _ :: tail -> 1 + length tail


let rec length xs =
    match xs with
    | [] -> 0
    | _ :: tail -> 1 + length tail

// tail recursion
let length xs =
    let rec loop item acc =
        match item with
        | [] -> acc
        | _ :: tail -> loop tail (acc + 1)
    loop xs 0



let rec map f l =
    match l with
    | [] -> []
    | head :: tail -> [ f head ] @ map f tail

let rec map f l =
    match l with
    | [] -> []
    | head :: tail -> (f head) :: map f tail

let rec map f xs =
    match xs with
    | [] -> []
    | head :: tail -> f head :: map f tail



let rec filter f l =
    match l with
    | [] -> []
    | head :: tail -> if f head then [ head ] @ filter f tail else filter f tail

let rec filter f l =
    match l with
    | [] -> []
    | head :: tail -> if f head then head :: filter f tail else filter f tail

let rec filter f xs =
    match xs with
    | [] -> []
    | head :: tail -> if f head then head :: filter f tail else filter f tail



let rec fold f state l =
    match l with
    | [] -> state
    | head :: tail -> fold f (f state head) tail

let rec fold f state xs =
    match xs with
    | [] -> state
    | head :: tail -> fold f (f state head) tail


let rec reduce f l =
    match l with
    | [] -> failwith "list must contain elements"
    | [ elem ] -> elem
    | first :: second :: tail -> [ f first second ] @ tail |> reduce f

let rec reduce f l =
    match l with
    | [] -> failwith "list must contain elements"
    | [ elem ] -> elem
    | first :: second :: tail -> [ f first second ] @ tail |> reduce f

let rec reduce f xs =
    match xs with
    | [] -> failwith "list can not be empty"
    | head :: tail -> fold f head tail



let rec take c l =
    let rec loop count taken remaining =
        if count = 0 then
            taken
        else
            match remaining with
            | [] -> taken
            | head :: tail -> loop (count - 1) (taken @ [ head ]) tail
    loop c [] l

let rec take c l =
    match c, l with
    | 0, _ -> []
    | _, [] -> []
    | 1, head :: _ -> [ head ]
    | n, head :: tail -> head :: take (n - 1) tail


let rec take n xs =
    match xs, n with
    | [], _
    | _, 0 -> []
    | head :: tail, _ -> head :: take (n - 1) tail



let rec skip c l =
    match c, l with
    | 0, _ -> l
    | _, [] -> []
    | _, _ :: tail -> skip (c - 1) tail

let rec skip n xs =
    match xs, n with
    | [], _ -> []
    | _, 0 -> xs
    | _ :: tail, _ -> skip (n - 1) tail




let concat l1 l2 = l1 @ l2

let concat list1 list2 =
    let rec loop l1 l2 =
        match l1 with
        | [] -> l2
        | head :: tail -> loop tail (head :: l2)
    loop (reverse list1) list2

let rec concat list1 list2 =
    match list1, list2 with
    | [], _ -> list2
    | _, [] -> list1
    | head :: tail, _ -> head :: concat tail list2



let zip f l1 l2 =
    let listLength = List.min [ length l1; length l2 ]
    [ for i in 0 .. listLength - 1 -> f l1[i] l2[i] ]

let rec zip f l1 l2 =
    match l1, l2 with
    | [], _ -> []
    | _, [] -> []
    | h1 :: t1, h2 :: t2 -> (f h1 h2) :: zip f t1 t2

let rec zip f list1 list2 =
    match list1, list2 with
    | [], _
    | _, [] -> []
    | head1 :: tail1, head2 :: tail2 -> f head1 head2 :: zip f tail1 tail2



let rec collect f l =
    let rec loop acc remaining =
        match remaining with
        | [] -> acc
        | head :: tail -> loop (acc @ f head) tail
    loop [] l

let rec collect f l =
    let rec loop acc remaining =
        match remaining with
        | [] -> acc
        | head :: tail -> loop (concat acc (f head)) tail
    loop [] l
//Maybe fixed? :) cr_mn: 'the problem' with this implementation is that many times @ operator is used, calling "a @ b" rewrites the whole "a" list
// maybe you could use 'concat' and recursion somehow :)

let rec collect f xs =
    match xs with
    | [] -> []
    | head :: tail -> concat (f head) (collect f tail)



let reverse l =
    let rec loop r n =
        match n with
        | [] -> r
        | head :: tail -> loop (head :: r) tail
    loop [] l

let rec reverse xs =
    let rec rev lst state =
        match lst with
        | [] -> state
        | head :: tail -> rev tail (head :: state)
    rev xs []



let rec forall f l =
    match l with
    | [] -> true
    | head :: tail -> if f head = false then false else forall f tail

let rec forall f l =
    match l with
    | [] -> true
    | head :: tail -> f head && forall f tail

let rec forall f xs =
    match xs with
    | [] -> true
    | head :: tail -> f head && forall f tail


let rec exists f l =
    match l with
    | [] -> false
    | head :: tail -> if f head then true else exists f tail

let rec exists f xs =
    match xs with
    | [] -> false
    | head :: tail -> f head || exists f tail



let rec nth n l =
    match n, l with
    | _, [] -> failwith $"list doesn't contain element with index %i{n}"
    | 0, head :: _ -> head
    | remaining, _ :: tail -> nth (remaining - 1) tail

let rec nth n xs =
    match xs, n with
    | [], _ -> failwith "n out of bounds"
    | head :: _, 0 -> head
    | _ :: tail, n' -> nth (n' - 1) tail



let sequenceEqual f l1 l2 =
    if length l1 <> length l2 then
        false
    else
        zip f l1 l2 |> forall (fun e -> e = true)

let rec sequenceEqual f l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | [], _ :: _ -> false
    | _ :: _, [] -> false
    | h1 :: t1, h2 :: t2 -> f h1 h2 && sequenceEqual f t1 t2

let rec sequenceEqual f list1 list2 =
    match list1, list2 with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | head1 :: tail1, head2 :: tail2 -> f head1 head2 && sequenceEqual f tail1 tail2


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

//Fixed (removed function completely) cr_mn: ... pattern is incomplete maybe because the compiler is not able to handle 'when' clause
let test x =
    match x with
    | y when y = true -> true
    | y when y = false -> false




let merge l1 l2 =
    let rec loop l1 l2 acc =
        match l1, l2 with
        | [], _ -> acc @ l2
        | _, [] -> acc @ l1
        | h1 :: t1, h2 :: t2 ->
            let remainingList1, remainingList2, takenHead = if h1 <= h2 then t1, l2, h1 else l1, t2, h2
            loop remainingList1 remainingList2 (acc @ [ takenHead ])
    loop l1 l2 []


let rec merge l1 l2 =
    match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | h1 :: t1, h2 :: t2 -> if h1 > h2 then h2 :: (merge l1 t2) else h1 :: (merge l2 t1)

let rec mergeTwoLists list1 list2 =
    match list1, list2 with
    | lst, [] -> lst
    | [], lst -> lst
    | head1 :: tail1, head2 :: tail2 ->
        if head1 = head2 then head1 :: head2 :: mergeTwoLists tail1 tail2
        else if head1 < head2 then head1 :: mergeTwoLists tail1 list2
        else head2 :: mergeTwoLists list1 tail2
```
open System
open System.Collections.Generic
open System.Linq

let (===) actual expected = if actual = expected then () else failwithf "assertion failed: %A <> %A" actual expected
let (====) (actual: seq<_>) (expected: seq<_>) =
    if actual.SequenceEqual(expected) then () else failwithf "assertion failed: %A <> %A" actual expected

// val length: xs: seq<'a> -> int
let length (s: seq<_>) =
    use enumerator = s.GetEnumerator()
    let mutable count = 0
    while enumerator.MoveNext() do
        count <- count + 1
    count

// val map: f: ('a -> 'b) -> xs: seq<'a> -> seq<'b>
let map f s =
    seq {
        for i in s do
            yield f i
    }

// val filter: f: ('a -> bool) -> xs: seq<'a> -> seq<'a>
let filter f s =
    seq {
        for i in s do
            if (f i) = true then
                yield i
    }

// val fold: f: ('a -> 'b -> 'a) -> state: 'a -> xs: seq<'b> -> 'a
let fold f state (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) state =
        if not (enumerator.MoveNext()) then state else loop enumerator (f state enumerator.Current)
    use enumerator = s.GetEnumerator()
    loop enumerator state

// val reduce: f: ('a -> 'a -> 'a) -> xs: seq<'a> -> 'a
let reduce f (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) state =
        if not (enumerator.MoveNext()) then state else loop enumerator (f state enumerator.Current)

    use enumerator = s.GetEnumerator()
    if not (enumerator.MoveNext()) then
        failwith "sequence doesn't contain elements"
    else
        let first = enumerator.Current
        loop enumerator first

// val take: n: int -> xs: seq<'a> -> seq<'a>
let take n (s: seq<_>) =
    seq {
        use enumerator = s.GetEnumerator()
        for _ in 1..n do
            if enumerator.MoveNext() then
                yield enumerator.Current
    }

// val skip: n: int -> xs: seq<'a> -> seq<'a>
let skip n (s: seq<_>) =
    seq {
        let mutable currentIndex = 0
        for i in s do
            if currentIndex >= n then
                yield i
            currentIndex <- currentIndex + 1
    }

// val concat: list1: seq<'a> -> list2: seq<'a> -> seq<'a>
let concat s1 s2 =
    seq {
        for i in s1 do
            yield i
        for i2 in s2 do
            yield i2
    }

// val zip: f: ('a -> 'b -> 'c) -> list1: seq<'a> -> list2: seq<'b> -> seq<'c>
let zip f (s1: seq<_>) (s2: seq<_>) =
    seq {
        use enumerator1 = s1.GetEnumerator()
        use enumerator2 = s2.GetEnumerator()

        while enumerator1.MoveNext() && enumerator2.MoveNext() do
            yield f enumerator1.Current enumerator2.Current
    }

// val collect: f: ('a -> #seq<'c>) -> xs: seq<'a> -> seq<'c>
let collect f s =
    seq {
        for i in s do
            for subItem in f i do
                yield subItem
    }

// val reverse: xs: seq<'a> -> seq<'a>
let reverse (s: seq<_>) =
    let list = List<_>(s)
    let maxIndex = list.Count - 1
    seq {
        for i in 0..maxIndex do
            yield list[maxIndex - i]
    }

// val forall: f: ('a -> bool) -> xs: seq<'a> -> bool
let forall f (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) =
        if not (enumerator.MoveNext()) then true else (f enumerator.Current) && loop enumerator
    use enumerator = s.GetEnumerator()
    loop enumerator

// val exists: f: ('a -> bool) -> xs: seq<'a> -> bool
let exists f (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) =
        if not (enumerator.MoveNext()) then false
        else if (f enumerator.Current) then true
        else loop enumerator
    use enumerator = s.GetEnumerator()
    loop enumerator

// val nth: n: int -> xs: seq<'a> -> 'a
let nth n (s: seq<_>) =
    let rec loop n (enumerator: IEnumerator<_>) =
        if not (enumerator.MoveNext()) then failwith "index is out of range"
        else if n = 0 then enumerator.Current
        else loop (n - 1) enumerator
    use enumerator = s.GetEnumerator()
    loop n enumerator

// val sequenceEqual:
//   f: ('a -> 'b -> bool) -> list1: seq<'a> -> list2: seq<'b> -> bool
let sequenceEqual f (s1: seq<_>) (s2: seq<_>) =
    let rec loop (enumerator1: IEnumerator<_>) (enumerator2: IEnumerator<_>) =
        match enumerator1.MoveNext(), enumerator2.MoveNext() with
        | false, false -> true
        | true, false
        | false, true -> false
        | true, true -> (f enumerator1.Current enumerator2.Current) && (loop enumerator1 enumerator2)

    use enum1 = s1.GetEnumerator()
    use enum2 = s2.GetEnumerator()
    loop enum1 enum2

// val mergeTwoLists:
//   list1: seq<'a> -> list2: seq<'a> -> seq<'a> when 'a: comparison
let mergeTwoLists (s1: seq<_>) (s2: seq<_>) =
    let createEnumerator (enumerator: IEnumerator<_>) =
        (seq {
            yield enumerator.Current
            while enumerator.MoveNext() do
                yield enumerator.Current
        })
            .GetEnumerator()

    let rec loop (enum1: IEnumerator<_>) (enum2: IEnumerator<_>) =
        seq {
            match enum1.MoveNext(), enum2.MoveNext() with
            | false, false -> ()
            | false, true ->
                yield enum2.Current
                yield! loop enum1 enum2
            | true, false ->
                yield enum1.Current
                yield! loop enum1 enum2
            | true, true ->
                if enum1.Current < enum2.Current then
                    yield enum1.Current
                    yield! loop enum1 (createEnumerator enum2)
                else
                    yield enum2.Current
                    yield! loop (createEnumerator enum1) enum2
        }

    use enum1 = s1.GetEnumerator()
    use enum2 = s2.GetEnumerator()
    loop enum1 enum2


length [] === 0
length [ 1 ] === 1
length [ 1; 2 ] === 2

map ((+) 1) [ 1; 2; 3 ] ==== [ 2; 3; 4 ]
map ((+) 1) ([]: int list) ==== []

filter (fun x -> x > 5) [ 0; 5; 10; 20; 0 ] ==== [ 10; 20 ]

fold (+) 0 [ 1; 2; 3 ] === 6
reduce (+) [ 1; 2; 3 ] === 6

take 2 ([]: int list) ==== []
take 0 [ 1; 2; 3 ] ==== []
take 1 [ 1; 2; 3 ] ==== [ 1 ]
take 2 [ 1; 2; 3 ] ==== [ 1; 2 ]
take 3 [ 1; 2; 3 ] ==== [ 1; 2; 3 ]
take 4 [ 1; 2; 3 ] ==== [ 1; 2; 3 ]

skip 2 ([]: int list) ==== []
skip 0 [ 1; 2; 3 ] ==== [ 1; 2; 3 ]
skip 1 [ 1; 2; 3 ] ==== [ 2; 3 ]
skip 2 [ 1; 2; 3 ] ==== [ 3 ]
skip 3 [ 1; 2; 3 ] ==== []
skip 4 [ 1; 2; 3 ] ==== []

concat [ 1; 2 ] [] ==== [ 1; 2 ]
concat [] [ 1; 2 ] ==== [ 1; 2 ]
concat [ 1; 2 ] [ 3; 4 ] ==== [ 1; 2; 3; 4 ]

zip (+) [ 10; 100; 1000 ] [ 3; 7; 9 ] ==== [ 13; 107; 1009 ]
zip (+) [ 10; 100 ] [ 3; 7; 9 ] ==== [ 13; 107 ]
zip (+) [ 10; 100; 1000 ] [ 3; 7 ] ==== [ 13; 107 ]

collect (fun x -> [ x; x ]) [ 1; 2 ] ==== [ 1; 1; 2; 2 ]

reverse [ 1; 2; 3 ] ==== [ 3; 2; 1 ]

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

module IntroductionToSeq

open System
open System.Collections
open System.Collections.Generic
open System.Linq

let (===) actual expected = if actual = expected then () else failwithf "assertion failed: %A <> %A" actual expected

let (====) (actual: seq<_>) (expected: seq<_>) =
    if actual.SequenceEqual(expected) then () else failwithf "assertion failed: %A <> %A" actual expected



// immutable linked list

type LList<'a> =
    | Empty // []
    | Cons of head: 'a * tail: LList<'a> // .. :: ...


let lst1 = Cons(1, Cons(2, Empty))
let lst2 = Cons(1, Cons(2, Empty))
let _ = lst1 = lst2


let rec lengthLL l =
    match l with
    | Empty -> 0
    | Cons (head, tail) -> 1 + lengthLL tail

let lengthLL' l =
    let rec len l acc =
        match l with
        | Empty -> acc
        | Cons (head, tail) -> len tail (acc + 1) // tail recursion
    len l 0

let _ = lengthLL lst1
let _ = lengthLL' lst1




// ** ** ** ** ** ** ** ** **
// List<T> / ResizeArray<T>

let filterRA<'a> f (items: ResizeArray<'a>) =
    let result = ResizeArray<'a>()
    for item in items do
        if f item then result.Add(item)
    result

let mapRA f (items: ResizeArray<_>) =
    let result = ResizeArray<_>()
    for item in items do
        result.Add(f item)
    result

let items = ResizeArray<int>([| 1; 2; 3; 4; 5 |])

let isEven x = x % 2 = 0
let _ = filterRA isEven items // -> [2 ; 4]
let _ = mapRA (fun x -> $"{x} zł") items
let _ = mapRA (fun x -> $"{x} zł") (filterRA isEven items)
let _ = items |> filterRA isEven |> mapRA (fun x -> $"{x} zł")


// IEnumerable<T>, seq<T>

let listFrom1To5 = ResizeArray([ 1; 2; 3; 4; 5 ])

for i in listFrom1To5 do
    printfn "%d" i

let en: IEnumerator<int> = listFrom1To5.GetEnumerator()

while en.MoveNext() do
    let i = en.Current
    printfn "%d" i


let newEnumerator moveNext current dispose =
    { new IEnumerator<_> with
        member this.Current = current ()
      interface IEnumerator with
          member this.MoveNext() = moveNext ()
          member this.Current = current () :> obj
          member this.Reset() = ()
      interface IDisposable with
          member this.Dispose() = dispose () }

let newEnumerable getEnumerator =
    { new IEnumerable<_> with
        member this.GetEnumerator() = getEnumerator ()
      interface IEnumerable with
          member this.GetEnumerator() = (this :?> IEnumerable<_>).GetEnumerator() :> IEnumerator }



let rangeSeq start finish =
    newEnumerable (fun () ->
        let mutable i = start - 1
        newEnumerator
            (fun () ->
                i <- i + 1
                i <= finish)
            (fun () -> i)
            id)

let rangeFrom1To5 = rangeSeq 1 5

for i in rangeFrom1To5 do
    printfn "%d" i


let e = rangeFrom1To5.GetEnumerator()

while e.MoveNext() do
    let i = e.Current
    printfn "%d" i

let _ = rangeSeq 1 5 |> Seq.truncate 10 |> Seq.toArray




let filterSeq f (items: IEnumerable<_>) =
    newEnumerable (fun () ->
        let e = items.GetEnumerator()
        let rec moveNext () =
            if e.MoveNext() = false then false
            elif f e.Current then true
            else moveNext ()
        newEnumerator moveNext (fun () -> e.Current) id)

let mapSeq f (items: IEnumerable<_>) =
    newEnumerable (fun () ->
        let e = items.GetEnumerator()
        newEnumerator (fun () -> e.MoveNext()) (fun () -> f e.Current) id)

let _ = rangeSeq 1 10 |> filterSeq (fun x -> x % 2 = 0) |> mapSeq string |> Seq.toArray
let repeatSeq value = newEnumerable (fun () -> newEnumerator (fun () -> true) (fun () -> value) id)
let _ = repeatSeq "666"


let range start finish =
    seq {
        for i = start to finish do
            yield i
    }

let filter f xs =
    seq {
        for x in xs do
            if f x then yield x
    }

let map f xs =
    seq {
        for item in xs do
            yield f item
    }

let repeat value =
    seq {
        while true do
            yield value
    }

let _ = range 1 10 |> filter (fun x -> x % 2 = 0) |> map string |> Seq.toArray
let _ = repeat "666"


let return123 () =
    seq {
        yield 1
        yield 2
        yield 3
    }

let return123123123 () =
    seq {
        yield! return123 ()
        yield! return123 ()
        yield! return123 ()
    }

let return123123123' () =
    seq {
        for e in return123 () do
            yield e
        for e in return123 () do
            yield e
        for e in return123 () do
            yield e
    }

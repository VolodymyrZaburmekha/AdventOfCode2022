
### 2023.06.14 Meeting 4 sequence operators

#### Homework assignment review

```fsharp
let length (s: seq<_>) =
    use enumerator = s.GetEnumerator()
    let mutable count = 0
    while enumerator.MoveNext() do
        count <- count + 1
    count
// cr_mn: here could use for/in loop but of course it is perfectly fine

let length (s: seq<_>) =
    let mutable count = 0
    for _ in s do
        count <- count + 1
    count

let length xs =
    let mutable i = 0
    for _ in xs do
        i <- i + 1
    i




let map f s =
    seq {
        for i in s do
            yield f i
    }

let map f xs =
    seq {
        for item in xs do
            yield f item
    }




let filter f s =
    seq {
        for i in s do
            if (f i) = true then yield i
    }

let filter f xs =
    seq {
        for x in xs do
            if f x then yield x
    }




let fold f state (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) state =
        if not (enumerator.MoveNext()) then state else loop enumerator (f state enumerator.Current)
    use enumerator = s.GetEnumerator()
    loop enumerator state

let fold f state xs =
    let mutable total = state
    for x in xs do
        total <- f total x
    total





let reduce f (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) state =
        if not (enumerator.MoveNext()) then state else loop enumerator (f state enumerator.Current)

    use enumerator = s.GetEnumerator()
    if not (enumerator.MoveNext()) then
        failwith "sequence doesn't contain elements"
    else
        let first = enumerator.Current
        loop enumerator first


let reduce f (xs: seq<_>) =
    use e = xs.GetEnumerator()
    if e.MoveNext() = false then
        failwith "list can not be empty"
    else
        let mutable total = e.Current
        while e.MoveNext() do
            total <- f total e.Current
        total




let take n (s: seq<_>) =
    seq {
        use enumerator = s.GetEnumerator()
        for _ in 1..n do
            if enumerator.MoveNext() then yield enumerator.Current
    }
// cr_mn: when we have less elements in "s" than "n", we unnecessarily call ".MoveNext" many times

let take n (s: seq<_>) =
    seq {
        use enumerator = s.GetEnumerator()
        let mutable taken = 0
        while enumerator.MoveNext() && taken < n do
            taken <- taken + 1
            yield enumerator.Current
    }


let take n (xs: seq<_>) =
    seq {
        let mutable c = n
        use e = xs.GetEnumerator()
        while c > 0 && e.MoveNext() do
            yield e.Current
            c <- c - 1
    }





let skip n (s: seq<_>) =
    seq {
        let mutable currentIndex = 0
        for i in s do
            if currentIndex >= n then yield i
            currentIndex <- currentIndex + 1
    }
// cr_mn: for each iteration till the end of sequence we will be unnecessarily using "currentIndex" variable

let skip n (s: seq<_>) =
    seq {
        use enumerator = s.GetEnumerator()
        for _ in 1..n do
            enumerator.MoveNext() |> ignore
        while enumerator.MoveNext() do
            yield enumerator.Current
    }

let skip n (xs: seq<_>) =
    seq {
        let mutable c = n
        use e = xs.GetEnumerator()
        while c > 0 && e.MoveNext() do
            c <- c - 1
        while e.MoveNext() do
            yield e.Current
    }





let concat s1 s2 =
    seq {
        for i in s1 do
            yield i
        for i2 in s2 do
            yield i2
    }    
// cr_mn: we can use yield!

let concat s1 s2 =
    seq {
        yield! s1
        yield! s2
    }


let concat list1 list2 =
    seq {
        yield! list1
        yield! list2
    }





let zip f (s1: seq<_>) (s2: seq<_>) =
    seq {
        use enumerator1 = s1.GetEnumerator()
        use enumerator2 = s2.GetEnumerator()

        while enumerator1.MoveNext() && enumerator2.MoveNext() do
            yield f enumerator1.Current enumerator2.Current
    }
// perfect! (ok)

let zip f (list1: seq<_>) (list2: seq<_>) =
    seq {
        use e1 = list1.GetEnumerator()
        use e2 = list2.GetEnumerator()
        while e1.MoveNext() && e2.MoveNext() do
            yield f e1.Current e2.Current
    }





let collect f s =
    seq {
        for i in s do
            for subItem in f i do
                yield subItem
    }
// cr_mn: we can use yield!

let collect f s =
    seq {
        for i in s do
            yield! f i
    }

let collect f xs =
    seq {
        for x in xs do
            yield! f x
    }






let reverse (s: seq<_>) =
    let list = List<_>(s)
    let maxIndex = list.Count - 1
    seq {
        for i in 0..maxIndex do
            yield list[maxIndex - i]
    }
// cr_mn: I was looking lately how immutable list is implemented internally :) next time i will show you what i have found
// in short, "Count" really iterators over all items in list, calling "list[max-i]" does the same


let reverse xs =
    seq {
        let mutable lst = []
        for x in xs do
            lst <- x :: lst
        yield! lst
    }





let forall f (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) =
        if not (enumerator.MoveNext()) then true else (f enumerator.Current) && loop enumerator
    use enumerator = s.GetEnumerator()
    loop enumerator


let forall f (xs: seq<_>) =
    let mutable result = true
    use e = xs.GetEnumerator()
    while result && e.MoveNext() do
        result <- result && f e.Current
    result





let exists f (s: seq<_>) =
    let rec loop (enumerator: IEnumerator<_>) =
        if not (enumerator.MoveNext()) then false
        else if (f enumerator.Current) then true
        else loop enumerator
    use enumerator = s.GetEnumerator()
    loop enumerator

let exists f (xs: seq<_>) =
    let mutable result = false
    use e = xs.GetEnumerator()
    while not result && e.MoveNext() do
        result <- result || f e.Current
    result



let nth n (s: seq<_>) =
    let rec loop n (enumerator: IEnumerator<_>) =
        if not (enumerator.MoveNext()) then failwith "index is out of range"
        else if n = 0 then enumerator.Current
        else loop (n - 1) enumerator
    use enumerator = s.GetEnumerator()
    loop n enumerator

let nth n (xs: seq<_>) =
    let mutable result = None
    let mutable i = 0
    use e = xs.GetEnumerator()
    while Option.isNone result && e.MoveNext() do
        if i = n then result <- Some e.Current else i <- i + 1
    match result with
    | None -> failwith "n out of bounds"
    | Some value -> value








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

let sequenceEqual f (list1: seq<_>) (list2: seq<_>) =
    use e1 = list1.GetEnumerator()
    use e2 = list2.GetEnumerator()
    let mutable result = None
    while Option.isNone result do
        result <-
            match e1.MoveNext(), e2.MoveNext() with
            | false, false -> Some true
            | true, true -> if not (f e1.Current e2.Current) then Some false else None
            | _ -> Some false
    Option.get result





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


// cr_mn: really nice implementation, you can avoid using helper "createEnumerator" function
// you call ".MoveNext" function at the beginning of each loop, maybe try to call it at the end


let mergeTwoLists (s1: seq<_>) (s2: seq<_>) =
    let rec loop (enum1: IEnumerator<_>) (enum2: IEnumerator<_>) =
        let createItems (enum: IEnumerator<_>) =
            seq {
                while enum.MoveNext() do
                    yield enum.Current
            }
        seq {
            if enum1.Current < enum2.Current then yield enum1.Current else yield enum2.Current

            if enum1.Current < enum2.Current then
                let movedNext = enum1.MoveNext()
                if not movedNext then yield! createItems enum2 else yield! loop enum1 enum2
            else
                let movedNext = enum2.MoveNext()
                if not movedNext then yield! createItems enum1 else yield! loop enum1 enum2
        }

    use enum1 = s1.GetEnumerator()
    let _ = enum1.MoveNext()
    use enum2 = s2.GetEnumerator()
    let _ = enum2.MoveNext()
    loop enum1 enum2

// mergeTwoLists [1]  [2]  |> Seq.toArray // -> [|1|]
// mergeTwoLists ([]  : list<int>)  []  |> Seq.toArray // InvalidOperationException: Enumeration already finished.

let mergeTwoLists (list1: seq<_>) (list2: seq<_>) =
    seq {
        use e1 = list1.GetEnumerator()
        use e2 = list2.GetEnumerator()
        let mutable pair = e1.MoveNext(), e2.MoveNext()
        while (fst pair) || (snd pair) do
            match pair with
            | false, false -> ()
            | false, true ->
                yield e2.Current
                pair <- false, e2.MoveNext()
            | true, false ->
                yield e1.Current
                pair <- e1.MoveNext(), false
            | true, true ->
                if e1.Current = e2.Current then
                    yield e1.Current
                    yield e2.Current
                    pair <- e1.MoveNext(), e2.MoveNext()
                elif e1.Current < e2.Current then
                    yield e1.Current
                    pair <- e1.MoveNext(), true
                else
                    yield e2.Current
                    pair <- true, e2.MoveNext()
    }

let moveNext (e: IEnumerator<_>) = if e.MoveNext() then Some e.Current else None

let mergeTwoLists' (list1: seq<_>) (list2: seq<_>) =
    seq {
        use e1 = list1.GetEnumerator()
        use e2 = list2.GetEnumerator()
        let rec loop pair =
            seq {
                match pair with
                | None, None -> ()
                | None, Some value ->
                    yield value
                    yield! loop (None, moveNext e2)
                | Some value, None ->
                    yield value
                    yield! loop (moveNext e1, None)
                | Some value1, Some value2 ->
                    if value1 = value2 then
                        yield value1
                        yield value2
                        yield! loop (moveNext e1, moveNext e2)
                    elif value1 < value2 then
                        yield value1
                        yield! loop (moveNext e1, Some value2)
                    else
                        yield value2
                        yield! loop (Some value1, moveNext e2)
            }
        yield! loop (moveNext e1, moveNext e2)
    }
```
open System

let (===) actual expected = if actual = expected then () else failwithf "assertion failed: %A <> %A" actual expected

type CustomOption<'a> =
    | SomeValue of 'a
    | NoValue

let bind f o =
    match o with
    | SomeValue v -> f v
    | NoValue -> NoValue

let result o = SomeValue o

let tryParseInt (str: string) =
    match Int32.TryParse str with
    | true, v -> SomeValue v
    | false, _ -> NoValue


type Delayed<'a> = unit -> CustomOption<'a>

type CustomOptionBuilder() =
    member x.Bind(o, f) = bind f o
    member x.Return(o) = result o
    member x.ReturnFrom(o) = o
    member x.For(sequence: seq<'a>, mapping: ('a -> CustomOption<_>)) =
        sequence |> Seq.fold (fun state element -> state |> bind (fun e -> mapping element)) (x.Zero())

    member x.Zero() = SomeValue Unchecked.defaultof<_>
    member x.Combine(first: CustomOption<_>, second: Delayed<_>) = bind (fun _ -> second ()) first
    member x.Delay(expr) : Delayed<_> = fun () -> expr ()
    member x.Run(delayed: Delayed<_>) = delayed ()

let customOption = CustomOptionBuilder()


let tryParseAndAddInts str1 str2 =
    customOption {
        let! a = tryParseInt str1
        let! b = tryParseInt str2
        return a + b
    }

(tryParseAndAddInts "23" "33") === SomeValue 56
tryParseAndAddInts "" "3" === NoValue

// let numbers = ["12"; "13"; "15"]
let sumNumers (numbers: seq<string>) =
    customOption {
        let mutable total = 0
        for number in numbers do
            let! value = tryParseInt number
            total <- total + value
        return total
    }

seq {
    "1"
    "5"
    "4"
}
|> sumNumers
=== SomeValue 10

seq {
    "1"
    "5"
    ""
}
|> sumNumers
=== NoValue
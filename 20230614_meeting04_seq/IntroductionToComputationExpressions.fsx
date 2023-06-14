module IntroductionToComputationExpressions

open System
open System.Collections
open System.Collections.Generic
open System.Linq

let (===) actual expected = if actual = expected then () else failwithf "assertion failed: %A <> %A" actual expected


// custom "Option" type

type MyOption<'T> =
    | MNone
    | MSome of 'T

let o1: MyOption<int> = MNone
let o2: MyOption<int> = MSome 12

// helper function for parsing string to int safely 
let parseInt (str: string) =
    let (success, value) = Int32.TryParse str
    if success then MSome value else MNone

// parsing then adding two numbers, potentially exception can be thrown
let parseTwoInts' str1 str2 =
    let int1 = Int32.Parse str1
    let int2 = Int32.Parse str2
    int1 + int2

// parsing then adding two numbers, here we handle lack of values
let parseTwoInts'' str1 str2 =
    let int1O = parseInt str1
    match int1O with
    | MNone -> MNone
    | MSome int1 ->
        let int2O = parseInt str2
        match int2O with
        | MNone -> MNone
        | MSome int2 -> MSome(int1 + int2)


// helper "bind" function for "Option" type
// ('a -> Option'<'b>) -> Option'<'a> -> Option'<'b>
let bind binder option =
    match option with
    | MNone -> MNone
    | MSome value -> binder value

// parsing and adding .. using "bind" function
let parseTwoInts''' str1 str2 =
    parseInt str1
    |> bind
	    (fun int1 ->
		    parseInt str2
		    |> bind (fun int2 -> MSome(int1 + int2)))


// helper "map" function for "Option" type (signature of "map" is similar to "bind")
// ('a -> 'b) -> Option'<'a> -> Option'<'b>
let map binder option =
    match option with
    | MNone -> MNone
    | MSome value -> MSome(binder value)

// parsing and adding .. using "map" function
let parseTwoInts'''` str1 str2 =
    parseInt str1
    |> bind
	    (fun int1 ->
		    parseInt str2
		    |> map (fun int2 -> int1 + int2))
		    

// helper "return" function converting "normal value" into "elevated value"
//'a -> MyOption<'a>
let return' value = MSome value



// ------------------------------------------------------------------------
// Computation Expression

// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions#creating-a-new-type-of-computation-expression


// note: here we will be using standard Option type instead of custom MyOption
let tryParseInt (str: string) =
    let (success, value) = Int32.TryParse str
    if success then Some value else None



type DelayedO<'T> = unit -> Option<'T>
let returnO = Some
let bindO = Option.bind

type OptionBuilder() =
    member this.Return(value) = returnO value
    member this.Bind(monad, binder) = bindO binder monad

    member this.Delay(f: DelayedO<_>) = f
    member this.Run(delayed: DelayedO<_>) = delayed ()

    member this.Zero() = returnO Unchecked.defaultof<_>
    member this.For(sequence: seq<_>, body) =
        sequence 
        |> Seq.fold (fun state value -> bindO (fun _ -> body value) state) (this.Zero())

    member this.Combine(monad1, monad2: DelayedO<_>) =  bindO (fun _ -> this.Run(monad2)) monad1



let option = OptionBuilder()

// string -> string -> Option<int>
let parseTowInts str1 str2 =
    option {
        let! int1 = tryParseInt str1
        let! int2 = tryParseInt str2
        return int1 + int2
    }

parseTowInts "1" "2" === Some 3
parseTowInts "1" "" === None



let sumNumers numbers = 
    option {
        let mutable total = 0
        for number in numbers do
            let! value = tryParseInt number
            total <- total + value
        return total
    }

sumNumers ["1"; "2"; "3"] === Some 6
sumNumers ["1"; ""; "3"] === None


// type OptionBuilder() =
//     member this.Return(value) = ...
//     member this.ReturnFrom(value) = ...
//     member this.Zero() = ...
//     member this.Delay(f: DelayedO<_>) =  ...
//     member this.Run(delayed: DelayedO<_>) =  ...
//     member this.Bind(monad, binder) = .. .
//     member this.Combine(monad1, monad2: DelayedO<_>) = ...
//     member this.TryFinally(body: DelayedO<_>, finallyBody) = ...    
//     member this.TryWith(body: DelayedO<_>, catchBody) = ... 
//     member this.Using(res: #IDisposable, body) = ...
//     member this.While(guard, body: DelayedO<_>) = ...
//     member this.For(sequence: seq<_>, body) = ...



### 2023.06.21 Meeting 5 monads

#### Homework assignment review

```fsharp
type CustomOption<'a> =
    | SomeValue of 'a
    | NoValue

let bind f o =
    match o with
    | SomeValue v -> f v
    | NoValue -> NoValue

let result o = SomeValue o


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





type DelayedO<'T> = unit -> Option<'T>

let returnO = Some
let bindO = Option.bind

type OptionBuilder() =    
    member this.Bind(monad, binder) = bindO binder monad
    member this.Return(value) = returnO value

    member this.For(sequence: seq<_>, body) =
        sequence 
        |> Seq.fold (fun state value -> bindO (fun _ -> body value) state) (this.Zero())

    member this.Zero() = returnO Unchecked.defaultof<_>

    member this.Combine(monad1, monad2: DelayedO<_>) =  bindO (fun _ -> this.Run(monad2)) monad1

    member this.Delay(f: DelayedO<_>) = f
    member this.Run(delayed: DelayedO<_>) = delayed ()
```


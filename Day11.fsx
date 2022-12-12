open System.IO

type OperationArgument =
    | Old
    | Constant of uint64

type Operation =
    { FirstArgument: OperationArgument
      SecondArgument: OperationArgument
      Action: uint64 -> uint64 -> uint64 }

type Monkey =
    { Id: int
      Throws: uint64
      Items: uint64 list
      Operation: Operation
      TestNumber: uint64
      Test: uint64 -> uint64 }

let getMonkeyId (line: string) =
    line.Split(" ").[1].Replace(":", "") |> int

let skipPattern (pattern: string) (line: string) =
    line.Substring(pattern.Length, line.Length - pattern.Length)

let getStartingItems (line: string) =
    let pattern = "Starting items: "
    let idsParts = line |> skipPattern pattern

    idsParts.Split(", ")
    |> Array.map (fun i -> uint64 i)
    |> List.ofArray

let getOperation (line: string) =
    let pattern = "Operation: "

    match line
          |> skipPattern pattern
          |> (fun l -> l.Split " ")
        with
    | [| "new"; "="; firstArgument; operator; secondArgument |] ->
        let getArgument arg =
            if arg = "old" then
                Old
            else
                Constant(uint64 arg)

        { FirstArgument = getArgument firstArgument
          SecondArgument = getArgument secondArgument

          Action =
              match operator with
              | "+" -> (fun a b -> a + b)
              | "-" -> (fun a b -> a - b)
              | "/" -> (fun a b -> a / b)
              | "*" -> (fun a b -> a * b)
              | _ -> failwith $"invalid operator :%s{operator}" }
    | _ -> failwith $"wrong input %A{line}"

let getTest (testLines: string []) =
    let conditionPattern = "Test: divisible by "

    let throwIfTruePattern =
        "If true: throw to monkey "

    let throwIfFalsePattern =
        "If false: throw to monkey "

    match testLines with
    | [| condition; throwIfTrue; throwIfFalse |] when
        condition.StartsWith(conditionPattern)
        && throwIfTrue.StartsWith(throwIfTruePattern)
        && throwIfFalse.StartsWith(throwIfFalsePattern)
        ->
        let conditionValue =
            condition |> skipPattern conditionPattern |> uint64

        let trueIndex =
            throwIfTrue
            |> skipPattern throwIfTruePattern
            |> uint64

        let falseIndex =
            throwIfFalse
            |> skipPattern throwIfFalsePattern
            |> uint64

        let checkFunc =
            (fun value ->
                if value % conditionValue = 0UL then
                    trueIndex
                else
                    falseIndex)

        (checkFunc, conditionValue)
    | _ -> failwith $"invalid test block %A{testLines}"


let processRound worryLevelRegulation (inputMonkeys: Monkey list) =
    let rec loopMonkeys currentMonkey (monkeys: Monkey list) =
        let replace items monkey =
            let index =
                items
                |> List.findIndex (fun m -> m.Id = monkey.Id)

            let before = items |> List.take index
            let after = items |> List.skip (index + 1)
            before @ [ monkey ] @ after

        let recipients =
            currentMonkey.Items
            |> List.map (fun worry ->
                let getArgumentValue arg =
                    match arg with
                    | Constant c -> c
                    | Old -> worry

                let firstArgument =
                    getArgumentValue currentMonkey.Operation.FirstArgument

                let secondArgument =
                    getArgumentValue currentMonkey.Operation.SecondArgument

                let operationResult =
                    currentMonkey.Operation.Action firstArgument secondArgument

                let afterRegulation =
                    worryLevelRegulation operationResult

                let nextMonkey =
                    currentMonkey.Test afterRegulation

                (nextMonkey, afterRegulation))
            |> List.groupBy (fun (i, _) -> i)
            |> List.map (fun (index, throws) ->
                let recipient =
                    monkeys |> List.find (fun m -> uint64 m.Id = index)

                let itemsToAdd =
                    throws |> List.map (fun (_, value) -> value)

                { recipient with Items = recipient.Items @ itemsToAdd })

        let newMonkeyState =
            { currentMonkey with
                Throws = currentMonkey.Throws + uint64 currentMonkey.Items.Length
                Items = [] }

        let itemsToReplace =
            [ newMonkeyState ] @ recipients

        let newState =
            itemsToReplace
            |> List.fold (fun items m -> m |> replace items) monkeys

        if currentMonkey.Id + 1 = monkeys.Length then
            newState
        else
            loopMonkeys newState.[currentMonkey.Id + 1] newState

    loopMonkeys inputMonkeys.Head inputMonkeys

let parsedInput =
    File.ReadAllLines "Data/Day11.txt"
    |> Array.chunkBySize 7
    |> Array.map (fun l ->
        l
        |> Array.map (fun l -> l.Trim())
        |> Array.takeWhile (fun l -> l <> ""))
    |> List.ofArray
    |> List.map (fun group ->
        match group with
        | [| index; startItems; operation; testValue; throwIfTrue; throwIfFalse |] ->
            let testFunc, testValue =
                getTest [| testValue
                           throwIfTrue
                           throwIfFalse |]

            { Id = getMonkeyId index
              Items = getStartingItems startItems
              Operation = getOperation operation
              Throws = 0UL
              TestNumber = testValue
              Test = testFunc }
        | _ -> failwith $"incorrect group  %A{group}")

let rec repeatIterations iterations worryRegulation input =
    if iterations > 0 then
        let result =
            processRound worryRegulation input

        result
        |> repeatIterations (iterations - 1) worryRegulation
    else
        input

let solve input worryLevelFunc iterations =
    input
    |> repeatIterations iterations worryLevelFunc
    |> List.sortByDescending (fun l -> l.Throws)
    |> List.take 2
    |> List.fold (fun acc mon -> acc * mon.Throws) 1UL
    
let part1 = solve parsedInput (fun w -> w / 3UL) 20

let part2 =
    let sum =
        parsedInput
        |> List.map(fun monkey -> monkey.TestNumber)
        |> List.fold(fun acc n -> n*acc)1UL
    printfn $"SUM {sum}"
    solve parsedInput (fun inp -> inp % sum) 10000

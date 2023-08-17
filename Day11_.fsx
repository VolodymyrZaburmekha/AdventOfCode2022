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

let getMonkeyId (line: string) = line.Split(" ").[1].Replace(":", "") |> int

let skipPattern (pattern: string) (line: string) = line.Substring(pattern.Length, line.Length - pattern.Length)


let getStartingItems (line: string) =
    let idsParts = skipPattern "Starting items: " line
    idsParts.Split(", ") |> Seq.map uint64 |> Seq.toList


let getOperation (line: string) =
    let operationParts = skipPattern "Operation: " line
    match operationParts.Split(" ") with
    | [| "new"; "="; firstArgument; operator; secondArgument |] ->
        let getArgument arg = if arg = "old" then Old else Constant(uint64 arg)
        { FirstArgument = getArgument firstArgument
          SecondArgument = getArgument secondArgument
          Action =
            match operator with
            | "+" -> (+)
            | "-" -> (-)
            | "/" -> (/)
            | "*" -> (*)
            | _ -> failwith $"invalid operator :%s{operator}" }
    | _ -> failwith $"wrong input %A{line}"


let getTest (testLines: string []) =
    let conditionPattern = "Test: divisible by "
    let throwIfTruePattern = "If true: throw to monkey "
    let throwIfFalsePattern = "If false: throw to monkey "

    match testLines with
    | [| condition; throwIfTrue; throwIfFalse |] when
        condition.StartsWith(conditionPattern)
        && throwIfTrue.StartsWith(throwIfTruePattern)
        && throwIfFalse.StartsWith(throwIfFalsePattern)
        ->
        let conditionValue = condition |> skipPattern conditionPattern |> uint64
        let trueIndex = throwIfTrue |> skipPattern throwIfTruePattern |> uint64
        let falseIndex = throwIfFalse |> skipPattern throwIfFalsePattern |> uint64
        let checkFunc = (fun value -> if value % conditionValue = 0UL then trueIndex else falseIndex)
        (checkFunc, conditionValue)
    | _ -> failwith $"invalid test block %A{testLines}"


let getArgumentValue arg worry =
    match arg with
    | Constant c -> c
    | Old -> worry


let replace items monkey = List.updateAt monkey.Id monkey items
// let index = items |> List.findIndex (fun m -> m.Id = monkey.Id)
// let before = items |> List.take index
// let after = items |> List.skip (index + 1)
// before @ [ monkey ] @ after

let processRound worryLevelRegulation (inputMonkeys: Monkey list) =
    let rec loopMonkeys currentMonkey (monkeys: Monkey list) =
        let recipients =
            currentMonkey.Items
            |> Seq.map (fun worry ->
                let first = getArgumentValue currentMonkey.Operation.FirstArgument worry
                let second = getArgumentValue currentMonkey.Operation.SecondArgument worry
                let result = currentMonkey.Operation.Action first second
                let afterRegulation = worryLevelRegulation result
                let nextMonkey = currentMonkey.Test afterRegulation
                (nextMonkey, afterRegulation))
            |> Seq.groupBy fst
            |> Seq.map (fun (index, throws) ->
                let recipient = List.find (fun m -> uint64 m.Id = index) monkeys
                let itemsToAdd = Seq.map snd throws |> Seq.toList
                { recipient with Items = recipient.Items @ itemsToAdd })

        let newMonkeyState =
            { currentMonkey with Throws = currentMonkey.Throws + uint64 currentMonkey.Items.Length; Items = [] }

        let newState = Seq.append [ newMonkeyState ] recipients |> Seq.fold replace monkeys

        if currentMonkey.Id + 1 = monkeys.Length then newState else loopMonkeys newState.[currentMonkey.Id + 1] newState

    loopMonkeys inputMonkeys.Head inputMonkeys



let parsedInput =
    File.ReadAllLines "Data/Day11.txt"
    |> Seq.chunkBySize 7
    |> Seq.map (fun l -> l |> Array.map (fun l -> l.Trim()) |> Array.takeWhile (fun l -> l <> ""))
    |> Seq.map (fun group ->
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
    |> Seq.toList

let rec repeatIterations iterations worryRegulation input =
    if iterations > 0 then
        let result = processRound worryRegulation input
        repeatIterations (iterations - 1) worryRegulation result
    else
        input

let solve input worryLevelFunc iterations =
    input
    |> repeatIterations iterations worryLevelFunc
    |> Seq.map (fun l -> l.Throws)
    |> Seq.sortByDescending id
    |> Seq.take 2
    |> Seq.reduce (*)

let part1 = solve parsedInput (fun w -> w / 3UL) 20

let part2 =
    let sum = parsedInput |> Seq.map (fun monkey -> monkey.TestNumber) |> Seq.reduce (*)
    printfn $"SUM {sum}"
    solve parsedInput (fun inp -> inp % sum) 10000

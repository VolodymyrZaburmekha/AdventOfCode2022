module AsyncProgrammingUsingYield


open System.Threading.Tasks

let downloadPageContent (url: string) = Task.Delay(1000).ContinueWith(fun _ -> $"<html> ... {url} ...  </html> ")

let printTaskResult (t: Task<_>) = t.ContinueWith(fun (tt: Task<_>) -> printfn "task result: %A" tt.Result) |> ignore

// using built in task { ... }
let sumUpPagesSize urls =
    task {
        let mutable total = 0
        for url in urls do
            let! content = downloadPageContent url
            printfn "downloaded: %s" content
            total <- total + content.Length
        return total
    }

let asyncResult =
    sumUpPagesSize [ "google.com"
                     "facebook.com" ]

asyncResult.ContinueWith(printTaskResult)



// using seq { ... } to execute async code

let executeFlow<'r> (asyncFlow: seq<Task>) =
    let e = asyncFlow.GetEnumerator()
    let tcs = TaskCompletionSource<'r>()
    let rec moveNext (t: Task) =
        if e.MoveNext() then
            e.Current.ContinueWith moveNext |> ignore
        else
            let result = if typedefof<'r> = typedefof<Unit> then Unchecked.defaultof<'r> else (t :?> Task<'r>).Result
            tcs.SetResult(result)
    moveNext (Task.FromResult(Unchecked.defaultof<'r>))
    tcs.Task

let sumUpPagesSize' urls : seq<Task> =
    seq {
        let mutable total = 0
        for url in urls do
            let t = downloadPageContent url
            yield t
            let content = t.Result
            printfn "downloaded: %s" content
            total <- total + content.Length
        yield Task.FromResult(total)
    }

let asyncResult' =
    sumUpPagesSize' [ "google.com"
                      "facebook.com" ]
    |> executeFlow<int>

asyncResult'.ContinueWith(printTaskResult)


// function returning void/unit
let tt =
    seq {
        yield Task.Delay(1000)
        printfn "after 1s"
        yield Task.Delay(1000)
        printfn "after 1s"
    }
    |> executeFlow<Unit>

tt.ContinueWith(printTaskResult)

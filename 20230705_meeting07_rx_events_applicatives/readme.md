

- Rx  (Reactive Extensions)
- Event and Observable modules
- Applicatives
- General theory of reactivity

### Rx (Reactive Extensions)

#### `IObservable<T>` interface introduced in .net 4.0

```csharp
public interface IObservable<T>
{
  IDisposable Subscribe(IObserver<T> observer);
}

public interface IObserver<T>
{
  void OnCompleted();
  void OnError(Exception exception);
  void OnNext(T value);
}

// OnNext * ( OnCompleted | OnError ) ?
```

#### Basic usage

```csharp
IObservable<int> return666 = Observable.Return(666);

IDisposable subscription1 = return666.Subscribe(new MyObserver(...));
subscription1.Dispose();

// 'Subscribe' method overload using extension methods
return666.Subscribe(onNext, onError, onCompleted);
return666.Subscribe(
	v => Console.WriteLine(v), e => Console.WriteLine(e), () => Console.WriteLine("completed"));


// creating observable objects 
var observable = Observable.Return|Empty|Throw|Range|Repeat|Timer|Interval|...

... = Observable.FromEvent<EventArgs>(button, "Click");
... = Observable.FromAsyncPattern<byte[],int,int>(stream.BeginRead, stream.EndRead);
```

#### Creation `IObservable<T>` from `IEnumeration<T>` 

```csharp
// switching arguments with results
public interface IEnumerable<T>
{
  IEnumerator<T> GetEnumerator();
}
--> 
public interface IObservable<T>
{
  IDisposable Subscribe(IObserver<T> observer);
}


public interface IEnumerator<T>
{
  T Current { get; }
  bool MoveNext();
}
--> 
public interface IObserver<T>
{
  void Current(T value);
  void MoveNext(bool completed);
  void MoveNext(Exception exception);
}
--> 
public interface IObserver<T>
{
  void OnNext(T value);
  void OnCompleted();
  void OnError(Exception exception);
}
```

#### Rx operators

- https://reactivex.io/languages.html
-  Rx.JS provides >130 operators
- ~28 creating observable objects 
	- ajax, zip, bindCallback, bindNodeCallback, combineLatest, concat, defer, , empty, forkJoin, from, fromEvent, fromEventPattern, fromPromise, webSocket, if, interval, merge, never, of, onErrorResumeNext, pairs, race, range, throw, timer, using, generate
- ~102  operators    
	- audit, zipAll, buffer, bufferCount, bufferTime, bufferToggle, bufferWhen, catch, combineAll, combineLatest, concat, concatAll, concatMap, concatMapTo, count, debounce, debounceTime, defaultIfEmpty, delay, delayWhen, dematerialize, distinct, distinctUntilChanged, distinctUntilKeyChanged, do, elementAt, every, exhaust, exhaustMap, expand, filter, finally, find, findIndex, first, groupBy, ignoreElements, isEmpty, last, let, map, mapTo, materialize, max, merge, mergeAll, mergeMap, mergeMapTo, mergeScan, min, multicast, auditTime, onErrorResumeNext, pairwise, partition, pluck, publish, publishBehavior, publishLast, publishReplay, race, reduce, repeat, repeatWhen, retry, retryWhen, sample, sampleTime, scan, sequenceEqual, share, shareReplay, single, skip, skipLast, skipUntil, skipWhile, startWith, subscribeOn, switch, switchMap, switchMapTo, take, takeLast, takeUntil, takeWhile, throttle, throttleTime, timeInterval, timeout, timeoutWith, timestamp, toArray, toPromise, window, windowCount, windowTime, windowToggle, windowWhen, withLatestFrom, zip, observeOn


#### Operators

```csharp
IObservable<string> q = 
(
	from i in Observable.Interval(TimeSpan.FromSeconds(1))
	where i % 2 == 0
	select new string('a', i)
)
.Skip(1)
.Take(5)

var sub = q.Subscribe(v => Console.WriteLine(v));


// drag and drop using Rx
var mouseDown = Observable.FromEvent<...>(control, "MouseLeftButtonDown");
var mouseMove = Observable.FromEvent<...>(control, "MouseMove");
var mouseUp = Observable.FromEvent<...>(control, "MouseLeftButtonUp");

var q =
    from d in mouseDown
    let offset = d.EventArgs.GetPosition(c)
    from m in mouseMove.TakeUntil(mouseUp)
    let end = m.EventArgs.GetPosition(canvas)
    select new { X = end.X - offset.X, Y = end.Y - offset.Y };

q.Subscribe(v =>
{
    Canvas.SetLeft(c, v.X);
    Canvas.SetTop(c, v.Y);
});
```

#### Rx in JavaScript world

```typescript
// - Rx is the only JS library required by Angular, it is used for: routing, http requests, ...
// - in contrast to Promise<T> it supports cancellation and it's lazy

interface TodoItemDto { _id: any; name: string; }

export class Component  {
  constructor(private httpClient: HttpClient) { }

  loadData() {
    let result: Observable<TodoItemDto[]> = this.httpClient.get<TodoItemDto[]>( "/api/todos");
    result.subscribe(console.log, console.error);

	// convertion to Promise
    const resultPromise: Promise<TodoItemDto[]> = result.toPromise();

    // retrying
    result = result.retry(3);
    result.subscribe(console.log, console.error);

	// cancellation
    const sub = result.subscribe(console.log, console.error);
    setTimeout(function() { sub.unsubscribe(); }, 100);
  }
}
```


### F# first-class events


#### `IEvent` interface

```csharp
interface IDelegateEvent<TDelegate>
    where TDelegate : Delegate
{
    void AddHandler(TDelegate handler);
    void RemoveHandler(TDelegate handler);
}

interface IEvent<TDelegate, TArgs> : IDelegateEvent<TDelegate>, IObservable<TArgs>
    where TDelegate : Delegate { }

delegate void Handler<TArgs>(object sender, TArgs args);
interface IEvent<TArgs> : IEvent<Handler<TArgs>, TArgs> { }
```

#### `Event` and `Observable` modules

```fsharp
// Event|Observable . add|choose|filter|map|merge|pairwise|partition|scan|split|(subscribe)

let timer = new Timers.Timer(Interval = 1000, AutoReset = true, Enabled = true)

timer.Elapsed // regular .NET event (IEvent<ElapsedEventHandler,ElapsedEventArgs>)
|> Event.filter (fun e -> e.SignalTime.Second % 2 = 0)
|> Event.map (fun e -> string e.SignalTime) // IEvent<string>
|> Event.add (fun text -> printfn $"{text}")
// timer.Stop()

let subscription =
    Observable.Interval(TimeSpan.FromMilliseconds(1000))
    |> Observable.map (fun _ -> DateTime.Now)
    |> Observable.filter (fun e -> e.Second % 2 = 0)
    |> Observable.map (fun e -> string e) // IObservable<string>
    |> Observable.subscribe (fun text -> printfn $"{text}")
// subscription.Dispose()
```

### Applicatives

#### Computation expressions and! applicatives  (`Option<T>` )

```fsharp
// 2020 .NET5 F#5 introduction new "and!" keyword
// https://devblogs.microsoft.com/dotnet/announcing-f-5-preview-1/#applicative-computation-expressions 

let tryParseInt (str: string) =
    let (success, value) = Int32.TryParse str
    if success then Some value else None

let returnO = Some
let mapO = Option.map
let bindO = Option.bind

// Option<('a -> 'b)> -> Option<'a> -> Option<'b>
let applyO f option =
    match f, option with
    | Some ff, Some value -> Some(ff value)
    | _ -> None

let applyO_ option f = applyO f option // helper function reversing arguments

let liftAO2 f o1 o2 = o1 |> mapO f |> applyO_ o2
let liftAO3 f o1 o2 o3 = o1 |> mapO f |> applyO_ o2 |> applyO_ o3
let liftAO4 f o1 o2 o3 o4 = o1 |> mapO f |> applyO_ o2 |> applyO_ o3 |> applyO_ o4

type OptionBuilder() =
    member this.Return(value) = returnO value
    member this.Bind(monad, binder) = bindO binder monad
    member this.MergeSources(monad1, monad2) = liftAO2 (fun value1 value2 -> value1, value2) monad1 monad2

let option = OptionBuilder()

let parseIntsOMonadic str1 str2 =
    option {
        let! int1 = tryParseInt str1
        let! int2 = tryParseInt str2
        return int1 + int2
    }

let parseIntsOApplicative str1 str2 =
    option {
        let! int1 = tryParseInt str1
        and! int2 = tryParseInt str2
        return int1 + int2
    }

parseIntsOMonadic "1" "2" |> ignore
parseIntsOApplicative "1" "2" |> ignore
```

#### Computation expressions and! applicatives  (`IObservable<T>` )

```fsharp
let returnR value = Observable.Return(value)
let mapR f obs = Observable.Select(obs, (f: _ -> _))
let bindR f obs = Observable.SelectMany(obs, (f: _ -> IObservable<_>))
let applyR f obs = Observable.CombineLatest(f, obs, (fun f v -> f v))

let applyR_ o f = applyR f o
let liftAR2 f t1 t2 = t1 |> mapR f |> applyR_ t2

type ObservableBuilder() =
    member this.Return(value) = returnR value
    member this.Bind(monad, binder) = bindR binder monad
    member this.MergeSources(monad1, monad2) = liftAR2 (fun value1 value2 -> value1, value2) monad1 monad2

let observable = ObservableBuilder()

// string -> IObservable<int>
let parseIntObs str =
	Observable.Delay(Observable.Return(Int32.Parse str), TimeSpan.FromMilliseconds(1000))

let parseIntsObsMonadic str1 str2 str3 =
    observable {
        let! int1 = parseIntObs str1
        let! int2 = parseIntObs str2
        let! int3 = parseIntObs str3
        return int1 + int2 + int3
    }

let parseIntsObsApplicative str1 str2 str3 =
    observable {
        let! int1 = parseIntObs str1
        and! int2 = parseIntObs str2
        and! int3 = parseIntObs str3
        return int1 + int2 + int3
    }

(parseIntsObsMonadic "1" "2" "3")
	.Subscribe(fun value -> printfn "value1: %A" value) |> ignore // after 3 secs
	
(parseIntsObsApplicative "1" "2" "3")
	.Subscribe(fun value -> printfn "value2: %A" value) |> ignore // after 1 sec
```

### Async Iterators vs Observables

- Duality and the End of Reactive
	- https://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote-Duality
	- https://web.archive.org/web/20210209213234/https://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-
	- https://walkingcat.github.io/ch9-index/
- General theory of reactivity
	- https://www.youtube.com/watch?v=2p51PE1MZ8U video
	- https://github.com/kriskowal/gtor
	- https://kriskowal.gitbooks.io/gtor/content/intro.html
- `await` + `yield` = 
	- `IAsyncEnumerable<T>` - C#8 2019 (async streams), JavaScript 2018 (async iterators)
	- `IObservable<T>`- Dart (streams, `Stream<T>` = `IObservable<T>`)
		- https://dart.dev/codelabs/async-await
		- https://dart.dev/tutorials/language/streams
		- https://dartcn.com/articles/language/beyond-async
		- https://api.dart.dev/stable/3.0.5/dart-async/Stream-class.html


| |  synchronous | asynchronous |
| -- |--|--|
| **one** | `T` | `Task<T>(Async<T>)` |
| **many** | `IEnumerable<T>` `IAsyncEnumerable<T>` | `IObservable<T>` / `IAsyncObservable<T>`|


#### `IEnumerable<T>` vs `IAsyncEnumerable<T>`

```csharp
public interface IEnumerable<T>
{
    IEnumerator<T> GetEnumerator();
}
public interface IEnumerator<T> : IDisposable
{
    T Current { get; }
    bool MoveNext();
}
foreach (var item in enumerable) { ... }

public interface IAsyncEnumerable<T>
{
    IAsyncEnumerator<T> GetAsyncEnumerator(CancellationToken cancellationToken);
}
public interface IAsyncEnumerator<T> : IAsyncDisposable
{
    T Current { get; }
    ValueTask<bool> MoveNextAsync();
}

await foreach (var item in asyncEnumerable) { }
```

####  `yield return ...` and `await ...` in the same function

```csharp
static async IAsyncEnumerable<int> RangeAsync(int from, int to)
{
    for (int i = from; i <= to; i++)
    {
        await Task.Delay(1000);
        yield return i;
    }
}

var items = RangeAsync(1, 10);

await foreach (var item in items)
{
    Console.WriteLine(item);
}
```

#### LINQ and async streams

```csharp
var q = 
	from i in RangeAsync(1, 10)
	where x % 2 == 0
	select x * 1000;
	
static class AsyncEnumerable
{
    public static async IAsyncEnumerable<T> Where<T>(this IAsyncEnumerable<T> source, Func<T, bool> f)
    {
        await foreach (var item in source)
        {
            if (f(item))
            {
                yield return item;
            }
        }
    }
    public static async IAsyncEnumerable<R> Select<T, R>(this IAsyncEnumerable<T> source, Func<T, R> f)
    {
        await foreach (var item in source)
        {
            yield return f(item);
        }
    }
    public static async Task<List<T>> ToList<T>(this IAsyncEnumerable<T> source)
    {
        var result = new List<T>();
        await foreach (var item in source)
        {
            result.Add(item);
        }
        return result;
    }
}

// https://github.com/dotnet/reactive Interactive Extensions (Ix) 
```

#### Dart `Future<T>`, `Iterable<T>`, `Stream<T>`

```dart
Iterable<int> naturalsTo(n) sync* {
  int k = 0;
  while (k < n) yield k++;
}

Stream<int> asynchronousNaturalsTo(n) async* {
  int k = 0;
  while (k < n) yield k++;
}

Stream<int> naturals async* {
  int k = 0;
  while (true) {
	  await ...
	  yield k++; 
  }
}


Future<void> printOrderMessage() async {
  var order = await fetchUserOrder();
}

Future<int> sumStream(Stream<int> stream) async {
  var sum = 0;
  await for (final value in stream) {
    sum += value;
  }
  return sum;
}
```
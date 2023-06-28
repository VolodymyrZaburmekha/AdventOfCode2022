
#### Imaginary representation of monads in C#

```csharp
// this code is not compiling, it uses unexisting C# syntax :)

interface IFunctor<E>
{
	static E<R> Map<T,R>(E<T> e, Func<T, R> f);
}

interface IApplicativeFunctor<E> : Functor<E>
{
	static E<T> return<T>(value T);
	static E<T> Map<T,R>(E<T> e, E<Func<T, R>> f);
}

interface IMonad<E> : IApplicativeFunctor<E>
{
	static E<R> Bind<T,R>(E<T> e, Func<T, E<R>> f);
}

class Option<T> : IMonad<Option<T>>
{
	static Option<R> Map<T,R>(Option<T> e, Func<T, R> f) => ...
	...
}
class Task<T> : IMonad<Task<T>> { ... }
class Array<T> : IMonad<Array<T>> { ... }

class MonadicFunctions
{
	static E<IEnumerable<R>> MapM<E,T,R>(IEnumerable<T> items, Func<T,E<R>> f) where E : IMonad<E> ; 
	static E<IEnumerable<R>> FilterM<E,T,R>(IEnumerable<T> items, Func<T,E<bool>> f) where E : IMonad<E>  ;
	static E<A> ReduceM<E,T,A>(IEnumerable<T> items, Func<A,T,E<A>> f, A seed) where E : IMonad<E>;
}

Option<IEnumerable<int>> numbers = MonadicFunctions.MapM(new []{ "123", "456"}, TryParseInt);

option Option<int> AddNumbers(string number1, string number2)
{
	int n1 = bind TryParseInt(number1);
	int n2 = bind TryParseInt(number2);
	return n1 + n2;
}
```
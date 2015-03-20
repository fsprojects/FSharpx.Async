namespace FSharpx.Control

open System
open System.Threading
open FSharpx.Control.Utils

/// <summary>
/// A function which produces an async computation as output.
/// </summary>
type AsyncFunc<'a, 'b> = 'a -> Async<'b>

/// Operations on async functions.
module AsyncFunc =

  /// Lifts a function into an an async function.  
  let inline lift (f:'a -> 'b) : AsyncFunc<'a, 'b> = 
    f >> async.Return

  /// The identity async function.
  [<GeneralizableValue>]
  let identity<'a> : AsyncFunc<'a, 'a> = 
    lift id

  /// An async function which applies an input async function to an argument.
  [<GeneralizableValue>]
  let inline app<'a, 'b> : AsyncFunc<AsyncFunc<'a, 'b> * 'a, 'b> =
    fun (ar,a) -> ar a

  /// Creates an async function which first invokes async function f then g with the output of f.
  let inline compose (g:AsyncFunc<'b, 'c>) (f:AsyncFunc<'a, 'b>)  : AsyncFunc<'a, 'c> =
    fun a -> f a |> Async.bind g

  /// Creates an async function which first invokes async function f then g with the output of f.
  /// Alias for compose.
  let andThen = compose

  /// Maps over the input to an async function.
  let mapl (f:'a2 -> 'a) (a:AsyncFunc<'a, 'b>) : AsyncFunc<'a2, 'b> =
    f >> a

  /// Maps over the input to an async function.
  let maplAsync (f:AsyncFunc<'c, 'a>) (a:AsyncFunc<'a, 'b>) : AsyncFunc<'c, 'b> =
    f >> Async.bind a

  /// Maps over the output of an async function.
  let mapr (f:'b -> 'c) (a:AsyncFunc<'a, 'b>) : AsyncFunc<'a, 'c> =
    a >> Async.map f

  /// Maps over the output of an async function.
  let maprAsync (f:AsyncFunc<'b, 'c>) (a:AsyncFunc<'a, 'b>) : AsyncFunc<'a, 'c> =
    a >> Async.bind f

  /// Maps over both the input and the output of an async function.
  let dimap (f:'c -> 'a) (g:'b -> 'd) (a:AsyncFunc<'a, 'b>) : AsyncFunc<'c, 'd> =
    f >> a >> Async.map g

  /// Maps over both the input and the output of an async function.
  let dimapAsync (f:AsyncFunc<'c, 'a>) (g:AsyncFunc<'b, 'd>) (a:AsyncFunc<'a, 'b>) : AsyncFunc<'c, 'd> =
    f >> Async.bind a >> Async.bind g

  /// Creates an async function which splits its inputs among the provided async functions.
  let inline split (f:AsyncFunc<'a, 'b>) (g:AsyncFunc<'a2, 'b2>) : AsyncFunc<'a * 'a2, 'b * 'b2> =
    fun (a,a2) -> Async.Parallel(f a, g a2)

  /// Creates an async function which fans out the input to both async functions in parallel and collects the results.
  let inline fanout (f:AsyncFunc<'a, 'b>) (g:AsyncFunc<'a, 'c>) : AsyncFunc<'a, 'b * 'c> =
    fun a -> Async.Parallel(f a, g a)

  /// Creates an async function which splits the input choice value between the two argument async functions.
  let inline fanin (f:AsyncFunc<'a, 'b>) (g:AsyncFunc<'c, 'b>) : AsyncFunc<Choice<'a, 'c>, 'b> =
    function
    | Choice1Of2 a -> f a 
    | Choice2Of2 c -> g c

  /// Creates an async function which feeds Choice1Of2 inputs through the argument async function, passing the rest through unchanged to the output.
  let inline left (f:AsyncFunc<'a, 'b>) : AsyncFunc<Choice<'a, 'c>, Choice<'b, 'c>> =
    function
    | Choice1Of2 a -> f a |> Async.map Choice1Of2
    | Choice2Of2 c -> Choice2Of2 c |> async.Return

  /// Creates an async function which feeds Choice2Of2 inputs through the argument async function, passing the rest through unchanged to the output.
  let inline right (f:AsyncFunc<'a, 'b>) : AsyncFunc<Choice<'c, 'a>, Choice<'c, 'b>> =
    function
    | Choice1Of2 c -> Choice1Of2 c |> async.Return
    | Choice2Of2 a -> f a |> Async.map Choice2Of2

  /// Creates an async function which feeds the first argument to the argument async function and passes the second one through.
  let inline first (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a * 'c, 'b * 'c> =
    fun (a,c) -> f a |> Async.map (fun b -> b,c)

  /// Creates an async function which feeds the first argument to the argument async function and passes the second one through.
  let inline second (f:AsyncFunc<'a, 'b>) : AsyncFunc<'c * 'a, 'c * 'b> =
    fun (c,a) -> f a |> Async.map (fun b -> c,b)

  /// Creates an async function which propagates its input into the output.
  let inline inout (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a, 'a * 'b> =
    fun a -> f a |> Async.map (fun b -> a,b)

  //// Invokes the argument async function, but discards the result and passes its argument through the result.
  let inline inoutIgnore (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a, 'a> =
    fun a -> f a |> Async.map (fun _ -> a)

  /// Creates an async function which feeds a Some a to the argument async function, otherwise returns None.
  let option (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a option, 'b option> =
    function
    | Some a -> f a |> Async.map Some
    | None -> async.Return None

  /// Creates an async function which invokes the argument error on input Some a otherwise returns b.
  let optionOr (b:'b) (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a option, 'b> =
    function
    | Some a -> f a
    | None -> async.Return b

  /// Creates an async function which invokes the argument error on input Some a otherwise returns
  /// the result of b.
  let optionOrLazy (b:unit -> 'b) (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a option, 'b> =
    function
    | Some a -> f a
    | None -> async.Return (b())  

  /// Creates an async function which catches exceptions thrown by the async computation produces
  /// by the argument async function.
  let inline catch (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a, Choice<'b, exn>> =
    f >> Async.Catch

  /// Creates an async function which throws an exception when the choice value
  /// produces by the argument async function contains an error.
  let inline throw (f:AsyncFunc<'a, Choice<'b, exn>>) : AsyncFunc<'a, 'b> =
    fun a -> async {
      let! r = f a
      match r with
      | Choice1Of2 b -> return b
      | Choice2Of2 e -> return raise e
    }

  /// Creates an async function which invokes the argument async function and ignotes its output value.
  let inline ignore (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a, unit> =
    f >> Async.Ignore

  /// Creates an async function which filters inputs to an argument async function based on the specified predicate.
  /// Returns the provided default value when the predicate isn't satisfied.
  let filterAsync (df:Async<'b>) (p:AsyncFunc<'a, bool>) (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a, 'b> =
    fun a -> p a |> Async.bind (function true -> f a | false -> df)  

  /// Creates an async function which first invokes f then g with the input and output of f. The output of g is discarded.
  /// This is thenDoIn flipped.
  let doAfterIn (f:AsyncFunc<'a, 'b>) (g:AsyncFunc<'a * 'b, _>) : AsyncFunc<'a, 'b> =
    fun a -> f a |> Async.bind (fun b -> g (a,b) |> Async.map (fun _ -> b))

  /// Runs an async function after the target async function passing in the output of target async function.
  /// This is thenDo flipped.
  let doAfter (f:AsyncFunc<'a, 'b>) (g:AsyncFunc<'b, _>)  : AsyncFunc<'a, 'b> =
    fun a -> f a |> Async.bind (fun b -> g b |> Async.map (fun _ -> b))
  
  /// Creates an async function which first invokes f then g with the results of f. The output value of g is discarded.
  /// This is doAfterIn flipped.
  let inline thenDoIn (g:AsyncFunc<'a * 'b, _>) (f:AsyncFunc<'a, 'b>)  : AsyncFunc<'a, 'b> =
    doAfterIn f g    

  /// Creates an async function which first invokes f then g with the results of f. The results (but not side-effects) of g are discarded.
  /// This is doAfter flipped.
  let inline thenDo (g:AsyncFunc<'b, _>) (f:AsyncFunc<'a, 'b>)  : AsyncFunc<'a, 'b> =
    doAfter f g

  /// Runs an async function after the target async function.    
  let doBefore (f:AsyncFunc<'a, 'b>) (g:AsyncFunc<'a, _>) : AsyncFunc<'a, 'b> =
    fun a -> g a |> Async.bind (fun _ -> f a)
   
  /// Creates an async function which operates on arrays of inputs, applying in parallel but preserving input order.
  let arrayPar (a:AsyncFunc<'a, 'b>) : AsyncFunc<'a[], 'b[]> =
    fun aa -> aa |> Array.map a |> Async.Parallel

  /// Lifts an async function to operate on arrays of inputs, applying sequentially.
  let array (a:AsyncFunc<'a, 'b>) : AsyncFunc<'a[], 'b[]> =
    fun xs -> async {
      let ys = Array.zeroCreate (xs.Length)
      for i = 0 to xs.Length - 1 do
        let! y = a (xs.[i])
        ys.[i] <- y
      return ys        
    }

  /// Invokes the async function g before async function f. If g returns a left choice value which contains the input to f, 
  /// the resulting async function will continue. Otherwise the right choice value is propagated.
  /// Reference implementation: g >>> (AsyncFunc.left f)
  let beforeChoice (g:AsyncFunc<'a, Choice<'a, 'c>>) (f:AsyncFunc<'a, 'b>) : AsyncFunc<'a, Choice<'b, 'c>> =
    compose (left f) g

  /// Invokes the async function g with the successful result 'b of async function f. If f returns a failure no action is performed.
  let afterSuccessAsync (g:AsyncFunc<'b, _>) (f:AsyncFunc<'a, Choice<'b, 'e>>) : AsyncFunc<'a, Choice<'b, 'e>> =
    f |> doAfter <| function
      | Choice1Of2 b -> g b
      | Choice2Of2 _ -> Async.unit

  /// Creates an async function which first runs async function g then if it returns a successful result runs f otherwise it
  /// propagates the error 'e.
  let composeChoice (g:AsyncFunc<'a, Choice<'b, 'e>>) (f:AsyncFunc<'b, Choice<'c, 'e>>) : AsyncFunc<'a, Choice<'c, 'e>> =
    g >> Async.bindChoice f

  /// Maps over the successful output of an async function.
  let mapSuccess (g:'b -> 'c) (f:AsyncFunc<'a, Choice<'b, 'e>>) : AsyncFunc<'a, Choice<'c, 'e>> =
    f |> mapr (Choice.mapl g)

  /// Maps over the erroneous output of an async function.
  /// Reference implementation: f |> AsyncFunc.mapr (Choice.mapr g)
  let mapError (g:'e -> 'f) (f:AsyncFunc<'a, Choice<'b, 'e>>) : AsyncFunc<'a, Choice<'b, 'f>> =
    f |> mapr (Choice.mapr g)

  /// Creates an async function which first runs async function g then if it returns a successful result runs f otherwise it
  /// propagates the error 'e1. If the async function f fails, the error 'e2 is propagated.
  let andThenChoice_ (f:AsyncFunc<'b, Choice<'c, 'e2>>) (g:AsyncFunc<'a, Choice<'b, 'e1>>) : AsyncFunc<'a, Choice<'c, Choice<'e1, 'e2>>> =
    g >> Async.bindChoices f

  /// Creates an async function which first runs async function g then if it returns a successful result runs f otherwise it
  /// propagates the error 'e.
  /// Reference implementation: andThenTry_ f g |> mapError Choice.codiag     
  let andThenChoice (f:AsyncFunc<'b, Choice<'c, 'e>>) (g:AsyncFunc<'a, Choice<'b, 'e>>) : AsyncFunc<'a, Choice<'c, 'e>> =
    g >> Async.bindChoice f

  /// Creates an async function which calls the specified callback function when an error 'e occurs.
  let notifyErrors (cb:'a * 'e -> unit) : AsyncFunc<'a, Choice<'b, 'e>> -> AsyncFunc<'a, Choice<'b, 'e>> =
    thenDoIn <| fun (a,e) -> 
      match e with
      | Choice1Of2 _ -> Async.unit
      | Choice2Of2 e -> cb (a,e) ; Async.unit


/// Async sink - a function which takes a value and produces an async computation.
type AsyncSink<'a> = AsyncFunc<'a, unit>

/// Operations on async sinks.
module AsyncSink =
    
  /// Maps a function over the input of a sink.
  let contramap (f:'b -> 'a) (s:AsyncSink<'a>) : AsyncSink<'b> =
    AsyncFunc.mapl f s

  /// Maps a function over the input of a sink.
  let contramapAsync (f:'b -> Async<'a>) (s:AsyncSink<'a>) : AsyncSink<'b> =
    AsyncFunc.maplAsync f s

  /// Filters inputs to a sink.
  let filterAsync (f:'a -> Async<bool>) (s:AsyncSink<'a>) : AsyncSink<'a> =
    s |> AsyncFunc.filterAsync Async.unit f

  /// Filters inputs to a sink.
  let filter (f:'a -> bool) (s:AsyncSink<'a>) : AsyncSink<'a> =
    filterAsync (f >> async.Return) s

  /// Maps and filters inputs to a sink.
  let contrachooseAsync (f:'b -> Async<'a option>) (s:AsyncSink<'a>) : AsyncSink<'b> =
    f >> Async.bind (function
      | Some a -> s a
      | None -> Async.unit
    )          
  
  /// Maps and filters inputs to a sink.
  let contrachoose (f:'b -> 'a option) (s:AsyncSink<'a>) : AsyncSink<'b> =
    contrachooseAsync (f >> async.Return) s

  /// Merges two sinks such that the resulting sink invokes the first one, then the second one.
  let merge (s1:AsyncSink<'a>) (s2:AsyncSink<'a>) : AsyncSink<'a> =
    fun a -> s1 a |> Async.bind (fun _ -> s2 a)

  /// Merges two sinks such that the resulting sink invokes the first one, then the second one.
  /// This merge with arguments flipped.
  let inline andThen s2 s1 = merge s1 s2

  /// Creates a sink which invokes the underlying sinks sequentially.
  let mergeAll (xs:seq<AsyncSink<'a>>) : AsyncSink<'a> =
    fun a -> async {
      for s in xs do
        do! s a }

  /// Creates a sink which invokes the underlying sinks in parallel.
  let mergeAllPar (xs:seq<AsyncSink<'a>>) : AsyncSink<'a> =
    fun a -> xs |> Seq.map ((|>) a) |> Async.Parallel |> Async.Ignore

  /// Merges two sinks such that the resulting sink invokes the first one, then the second one.
  let mergePar (s1:AsyncSink<'a>) (s2:AsyncSink<'a>) : AsyncSink<'a> =
    mergeAllPar [ s1 ; s2 ]

  /// Lifts an async sink to operate on sequences of the input type in parallel.
  let seqPar (s:AsyncSink<'a>) : AsyncSink<seq<'a>> =
    Seq.map s >> Async.Parallel >> Async.Ignore

  /// Lifts an async sink to operate on sequences of the input type sequetially.
  let seq (s:AsyncSink<'a>) : AsyncSink<seq<'a>> =
    fun xs -> async { for x in xs do do! s x }
  
(**

# F# Async: AsyncFunc

An `AsyncFunc<'a, 'b>` is a function which takes a value of type `'a` as input and returns
an async computation which produces a value of type `'b`. An async function is represented
as a type alias `type AsyncFunc<'a, 'b> = 'a -> Async<'b>`. Many of the operations defined on 
ordinary functions, such as function composition `>>`, can also be defined for async functions with 
the help of the `Async` type. Async functions can be used to represent request-reply interactions allowing 
these interactions to be composed and transformed in various ways. 

The `AsyncFunc` type is located in the `FSharpx.Async.dll` assembly which can be loaded in F# Interactive as follows:
*)

#r "../../../bin/FSharpx.Async.dll"
#r "System.Net.Http.dll"
open FSharpx.Control


(**
## Motivation

For example, consider an HTTP request-reply operation as provided
by [System.Net.Http.HttpClient](https://msdn.microsoft.com/en-us/library/system.net.http.httpclient%28v=vs.118%29.aspx). 
The method [HttpClient.SendAsync](https://msdn.microsoft.com/en-us/library/hh138176(v=vs.118).aspx) 
when adapted to `Async` has type `HttpRequestMessage -> Async<HttpResponseMessage>`. A specific instance
of `HttpClient` can be constructed and then passed around as a function value. For brevity we also alias
the HTTP message types:
*)

open System.Net
open System.Net.Http

type HttpReq = HttpRequestMessage

type HttpRes = HttpResponseMessage

let httpClient = new HttpClient()

let httpService : HttpReq -> Async<HttpRes> = 
  httpClient.SendAsync >> Async.AwaitTask


(**
Suppose you wanted to modify this function such that a header is added to the HTTP response before it is returned. This can be
done as follows:
*)

let httpServiceWithHeader : HttpReq -> Async<HttpRes> =
  httpService >> Async.map (fun res ->
    res.Content.Headers.Add("X-Hello", "World")
    res)

(**
Similarly to modify the headers of the incoming HTTP request:
*)

let httpServiceWithHeader' : HttpReq -> Async<HttpRes> =
  fun req -> 
    req.Headers.Add("X-Foo", "Bar")
    httpServiceWithHeader req

(**
Now the value `httpServiceWithHeader'` is an HTTP service which calls `HttpClient` but adds a header to both
the incoming HTTP request and to the outgoing HTTP response. Up to this point we've relied on existing mechanisms
provided by `Async` itself. Suppose next that we wanted to measure the latency of HTTP interactions. This can be
done as follows:
*)

let httpServiceTimed : HttpReq -> Async<HttpRes> =
  fun req -> async {
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let! res = httpServiceWithHeader' req
    sw.Stop()
    printfn "elapsed_ms=%i" sw.ElapsedMilliseconds
    return res
  }

(**
This timing logic can be useful outside of the specific HTTP service that we've composed. Let us abstract it:
*)

let httpTimer (service:HttpReq -> Async<HttpRes>) (req:HttpReq) =
  async {
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let! res = service req
    sw.Stop()
    printfn "elapsed_ms=%i" sw.ElapsedMilliseconds
    return res    
  }

(**
The value `httpTimer` is a function which takes an HTTP service and produces another HTTP service which wraps the
argument service with a timer. If we view the HTTP service as an async function `AsyncFunc<HttpReq, HttpRes>`
then the above value is a mapping between async functions. We can type alias this mapping between async functions as
an async filter:
*)

type AsyncFilter<'a, 'b, 'c, 'd> = AsyncFunc<'a, 'b> -> AsyncFunc<'c, 'd>

type AsyncFilter<'a, 'b> = AsyncFilter<'a, 'b, 'a, 'b>

(**
The operations which modify HTTP request and response headers (shown above) can be abstracted into async filters 
as follows:
*)

let reqHeaderFilter : AsyncFilter<HttpReq, HttpRes> =
  fun service req -> async {
    req.Headers.Add("X-Foo", "Bar") 
    return! service req 
  }

let resHeaderFilter : AsyncFilter<HttpReq, HttpRes> =
  fun service req -> async {
    let! res = service req 
    res.Content.Headers.Add("X-Hello", "World")
    return res
  }


(**
What do we gain by reifying the async function and async filter types? Composition of course! Once we abstract the above 
logic into filters we can glue the filters back together in various ways. Here is one:
*)

let compositeFilter : AsyncFilter<HttpReq, HttpRes> =
  reqHeaderFilter  
  >> resHeaderFilter
  >> httpTimer

(**
We've created a filter pipeline using the function composition operator `>>` which allows us to compose two filters into one. 
Since an async filter is just a function between async functions, we can apply it to the original HTTP service by simply
passing it in:
*)

let filteredService : AsyncFunc<HttpReq, HttpRes> =
  httpService |> compositeFilter

(**
Note that the order of composition matters - it determines the order in which the filters in the pipeline are invoked.

The filters defined above leave the type of the async functions unchanged. However in general, since a filter is a mapping between
async functions, we can define filters which change the input and output types of the corresponding async functions. Suppose that we are 
implementing an HTTP service which exposes an underlying domain model. We would like to use the rich F# type system to
define our domain and then adapt it to the HTTP protocol. Filters allow us to separate concerns - we can define a filter
which takes an async function operating on domain specific input and output types and map it to an async function based on HTTP 
types as seen above. To do this we first define encoding functions:
*)

/// A domain-specific input type.
type Input = {
  id : string
}

/// A domain-specific output type.
type Output = {
  text : string
}

/// Decodes an HTTP request into a domain-specific input.
let decode (req:HttpReq) = async {
  let! id = req.Content.ReadAsStringAsync() |> Async.AwaitTask
  return {
    Input.id = id
  }
}

/// Encodes a domain-specific output into an HTTP response.
let encode (o:Output) = async {
  let res = new HttpRes()
  res.Content <- new StringContent(o.text)
  return res
}

(**
Next we abstract the encoding mechanism into a filter. In this case however, the filter will change the input and output types
of the corresponding async functions:
*)

let codecFilter 
  (dec:HttpReq -> Async<'i>, 
   enc:'o -> Async<HttpRes>) : AsyncFilter<'i, 'o, HttpReq, HttpRes> =
  // map over the input with the decoder
  AsyncFunc.maplAsync dec 
  // and map over the output with the decoder
  >> AsyncFunc.maprAsync enc

(**
The value `codecFilter` is a function which when given decoder and encoder function creates an async filter which takes an
async function based on the encoded types `'i` and `'o` and maps it to an async function based on HTTP. This filter can be used as
follows:
*)

let myService (i:Input) = async {
  return {
    Output.text = i.id
  }
}

let myHttpService : AsyncFunc<HttpReq, HttpRes> = 
  myService |> codecFilter (decode,encode)

(**
Async functions and filters allowed us to separate the concerns of HTTP from the concerns of the domain-specific service as well as cross-cutting
concerns such as timing. The `codecFilter` defined above can be made more generic or defined for a specific format such as JSON. This 
example merely scratches the surface of what can be done with filters and there is a myriad of other filters we can define - authorization, logging, 
routing, etc.
*)


(**

## Async sinks

A async sink is a specific type of async function which produces `unit` as output `type AsyncSink<'a> = AsyncFunc<'a, unit>`. This particular 
type of async function is interesting because it captures the notion of a side-effect. Since we've specialized the output type of an async function to `unit` 
we can define operations on async sinks that can't be defined on async functions in general.

The domain-specific service `myService` defined above simply echoes its input. A real world service will usually do much more. A 
common task in services is to store the results of the operation in a database. We can model a service which stores output in
a database with type `Output -> Async<unit>`. In addition to storing the output of a service to a database we may wish to publish
a message on a queue to notify subscribing parties. A service to publish a message on a queue can also be modeled type with type
`Output -> Async<unit>`. In both cases we have an async sink `AsyncSink<Output>`. Representing these capabilities as sinks allows
us to compose them as follows:
*)


let saveToDb : AsyncSink<Output> =
  fun o -> Async.unit

let publishToQueue : AsyncSink<Output> =
  fun o -> Async.unit

let saveThenPublish : AsyncSink<Output> =
  saveToDb 
  |> AsyncSink.andThen publishToQueue


(**
The resulting sink `saveThenPublish` first invokes service `saveToDb` then `publishToQueue`. Order may be of the essence - if the
database operation fails, publishing an message on a queue can cause inconsistencies. If the operations are completely independent 
then we can compose them in parallel using `AsyncSink.mergePar`. We can apply this sink to the service defined above as follows:
*)

let myServiceWithSink : AsyncFunc<Input, Output> =
  myService
  |> AsyncFunc.thenDo saveThenPublish


(**

## Error handling

F# encourages the representation of errors explicitly in the type system rather than using an out of band mechanism such as exceptions.
An operation which produces a value of type `'a` but which may fail with error type `'e` can be represented using the choice type as 
`Choice<'a, 'e>`. The operator `Async.Catch` catches any exception thrown by an async computation and reifies it as a value of type
`Choice<'a, exn>`. Async functions can capture common patterns of handling errors and help us compose services which may error.

For example, suppose the domain-specific service `myService` define above is extended to support explicit errors. Changing its output type
to `Choice<Output, exn>` will force us to handle the error explicitly. Async functions and filters once again allows us to separate concerns
into well defined modules:
*)

/// A domain-specific service which may error.
/// AsyncFunc<Input, Choice<Output, exn>>
let myServiceErr (i:Input) = async {
  if (i.id = "foo") then 
    return Choice2Of2 (exn("oh no!"))
  else
    return {
      Output.text = i.id
    }
    |> Choice1Of2
}

/// An explicit encoder for errors.
/// AsyncFunc<exn, HttpRes>
let encodeErr (ex:exn) = async {
  let res = new HttpRes(HttpStatusCode.BadRequest)
  res.Content <- new StringContent(ex.Message)
  return res
}


module Choice =
  
  /// Folds a choice value by handling both cases explicitly.  
  let fold (f:'a -> 'b) (g:'e -> 'b) = function
    | Choice1Of2 a -> f a
    | Choice2Of2 e -> g e


/// A service which invokes the sink defined above upon success;
/// otherwise propagates the error.
let myServiceErrSink : AsyncFunc<Input, Choice<Output, exn>> =
  myServiceErr 
  |> AsyncFunc.afterSuccessAsync saveThenPublish
  

/// An HTTP service which handles errors explicitly.
let myHttpServiceErr : AsyncFunc<HttpReq, HttpRes> = 
  myServiceErrSink |> codecFilter (decode, Choice.fold encode encodeErr)



(**

We are able to introduce explicit errors into our services while still retaining compositionality and separation of concerns. To this end
we've defined an explicit encoder for errors which simply returns an HTTP 404 response. Next we composed the service such that errors
are threaded through but the operation to write to the database and publish on a queue is only invoked when the output is successful.

*)


(**

## Caution

It can be easy to get carried away with async functions. Many of the operations on async functions simply compose `Async` in 
specific ways. As such, it is possible to duplicate a lot of functionality already provided by `Async` itself. It can be tempting
to define entire libraries in terms of async functions so as to provide a uniform programming model. Its important to remember that async functions
and filters should serve as the *glue* for composing services, not services in their own right. Before defining a new async function or filter
consider whether it can be implemented with existing operators.

*)


(**

## Relationship to Async

If you've used the F# `Async` type then you've already used async functions. In fact, one way to think of the `Async` type is as enabling
composition of async functions. If you have one function of type `'a -> 'b` and another function of type `'b -> 'c` then you also
have a function of type `'a -> 'c` which can be *composed* from the first two using the `>>` operator (or `<<`). The implementation of
this operator is as follows:

*)

let (>>) (f:'a -> 'b) (g:'b -> 'c) : 'a -> 'c =
  fun a -> 
    let b = f a // first evaluate f at a
    g b // then call g with the result

(**

The reason this operator is simple to implement is because the output of function `f` is exactly the input that the second function
`g` is looking for. With *async* functions the story isn't as simple because the output of the first is wrapped by `Async`. This is 
where operations defined on `Async` come into play. The particular operation of interest is `async.Bind` defined on the `Async` 
[workflow builder](https://msdn.microsoft.com/en-us/library/dd233182.aspx) with type `async.Bind(a:Async<'a>, binder:'a -> Async<'b>) : Async<'b>`.
The `binder` argument is an async function and we can use this operation to implement composition of async functions as follows:

*)

let composeAsync (f:'a -> Async<'b>) (g:'b -> Async<'c>) : 'a -> Async<'c> =
  fun a -> 
    let b = f a // first evaluate f at a
    async.Bind(b, g) // then bind the result using async function g



(**

# Further Reading

* [Your Server as a Function](http://monkey.org/~marius/funsrv.pdf)
* [Generalizing Monads to Arrows](http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf)

*)
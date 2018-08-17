module FSharpx.Async.Tests.AsyncTest

open System
open System.Threading.Tasks
open Xunit
open FSharpx.Control
open System.Collections.Generic
open System.Collections

[<Fact>]
let ``Async.ParallelIgnore should run argument computations``() =  
  let bag = System.Collections.Concurrent.ConcurrentBag<_>()  
  let s = Seq.init 10 id |> Set.ofSeq    
  s 
  |> Seq.map (fun i -> bag.Add i ; Async.unit)
  |> Async.ParallelIgnore 1
  |> Async.RunSynchronously
  Assert.True((s = (bag |> Set.ofSeq)))

[<Fact>]
let ``Async.ParallelIgnore should fail upon first failure``() =
  let s =
    [
      async { return failwith "catch me if you can" }
    ]
  Assert.Throws<AggregateException>(fun() ->
    s
    |> Async.ParallelIgnore 1
    |> Async.RunSynchronously
  )
  |> ignore

[<Fact>]
let ``Async.ParallelIgnore should cancel upon first cancellation``() =
  let tcs = new TaskCompletionSource<unit>()
  let s =
    [
      tcs.Task |> Async.AwaitTask
    ]
  tcs.SetCanceled()
  try
    s
    |> Async.ParallelIgnore 1
    |> Async.RunSynchronously
    failwith "Should throw exception before here."
  with e ->
    Assert.Equal (typeof<TaskCanceledException>,e.InnerException.GetType())

[<Fact>]
let ``Parallel with throttle``() =
  let nums  = [|123|]
  let work n = async.Return(n)
  let tasks = nums |> Array.map work
  let resultAsync = Async.ParallelWithThrottle 1 tasks
  let result = (resultAsync |> Async.RunSynchronously) :> IEnumerable<int>
  Assert.Equal(nums, result)
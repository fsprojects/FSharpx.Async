module FSharpx.Async.Tests.AsyncTest

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FSharpx.Control

[<Test>]
let ``Async.ParallelIgnore should run argument computations``() =  
  let bag = System.Collections.Concurrent.ConcurrentBag<_>()  
  let s = Seq.init 10 id |> Set.ofSeq    
  s 
  |> Seq.map (fun i -> bag.Add i ; Async.unit)
  |> Async.ParallelIgnore 1
  |> Async.RunSynchronously
  Assert.True((s = (bag |> Set.ofSeq)))

[<Test>]
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

[<Test>]
let ``Async.ParallelIgnore should cancel upon first cancellation``() =
  let tcs = new TaskCompletionSource<unit>()
  let s =
    [
      tcs.Task |> Async.AwaitTask
    ]
  tcs.SetCanceled()
  Assert.Throws<OperationCanceledException>(fun() ->
    s
    |> Async.ParallelIgnore 1
    |> Async.RunSynchronously
  )
  |> ignore

[<Test>]
let ``Parallel with throttle``() =
  let nums = [|123|]
  let work n = async.Return(n)
  let tasks = nums |> Array.map work
  let result = Async.ParallelWithThrottle 1 tasks
  Assert.AreEqual(nums, result |> Async.RunSynchronously)

[<Test>]
let ``Cache should catch and bubble up exceptions``() =
  let workflow = async { invalidOp "fail" } |> Async.Cache
  Assert.Throws<InvalidOperationException>(fun () -> workflow |> Async.RunSynchronously) |> ignore

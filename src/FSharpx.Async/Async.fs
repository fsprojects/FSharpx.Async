﻿// ----------------------------------------------------------------------------
// F# async extensions
// (c) Tomas Petricek, David Thomas 2012, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharpx.Control

open System
open System.Threading
open System.Threading.Tasks

// ----------------------------------------------------------------------------

[<AutoOpen>]
module AsyncExtensions = 
    type Microsoft.FSharp.Control.Async with 

    /// Creates an asynchronous workflow that runs the asynchronous workflow
    /// given as an argument at most once. When the returned workflow is 
    /// started for the second time, it reuses the result of the 
    /// previous execution.
    static member Cache (input:Async<'T>) = 
      let agent = Agent<AsyncReplyChannel<_>>.Start(fun agent -> async {
          let! repl = agent.Receive()
          let! res = input
          repl.Reply(res)
          while true do
              let! repl = agent.Receive()
              repl.Reply(res) })

      async { return! agent.PostAndAsyncReply(id) }

    /// Starts the specified operation using a new CancellationToken and returns
    /// IDisposable object that cancels the computation. This method can be used
    /// when implementing the Subscribe method of IObservable interface.
    static member StartDisposable(op:Async<unit>) =
      let ct = new System.Threading.CancellationTokenSource()
      Async.Start(op, ct.Token)
      { new IDisposable with 
          member x.Dispose() = ct.Cancel() }

    /// Provided an array of Async computations, runs each and returns the first
    /// to complete or raise an exception.
    static member Any(tasks: Async<'T>[]) =
        let tcs = new TaskCompletionSource<'T>()
        let exec work =
            async {
                try
                    let! result = work
                    tcs.TrySetResult result |> ignore
                with exn ->
                    tcs.TrySetException exn |> ignore
            } |> Async.Start

        tasks
        |> Array.Parallel.map exec
        |> ignore

        Async.AwaitTask tcs.Task

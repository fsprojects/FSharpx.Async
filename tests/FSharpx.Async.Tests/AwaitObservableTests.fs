﻿namespace FSharpx.Control.Tests

open System
open System.Threading
open System.Threading.Tasks

open FSharpx.Control.Observable

open NUnit.Framework

[<TestFixture>]
type ``AwaitObservable Tests``() = 

    [<Test; Repeat(1000)>]
    member test.``AwaitObservable yields a value from the sources Next``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Next("DONE")
        source.Completed()
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        Assert.AreEqual(Result "DONE", result)

    [<Test; Repeat(1000)>]
    member test.``AwaitObservable yields the first value from the sources Next``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Next("ONE")
        source.Next("TWO")
        source.Completed()
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        Assert.AreEqual(Result "ONE", result)

    [<Test; Repeat(10)>]
    member test.``AwaitObservable is canceled if the source completes without a single result``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(0.1))
        source.Completed()
        let result = awaiter(TimeSpan.FromSeconds(0.1))
        Assert.AreEqual(AwaiterResult<string>.Canceled, result)
        
    [<Test; Repeat(1000)>]
    member test.``AwaitObservable is unsubscribed from the source after a value was received``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Next("Done")
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))

    [<Test; Repeat(1000)>]
    member test.``AwaitObservable is unsubscribed from the source after the source completes without a result``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Completed()
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))

    [<Test>]
    member test.``AwaitObservable is unsubscribed from the source after OnError was called``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Error(exn "test-error")
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))

    [<Test; Repeat(1000)>]
    member test.``AwaitObservable is unsubscribed from the source if it's resulting async-workflow gets cancelled``() =
        let cts = new CancellationTokenSource()
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiterWithCancellation (wf, Some cts.Token)
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        cts.Cancel()
        let result = awaiter (TimeSpan.FromSeconds(1.0)) 
        Assert.AreEqual(AwaiterResult<string>.Canceled, result)
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))
    
    [<Test; Repeat(1000)>]
    member test.``AwaitObservable yields the first value from a hot observable``() =
        let source = { new IObservable<string> with
            member __.Subscribe(observer) =
                observer.OnNext("ONE")
                observer.OnNext("TWO")
                { new IDisposable with 
                    member __.Dispose () = () } }

        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        Assert.AreEqual(Result "ONE", result)
        
    [<Test; Repeat(1000)>]
    member test.``AwaitObservable yields the first value from a hot observable with error``() =
        let source = { new IObservable<string> with
            member __.Subscribe(observer) =
                observer.OnNext("ONE")
                observer.OnError (exn "test-error")
                { new IDisposable with 
                    member __.Dispose () = () } }

        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        Assert.AreEqual(Result "ONE", result)
            
    [<Test; Repeat(1000)>]
    member test.``AwaitObservable yields the first value from a hot observable which has completed``() =
        let source = { new IObservable<string> with
            member __.Subscribe(observer) =
                observer.OnNext("ONE")
                observer.OnCompleted()
                { new IDisposable with 
                    member __.Dispose () = () } }

        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        Assert.AreEqual(Result "ONE", result)
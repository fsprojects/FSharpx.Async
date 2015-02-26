module FSharpx.Async.Tests.AsyncTest

open NUnit.Framework
open FSharpx.Control

let ( =? ) (actual: 'T) (expected: 'T) = Assert.AreEqual(expected, actual)

[<Test>]
[<TestCase(100, 1000, 1)>]
[<TestCase(1000, 100, 2)>]
let ``Any should return the first async computation to complete`` (sleep1, sleep2, expected) =
    let async1 = async {
        do! Async.Sleep sleep1
        return 1 }
    let async2 = async {
        do! Async.Sleep sleep2
        return 2 }

    let actual = Async.Any [| async1; async2 |] |> Async.RunSynchronously

    actual =? expected

[<Test>]
[<TestCase(100, 1000, "1")>]
[<TestCase(1000, 100, "One or more errors occurred.")>]
let ``Any should return the first async computation to raise an exception`` (sleep1, sleep2, expected) =
    let async1 = async {
        if sleep1 > 100 then
            failwith "Not so fast!"
        do! Async.Sleep sleep1
        return 1 }
    let async2 = async {
        do! Async.Sleep sleep2
        return 2 }

    let result =
        [| async1; async2 |]
        |> Async.Any 
        |> Async.Catch
        |> Async.RunSynchronously

    let actual =
        match result with
        | Choice1Of2 res -> res.ToString()
        | Choice2Of2 exn -> exn.Message

    actual =? expected

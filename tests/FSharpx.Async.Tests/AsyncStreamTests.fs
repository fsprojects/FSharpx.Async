﻿module AsyncStreamTests

open FSharp.Control
open FSharpx.Control
open Xunit

[<Fact>]
let ``AsyncStream.repeat``() =
  
  let n = 3

  let s = 
    AsyncStream.repeat 1 
    |> AsyncStream.take n 
    |> AsyncSeq.toList 
  
  Assert.True ((List.init n (fun _ -> 1) = s))

[<Fact>]
let ``AsyncStream.unfoldAsync``() =
  
  let s = 
    AsyncStream.unfoldAsync (fun i -> (i,i + 1) |> async.Return) 0
    |> AsyncStream.take 3
    |> AsyncSeq.toList

  Assert.True ((List.init 3 id = s))

[<Fact>]
let ``AsyncStream.mapAsync``() =
  
  let s = 
    AsyncStream.repeat 1
    |> AsyncStream.mapAsync (fun a -> a.ToString() |> async.Return)
    |> AsyncStream.take 3
    |> AsyncSeq.toList

  Assert.True ((List.init 3 (fun _ -> "1") = s))

[<Fact>]
let ``AsyncStream.cycleList``() =
  
  let s = 
    AsyncStream.cycleList [1;2;3]
    |> AsyncStream.take 6
    |> AsyncSeq.toList

  Assert.True(([1;2;3;1;2;3] = s))

[<Fact>]
let ``AsyncStream.tails``() =
  
  let s = 
    AsyncStream.cycleList [1;2;3;4]
    |> AsyncStream.tails
    |> AsyncStream.take 3
    |> AsyncSeq.map (AsyncStream.take 3 >> AsyncSeq.toList)
    |> AsyncSeq.toList
    
  Assert.True(([ [2;3;4] ; [3;4;1] ; [4;1;2] ] = s))

[<Fact>]
let ``AsyncStream.prefixAsyncSeq``() =
  
  let s = 
    AsyncStream.repeat 3
    |> AsyncStream.prefixAsyncSeq (AsyncSeq.ofSeq [1;2])
    |> AsyncStream.take 3
    |> AsyncSeq.toList
    
  Assert.True(([1;2;3] = s))

[<Fact>]
let ``AsyncStream.splitAtList``() =
  
  let (ls,tl) = 
    AsyncStream.cycleList [1;2;3]
    |> AsyncStream.splitAtList 3
    |> Async.RunSynchronously

  Assert.True(([1;2;3] = ls))

[<Fact>]
let ``AsyncStream.filterAsync``() =
  
  let s = 
    AsyncStream.cycleList [1;2;3;4]
    |> AsyncStream.filterAsync (fun a -> (a % 2 = 0) |> async.Return)
    |> AsyncStream.take 4
    |> AsyncSeq.toList

  Assert.True([2;4;2;4] = s) 

[<Fact>]
let ``AsyncStream.chooseAsync``() =
  
  let s = 
    AsyncStream.cycleList [1;2;3;4]
    |> AsyncStream.chooseAsync (fun a -> if (a % 2 = 0) then Some (a.ToString()) |> async.Return else async.Return None)
    |> AsyncStream.take 4
    |> AsyncSeq.toList

  Assert.True(["2";"4";"2";"4"] = s) 

[<Fact>]
let ``AsyncStream.scanAsync``() =
  
  let s = 
    AsyncStream.cycleList [1;2;3;4]
    |> AsyncStream.scanAsync (fun a b -> b @ [a]  |> async.Return) []
    |> AsyncStream.take 4
    |> AsyncSeq.toList

  Assert.True([[1];[1;2];[1;2;3];[1;2;3;4]] = s) 

[<Fact>]
let ``AsyncStream.iterAsync``() =  
  let items = ResizeArray<_>()
  use cts = new System.Threading.CancellationTokenSource()
  let s = 
    AsyncStream.cycleList [1;2;3]
    |> AsyncStream.iterAsync (fun a -> async {
      items.Add(a)
      if a = 3 then cts.Cancel()
    })
  try Async.RunSynchronously (s, -1, cts.Token)
  with _ -> ()  
  Assert.True((List.ofSeq items = [1;2;3]))

  

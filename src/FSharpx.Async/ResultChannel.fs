namespace FSharpx.Control

/// A single-fire result channel which can be used to register a result and await it asynchronously.
type ResultChannel<'T>() =               
    let mutable result = None   // result is None until one is registered
    let mutable savedConts = [] // list of continuations which will be applied to the result
    let syncRoot = new obj()    // all writes of result are protected by a lock on syncRoot

    /// Record the result, starting any registered continuations.
    member channel.RegisterResult (res : 'T) =
        let grabbedConts = // grab saved continuations and register the result
            lock syncRoot (fun () ->
                if channel.ResultAvailable then // if a result is already saved the raise an error
                    failwith "Multiple results registered for result channel."
                else // otherwise save the result and return the saved continuations
                    result <- Some res
                    List.rev savedConts)

        // run all the grabbed continuations with the provided result
        grabbedConts |> List.iter (fun cont -> cont res)

    /// Check if a result has been registered with the channel.
    member channel.ResultAvailable = result.IsSome

    /// Wait for a result to be registered on the channel asynchronously.
    member channel.AwaitResult () = async {
        let! ct = Async.CancellationToken // capture the current cancellation token
        
        // create a flag which indicates whether a continuation has been called (either cancellation
        // or success, and protect access under a lock; the performCont function sets the flag to true
        // if it wasn't already set and returns a boolen indicating whether a continuation should run
        let performCont = 
            let continued = ref false
            let localSync = obj()
            (fun () ->
                lock localSync (fun () ->
                    if not !continued 
                    then continued := true ; true
                    else false))
        
        // wait for a result to be registered or cancellation to occur asynchronously
        return! Async.FromContinuations(fun (cont, _, ccont) ->
            let resOpt = 
                lock syncRoot (fun () ->
                    match result with
                    | Some _ -> result // if a result is already set, capture it
                    | None   ->
                        // otherwise register a cancellation continuation and add the success continuation
                        // to the saved continuations
                        let reg = ct.Register(fun () -> 
                            if performCont () then 
                                ccont (new System.OperationCanceledException("The operation was canceled.")))
                                
                        let cont' = (fun res ->
                            // modify the continuation to first check if cancellation has already been
                            // performed and if not, also dispose the cancellation registration
                            if performCont () then
                                reg.Dispose()
                                cont res)
                        savedConts <- cont' :: savedConts
                        None)

            // if a result already exists, then call the result continuation outside the lock
            match resOpt with
            | Some res -> cont res
            | None     -> ()) }
(* 
  The Reader Monad is a functional programming design pattern that's all about dependency injection
  and decoupling side effects from business logic. 
  It allows a function to access a shared environment (like configuration data or service dependencies)
  without explicitly passing it through every function call. 
  This makes your code more modular and easier to test.

  In essence, the Reader monad encapsulates a computation that requires a context 
  (the "environment") and defers the supply of this context. It's like saying, 
  "Here's a function that will do some work, but it needs some information which I'll give you later."

  This can be particularly useful in:

  Configuration: Your functions can access configuration data without having to pass it around everywhere.
  Service Dependencies: If you have common services needed across different functions, Reader can provide them without explicit threading through all function calls.
*)

type Reader<'env,'a> = Reader of action:('env -> 'a)

module Reader =
  /// Run a Reader with a given environment
  let run env (Reader action)  =
    action env  // simply call the inner function

  /// Create a Reader which returns the environment itself
  let ask = Reader id

  /// Map a function over a Reader
  let map f reader =
    Reader (fun env -> f (run env reader))

  /// flatMap a function over a Reader
  let bind f reader =
    let newAction env =
      let x = run env reader
      run env (f x)
    Reader newAction

  let retrn x = Reader (fun _ -> x)


module Logger =
    type ILogger =
        abstract member Log: string -> unit

    let consoleLogger: ILogger =
        { new ILogger with
            member this.Log message = printfn "%s" message }

module BusinessLogic =

    //Reader<Logger.ILogger, Logger.ILogger>
    let readerEnv = Reader (fun env -> env) 

    let doSomeCalculation x y =
        Reader.bind (fun (logger: Logger.ILogger) ->
            logger.Log "Starting calculation"
            let result = x + y  // Some business logic
            logger.Log (sprintf "Calculation result: %A" result)
            Reader.retrn result  // Wrapping result in a Reader
        ) readerEnv//Reader.ask

let runBusinessLogic =
    let calculation = BusinessLogic.doSomeCalculation 5 10
    (Logger.consoleLogger, calculation) ||> Reader.run 


let (<!>) = Option.map   // Option is a functor
let lift x = Some x // not used, but still conceptually important

let compose fOpt xOpt =
    match fOpt, xOpt with
        | Some f, Some x -> Some (f x)
        | _ -> None

let (<*>) = compose

let mult = (*)
mult <!> Some 3 <*> Some 4 |> printfn "%A"
mult <!> None   <*> Some 4 |> printfn "%A"
mult <!> Some 3 <*> None   |> printfn "%A"

lift mult <*> Some 3 // lift mult <*> Some 3 == mult <!> Some 3
Option.map mult (Some 3)
mult <!> Some 3
mult 3 |> (lift >> compose) <| Some 4
lift (mult 3) |> (fun f -> compose f (Some 4))
// (*) 3 4 |> lift
// (fun x -> x * 3) |> lift
// mult 3 |> lift
Option.map mult

// functor: times3 takes one argument
let times3 = mult 3
Option.map times3 (Some 4)   // Some 12
Option.map times3 None       // None
Option.map (mult 3) (Some 4)
Option.map (mult 3) None       // None

// applicative: mult takes two arguments
mult <!> Some 3 <*> Some 4   // Some 12
mult <!> None   <*> Some 4   // None
mult <!> Some 3 <*> None     // None

// piping
let pipe2 a b f = f <!> a <*> b
pipe2 (Some 3) (Some 4) mult
pipe2 (Some 3) None mult

// more than 2 args
Option.map (mult 2) (Option.map (mult 3) (Some 4))
mult <!> Some 2 <*> (mult <!> Some 3 <*> Some 4) // 24

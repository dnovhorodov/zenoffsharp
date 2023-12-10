// Monad
//
type Maybe<'a> =
    | Nothing
    | Just of 'a 

    // In the Maybe<'a> Discriminated Union, 
    // Just is a case constructor that takes a single argument 
    // of type 'a and returns a Maybe<'a>

    // Define 'bind' and 'return' as instance members
    member this.bind(f) =
        match this with
        | Nothing -> Nothing
        | Just a -> f a

    // return
    static member return'(a) = Just a

let maybeSquareRoot x =
    if x < 0.0 then Nothing
    else Just (sqrt x)

let maybeReciprocal x =
    if x = 0.0 then Nothing
    else Just (1.0 / x)

// Define the >>= operator
let (>>=) (m: Maybe<'a>) (f: 'a -> Maybe<'b>) = m.bind(f)
// OR
// let (>>=) m f =
//     match m with
//     | Nothing -> Nothing
//     | Just a -> f a

(* Monad laws *)
(* Left Identity Law:
    The Left Identity law states that if you take a value, put it into a monadic context 
    using the return function, and then feed it to a function f using the bind operation (>>=),
    it should be the same as just applying the function f to the value directly.
    In a formal notation, the Left Identity law is expressed as: 
    
    return a >>= f ≡ f a

    This law ensures that return acts as a sort of neutral element with respect to the bind
    operation, on the left-hand side. *)

//let leftIdentity = (Maybe.return'<float> 4.0) >>= maybeSquareRoot = maybeSquareRoot 4.0
let leftIdentity = Just 4.0 >>= maybeSquareRoot = maybeSquareRoot 4.0

(* Right Identity Law:
    The Right Identity law states that if you have a monadic value and you use the bind 
    operation to feed it to return, you should get the original monadic value back.
    In a formal notation, the Right Identity law is expressed as: 
    
    m >>= return ≡ m

    This law ensures that return acts as a sort of neutral element with respect to the bind 
    operation, on the right-hand side. *)

// let rightIdentity = (Just 4.0) >>= Maybe.return'<float> = Just 4.0 
let rightIdentity = Just 4.0 >>= Just = Just 4.0

(* Associativity Law: 
    The Associativity Law for a Monad dictates how operations (specifically, bind operations)
    can be chained or nested together. For the Maybe monad, this law ensures that the order in
    which operations are chained does not affect the final result. 
    The law is formulated as:

    (m >>= f) >>= g ≡ m >>= (fun x -> f x >>= g)

    In this expression, >>= represents the bind operation. 
    This law states that binding m to f and then binding the result to g should be equivalent
    to binding m to a function that applies f and then binds the result to g.
*)
let m = Maybe<float>.return' 4.0
let leftSide = (m >>= maybeSquareRoot) >>= maybeReciprocal
let rightSide = m >>= (fun x -> maybeSquareRoot x >>= maybeReciprocal)
leftSide = rightSide

(* Read-world example: monadic validation *)
open System
open System.Text.RegularExpressions

type ValidationResult<'a> =
    | Success of 'a
    | Failure of string list

module ValidationResult =

    let bind (f: 'a -> ValidationResult<'b>) (result: ValidationResult<'a>) : ValidationResult<'b> =
        match result with
        | Success x -> f x
        | Failure errs -> Failure errs

    let return' x = Success x

// Define a custom operator for chaining validations
let (>=>) f g x =
    match f x with
    | Success v -> g v
    | Failure err -> Failure err

let validateNotEmpty field value =
    if String.IsNullOrWhiteSpace(value)
    then Failure([sprintf "%s should not be empty." field])
    else Success(value)

let validateLength field maxLength value =
    if String.length value > maxLength
    then Failure([sprintf "%s should not exceed %d characters." field maxLength])
    else Success(value)

let validateEmail (email: string) : ValidationResult<string> =
    if Regex.IsMatch(email, @"^\S+@\S+\.\S+$")
    then Success(email)
    else Failure(["Invalid email format."])

type User = {
    Name: string
    Email: string
    Username: string
}

let validateName user =
    match validateNotEmpty "Name" user.Name with
    | Success _ -> Success user
    | Failure err -> Failure err

let validateUserEmail user =
    match validateEmail user.Email with
    | Success _ -> Success user
    | Failure err -> Failure err

let validateUsernameLength user =
    match validateLength "Username" 10 user.Username with
    | Success _ -> Success user
    | Failure err -> Failure err

// Example usage
// let validateUserDetails user =
//     ValidationResult.return' user
//     |> ValidationResult.bind (fun _ -> validateNotEmpty "Name" user.Name)
//     |> ValidationResult.bind (fun _ -> validateEmail user.Email)
//     |> ValidationResult.bind (fun _ -> validateLength "Username" 10 user.Username)

let validateUserDetails =
    ValidationResult.return'
    >=> validateName
    >=> validateUserEmail
    >=> validateUsernameLength

// Example invocation
let validUser = { Name = "John Doe"; Email = "john@example.com"; Username = "johndoe123" }
let invalidUser = { Name = ""; Email = "john@example.com"; Username = "johndoe123" }
let validationResult1 = validateUserDetails validUser
validateUserDetails invalidUser


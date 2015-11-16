// Piping and Composition

let add1 x = x + 1
let double x = x * 2
let square x = x * x

add1 5
square (double (add1 5))

5
|> add1
|> double
|> square

let add1_double = add1 >> double
let add1_double_square = add1_double >> square

add1_double_square 5

// FizzBuzz - composition
let isFizz x = 
    if (x % 3 = 0) then printf "Fizz" else printf ""
    x

let isBuzz x = 
    if (x % 5 = 0) then printf "Buzz" else printf ""
    x

let isNotFizzBuzz x =
    let isNotFizz = (x % 3 <> 0)
    let isNotBuzz = (x % 5 <> 0)

    if (isNotFizz && isNotBuzz) then (printf "%i" x) else printf ""
    x
    
let isFizzBuzz = isFizz >> isBuzz >> isNotFizzBuzz >> (fun x -> printfn "")


[1..30]
|> List.map isFizzBuzz
|> ignore

// FizzBuzz - pattern match
let fizzbuzz x = 
    match x with 
    | x when x % 15 = 0 -> "FizzBuzz"
    | x when x % 3 = 0 -> "Fizz"
    | x when x % 5 = 0 -> "Buzz"
    | _ -> x.ToString()

[1..30]
|> List.map fizzbuzz

// FizzBuzz - recursive function
let rec buildFizzBuzz results numbers =
    match numbers with
    | head::tail -> 
        let newResults = results @ [fizzbuzz head]
        buildFizzBuzz newResults tail
    | [] -> results

[1..100]
|> buildFizzBuzz []

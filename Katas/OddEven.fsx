
// Prints numbers from 1 to 100
[1..100]
|> List.map (printfn "%i")
|> ignore

// Print "Even" instead of number
let isEven x = 
    if x % 2 = 0 then 
        printfn "%i is Even" x
    x

[1..100]
|> List.map isEven
|> ignore

// Print "Odd" instead of number
let isOdd x = 
    if x % 2 <> 0 then 
        printfn "%i is Odd" x
    x

[1..100]
|> List.map isOdd
|> ignore

// Print "Prime" if it is Prime
let isPrime x =
    let determinePrimeness = 
        match x with
        | i when i % 2 = 0 -> 
            false
        | j when j > 2 -> 
            let tries = [2..(x-1)]
            tries
            |> List.forall (fun z -> x % z <> 0)
        | _ -> 
            false

    if determinePrimeness then
        printfn "%i is Prime" x
    x

[1..100]
|> List.map isPrime
|> ignore

// Make method to accept any number of range [currently we have 1 to 100]
let runForRange fn max =
    [1..max]
    |> List.map fn
    |> ignore

runForRange isEven 10
runForRange isOdd 25
runForRange isPrime 37
runForRange isOdd -38

// Create a new method to check Odd/Even/Prime of a single supplied method
let runAllForValue = isEven >> isOdd >> isPrime

runAllForValue 344
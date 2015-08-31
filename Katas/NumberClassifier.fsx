// a number is perfect if the sum of all factors (except the number) is equal to the number
// it is abundant if the sum of all factors (except the number) is larger
// it is deficient if the sum of all factors (except the number) is less

let isFactor number test =
    number % test = 0

let sumOfFactors number = 
    let potentials = [1..number]
    let sumOfFactors = 
        potentials
            |> List.filter (isFactor number)
            |> List.sum
    sumOfFactors

type Classification =
    | Prime
    | Perfect
    | Abundant
    | Deficient

let classify number =
    match sumOfFactors number with
    | i when number * 2 < i -> Deficient
    | i when number + 1 = i -> Prime
    | i when number * 2 > i -> Abundant
    | _ -> Perfect

let results =
    [1..1000]
    |> List.map classify
    |> ignore


// with caching (hint: use #time;; in the FSI to switch on real time, CPU time and GC stats)
let memoize (f:'a->'b) =
    let cache = new System.Collections.Generic.Dictionary<'a, 'b>()
    fun value ->
        if cache.ContainsKey(value) then cache.[value]
        else 
            let result = f value
            cache.Add(value, result)
            result

let memoizeSumOfFactors = memoize sumOfFactors

let classifyWithCaching number =
    match memoizeSumOfFactors number with
    | i when number * 2 < i -> Deficient
    | i when number + 1 = i -> Prime
    | i when number * 2 > i -> Abundant
    | _ -> Perfect

let results' =
    [1..15000]
    |> List.map classifyWithCaching
    |> ignore

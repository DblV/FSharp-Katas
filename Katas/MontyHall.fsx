type Prize = Goat | Car

let doors = [| Goat; Goat; Car |]

let r = new System.Random()

let stickStrategy doorNumber = 
    doorNumber

let switchStrategy doorNumber =
    match doorNumber with
    | 0 -> 2
    | 1 -> 2
    | _ -> 0

let getResultUsing strategy = 
    let pickedDoor = r.Next(0,3) |> strategy
    match doors.[pickedDoor] with
    | Car -> true
    | Goat -> false

let getProbabilityUsing strategy = 
    [for i in 1..10000 -> getResultUsing strategy]
    |> List.filter (fun x -> x = true)
    |> List.length
    |> (fun x -> float x / 10000.0)
    |> printfn "With this strategy, the probability of winning is %f"

getProbabilityUsing stickStrategy
getProbabilityUsing switchStrategy

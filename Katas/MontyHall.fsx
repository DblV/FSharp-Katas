type Prize = Goat | Car

let doors = [| Goat; Goat; Car |]

let r = new System.Random()

let stickStrategy door = 
    door

let switchStrategy door =
    match door with
    | 0 -> 2
    | 1 -> 2
    | _ -> 0

let applyStrategy strategy = 
    let pickedDoor = r.Next(0,3) |> strategy
    match doors.[pickedDoor] with
    | Car -> 1.0
    | Goat -> 0.0

let runGame strategy = 
    [1..10000]
    |> List.map (fun unit -> applyStrategy strategy)
    |> List.sum
    |> (fun x -> x / 10000.0)
    |> printf "With this strategy, the probability of winning is %f\n"

runGame stickStrategy
runGame switchStrategy

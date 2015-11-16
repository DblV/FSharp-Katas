// _ _ _ A _ _ _ 
// _ _ B _ B _ _
// _ C _ _ _ C _
// D _ _ _ _ _ D
// _ C _ _ _ C _
// _ _ B _ B _ _
// _ _ _ A _ _ _ 

// 1 - build grid
let diamond letter = 
    let letters = ['A'..letter]
    let rows = letters @ (letters |> List.rev |> List.tail)
    let columns = (letters |> List.tail |> List.rev) @ letters

    for r in rows do
        for c in columns do
            if r = c then
                printf "%c" r
            else
                printf " "
        printf "\n"

// 2 - recursion
let rec buildStr theList length pos1 pos2 result = 
    match theList with
    | [] -> result
    | head :: tail -> 
        let a = Array.create((length * 2) - 1) " "
        a.[pos1] <- head
        a.[pos2] <- head
        let returnThis = buildStr tail length (pos1 - 1) (pos2 + 1) (result @ [ Array.reduce (fun x y -> x + y) a ])
        returnThis

let diamond2 letter = 
    let letters = ['A'..letter]
    let stringsToPrint = buildStr (letters |> List.map (fun x -> x.ToString())) letters.Length (letters.Length - 1) (letters.Length - 1) []

    stringsToPrint |> List.map (printfn "%s") |> ignore
    stringsToPrint |> List.rev |> List.tail |> List.map (printfn "%s") |> ignore

// 3 - mapi?
// builds a new collection by applying a function to each of the elements in the collection.
// the integer index passed to the function indicates the index of the element being transformed
// e.g.
let mapping i j =
    printfn "%i" i
    j + 1

List.mapi mapping [1..10]

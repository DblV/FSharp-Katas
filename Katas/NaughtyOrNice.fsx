// Naughty Or Nice  

// 1. Given a childs name decide whether they should be naughty or nice (randomly)
type Child = {
    Name: string;
    IsNice: bool
    }

let isNice (name:string) = 
    match name.Length with
    | i when i % 2 = 0 -> false
    | _ -> true


// 2. Given a list of childrens names return a list of good and bad children
let names = ["+?Annie";"-?Bobby";"-?Charlie";"+?Debbie";"-Eddie";"-Freddie";"+Greta";"+Herb";"Iolanthe";"Jerry";"Kerry";"Lenny";"";"a";"ik"]

let printItems child = 
    let name = (if child.Name = "" then "Err..." else child.Name)
    let nice = (if child.IsNice then "Good" else "Bad")
    printfn "%s was %s this year" name nice

let determineTendency naughtyOrNice = 
    names
    |> List.map naughtyOrNice
    |> List.map printItems
    |> ignore

determineTendency (fun x -> { Name=x; IsNice=isNice x })


// 3. Prefix names with + or - to pre-determine which children should be naughty or nice
let getChars (s:string) = 
    [ for c in s -> c ]

let isNiceChild child = 
    match getChars child with
    | '+'::_ -> true
    | '-'::_ -> false
    | _ -> isNice child

determineTendency (fun x -> { Name=x; IsNice=isNiceChild x })


// 4. Prefix names with +? or -? to make it more likely that they be nice or naughty
let r = new System.Random()
let isLikelyNiceChild child = 
    match getChars child with
    | '+'::'?'::_ -> (r.NextDouble() + 0.25) > 0.5
    | '-'::'?'::_ -> (r.NextDouble() - 0.25) > 0.5
    | '+'::_ -> true
    | '-'::_ -> false
    | _ -> isNice child

determineTendency (fun x -> { Name=x; IsNice=isLikelyNiceChild x })

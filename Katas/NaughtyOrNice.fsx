// Naughty Or Nice  

// 1. Given a childs name decide whether they should be naughty or nice (randomly)
type SantasListItem = {
    Name: string;
    IsNice: bool
    }

let r = new System.Random()
let isNice unit = 
    match r.Next(2) with
    | 0 -> false
    | _ -> true


// 2. Given a list of childrens names return a list of good and bad children
let names = ["+?Annie";"-?Bobby";"-?Charlie";"+?Debbie";"-Eddie";"-Freddie";"+Greta";"+Herb";"Iolanthe";"Jerry";"Kerry";"Lenny";"";"a";"ik"]

let printSantasListItem santasListItem = 
    let name = (if santasListItem.Name = "" then "Err..." else santasListItem.Name)
    let nice = (if santasListItem.IsNice then "Good" else "Bad")
    printfn "%s was %s this year" name nice

let goodBadChildren niceFunction = 
    names
    |> List.map niceFunction
    |> List.map (fun x -> printSantasListItem x)

goodBadChildren (fun x -> { Name=x; IsNice=isNice() })


// 3. Prefix names with + or - to pre-determine which children should be naughty or nice
let isNiceChild (child:string) = 
    if child.Length > 0 then
        match child.[0] with
        | '+' -> true
        | '-' -> false
        | _ -> isNice()
    else
        isNice()

goodBadChildren (fun x -> { Name=x; IsNice=isNiceChild x })


// 4. Prefix names with +? or -? to make it more likely that they be nice or naughty
let isLikelyNiceChild (child:string) = 
    match child.Length with
    | 0 | 1 -> isNiceChild child
    | _ -> 
        match child.[0..1] with
        | "+?" -> (r.NextDouble() + 0.25) > 0.5
        | "-?" -> (r.NextDouble() - 0.25) > 0.5
        | _  -> isNiceChild child

goodBadChildren (fun x -> { Name=x; IsNice=isLikelyNiceChild x })

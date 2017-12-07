// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type InputField = {Name : string; Weight : int; Connections : string[]}
type Node = {Name : string; Weight : int; Children : Node[]}

let processLine (line : string) =
    let arrow = line.Split([|"->"|], System.StringSplitOptions.None)
    let name = arrow.[0].Split('(').[0].Trim()
    let weight = arrow.[0].Split('(').[1].Split(')').[0] |> int
    if Array.length arrow > 1 then
        {InputField.Name = name; 
        Weight = weight;
        Connections = arrow.[1].Split(',') |> Array.map (fun x -> x.Trim())}
    else
        {InputField.Name = name; 
        Weight = weight;
        Connections = [||]}

let solve1 input =
    let parents = List.filter (fun x -> x.Connections.Length > 0) input
    let parentsNames = parents |> List.map (fun x -> x.Name)
    let childrenNames = List.map (fun x -> x.Connections) parents |> List.collect Array.toList
    List.filter (fun x -> not (List.contains x childrenNames)) parentsNames 
        |> List.head


let rec findWeight cur = 
    if cur.Children.Length <= 0 then
        cur.Weight
    else
        let weights = Array.map findWeight cur.Children
        cur.Weight + (Array.sum weights)

let solve2 (inputs : List<InputField>) root =
    let map = List.map (fun (x : InputField) -> (x.Name, x)) inputs |> Map.ofList
    let rec make (name : string) =
        let input = map.[name]
        {Node.Name = name; Weight = input.Weight; Children = Array.map make input.Connections}
    let tree = make root
    let rec findFlaw cur =
        if cur.Children.Length <= 0 then
            (false, "", [||])
        else
            let results = Array.map findFlaw cur.Children
            match Array.tryFind (fun (found, _, _) -> found) results with
                | None  -> 
                    let weights = Array.map findWeight cur.Children
                    let off = Array.distinct weights
                    if off.Length > 1 then 
                        let childrenNames = Array.map (fun x -> x.Name) cur.Children
                        (true, cur.Name, Array.zip weights childrenNames)
                    else 
                        (false, "", [||])
                | Some (_, name, children) -> (true, name, children)
    let (_, wrongNode, children) = findFlaw tree
    (wrongNode, children)

[<EntryPoint>]
let main argv = 
    let input = [for x in System.IO.File.ReadLines("input") -> processLine x]
    let root = (solve1 input)
    printfn "%A" root
    printfn "%A" (solve2 input root)
    0 // return an integer exit code

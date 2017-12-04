// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let combinations array =
    let length = Array.length array
    let mutable answer = []
    for i in 0 .. length-1 do
        for j in i+1 .. length-1  do
            answer <- (array.[i], array.[j]) :: answer
    answer

let solve1 (input : List<string>) =
    [for x in input -> x.Split([|' '|])] 
    |> List.map combinations
    |> List.map (List.tryFind (fun (x,y) -> x = y))
    |> List.fold (fun acc x ->
        match x with
        | None -> acc + 1
        | Some _ -> acc) 0

let remove1 value list =
    let rec func current = 
        match current with 
        | head :: tail -> 
            if head = value then
                tail
            else
                head :: func tail
        | [] -> []
    func list

let isAnagram x y =
    let rec checkChars xList yList = 
        match (xList, yList) with 
        | (ch :: tail, _) -> 
            if List.contains ch yList then
                checkChars tail (remove1 ch yList)
            else
                false
        | ([], []) -> true
        | ([], _) | (_, []) -> false
    checkChars (List.ofSeq x) (List.ofSeq y)

let solve2 (input : List<string>) =
    [for x in input -> x.Split([|' '|])] 
    |> List.map combinations
    |> List.map (List.tryFind (fun (x,y) -> isAnagram x y))
    |> List.fold (fun acc x ->
        match x with
        | None -> acc + 1
        | Some _ -> acc) 0

[<EntryPoint>]
let main argv = 
    let input = [for x in System.IO.File.ReadLines("input") -> x]
    printfn "%A" (solve1 input)
    printfn "%A" (solve2 input)
    0 // return an integer exit code


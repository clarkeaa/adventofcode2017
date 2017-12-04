// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let input = [for x in System.IO.File.ReadLines("input") -> x.Split() |> Array.map int] |> List.toArray

let sample = [|
    [|5; 1; 9; 5|];
    [|7; 5; 3|];
    [|2; 4; 6; 8|];
|]

let sample2 = [|
        [|5; 9; 2; 8|];
        [|9; 4; 7; 3|];
        [|3; 8; 6; 5|];
    |]

let solve1 ss =
    Array.map Array.sort ss 
    |> Array.map (fun (x : int[]) -> (Array.last x) - x.[0]) 
    |> Array.sum   

let combinations array =
    let length = Array.length array
    let mutable answer = []
    for i in 0 .. length-1 do
        for j in i+1 .. length-1  do
            answer <- [array.[i]; array.[j]] :: answer
    answer

let solve2 (ss : int[][]) = 
    Array.map combinations ss
    |> Array.map (List.map List.sort) 
    |> Array.map (List.find (fun x -> (x.[1] % x.[0]) = 0)) 
    |> Array.map (fun x -> x.[1] / x.[0])
    |> Array.sum


[<EntryPoint>]
let main argv = 
    printfn "%A" (solve1 input)
    printfn "%A" (solve2 input)
    0 // return an integer exit code


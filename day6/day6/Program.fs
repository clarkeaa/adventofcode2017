// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let solve1 array =
    let rec func (seen :Set<int[]>) count =
        if seen.Contains(array) then
            count
        else
            let inputArray = Array.copy array 
            let max = Array.max array
            let idx = Array.findIndex (fun x -> x = max) array
            let value = array.[idx]
            array.[idx] <- 0
            for i in 1 .. value do
                let currentIdx = (idx + i) % array.Length
                array.[currentIdx] <- array.[currentIdx] + 1
            func (seen.Add(inputArray)) (count+1)
    func Set.empty 0

let solve2 array =
    let rec func (seen :Map<int[], int>) count =
        if seen.ContainsKey(array) then
            count - seen.[array]
        else
            let inputArray = Array.copy array 
            let max = Array.max array
            let idx = Array.findIndex (fun x -> x = max) array
            let value = array.[idx]
            array.[idx] <- 0
            for i in 1 .. value do
                let currentIdx = (idx + i) % array.Length
                array.[currentIdx] <- array.[currentIdx] + 1
            func (seen.Add(inputArray, count)) (count+1)
    func Map.empty 0

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllText("input").Split('\t') |> Array.map int
    printfn "%A" (solve1 input)
    printfn "%A" (solve2 input)
    0 // return an integer exit code


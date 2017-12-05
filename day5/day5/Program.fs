// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let solve1 (startCommands : int[]) =
    let commands = Array.copy startCommands
    let rec func pos count =
        let newPos = commands.[pos] + pos
        Array.set commands pos (1 + commands.[pos])
        if newPos < 0 || newPos >= Array.length commands then
            count
        else
            func newPos (1 + count)
    func 0 1

let solve2 (startCommands : int[]) =
    let commands = Array.copy startCommands
    let rec func (pos : int) (count : int64) =
        let newPos = commands.[pos] + pos
        if commands.[pos] >= 3 then
            Array.set commands pos (commands.[pos] - 1)
        else
            Array.set commands pos (1 + commands.[pos])
        if newPos < 0 || newPos >= Array.length commands then
            count
        else
            func newPos (1L + count)
    func 0 1L

[<EntryPoint>]
let main argv = 
    let commands = 
        [for x in System.IO.File.ReadLines("input") -> int x] |> List.toArray    
    printfn "%A" (solve1 commands)
    printfn "%A" (solve2 commands)
    0 // return an integer exit code


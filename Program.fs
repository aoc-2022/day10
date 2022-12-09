open System.IO

let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

input |> List.map (printfn "%A")
open System.IO

let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList


let rec execute (hist: int list) (reg: int) (input: string list) =
    match input with
    | [] -> hist
    | inst :: rest ->
        match inst.Split [| ' ' |] with
        | [| "noop" |] -> execute (reg :: hist) reg rest
        | [| "addx"; i |] ->
            let i = int i
            execute (reg :: reg :: hist) (reg + i) rest
        | _ ->
            printfn $"FAIL! {input}"
            []

let curr = input |> execute [ 1 ] 1 |> List.rev

// curr |> List.map (printfn "%A")

printfn "########### TASK 1 ########## "

let task1 =
    let first = (curr |> List.skip 20 |> List.head) * 20
    let hist = curr |> List.skip 60
    let rec sum (it:int) (hist:int list) =
        let fact = it*40+20
        let it = it + 1
        let curr = if hist.Length > 0 then hist.Head * fact else 0
        if hist.Length > 39 then curr + (hist |> List.skip 40 |> sum it) else curr
    (sum 1 hist) + first |> printfn "%A"          

printfn "###### TASK 2 ######"
let renderLine (hist:int list) =
    let row = hist |> List.take 40
    let rec draw (pos:int) (row:int list) =
        match row with
        | [] -> []
        | x::rest when x > (pos - 2) && x < (pos + 2) ->
            printfn $"x={x} pos={pos} -> #"
            "#" :: (rest |> draw (pos + 1))
        | x::rest ->
            printfn $"x={x} pos={pos} -> ."
            "."  :: (rest |> draw (pos + 1))
    draw 0 row |> String.concat ""
          
// printfn $"{(hist |> List.take 3)}"
let rec render (line: int list) =
    if  line.Length > 39 then
        (renderLine line) :: (line |> List.skip 40 |> render)
    else []

curr |> List.skip 1 |> render |> printfn "%A"

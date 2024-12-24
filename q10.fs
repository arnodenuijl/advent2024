module q10
open System
open utils 
open System.IO

let testinput = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""

let input  = 
    // testinput |> Array2D.createGrid
    File.ReadAllText "10.txt" |> Array2D.createGrid 
    |> Array2D.map (Char.ToString  >> int)

type Coord = int * int
let starts : Coord list = [
    for x in 0 .. Array2D.length1 input - 1 do
    for y in 0 .. Array2D.length2 input - 1 do
        if input[x,y] = 0 then yield (x,y)
]

let rec step (steps: Coord list) =
    let x,y =  List.last steps
    let currentVal = input[x,y]
    if currentVal = 9 then 
        [steps]
    else 
        let nextVal = currentVal + 1
        [
            x - 1, y
            x + 1, y
            x, y - 1
            x, y + 1
        ] 
        |> List.filter (Array2D.inBounds input)
        |> List.filter (fun (nextX,nextY) -> input[nextX,nextY] = nextVal)
        |> List.collect (fun nextC -> step (steps @ [nextC]))

let q10a () =
    let allPathsPerStart = 
        starts
        |> List.map (fun start -> start, (step [start]))
    
    let scorePerStart = 
        allPathsPerStart
        |> List.map (fun (_, paths) ->
                        paths
                        |> List.map List.last
                        |> Set.ofList
                        |> Set.count

        )
    let result = List.sum scorePerStart
    Console.WriteLine $"q10a: {result}"  
    ()

let q10b () = 
    let allPathsPerStart = 
        starts
        |> List.map (fun start -> start, (step [start]))
    
    let scorePerStart = 
        allPathsPerStart
        |> List.map (fun (_, paths) ->
                        paths
                        |> List.length

        )
    let result = List.sum scorePerStart
    Console.WriteLine $"q10b: {result}"
    ()
module q08
open System
open utils 
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

let testinput = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

// let testinput = """............
// ........0...
// .....0......
// .......0....
// ....0.......
// ......A.....
// ............
// ............
// ........A...
// .........A..
// ............
// ............"""

let input  = 
    // testinput |> Array2D.createGrid
    File.ReadAllText "08.txt" |>  Array2D.createGrid

type Coord = int * int

let allNodeGroups = 
    seq {
        for x in 0 .. (Array2D.length1 input - 1) do
        for y in 0 .. (Array2D.length2 input - 1) do
            yield ((x,y), input[x,y]) 
    }
    |> Seq.filter (fun (_, item) -> item <> '.')
    |> Seq.groupBy (fun (_, item) -> item)
    |> Seq.map (fun (item, coordsWithItem) -> 
                    let coords = 
                        coordsWithItem
                        |> Seq.map fst
                        |> List.ofSeq
                    (item, coords))
    |> List.ofSeq

let antinodes (x1,y1) (x2,y2) = 
    let diffX = x2 - x1
    let diffY = y2 - y1
    [
        (x1 - diffX, y1 - diffY)
        (x2 + diffX, y2 + diffY)
    ]

let allRepeatingAntiNodes (x1,y1) (x2,y2) (inBounds: Coord -> bool)= 
    let diffX = x2 - x1
    let diffY = y2 - y1 
    let oneWay = 
        Seq.unfold 
            (fun (x, y) -> 
                let next = (x - diffX, y - diffY)
                if inBounds next 
                then Some (next, next) 
                else None
            )
            (x1,y1)
    let otherWay = 
        Seq.unfold 
            (fun (x, y) -> 
                let next = (x + diffX, y + diffY)
                if inBounds next 
                then Some (next, next) 
                else None
            )
            (x2,y2)
    seq {
        yield(x1,y1)
        yield(x2,y2)
        yield! oneWay
        yield! otherWay
    } 



let repeatingAntiNodesForGroup (item: Char, coords: Coord list) =
    List.allPairs coords coords
    |> Seq.filter (fun (c1, c2) -> c1 <> c2) 
    |> Seq.collect (fun (c1,c2) -> allRepeatingAntiNodes c1 c2 (Array2D.inBounds input))
    |> Set.ofSeq |> List.ofSeq


let antiNodesForGroup (item: Char, coords: Coord list) =
    List.allPairs coords coords
    |> Seq.filter (fun (c1, c2) -> c1 <> c2) 
    |> Seq.collect (fun (c1,c2) -> antinodes c1 c2)
    |> Set.ofSeq |> List.ofSeq
    |> List.filter (fun (x,y) -> Array2D.inBounds input (x,y))

let antiNodesAsString (x1,y1) (x2,y2) =
    antinodes (x1,y1) (x2,y2)
    |> List.map (fun (x,y) -> $"({x},{y})")
    |> fun ss -> String.Join(", ", ss)


let q08a () =
    let result = 
        allNodeGroups
        |> List.collect antiNodesForGroup
        |> Set.ofList
        |> Set.count

    Console.WriteLine $"q08a: {result}"  
    ()


let q08b () = 
    let result = ()
    let inBounds = Array2D.inBounds input
    let aAnti = 
        allRepeatingAntiNodes (2,4) (3,3) inBounds
        |> Seq.toList
    Console.WriteLine (listAsString aAnti)

    let result = 
        allNodeGroups
        |> List.collect repeatingAntiNodesForGroup
        |> Set.ofList
        |> Set.count

    Console.WriteLine $"q08b: {result}"
    ()
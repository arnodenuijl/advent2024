module q06
open System
open utils 
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

let testinput = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

type Direction = North | East | South | West

let grid  = 
    // Array2D.createGrid testinput
    File.ReadAllText "06.txt" |> Array2D.createGrid

let initialObstacles, initialLocation =
    let mutable personLocation = None
    let items = 
        seq {
            for x in 0 .. Array2D.length1 grid - 1 do
            for y in 0 .. Array2D.length2 grid - 1 do
                let item = Array2D.get grid x y
                if item = '^' then
                    personLocation <- Some(x,y)
                else if item = '#' then 
                    yield (x,y)
        } |> Set.ofSeq
    items, personLocation.Value

let nextLocation ((x, y): int * int) (direction : Direction) =
    match direction with    
    | North -> (x - 1, y)
    | East -> (x, y + 1)
    | South -> (x + 1, y)
    | West -> (x, y - 1)

let turn (currentDirection : Direction) =
    match currentDirection with    
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let rec takeStep (someObstacles: Set<int*int>) ((x, y): int * int) (currentDirection : Direction) =
    let next = nextLocation (x,y) currentDirection
    if someObstacles.Contains(next) then 
        takeStep someObstacles (x,y) (turn currentDirection)
    else 
        next, currentDirection

let isInBounds ((x,y): (int * int)) =
    x >= 0 &&
    y >= 0 &&
    x <  Array2D.length1 grid &&
    y <  Array2D.length2 grid 

let allSteps (someObstacles: Set<int*int>) =
    Seq.unfold 
        (fun ((currentLocation: int * int), (currentDirection : Direction) ) -> 
            if not <| isInBounds currentLocation then
                None
            else
                let nextLocation, nextDirection = takeStep someObstacles currentLocation currentDirection
                Some((currentLocation, currentDirection), (nextLocation, nextDirection))
        )
        (initialLocation, North)

let q06a () =
    let result = 
        allSteps initialObstacles 
        |> Seq.map fst
        |> Set.ofSeq 
        |> Set.count 
    Console.WriteLine $"q06a: {result}"  
    ()


let hasLoop (steps: seq<(int*int) * Direction>) =
    let enumerator = steps.GetEnumerator()

    let rec inner (visited: Set<(int*int) * Direction>) = 
        if enumerator.MoveNext() then
            if Set.contains enumerator.Current visited then
                true
            else 
                inner (Set.add enumerator.Current visited)
        else 
            false
    inner Set.empty

let q06b () = 
    let visitedInNormalRun = 
        allSteps initialObstacles 
        |> Seq.map fst
        |> Set.ofSeq 
        
    let potentialSpotForExtraObstacle = 
        Set.union visitedInNormalRun initialObstacles
        |> Set.remove initialLocation

    let result = 
        potentialSpotForExtraObstacle
        |> PSeq.filter(fun extraObstacle ->
            hasLoop <| allSteps (Set.add extraObstacle initialObstacles))
        |> PSeq.length

    Console.WriteLine $"q06b: {result}"
    ()
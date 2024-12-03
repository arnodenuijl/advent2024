module q02
open System
open System.IO

let testinput = @"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

let getInput (text: string) = 
    text.Split Environment.NewLine 
    |> Array.map 
        (fun line -> 
            line.Split " " 
            |> Array.map int
        )

let input = 
    File.ReadAllText "02.txt" |> getInput
    // getInput testinput

let getVariants (nums: array<int>) = 
    nums
    |> Array.mapi (fun i _ -> Array.removeAt i nums)
    |> Array.append [|nums|]

let isSafe (nums: int[]) = 
    let windowedDiff = 
        nums
        |> Array.windowed 2
        |> Array.map (fun [|a;b|] -> a - b)

    let correctDirection = 
        Array.forall (fun x -> x > 0) windowedDiff ||
        Array.forall (fun x -> x < 0) windowedDiff
    let correctLevel =  Array.forall (fun (x:int) -> Math.Abs(x) >= 1 && Math.Abs(x) <= 3) windowedDiff
    correctDirection && correctLevel
   
let isSafeWithDamper (nums: int[]) = 
    nums
    |> getVariants
    |> Array.exists isSafe

let q02a () =
    let result = 
        input
        |> Array.filter isSafe
        |> Array.length
    Console.WriteLine $"q02a: {result}"

let q02b () = 
    let result = 
        input
        |> Array.filter isSafeWithDamper
        |> Array.length
    Console.WriteLine $"q02b: {result}"

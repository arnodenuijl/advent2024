module q01
open System
open System.IO

let testinput = @"3   4
4   3
2   5
1   3
3   9
3   3"

let getInput (text: string) = 
    text.Split Environment.NewLine 
    |> Array.map 
        (fun line -> 
            line.Split "   "
            |> fun parts -> (int parts[0], int parts[1])
        )

let input = 
    File.ReadAllText "01.txt" |> getInput
    // getInput testinput

let leftNumbers =  Array.map fst input
let rightNumbers = Array.map snd input
    
let q01a () =
    let result = 
        Seq.zip (Array.sort leftNumbers) (Array.sort rightNumbers)
        |> Seq.map (fun (a,b) -> Math.Abs(a-b))
        |> Seq.sum
    
    Console.WriteLine $"q01a: {result}"

let q01b () = 
    let groupedRight = 
        rightNumbers 
        |> Array.countBy id 
        |> Map.ofArray
        
    let result = 
        leftNumbers
        |> Array.map 
            (fun num -> 
                let count = 
                    Map.tryFind num groupedRight
                    |> Option.defaultValue 0
                count * num
            )
        |> Array.sum
    Console.WriteLine $"q01b: {result}"


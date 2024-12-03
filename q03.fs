module q03
open System
open System.IO
open System.Text.RegularExpressions

let testinput = @"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))9"

let getInput (text: string) = 
    text.Split Environment.NewLine 

let input = 
    File.ReadAllText "03.txt" |> getInput
    // getInput testinput


let countInLine s = 
    let matches = Regex.Matches(s, """(mul\(\d*,\d*\))""")
    matches
    |> Seq.map (fun x -> x.Captures[0].Value)
    |> Seq.map (fun x -> x.Replace("mul(", "").Replace(")", ""))
    |> Seq.map (fun x -> x.Split "," |> Array.map int)
    |> Seq.map (fun [|a;b|] -> a * b)
    |> Seq.sum
    
let q03a () =
    let result = 
        input
        |> Array.map countInLine
        |> Array.sum
    Console.WriteLine $"q03a: {result}"

let q03b () = 
    let result = 
        input
        |> Array.map countInLine
        |> Array.sum
    Console.WriteLine $"q03b: {result}"

module q03
open System
open System.IO
open System.Text.RegularExpressions

let testinput = @"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let getInput (text: string) = 
    text.Replace(Environment.NewLine , "")

let input = 
    File.ReadAllText "03.txt" |> getInput
    // getInput testinput

let getMuls s = 
    let matches = Regex.Matches(s, """(mul\(\d*,\d*\))""")
    matches
    |> Seq.map (fun x -> x.Captures[0].Index, x.Captures[0].Value)
    |> Seq.map (fun (idx, x) -> idx, x.Replace("mul(", "").Replace(")", ""))
    |> Seq.map (fun (idx, x) -> idx, x.Split "," |> Array.map int)
    |> Seq.map (fun (idx, [|a;b|]) -> idx, a * b)
    
let getDosAndDonts s = 
    let donts = 
        let matches = Regex.Matches(s, """(don't\(\))""")
        matches
        |> Seq.map (fun x -> x.Captures[0].Index, false)
    let dos = 
        let matches = Regex.Matches(s, """(do\(\))""")
        matches
        |> Seq.map (fun x -> x.Captures[0].Index, true)
    seq { 
        yield (-1,true)
        yield! donts
        yield! dos
    }  |> Array.ofSeq
       |> Array.sortByDescending fst

    
let q03a () =
    let result = 
        getMuls input
        |> Seq.map snd
        |> Seq.sum
        
    Console.WriteLine $"q03a: {result}"

let q03b () = 
    let dosAndDonts = getDosAndDonts input

    let result = 
        getMuls input
        |> Seq.filter (fun (idx, _) -> 
                dosAndDonts
                |> Array.find (fun (doIdx,_) -> doIdx < idx)
                |> snd
            )
        |> Seq.map snd
        |> Seq.sum
    Console.WriteLine $"q03b: {result}"

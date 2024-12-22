module q07
open System
open utils 
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

let testinput = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

module Parsing =
    open FParsec
    let lineParser = pint64 .>> pstring(": ") .>>. (sepBy pint64 (pchar ' '))
    let inputParser = sepBy lineParser newline

    let parseInput s = 
        run inputParser s
        |> unwrapParserResult

let input  = 
    // testinput |> Parsing.parseInput
    File.ReadAllText "07.txt" |>  Parsing.parseInput

type Op = int64 -> int64 -> int64

let rec hasAnswer (target: int64) (numbers: int64 list) (ops : Op list) =
    let rec inner (currentValue: int64) (rest: int64 list) =
        if List.isEmpty rest  
        then target = currentValue
        else
            let hasOvershot = currentValue > target
            if hasOvershot  
            then false
            else 
                ops
                |> List.map (fun op -> 
                    inner (op currentValue rest[0]) (List.skip 1 rest) )
                |> List.fold ((||)) false 
    inner numbers[0] (List.skip 1 numbers)

let q07a () =
    let ops = [(*);(+)]
    let result =  
        input
        |> Seq.filter (fun (target, numbers) -> hasAnswer target numbers ops)
        |> Seq.map fst
        |> Seq.fold (fun agg n -> agg + (bigint n)) (bigint 0)
    Console.WriteLine $"q07a: {result}"  
    ()


let q07b () = 
    let ops = [(*);(+);(fun a b -> int64 $"{a}{b}")]
    let result =  
        input
        |> Seq.filter (fun (target, numbers) -> hasAnswer target numbers ops)
        |> Seq.map fst
        |> Seq.fold (fun agg n -> agg + (bigint n)) (bigint 0)

    Console.WriteLine $"q07b: {result}"
    ()
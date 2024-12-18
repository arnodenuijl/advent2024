module q05
open System
open utils 
open System.IO
open System.Text.RegularExpressions

let testinput = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

type Update = int32 list
type Rule = int32 * int32

module Parsing =
    open FParsec
    let ruleParser = many (pint32 .>> pchar '|' .>>. pint32 .>> newline)
    let updateParser = sepBy (sepBy1 pint32 (pchar ',')) newline
    let inputParser = ruleParser .>> newline .>>. updateParser

    let parseInput s = 
        run inputParser s
        |> unwrapParserResult
        
let rules, updates  = 
    File.ReadAllText "05.txt" |> Parsing.parseInput
    // Parsing.parseInput testinput

let checkUpdateWithRule  (update: Update) ((a,b) : Rule)  =
    let indexA = List.tryFindIndex ((=) a) update
    let indexB = List.tryFindIndex ((=) b) update
    match indexA, indexB with 
    | Some iA, Some iB -> iB > iA
    | _ -> true 

let checkUpdateWithRules (update: Update) (rules: Rule list) =
    rules
    |> List.forall (checkUpdateWithRule update)

let q05a () =
    let result = 
        updates
        |> List.filter (fun update -> checkUpdateWithRules update rules )
        |> List.map (fun pages -> 
                let pageCount = List.length pages
                pages[((pageCount + 1) / 2) - 1]
            )
        |> List.sum
    Console.WriteLine $"q05a: {result}"  
    ()

let checkOrFixRule (update: Update) ((a,b): Rule) : Update  =
    let indexA = List.tryFindIndex ((=) a) update
    let indexB = List.tryFindIndex ((=) b) update
    match indexA, indexB with 
    | Some iA, Some iB -> 
        if iB > iA then update
        else 
            update 
            |> List.updateAt iA b
            |> List.updateAt iB a
    | _ -> update 

let rec checkOrFixUpdate (update: Update) (rules: Rule list) = 
    if  rules |> List.forall (checkUpdateWithRule update) then
        update
    else
        let updatedUpdate : Update = 
            rules
            |> List.fold
                (fun update rule -> checkOrFixRule update rule)
                update 
        checkOrFixUpdate updatedUpdate rules

let q05b () = 
    let incorrectUpdates =      
           updates
            |> List.filter (fun update -> not <| checkUpdateWithRules update rules )

    let result = 
        incorrectUpdates
        |> List.map (fun update -> checkOrFixUpdate update rules ) 
        |> List.map (fun pages -> 
                let pageCount = List.length pages
                pages[((pageCount + 1) / 2) - 1]
            )
        |> List.sum
    Console.WriteLine $"q05b: {result}"
    ()
﻿open q01;
open q02;
open q03;
open q04;
open q05;
open q06;
open q07;
open q08;
open q09;
open q10;
open System

let tasks = Map.ofList([
    ("q01a", q01a)
    ("q01b", q01b)
    ("q02a", q02a)
    ("q02b", q02b)
    ("q03a", q03a)
    ("q03b", q03b)
    ("q04a", q04a)
    ("q04b", q04b)
    ("q05a", q05a)
    ("q05b", q05b)
    ("q06a", q06a)
    ("q06b", q06b)
    ("q07a", q07a)
    ("q07b", q07b)
    ("q08a", q08a)
    ("q08b", q08b)
    ("q09a", q09a)
    ("q09b", q09b)
    ("q10a", q10a)
    ("q10b", q10b)
])

[<EntryPoint>]
let Main(args) = 
    if args.Length = 0 then
        tasks
        |> Map.toList
        |> List.sortBy fst
        |> List.iter (fun (_, func) -> func())
    else
        let toRun = args[0]
        match Map.tryFind toRun tasks with
        | Some f -> f()
        | None -> 
            Console.WriteLine $"Can't find task {toRun}"
            let possibleTasks = 
                Map.keys tasks 
                |> List.ofSeq
                |> List.sort
                |> fun names -> String.Join (", ", names)
            Console.WriteLine $"Possible values are {possibleTasks}"
    0
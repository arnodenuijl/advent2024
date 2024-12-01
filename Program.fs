﻿open q01;
open System

let tasks = Map.ofList([
    ("q01a", q01a)
    ("q01b", q01b)
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
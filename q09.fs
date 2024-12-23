module q09
open System
open utils 
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

let testinput = """2333133121414131402"""

let input  = 
    // testinput 
    File.ReadAllText "09.txt" 

let checksum (line: Option<int> array) = 
    line 
    |> Array.mapi 
        (fun i b -> 
            match b with 
            | Some b -> (bigint i) * (bigint b)
            | None -> bigint 0 )
    |> Array.fold ((+)) (bigint 0)

let printLine xs =
    xs |> Seq.iteri (fun i x ->  
        match x with 
        | Some x -> Console.WriteLine $"{i,4}: {x}"
        | None -> Console.WriteLine $"{i,4}: ."
    )

let q09a () =
    Console.WriteLine "expand ...."
    let expanded = 
        input.ToCharArray()
        |> Array.mapi 
            (fun i c -> 
                let count = Char.ToString c |> int32
                if Int32.IsEvenInteger i then 
                    Array.replicate count (Some (i / 2))
                else
                    Array.replicate count None

            )
        |> Array.collect id

    Console.WriteLine "find blocks and spaces ...."

    let spaces, blocks = 
        Array.fold 
            (fun (i, spaces, blocks) item -> 
                match item with 
                | Some _ -> (i + 1, spaces, [yield! blocks; i])
                | None -> (i + 1, [yield! spaces;i], blocks)
            )
            (0, [], [])
            expanded
        |> fun (_, spaces, blocks) -> spaces, blocks
    
    Console.WriteLine "Find changes ...."
    let altered = Array.copy expanded
    let changes = 
        let itemsToProcess = Math.Min(spaces.Length, blocks.Length) 
        List.zip 
            (List.take itemsToProcess spaces) 
            (List.take itemsToProcess (List.rev blocks)) 
    
    Console.WriteLine "Apply changes ...."
    changes 
    |> List.iter 
        (fun (spaceIndex, blockIndex) -> 
            if spaceIndex < blockIndex then
                altered[spaceIndex] <- altered[blockIndex]
                altered[blockIndex] <- None
        )
    
    Console.WriteLine "Calculate checksum ...."
    let result = checksum altered

    Console.WriteLine $"q09a: {result}"  
    ()


let q09b () = 
    let result = ()
    Console.WriteLine $"q09b: {result}"
    ()
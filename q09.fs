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

let checksum (line: Option<int> seq) = 
    line 
    |> Seq.mapi 
        (fun i b -> 
            match b with 
            | Some b -> (bigint i) * (bigint b)
            | None -> bigint 0 )
    |> Seq.fold ((+)) (bigint 0)

let printLine xs =
    xs |> Seq.iteri (fun i x ->  
        match x with 
        | Some x -> Console.WriteLine $"{i,4}: {x}"
        | None -> Console.WriteLine $"{i,4}: ."
    )

let q09a () =
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

    let blocks = 
        expanded
        |> Seq.mapi 
            (fun i item -> 
                if item.IsSome then 
                    Seq.singleton i 
                else Seq.empty
            )
        |> Seq.collect id
        |> List.ofSeq
    
    let spaces = 
        expanded
        |> Seq.mapi 
            (fun i item -> 
                if not item.IsSome then 
                    Seq.singleton i 
                else Seq.empty
            )
        |> Seq.collect id
        |> List.ofSeq
    
    let altered = Array.copy expanded
    let changes = 
        let itemsToProcess = Math.Min(spaces.Length, blocks.Length) 
        List.zip 
            (List.take itemsToProcess spaces) 
            (List.take itemsToProcess (List.rev blocks)) 
    
    changes 
    |> List.iter 
        (fun (spaceIndex, blockIndex) -> 
            if spaceIndex < blockIndex then
                altered[spaceIndex] <- altered[blockIndex]
                altered[blockIndex] <- None
        )
    
    let result = checksum altered

    Console.WriteLine $"q09a: {result}"  
    ()

type Size = int32
type FileId = int32
type Block = Size * FileId

type Spot = 
| Block of Block
| Spaces of Size

let checksumSpotList spotList =
    let reworked = 
        spotList
        |> List.fold (fun xs spot -> 
                match spot with 
                | Spaces s -> [ 
                        yield! xs
                        yield! List.replicate s None 
                    ]
                | Block (s,fileId) -> [
                                yield! xs
                                yield! List.replicate s (Some fileId)
                            ] 
           )
           []
    checksum reworked

let q09b () = 
    let numbers  = 
        input.ToCharArray()
        |> Array.map (Char.ToString >> int32)
        |> Array.toList
    
    let blocksAndSpaces = [
        let mutable start = 0
        let mutable i = 0
        for n in numbers do
            yield
                if Int32.IsEvenInteger i 
                then Block (n, (i / 2))
                else Spaces n
            i <- i + 1
            start <- start + n
    ]
    
    let insertBlockInSpace space block =
        match block, space with 
        | Block (bSize, fileId), Spaces sSize when bSize <= sSize ->
            [
                Spaces 0
                Block (bSize, fileId)
                Spaces (sSize - bSize)

            ]
        | _ -> failwith $"kan niet een {block} in een {space} doen"

    let processBlock (currentList: Spot list) ((blockSize, fileId) : Block) =
        let indexOfBlock = List.findIndex ((=) (Spot.Block (blockSize, fileId))) currentList
        
        let indexOfFreeSpot = 
            List.tryFindIndex
                (fun ss ->
                    match ss with 
                    | Spaces s when s >= blockSize  -> true
                    | _ -> false
                ) 
                currentList
        
        match indexOfFreeSpot with
        | Some i when i < indexOfBlock -> 
            let space = match currentList[i] with | Spaces s -> Spaces s ; | _ -> failwith "errrrrrrr."
            let (blockSize, fileId) = match currentList[indexOfBlock] with | Block b -> b ; | _ -> failwith "errrrrrrr."
            
            let updatedList = 
                currentList
                |> List.removeAt indexOfBlock
                |> List.insertAt indexOfBlock (Spaces blockSize)
                |> List.removeAt i 
                |> List.insertManyAt i (insertBlockInSpace space (currentList[indexOfBlock]))
            updatedList
        | _ -> 
            currentList

    let blocksReversed = 
        blocksAndSpaces
        |> List.collect (fun bs -> 
            match bs with 
            | Block b -> List.singleton b
            | _ -> List.empty)
        |> List.rev
    
    let finalList = 
        blocksReversed 
        |> List.fold (fun xs b -> 
            processBlock xs b)
            blocksAndSpaces

    let result = checksumSpotList finalList
    Console.WriteLine $"q09b: {result}"
    ()
module q04
open System
open utils 
open System.IO
open System.Text.RegularExpressions

let testinput = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

let getInput (text: string) = 
    text.Split Environment.NewLine
    |> fun lines -> 
        Array2D.init 
            lines.Length
            lines[0].Length
            (fun x y ->  lines[x][y])
let input = 
    File.ReadAllText "04.txt" |> getInput
    // getInput testinput

let leftToRight (m: char[,]) =
    m
    |> Array2D.rowsAsList
    |> List.map (List.toArray >> String)

let upToDown (m: char[,]) =
    m
    |> Array2D.turnCounterClockwise
    |> leftToRight

let rightToLeft (m: char[,]) =
    m
    |> Array2D.turnCounterClockwise
    |> Array2D.turnCounterClockwise
    |> leftToRight

let downToUp (m: char[,]) =
    m
    |> Array2D.turnCounterClockwise
    |> Array2D.turnCounterClockwise
    |> Array2D.turnCounterClockwise
    |> leftToRight


let diagonalTopRigthToLeftBottem (m: char[,]) =
    let maxX = Array2D.length1 m - 1
    let maxY = Array2D.length2 m - 1 
    let takeSlice (x: int) (y: int) = 
        seq {
            let mutable _x : int = x
            let mutable _y : int = y
            while _y >= 0 && _x <= maxX do
                yield Array2D.get m _x _y
                _x <- _x + 1
                _y <- _y - 1

        } |> (List.ofSeq >> List.toArray >> String)
    seq {
        for y in 0 .. maxY do yield takeSlice 0 y
        for x in 1 .. maxX do yield takeSlice x maxY
    } |> Seq.toList

let diagonalBottemRigthToTopLeft m = 
    m
    |> Array2D.turnCounterClockwise 
    |> diagonalTopRigthToLeftBottem

let diagonalBottemLeftToTopRight m = 
    m
    |> Array2D.turnCounterClockwise 
    |> Array2D.turnCounterClockwise 
    |> diagonalTopRigthToLeftBottem
let diagonalTopLeftToBotemRight m = 
    m
    |> Array2D.turnCounterClockwise 
    |> Array2D.turnCounterClockwise 
    |> Array2D.turnCounterClockwise 
    |> diagonalTopRigthToLeftBottem

let q04a () =
    let result = 
        [
            leftToRight
            rightToLeft
            upToDown
            downToUp
            diagonalTopRigthToLeftBottem
            diagonalTopLeftToBotemRight
            diagonalBottemLeftToTopRight
            diagonalBottemRigthToTopLeft
        ] 
        |> List.collect (fun f -> f input)
        |> List.map (fun s -> Regex.Matches(s, "XMAS").Count)
        |> List.sum
    Console.WriteLine $"q04a: {result}"
    ()
let q04b () = 
    let isXMas (x,y) =
        let axis1 = set [input[x-1,y-1];input[x+1,y+1]]
        let axis2 = set [input[x+1,y-1];input[x-1,y+1]]
        axis1 = set ['M';'S'] && axis2 = set ['M';'S'] 

    let result = 
        Array2D.mapi (fun x y item -> 
            if item = 'A' then Some (x,y)
            else None) input
        |> Array2D.rowsAsList
        |> List.collect id
        |> List.collect Option.toList
        |> List.filter (
            fun (x,y) -> 
                x <> 0 && 
                y <> 0 && 
                x < (Array2D.length1 input - 1) && 
                y < (Array2D.length2 input - 1)) 
        |> List.filter isXMas
        |> List.length
    Console.WriteLine $"q04b: {result}"

    ()
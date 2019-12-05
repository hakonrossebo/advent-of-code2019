<Query Kind="FSharpProgram" />

open System.IO
let dump x =
    x.Dump()
let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let path = Path.Combine([|source_path; "coords.csv"|])
let lines = File.ReadAllLines(path)
            |> Array.skip 1
            |> Array.map (fun x -> x.Split(",")) 
            |> Array.map (fun (xy:string[]) -> (int xy.[0], int xy.[1]))
            |> Array.toList
            
//lines.Dump()
let startPos = (0, 0)            

let testData = [(1, 3); (1, 0); (3, 2)]
//let testData = [(1, 3); (1, 0); (3, 2); (3, 0); (3,2)]
//let testData = [(1, 0); (1, -2); (-1, -2); (-3, -3);  (-1, -1)]
//testData.Dump()

let getPath (initX, initY) (endX, endY) : (int * int) list =
    let posNegX = if endX >= initX then 1 else -1
    let posNegY = if endY >= initY then 1 else -1
    let xMove = [initX + posNegX .. posNegX .. endX]
                 |> List.map (fun x -> (x, initY))
                 
    let yMove = [initY + posNegY .. posNegY .. endY]
                 |> List.map (fun y -> (endX, y))
    let combined = List.append xMove yMove 
    combined
    
    
let accCoords (accList:(int * int) list) nextCoord =
    let newCoords = getPath (List.last accList) nextCoord
    let combined = List.append accList newCoords
    combined
    

let allDataPaths data =
   data 
    |> List.fold accCoords [startPos]
    //|> List.skip 1
    
//let allTestDataPaths = allDataPaths testData
let allRealDataPaths = allDataPaths lines 

//allTestDataPaths.Dump("AllP")
//getPath startPos (1, 3)

let calculateExtra n =
    let extra = n
    let range = [1..extra]
                |> List.sum
    range
    

let allGroupedAndCounted lst =
    lst
    |> List.countBy id
    |> List.map (fun (_, c) -> c)
    |> List.map calculateExtra
    |> List.sum
    |> (fun x -> x - 1)

//(allGroupedAndCounted allTestDataPaths).Dump()
(allGroupedAndCounted allRealDataPaths).Dump()


    
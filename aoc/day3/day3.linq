<Query Kind="FSharpProgram" />

//let startY = 0
//let d = 2
//
//let test = [|startY .. -1 .. startY-d|]

//let start = -5

//let endd = -9
//let test = [|start .. -1 ..endd|]
//test.Dump()
open System.IO
let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let path = Path.Combine([|source_path; "input"|])
let lines = File.ReadAllLines(path)
//dump lines
let dump x =
    x.Dump()
    
type Direction =
        | Up of int
        | Down of int
        | Right of int
        | Left of int
        | NoDirection

let testData1 = [|"R75,D30,R83,U83,L12,D49,R71,U7,L72";
                  "U62,R66,U55,R34,D71,R55,D58,R83"|]
let testData2 = [|"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R5";
                  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"|]
let testData3 = [|"L2,U2,R4,D2,L2";
                  "U62,R66,U55,R34,D71,R55,D58,R83"|]
let testDistance1 =  159
let testDistance2 =  135
//dump testData1
//dump testData2

let createDirection (direction: string) =
    let distance = direction.Substring(1)
                    |> int
    let dir = direction.[0]
            |> function
                | 'U' -> Up distance
                | 'D' -> Down distance
                | 'R' -> Right distance
                | 'L' -> Left distance
                | _ -> NoDirection
    dir 
    
//createDirection "U44" |> dump

//line1, to directions, perform walk, to sequence of x, y points
//let walk =
//    function
//    | Up d -> [0..d] |> List.map (fun _ -> (0, 1))
//    | Down d -> [0..d] |> List.map (fun _ -> (0, -1))
//    | Right d -> [0..d] |> List.map (fun _ -> (1, 0))
//    | Left d -> [0..d] |> List.map (fun _ -> (-1, 0))
//    | NoDirection -> [(0,0)] 

let walk (startX, startY) =
    function
    | Up d -> [|startY+1..d+startY|] |> Array.map (fun y -> (startX, y))
    | Down d -> [|startY-1 .. -1 .. startY-d|] |> Array.map (fun y -> (startX, y))
    | Right d -> [|startX+1..d+startX|] |> Array.map (fun x -> (x, startY))
    | Left d -> [|startX-1 .. -1 .. startX-d|] |> Array.map (fun x -> (x, startY))
    | NoDirection -> [|(startX, startY)|] 
    
let accDirections (accList:(int*int) array) direction =
    let lastPos = Array.last accList
    let walkedPath = walk lastPos direction
    [accList; walkedPath]
    |> List.toSeq
    |> Array.concat
    
//let testData1SetTest =
//    testData3.[0].Split(",")
//        |> Array.map createDirection
//        |> Array.fold accDirections [|(0,0)|]
//        |> dump

//let testData1Set =
//    testData1.[0].Split(",")
//        |> Array.map createDirection
//        |> Array.fold accDirections [|(0,0)|]
//        |> Set.ofArray
//let testData2Set =
//    testData1.[1].Split(",")
//        |> Array.map createDirection
//        |> Array.fold accDirections [|(0,0)|]
//        |> Set.ofArray
//
////testData1Set.Dump()
////testData2Set.Dump()
//let setUnion = Set.intersect testData1Set testData2Set
//                |> dump
//
//
let realData1Set =
    lines.[0].Split(",")
        |> Array.map createDirection
        |> Array.fold accDirections [|(0,0)|]
        |> Set.ofArray
let realData2Set =
    lines.[1].Split(",")
        |> Array.map createDirection
        |> Array.fold accDirections [|(0,0)|]
        |> Set.ofArray

let setUnionReal = Set.intersect realData1Set realData2Set
                    |> Seq.map (fun (x, y) -> (abs x) + (abs y))
                    |> Seq.sort
                    |> Seq.skip 1
                    |> Seq.take 1
                    
                    |> dump

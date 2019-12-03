<Query Kind="FSharpProgram" />

open System.IO
let dump x =
    x.Dump()
    
let rangeTest = [500.0 .. 1300.0]

let validSizes =
    rangeTest
    |> List.map (fun x -> 720720. / x)
    |> List.filter (fun x -> x = (Math.Truncate(x)))
    |> List.map int

let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let path = Path.Combine([|source_path; "img.txt"|])
let pathW = Path.Combine([|source_path; "Fixed_img.txt"|])
let imageData = File.ReadAllText(path).ToArray()

let chunkImage imgData size = 
   imgData 
    |> Array.chunkBySize size 
    |> Array.map (fun x -> new string(x))
    
    
validSizes
    |> List.map (fun s -> (s, chunkImage imageData s))
    |> List.iter (fun (s, i) -> File.WriteAllLines(Path.Combine([|source_path; sprintf "Fixed_img_%d_.txt" s|]), i))
    
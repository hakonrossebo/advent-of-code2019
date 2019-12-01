<Query Kind="FSharpProgram" />

open System.IO
let source_path = @"C:\source\aoc2019"
let path = Path.Combine([|source_path; "aoc_1_1.txt"|])
let lines = File.ReadAllLines(path)

let testData = [|12;14;1969;100756|]

let roundDown (n:double) =
    System.Convert.ToInt32(Math.Floor(n))

let calculateFuel m = 
    m
    |> float 
    |> (fun x -> x / 3.0)
    |> roundDown
    |> (fun x -> x - 2)
    
let rec calculateFuelWithFuel mass =
    let c = calculateFuel mass
    if c > 0 then
        mass + calculateFuelWithFuel c
    else
        mass

let testResults1 = 
    testData
    |> Array.map calculateFuel
    |> Array.map calculateFuelWithFuel
    
let testResults2 = 
    testData
    |> Array.map calculateFuel
    |> Array.map calculateFuelWithFuel
    
let realResults1 = 
    lines
    |> Array.map int
    |> Array.map calculateFuel
    |> Array.sum
    
let realResults2 = 
    lines
    |> Array.map int
    |> Array.map calculateFuel
    |> Array.map calculateFuelWithFuel
    |> Array.sum
    
testResults1.Dump()
testResults2.Dump()
realResults1.Dump()
realResults2.Dump()
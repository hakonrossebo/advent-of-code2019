<Query Kind="FSharpProgram" />

open System.IO

let testData = [|50; 52; 52; 49; 50; 47; 45; 43; 50; 56|]
let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let path = Path.Combine([|source_path; "sau.txt"|])
let realData = File.ReadAllText(path).Split(',')
                |> Array.map (fun (x:string) -> x.Trim())
                |> Array.map int

let eat (day, demand, days, extra) available = 
    let day = day + 1
    let remaining = 
        available - demand + extra
    if remaining >= 0 then
        (day, demand + 1, 5, remaining)
    else
        (day, demand - 1, days - 1, 0)

// Test data
let testDataResult =
    testData
    |> Array.scan eat (0, 50, 5, 0)

testDataResult.Dump()

let testResultDay =
    testDataResult
    |> Array.find (fun (_, _, days, _) -> days <= 0)
    
testResultDay.Dump()


// Real data
let results =
   realData
   |> Array.scan eat (0, 50, 5, 0)
  
let day =
    results 
    |> Array.find (fun (_, _, days, _) -> days <= 0)
    |> fun (d, _, _, _) -> d - 1
    
realData.Count().Dump("Total days")
    
day.Dump("Last day alive")
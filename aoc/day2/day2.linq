<Query Kind="FSharpProgram" />


open System.IO

let dump x =
    x.Dump()

let testInput1 = [|1;0;0;0;99|]
let testInput2 = [|2;3;0;3;99|]
let testInput3 = [|2;4;4;5;99;0|]
let testInput4 = [|1;1;1;4;99;5;6;0;99|]

let testResult1 = [|2;0;0;0;99|]
let testResult2 = [|2;3;0;6;99|]
let testResult3 = [|2;4;4;5;99;9801|]
let testResult4 = [|30;1;1;4;2;5;6;0;99|]

let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let path = Path.Combine([|source_path; "input.txt"|])
let realData = File.ReadAllText(path).Split(',')
                |> Array.map (fun (x:string) -> x.Trim())
                |> Array.map int
                
                
//realData.Dump()

let processOpcode index (program: byref<int array>) =
    let opCode = program.[index]
    if opCode = 99 then
        false
    else
        let pos1 = program.[index + 1]
        let pos2 = program.[index + 2]
        let value1 = program.[pos1]
        let value2 = program.[pos2]
        let result =
            match opCode with
            | 1 ->
                value1 + value2    
            | 2 ->
                value1 * value2    
            | _ -> 0
        let resultPos = program.[index + 3]
        program.[resultPos] <- result
        true

let run (program: byref<int array>) =
    let mutable index = 0
    let mutable keepRunning = true
    while keepRunning do
        keepRunning <- processOpcode index &program
        index <- index + 4
        
    program
    

let mutable p = testInput1
run &p
    |> Array.zip testResult1
    |> dump
p <- testInput2
run &p
    |> Array.zip testResult2
    |> dump
p <- testInput3
run &p
    |> Array.zip testResult3
    |> dump
p <- testInput4
run &p
    |> Array.zip testResult4
    |> dump

let mutable realData' = realData
realData'.[1] <- 12
realData'.[2] <- 2


let result = run &realData'
            |> Array.take 1
            
result.Dump("Result of part 1")









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

let processInstruction instructionPointer (program: byref<int array>) =
    let opCode = program.[instructionPointer]
    if opCode = 99 then
        false
    else
        let pos1 = program.[instructionPointer + 1]
        let pos2 = program.[instructionPointer + 2]
        let value1 = program.[pos1]
        let value2 = program.[pos2]
        let result =
            match opCode with
            | 1 ->
                value1 + value2    
            | 2 ->
                value1 * value2    
            | _ -> 0
        let resultPos = program.[instructionPointer + 3]
        program.[resultPos] <- result
        true

let run (program: int array) =
    let mutable memory = Array.copy program
    let mutable instructionPointer = 0
    let mutable keepRunning = true
    while keepRunning do
        keepRunning <- processInstruction instructionPointer &memory
        instructionPointer <- instructionPointer + 4
        
    memory
    

run testInput1 
    |> Array.zip testResult1
    |> dump
run testInput2
    |> Array.zip testResult2
    |> dump
run testInput3
    |> Array.zip testResult3
    |> dump
run testInput4
    |> Array.zip testResult4
    |> dump

let realData' = Array.copy realData
realData'.[1] <- 12
realData'.[2] <- 2


let result = run realData'
            |> Array.take 1
            
result.Dump("Result of part 1")

let replaceAndRun program (noun, verb) =
    let mutable programCopy = Array.copy program
    programCopy.[1] <- noun 
    programCopy.[2] <- verb 
    let result = run programCopy
    result.[0]

let findSolution =
    let correctOutput = 19690720
    let nounVerbCombinations = 
        seq {
            for noun in 0..99 do
                for verb in 0..99 ->
                    (noun, verb)
        }
    let (noun, verb) =
        nounVerbCombinations
        |> Seq.find (fun x -> replaceAndRun realData x = correctOutput)
    (noun, verb, 100 * noun + verb)
    
findSolution.Dump("Result of task 2")

let test = replaceAndRun realData (98, 20)

test.Dump()






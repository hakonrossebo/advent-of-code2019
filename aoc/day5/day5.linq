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

let newTestData = [|1002;4;3;4;33|]
let newTestDataResult = [|1002;4;3;4;99|]

let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let path = Path.Combine([|source_path; "input"|])
let realData = File.ReadAllText(path).Split(',')
                |> Array.map (fun (x:string) -> x.Trim())
                |> Array.map int
               
realData.Dump()

type OpCode = Add of int | Multiply of int | Input of int | Output of int | Halt | Unknown
type ParameterMode = Position | Immediate
type Instruction = {
                    opCode: OpCode
                    parameterMode1: ParameterMode
                    parameterMode2: ParameterMode
                    parameterMode3: ParameterMode
                }
                

let digitsFromInt c =
    string c
    |> fun x -> x.PadLeft(5, '0')
    |> fun x -> x.ToCharArray()
    |> Seq.map (string >> int)
    |> Seq.rev
    |> Seq.toArray
    
//(digitsFromInt 1).Dump()
//(digitsFromInt 1001).Dump()
//(digitsFromInt 4321).Dump()
//(digitsFromInt 11111).Dump()
                
let opCodeFromDigits digits = 
                digits
                |> Array.take 2
                |> Array.rev
                |> (fun x -> (x.[0] * 10) + x.[1])
                |> function
                    | 1 -> Add 4
                    | 2 -> Multiply 4 
                    | 3 -> Input 2
                    | 4 -> Output 2
                    | 99 -> Halt
                    | _ -> Unknown
//(opCode [|1;0|]).Dump()
//(opCode [|2;0|]).Dump()
//(opCode [|2;2|]).Dump()

let modeFromInt =
            function 
            | 0 -> Position
            | 1 -> Immediate
            | _ -> Position

let getInstructionFromInt (c:int) : Instruction =
    let digits = digitsFromInt c
    digits.Dump("Digits")
    let opCode = opCodeFromDigits digits 
    let p1 = digits.[2] |> modeFromInt
    let p2 = digits.[3] |> modeFromInt
    let p3 = digits.[4] |> modeFromInt
    {
        opCode = opCode
        parameterMode1 = p1 
        parameterMode2 = p2 
        parameterMode3 = p3 
    }
//(getInstructionFromInt 1).Dump("1")
//(getInstructionFromInt 1001).Dump("1101")
//(getInstructionFromInt 1102).Dump("1102")
//(getInstructionFromInt 1103).Dump("1103")
//(getInstructionFromInt 1104).Dump("1104")
//(getInstructionFromInt 1134).Dump("1134")
//(getInstructionFromInt 1).Dump("1")


let getParameterValueBasedOnMode (program: byref<int array>) i mode position =
    let posValue = program.[i + position]
    match mode with 
    | Position -> 
        program.[posValue]
    | Immediate -> 
        posValue
let writeResult (program: byref<int array>) i pos result =
    let resultPos = program.[i + pos]
    program.[resultPos] <- result
    ()


let processInstruction i (program: byref<int array>) (input:int) =
    program.[i].Dump("Step")
    let instruction = 
        program.[i]
        |> getInstructionFromInt
        
    
    match instruction.opCode with
    | Halt ->
        (false, 0, 0)
    | Add length -> 
        let value1 = getParameterValueBasedOnMode &program i instruction.parameterMode1 1
        let value2 = getParameterValueBasedOnMode &program i instruction.parameterMode2 2
        let result = value1 + value2
        writeResult &program i 3 result
        (true, 0, length)
    | Multiply length ->
        let value1 = getParameterValueBasedOnMode &program i instruction.parameterMode1 1
        let value2 = getParameterValueBasedOnMode &program i instruction.parameterMode2 2
        let result = value1 * value2
        writeResult &program i 3 result
        (true, 0, length)
    | Input length ->
        writeResult &program i 1 input 
        (true, 0, length)
    | Output length ->
        let posValue = program.[i + 1]
        let output =  program.[posValue]
        (true, output, length)
    | Unknown ->
        (false, 0, 0)
    
    
let run (program: int array) (input:int) =
    let mutable memory = Array.copy program
    let mutable instructionPointer = 0
    let mutable keepRunning = true
    while keepRunning do
        let result = processInstruction instructionPointer &memory input
        let (keepRunningTmp, output , nextInstructionPointerAdd) = result
        keepRunning <-keepRunningTmp 
        output.Dump()
        instructionPointer <- instructionPointer + nextInstructionPointerAdd
        
    Dump("Finished") |> ignore
    memory
    
    
run realData 1 |> ignore

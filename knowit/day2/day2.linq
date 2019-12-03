<Query Kind="FSharpProgram" />

open System.IO
let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let path = Path.Combine([|source_path; "world.txt"|])
let lines = File.ReadAllLines(path)
let dump x =
    x.Dump()
//lines.Dump()

let testData1 = "#### ####"
let testData2 = "     #   "
let testData3 = " #  #    "
let testData4 = "#  #   # "
let testData5 = "########"
let testData6 = "        "

let countFloodedLineCells (line:string) : int =
     line.Trim()
     |> String.filter (fun x -> x = ' ')
     |> String.length
    
testData1 |> countFloodedLineCells |> dump
testData2 |> countFloodedLineCells |> dump
testData3 |> countFloodedLineCells |> dump
testData4 |> countFloodedLineCells |> dump
testData5 |> countFloodedLineCells |> dump
testData6 |> countFloodedLineCells |> dump


let total =
    lines
    |> Array.map countFloodedLineCells
    |> Array.sum
    |> dump

        
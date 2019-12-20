// Learn more about F# at http://fsharp.org



open System
open FSharp.Collections.ParallelSeq
open Microsoft.FSharp.Core.Operators.Checked
open System.Diagnostics

type TimedOperation<'T> = {millisecondsTaken:int64; returnedValue:'T}

let timeOperation<'T> (func: unit -> 'T): TimedOperation<'T> =
    let timer = Stopwatch()
    timer.Start()
    let returnValue = func()
    timer.Stop()
    {millisecondsTaken=timer.ElapsedMilliseconds; returnedValue=returnValue}

let timeOperationPass<'T> (func: unit -> 'T): 'T =
    let timer = Stopwatch()
    timer.Start()
    let returnValue = func()
    timer.Stop()
    printfn "Time elapsed: %d" timer.ElapsedMilliseconds 
    returnValue

let inline reverser n =
  let mutable n = n
  let mutable reversert = 0UL
  while n <> 0UL do
    reversert <- (reversert * 10UL) + (n % 10UL)
    n <- n / 10UL
  reversert

let inline erPalindrom n = 
  n = reverser n

let erSkjultPalindrom n =
    if erPalindrom n then
        None
    else
        if erPalindrom (n + reverser n) then
          Some n
        else
          None

let erSkjultPalindromP n =
    let n = uint64 n
    not (erPalindrom n) && erPalindrom (n + reverser n)

let summerPalindromer ()=
    [|1UL..123454319UL|]
    |> Array.Parallel.choose erSkjultPalindrom
    |> Array.sum
    |> printfn "Summen er: %d"
    0
    
let summerPalindromerP ()=
    {1L..123454319L}
    |> PSeq.filter erSkjultPalindromP
    |> PSeq.sum
    |> printfn "Summen er: %d"
    0

[<EntryPoint>]
let main argv =
    printfn "Palindromer..."
    // summerPalindromer () |> ignore
    // summerPalindromer |> timeOperationPass |> ignore
    summerPalindromerP |> timeOperationPass |> ignore
    0 // return an integer exit code

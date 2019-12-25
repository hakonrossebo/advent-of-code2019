open System
open FSharp.Collections.ParallelSeq
open System.Diagnostics

let timeOperation func  =
    let timer = Stopwatch()
    timer.Start()
    let returnValue = func()
    timer.Stop()
    printfn "Time elapsed: %d" timer.ElapsedMilliseconds 
    returnValue


let sluttAntall = 98765432 
let primtall = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73] 
            |> Set.ofList

let inline sumAvSiffer n =
    let mutable sum = 0
    let mutable i = n
    while i > 0 do
        sum <- sum + i % 10
        i <- i / 10
    sum
    
let inline erPrimtall n = Set.contains n primtall

let inline erHarshadPrimtall n = 
    let sifferSum = sumAvSiffer n
    n % sifferSum = 0 && erPrimtall sifferSum

let svar () =
    {1..sluttAntall}
    |> PSeq.filter erHarshadPrimtall
    |> PSeq.length
    
[<EntryPoint>]
let main argv =
    printfn "Oppgave..."
    svar |> timeOperation |> ignore
    0

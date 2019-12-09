open System
open System.IO
open System.Diagnostics
open FSharp.Collections.ParallelSeq

let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "../krampus.txt" |])

let finnKrampusTall (liste: string seq) =
    seq {
        for input in liste do
            let inputNummer = int64 input
            let kvadratTall = string (inputNummer * inputNummer)
            for i in 1..kvadratTall.Length-1 do
                let part1 = int64 kvadratTall.[..i - 1]
                let part2 = int64 kvadratTall.[i..]
                if part2 <> 0L && part1 + part2 = inputNummer then
                    yield inputNummer
    }

let finnKrampusTallParallel (input: string) =
    seq {
            let inputNummer = int64 input
            let kvadratTall = string (inputNummer * inputNummer)
            for i in 1..kvadratTall.Length-1 do
                let part1 = int64 kvadratTall.[..i - 1]
                let part2 = int64 kvadratTall.[i..]
                if part2 <> 0L && part1 + part2 = inputNummer then
                    yield inputNummer
    }


//fasit = 445372L


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let timer = Stopwatch()
    timer.Start()
    let sum =
        File.ReadAllLines(filSti)
        |> finnKrampusTall
        |> Seq.sum
    timer.Stop()        
    printfn "Summen er %d på %dms" sum timer.ElapsedMilliseconds
    timer.Restart()
    let sumP =
        File.ReadAllLines(filSti)
        |> PSeq.collect finnKrampusTallParallel
        |> Seq.sum
    timer.Stop()        
    printfn "Summen er %d på %dms" sumP timer.ElapsedMilliseconds

    0 // return an integer exit code

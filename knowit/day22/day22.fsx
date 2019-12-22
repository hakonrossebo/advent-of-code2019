open System.IO

let skogFilSti = Path.Combine([| __SOURCE_DIRECTORY__; "forest.txt" |])

let lesFil filSti = File.ReadAllLines(filSti) 

let finnInntektsBudsjett skogFil =
    let skog = lesFil skogFil |> Array.rev
    let finnHoydeFraRot i =
        skog
        |> Array.takeWhile (fun rad -> rad.[i] ='#' )
        |> Array.length
        |> (fun x -> (float x * 20.) / 100.)
    let stammeLengder = Array.head skog
                        |> Seq.mapi (fun i c -> (i, c))
                        |> Seq.filter (fun (i, c) -> c = '#')
                        |> Seq.map (fun (i, _) -> i |> finnHoydeFraRot)
    stammeLengder
    |> Seq.sum
    |> (fun x -> x * 200.)

finnInntektsBudsjett skogFilSti
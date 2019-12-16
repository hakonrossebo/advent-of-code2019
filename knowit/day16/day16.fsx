open System.IO
open System

let filStiProd = Path.Combine([| __SOURCE_DIRECTORY__; "fjord.txt" |])
let filStiTest = Path.Combine([| __SOURCE_DIRECTORY__; "test.txt" |])

let lesFil filSti = File.ReadAllLines(filSti) //    |> Array.rev

type Tilstand =
    { X: int
      Y: int
      YVelocity: int }


let tellAntallKryss filSti =
    let fjord = lesFil filSti //  |> Array.rev


    let (startX, startY) =
        let c = "B"
        fjord
        |> Array.tryFindIndex (fun (linje: string) -> linje.IndexOf(c) > 0)
        |> Option.map (fun y -> (fjord.[y].IndexOf(c), y))
        |> Option.defaultValue (0, 0)

    printfn "StartY: %d" startY

    let fjordSlutt = fjord.[0].Length

    let startTilstand =
        { X = startX
          Y = startY
          YVelocity = -1 }

    let skalKrysse xPos yPos yVel =
        let sjekkY = yPos + (yVel * 3)
        // printfn "SjekkY: %d" sjekkY
        // printfn "SjekkX: %d" xPos
        let sjekkStreng: string = fjord.[sjekkY]
        // printfn "Char: %c" sjekkStreng.[xPos]
        // printfn "Char er lik: %b" (sjekkStreng.[xPos] = '#')
        sjekkStreng.[xPos] = '#'

    let rute =
        startTilstand
        |> Seq.unfold (fun b ->
            // printfn "Input: %A" b
            if b.X >= fjordSlutt then
                None
            else if not (skalKrysse b.X b.Y b.YVelocity) then
                let nyB =
                    { b with
                          X = b.X + 1
                          Y = b.Y + b.YVelocity }
                // printfn "Ikke kryss: %A" nyB
                Some((0, nyB.X, nyB.Y), nyB)
            else
                let nyB =
                    { b with
                          X = b.X + 1
                          YVelocity = b.YVelocity * -1 }
                // printfn "Krysser: %A" nyB
                Some((1, nyB.X, nyB.Y), nyB))

    rute


// tellAntallKryss filStiTest |> printfn "%A"
tellAntallKryss filStiTest |> Seq.toArray



tellAntallKryss filStiTest
|> Seq.filter (fun (k, _, _) -> k = 1)
|> Seq.length

tellAntallKryss filStiProd
|> Seq.filter (fun (k, _, _) -> k = 1)
|> Seq.length
h
h
h
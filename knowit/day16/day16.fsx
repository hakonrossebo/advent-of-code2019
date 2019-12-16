open System.IO

let filStiProd = Path.Combine([| __SOURCE_DIRECTORY__; "fjord.txt" |])
let filStiTest = Path.Combine([| __SOURCE_DIRECTORY__; "test.txt" |])

let lesFil filSti = File.ReadAllLines(filSti) //    |> Array.rev

type Tilstand =
    { X: int
      Y: int
      Lengder: int
      YVelocity: int }

let tellAntallKryss filSti =
    let fjord = lesFil filSti //  |> Array.rev

    let (startX, startY) =
        let c = "B"
        fjord
        |> Array.tryFindIndex (fun (linje: string) -> linje.IndexOf(c) > 0)
        |> Option.map (fun y -> (fjord.[y].IndexOf(c), y))
        |> Option.defaultValue (0, 0)

    let fjordSlutt = fjord.[0].Length

    let startTilstand =
        { X = startX
          Y = startY
          Lengder = 1
          YVelocity = -1 }

    let skalKrysse xPos yPos yVel = fjord.[yPos + (yVel * 3)].[xPos] = '#'

    startTilstand
    |> Seq.unfold (fun b ->
        if b.X >= fjordSlutt - 1 then
            None
        else if not (skalKrysse b.X b.Y b.YVelocity) then
            let ny =
                { b with
                      X = b.X + 1
                      Y = b.Y + b.YVelocity }
            Some((0, ny.Lengder, ny.X, ny.Y), ny)
        else
            let ny =
                { b with
                      X = b.X + 1
                      Lengder = b.Lengder + 1
                      YVelocity = b.YVelocity * -1 }
            Some((1, ny.Lengder, ny.X, ny.Y), ny))

// tellAntallKryss filStiTest |> Seq.toArray

// Antall testkryss
tellAntallKryss filStiTest
|> Seq.filter (fun (k, _, _, _) -> k = 1)
|> Seq.length

// Antall kryss
tellAntallKryss filStiProd
|> Seq.filter (fun (k, _, _, _) -> k = 1)
|> Seq.length

// Antall lengder
tellAntallKryss filStiProd
|> Seq.last
|> fun (_, lengder, _, _) -> lengder

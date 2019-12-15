open System
open Newtonsoft.Json
open System.IO
// open System.Text.Json
// open System.Text.Json.Serialization
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "MAZE.TXT" |])

type Rom = {
    x: int
    y: int
    nord: bool
    vest: bool
    syd: bool
    aust: bool
}

let lesJsonTilArr () =
    let file = File.ReadAllText(filSti)
    let json = JsonConvert.DeserializeObject<Rom array array>(file)
    
    let linje (arr : Rom array) =
        arr |> Array.map (fun r -> {x = r.x; y = r.y; aust = r.aust; nord = r.nord; vest = r.vest; syd = r.syd })
    json
    |> Array.map linje

// let finnSti (rom: Rom) (visited: bool [,]) =
//     1
// let finnAntallRomBesoktPaaRute maze robot (startX, startY) =
//     let visitedArray = Array2D.create 500 500 false
//     1

[<EntryPoint>]
let main argv =
    printfn "Maze"
    lesJsonTilArr () |> ignore

    0 // return an integer exit code

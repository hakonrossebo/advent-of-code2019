
open System
open System.IO
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "MAZE.TXT" |])


let readJson () =
    let file = File.ReadAllLines(filSti)
    1

readJson ()
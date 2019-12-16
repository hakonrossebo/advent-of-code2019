open System.IO
let filStiProd = Path.Combine([| __SOURCE_DIRECTORY__; "fjord.txt" |])
let filStiTest = Path.Combine([| __SOURCE_DIRECTORY__; "test.txt" |])

let lesFil filSti =
    File.ReadAllLines(filSti)


let tellAntallKryss filSti =
  1


tellAntallKryss filStiTest






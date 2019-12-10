// Peter Meter registrerer alt han bruker nitidig. På badet bruker han alltid den samme tannkremen, den samme sjampoen og det samme toalettpapiret. Om disse vet vi følgende:
//     Tannkremen kommer i tuber på 125 milliliter.
//     Sjampoen kommer i flasker på 300 milliliter.
//     Toalettpapiret kommer på ruller på 25 meter.
// I denne loggfilen ser vi forbruket til Peter for 2018. Svaret vi skal frem til er produktet av følgende 5 tall:
//     Antall hele tuber tannkrem brukt i 2018.
//     Antall hele flasker sjampo brukt i 2018.
//     Antall hele toalettruller brukt i 2018.
//     Antall milliliter sjampo brukt på søndager.
//     Antall meter toalettpapir brukt på onsdager.

open System.IO
let usCulture = System.Globalization.CultureInfo("en-US", true);
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "logg.txt" |])

type Dag = {
  dag : System.DateTime
  artikler : Map<string, int> 
}

let artiklerTilartikkelDict (input: string array) = 
  let reversed (art:string) = art.Replace("*", "").Trim().Split(' ') |> Array.rev
  let artiklerDict = Array.map (reversed >> (fun reversed -> (reversed.[0], int reversed.[2]))) input |> Map.ofArray
  if Map.count artiklerDict = 3 then Some artiklerDict else None


let dagTilDato (input:string) =
  try
    Some (System.DateTime.ParseExact(input.Replace(":",""), "MMM dd", usCulture))
  with ex ->
    printfn "Feil i datoparsing %A" ex
    None

let parseDag (dagInfo:string array) =
  let dag = dagTilDato dagInfo.[0]
  let artikler = dagInfo |> Array.skip 1 |> artiklerTilartikkelDict
  Option.map2 (fun d a -> {dag = d; artikler = a}) dag artikler

let grouped () =
  File.ReadAllLines(filSti)
  |> Array.chunkBySize 4
  |> Array.map parseDag
  |> Array.filter (Option.isNone)

grouped ()

open System
open System.IO
let usCulture = System.Globalization.CultureInfo("en-US", true);
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "logg.txt" |])

type Dag = {
  Dato : System.DateTime
  Ukedag : DayOfWeek
  Artikler : Map<string, int> 
}

type Totaler = {
  ToalettpapirM : int
  SjampoMl : int
  TannkremMl : int
}

let leggSammenTotaler (a: Totaler) (b: Totaler) =
  {
    ToalettpapirM = a.ToalettpapirM + b.ToalettpapirM
    SjampoMl = a.SjampoMl + b.SjampoMl
    TannkremMl = a.TannkremMl + b.TannkremMl
  }


let artiklerTilartikkelDict (input: string array) = 
  let reversed (art:string) = art.Replace("*", "").Trim().Split(' ') |> Array.rev
  let artiklerDict = Array.map (reversed >> (fun reversed -> (reversed.[0], int reversed.[2]))) input |> Map.ofArray
  if Map.count artiklerDict = 3 then Some artiklerDict else None


let dagTilDato (input:string) =
  try
    let strengDato = input.Replace(":","") + " 2018"
    Some (System.DateTime.ParseExact(strengDato, "MMM dd yyyy", usCulture))
  with ex ->
    printfn "Feil i dato parsing %A" ex
    None

let parseDag (dagInfo:string array) =
  let dato = dagTilDato dagInfo.[0]
  let ukedag = Option.map (fun (x:DateTime) -> x.DayOfWeek  ) dato
  let artikler = dagInfo |> Array.skip 1 |> artiklerTilartikkelDict
  Option.map3 (fun d u a -> {Dato = d; Ukedag = u; Artikler = a}) dato ukedag artikler


let lagStats (dagStats : Dag [] )= 
  let antallDager = Array.length dagStats
  let totaler =
    dagStats
    |> Array.map (fun dag -> {ToalettpapirM = dag.Artikler.["toalettpapir"]; SjampoMl = dag.Artikler.["sjampo"]; TannkremMl = dag.Artikler.["tannkrem"]      })
    |> Array.reduce leggSammenTotaler
  let totalerSondager =
    dagStats
    |> Array.filter (fun d -> d.Ukedag = DayOfWeek.Sunday)
    |> Array.map (fun dag -> {ToalettpapirM = dag.Artikler.["toalettpapir"]; SjampoMl = dag.Artikler.["sjampo"]; TannkremMl = dag.Artikler.["tannkrem"]      })
    |> Array.reduce leggSammenTotaler
  let totalerOnsdager =
    dagStats
    |> Array.filter (fun d -> d.Ukedag = DayOfWeek.Wednesday)
    |> Array.map (fun dag -> {ToalettpapirM = dag.Artikler.["toalettpapir"]; SjampoMl = dag.Artikler.["sjampo"]; TannkremMl = dag.Artikler.["tannkrem"]      })
    |> Array.reduce leggSammenTotaler
  
  printfn "Antall dager: %d" antallDager
  printfn "Totalt: %d meter toalettpapir" totaler.ToalettpapirM
  printfn "Totalt: %d ml sjampo" totaler.SjampoMl
  printfn "Totalt: %d ml tannkrem" totaler.TannkremMl
  let heleTuberTannkrem2018 = totaler.TannkremMl / 125
  let heleFlaskerSjampo2018 = totaler.SjampoMl / 300
  let heleToalettruller2018 = totaler.ToalettpapirM / 25
  printfn " "
  printfn "Hele tuber tannkrem i 2018: %d" heleTuberTannkrem2018
  printfn "Hele flasker sjampo i 2018: %d" heleFlaskerSjampo2018
  printfn "Hele toalettruller i 2018: %d" heleToalettruller2018


  let milliliterSjampoSondager = totalerSondager.SjampoMl
  let meterToalettpapirOnsdager = totalerOnsdager.ToalettpapirM
  printfn "Antall milliliter sjampo brukt paa sondager: %d" milliliterSjampoSondager
  printfn "Antall meter toalettpapir brukt paa onsdager: %d" meterToalettpapirOnsdager

  let totalProdukt = heleTuberTannkrem2018 * heleFlaskerSjampo2018 * heleToalettruller2018 * milliliterSjampoSondager * meterToalettpapirOnsdager
  printfn "Totalprodukt: %d" totalProdukt
  totalProdukt

let finnDagensTall () =
  File.ReadAllLines(filSti)
  |> Array.chunkBySize 4
  |> Array.choose parseDag
  |> lagStats

finnDagensTall ()

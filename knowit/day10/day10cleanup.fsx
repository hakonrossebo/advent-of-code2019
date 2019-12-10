
open System
open System.IO
let usCulture = System.Globalization.CultureInfo("en-US", true);
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "logg.txt" |])

// Kunne lagt inn sequence for samme jobb, men forenklet her
let sequenceListOfOptionsToOptionList l = 
    if Array.contains None l then None
    else Some (Array.map Option.get l)

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

let dagTilTotal dag =
    {
        ToalettpapirM = dag.Artikler.["toalettpapir"]; 
        SjampoMl = dag.Artikler.["sjampo"]; 
        TannkremMl = dag.Artikler.["tannkrem"]      
    }

let leggSammenTotaler (a: Totaler) (b: Totaler) =
  {
    ToalettpapirM = a.ToalettpapirM + b.ToalettpapirM
    SjampoMl = a.SjampoMl + b.SjampoMl
    TannkremMl = a.TannkremMl + b.TannkremMl
  }

let artiklerTilartikkelDict (input: string array) = 
  let artikkelTypeOgAntall (art:string) = art.Replace("*", "").Trim().Split(' ') |> Array.rev
  let artiklerDict = Array.map (artikkelTypeOgAntall >> (fun x -> (x.[0], int x.[2]))) input |> Map.ofArray
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

let lagStats (dagStats : Dag [])= 
  let totaler =
    dagStats
    |> Array.map dagTilTotal
    |> Array.reduce leggSammenTotaler
  let totalerSondager =
    dagStats
    |> Array.filter (fun d -> d.Ukedag = DayOfWeek.Sunday)
    |> Array.map dagTilTotal
    |> Array.reduce leggSammenTotaler
  let totalerOnsdager =
    dagStats
    |> Array.filter (fun d -> d.Ukedag = DayOfWeek.Wednesday)
    |> Array.map dagTilTotal
    |> Array.reduce leggSammenTotaler
  
  let heleTuberTannkrem2018 = totaler.TannkremMl / 125
  let heleFlaskerSjampo2018 = totaler.SjampoMl / 300
  let heleToalettruller2018 = totaler.ToalettpapirM / 25
  let milliliterSjampoSondager = totalerSondager.SjampoMl
  let meterToalettpapirOnsdager = totalerOnsdager.ToalettpapirM
  let totalProdukt = heleTuberTannkrem2018 * heleFlaskerSjampo2018 * heleToalettruller2018 * milliliterSjampoSondager * meterToalettpapirOnsdager
  totalProdukt

let finnDagensTall () =
    let totalProdukt =
      File.ReadAllLines(filSti)
      |> Array.chunkBySize 4
      |> Array.map parseDag
      |> sequenceListOfOptionsToOptionList
      |> Option.map lagStats

    match totalProdukt with
    | Some p ->
        printfn "Totalprodukt: %d" p
    | None ->
        printfn "En feil oppstod. Kunne ikke kalkulere totaler." 

finnDagensTall ()

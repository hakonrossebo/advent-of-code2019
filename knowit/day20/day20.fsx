open Microsoft.FSharp.Core.Operators.Checked

let sluttAntall = 1000740 

//From https://stackoverflow.com/questions/12014224/when-generating-primes-in-f-why-is-the-sieve-of-erosthenes-so-slow-in-this-p/12014908#12014908
let generatePrimesArray limit =
    let table = Array.create limit true //use bools in the table to save on memory
    let tlimit = int (sqrt (float limit)) //max test no for table, ints should be fine
    let mutable curfactor = 1;
    while curfactor < tlimit-2 do
        curfactor <- curfactor+2
        if table.[curfactor]  then //simple optimisation
            let mutable v = curfactor*2
            while v < limit do
                table.[v] <- false
                v <- v + curfactor
    let out = Array.create (100000) 0 //this needs to be greater than pi(limit)
    let mutable idx = 1
    out.[0]<-2
    let mutable curx=1
    while curx < limit-2 do
        curx <- curx + 2
        if table.[curx] then
            out.[idx]<-curx
            idx <- idx+1
    out
    |> Array.filter (fun x -> x <> 0)
    |> Array.map (fun x -> (x, true))
    |> Map.ofArray

let nesteAlv retning indeks =
  let antallAlver = 5
  (retning + indeks + antallAlver) % antallAlver


type Tilstand = {Retning: int; Alv: int; AntallStegPerAlv: int array}
let startTilstand = {Retning=1;Alv=0;AntallStegPerAlv=[|1;0;0;0;0|]}

let deleligPa28 n = n % 28 = 0
let deleligPa7 n = n % 7 = 0
let erPartall n = n % 2 = 0
let primtall = generatePrimesArray sluttAntall
let erPrimtall n = Map.containsKey n primtall

let enAlvHarMinstOppgaver (alver:int array) : int option =
  let (antallSteg, antallMedSamme) = 
    alver
    |> Array.groupBy id
    |> Array.map (fun (antall, liste) -> (antall, Array.length liste))
    |> Array.sortBy (fun (antallsteg, antallmedsamme) -> antallsteg)
    |> Array.head
  match antallMedSamme with 
  | 1 -> Array.tryFindIndex (fun x -> x = antallSteg) alver
  | _ -> None

let enAlvHarFlestOppgaver (alver:int array) : int option =
  let (antallSteg, antallMedSamme) = 
    alver
    |> Array.groupBy id
    |> Array.map (fun (antall, liste) -> (antall, Array.length liste))
    |> Array.sortByDescending (fun (antallsteg, antallmedsamme) -> antallsteg)
    |> Array.head
  match antallMedSamme with 
  | 1 -> Array.tryFindIndex (fun x -> x = antallSteg) alver
  | _ -> None

let regel1 nesteSteg (tilstand:Tilstand) =
  if erPrimtall nesteSteg then
    match enAlvHarMinstOppgaver tilstand.AntallStegPerAlv with
    | Some i -> 
        Some (tilstand.Retning, i)
    | None -> 
        None
  else
    None

let regel2 nesteSteg (tilstand:Tilstand) =
  if deleligPa28 nesteSteg then
        let nyRetning = tilstand.Retning * -1
        Some (nyRetning, nesteAlv nyRetning tilstand.Alv)
  else
        None

let regel3 nesteSteg (tilstand:Tilstand) =
  if erPartall nesteSteg then
    match enAlvHarFlestOppgaver tilstand.AntallStegPerAlv with
    | Some i -> 
        let neste = nesteAlv tilstand.Retning tilstand.Alv
        if i = neste then
          let neste2 = nesteAlv tilstand.Retning neste
          Some (tilstand.Retning, neste2)
        else
          None
    | None -> 
        None
  else
    None

let regel4 nesteSteg (tilstand:Tilstand) =
  if deleligPa7 nesteSteg then
    Some (tilstand.Retning, 4)
  else
    None

let regel5 (tilstand:Tilstand) =
  Some (tilstand.Retning,nesteAlv tilstand.Retning tilstand.Alv)

let finnDagensSvar () =
  let utforOppgave (tilstand: Tilstand) nesteSteg : Tilstand =
    let nyRetning, nyAlvIndeks = 
                   [
                    regel1 nesteSteg
                    regel2 nesteSteg
                    regel3 nesteSteg
                    regel4 nesteSteg
                    regel5
                   ]
                   |> List.pick (fun regel -> regel tilstand)

    let nyAntallStegPerAlv = 
      tilstand.AntallStegPerAlv
      |> Array.mapi (fun i x -> if i = nyAlvIndeks then x + 1 else x)

    let nyTilstand = {Retning=nyRetning;Alv=nyAlvIndeks;AntallStegPerAlv=nyAntallStegPerAlv}
    nyTilstand
  
  let slutt =
    [2..sluttAntall]
    |> List.fold utforOppgave startTilstand
  let topp = Array.max slutt.AntallStegPerAlv
  let min = Array.min slutt.AntallStegPerAlv
  topp - min

finnDagensSvar ()

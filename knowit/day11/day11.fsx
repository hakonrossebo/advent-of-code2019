open System.IO
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "terreng.txt" |])
let testLandingstripe = "IIGGFFAIISGIFFSGFAAASS"

type Tilstand = {
  Fart : int64
  AntallIs : int64
  ForrigeVarFjell : bool
}

let start = {
  Fart = 10703437L
  AntallIs = 0L
  ForrigeVarFjell = false
}

let testStart = {
  Fart = 300L
  AntallIs = 0L
  ForrigeVarFjell = false
}

let akkumulerTerreng (tilstand:Tilstand) (terreng: char) =
    match terreng with
    | 'G' -> 
      { Fart = tilstand.Fart - 27L
        AntallIs = 0L
        ForrigeVarFjell = false
      }
    | 'A' -> 
      {
        Fart = tilstand.Fart - 59L
        AntallIs = 0L
        ForrigeVarFjell = false
      }
    | 'S' -> 
      {
        Fart = tilstand.Fart - 212L
        AntallIs = 0L
        ForrigeVarFjell = false
      }
    | 'I' -> 
      let antallIs = tilstand.AntallIs + 1L
      {
        Fart = tilstand.Fart + (12L * antallIs)
        AntallIs = antallIs
        ForrigeVarFjell = false
      }
    | 'F' -> 
      let differanse = if tilstand.ForrigeVarFjell then 35L else -70L
      {
        Fart = tilstand.Fart + differanse
        AntallIs = 0L
        ForrigeVarFjell = not tilstand.ForrigeVarFjell
      }
    | _ ->
      {
        Fart = tilstand.Fart
        AntallIs = 0L
        ForrigeVarFjell = false
      }

let finnDagensTall() =
    File.ReadAllText(filSti)
    |> Seq.scan akkumulerTerreng start
    |> Seq.map (fun {Fart = fart; AntallIs = _; ForrigeVarFjell = _} -> fart)
    |> Seq.filter (fun x -> x >= 0L)
    |> Seq.length
finnDagensTall ()

let finnDagensTestTall () =
    testLandingstripe
    |> Seq.scan akkumulerTerreng testStart
    |> Seq.map (fun {Fart = fart; AntallIs = _; ForrigeVarFjell = _} -> fart)
    |> Seq.filter (fun x -> x >= 0L)
    |> Seq.length

finnDagensTestTall()

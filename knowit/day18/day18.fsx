open System.IO
open System
open System.Numerics

let filStiNavn = Path.Combine([| __SOURCE_DIRECTORY__; "names.txt" |])
let filStiAnsatte = Path.Combine([| __SOURCE_DIRECTORY__; "employees.csv" |])
let lesFil filSti = File.ReadAllLines(filSti) 
let alfabet = [|'a'..'z'|]

type Lister = { Menn: string array; Kvinner: string array; E1: string array; E2: string array }
type Ansatt = { Fornavn: string; EtternavnDel1: string; EtternavnDel2: string; Kjonn: string }

let parseNavnTilAnsatt (s:string) =
    let deler = s.Split(',')
    let etternavn = deler.[1].Replace(" ", "").Replace("'", "")
    let split = if etternavn.Length % 2 = 0 then
                    (Seq.length etternavn) / 2
                else
                    ((Seq.length etternavn) / 2) + 1
    let e1, e2 = etternavn |> Seq.toArray |> Array.splitAt split
    {Fornavn = deler.[0]; EtternavnDel1 = e1 |> String.Concat; EtternavnDel2 = e2 |> String.Concat; Kjonn = deler.[2]}

let hentDelAvListe liste fraIndeks = 
    liste |> Array.skip fraIndeks |> Array.takeWhile (fun s -> s <> "---") 

let lister fullListe = 
    let menn = hentDelAvListe fullListe  0
    let kvinnerStart = (Array.length menn + 1)
    let kvinner = hentDelAvListe fullListe kvinnerStart
    let etternavnDel1Start = (Array.length kvinner + 1 + kvinnerStart)
    let etternavnDel1 = hentDelAvListe fullListe etternavnDel1Start
    let etternavnDel2Start = (Array.length etternavnDel1 + 1 + etternavnDel1Start)
    let etternavnDel2 = Array.skip etternavnDel2Start fullListe
    {Menn = menn; Kvinner = kvinner; E1 = etternavnDel1; E2 = etternavnDel2}

let alleLister () = lesFil filStiNavn |> lister

let summerAsciiVerdier (s:string) = s |> Seq.sumBy int

let lagFornavn navn liste =
    let sum = summerAsciiVerdier navn
    let antall = Array.length liste
    liste.[sum % antall]
    
let lagFornavnFraNavnOgKjonn  menn kvinner (ansatt:Ansatt)=
    match ansatt.Kjonn with
    | "M" -> lagFornavn ansatt.Fornavn menn
    | "F" -> lagFornavn ansatt.Fornavn kvinner
    | _ -> ""

let posisjonAlfabet (bokstav:char) = 1 + Array.findIndex (fun x -> x = Char.ToLower(bokstav)) alfabet

let summerAlfabetPosisjoner (s:string) = s |> Seq.sumBy posisjonAlfabet

let produktAsciiVerdier (s:string) = s |> Seq.map (int >> bigint) |> Seq.reduce (*)

let produktAvDel2 (ansatt:Ansatt) =
    let kjonnFaktor =
        match ansatt.Kjonn with
        | "M" ->
            bigint ansatt.Fornavn.Length
        | "F" ->
            bigint (ansatt.Fornavn + ansatt.EtternavnDel1 + ansatt.EtternavnDel2).Length
        | _ -> 0I
            
    let asciiProdukt = produktAsciiVerdier ansatt.EtternavnDel2 
    asciiProdukt * kjonnFaktor

let sortertSynkende (tall:bigint) =
    string tall |> Seq.sortByDescending id |> String.Concat |> (fun x -> BigInteger.Parse(x))

let produserNavn (lister:Lister) (ansatt:Ansatt) =
    let generertFornavn = ansatt |> lagFornavnFraNavnOgKjonn lister.Menn lister.Kvinner
    let generertEtternavnDel1 =
        summerAlfabetPosisjoner ansatt.EtternavnDel1
        |> (fun sum -> lister.E1.[sum % (Array.length lister.E1)])
    let generertEtternavnDel2 =
        let sortertProduktDel2 = produktAvDel2 ansatt
                                |> sortertSynkende
        let (l:bigint) = bigint (Array.length lister.E2)
        let modL = int (sortertProduktDel2 % l)
        lister.E2.[modL]
    sprintf "%s %s%s" generertFornavn generertEtternavnDel1 generertEtternavnDel2


lesFil filStiAnsatte 
|> Array.skip 1 
|> Array.map parseNavnTilAnsatt
|> Array.map (produserNavn (alleLister ()))
|> Array.groupBy id
|> Array.map (fun (n, l) -> (Seq.length l, n) )
|> Array.sortByDescending id
|> Array.take 5








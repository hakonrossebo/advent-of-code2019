<Query Kind="FSharpProgram" />

type Operasjon =  PLUSS4 | PLUSS101  | MINUS9 | MINUS1 | REVERSERSIFFER | OPP7 | GANGEMSD | DELEMSD | PLUSS1TILPAR | TREKK1FRAODDE | ROTERPAR | ROTERODDE | ROTERALLE | STOPP
  
let automatHjul = [|
                    [| PLUSS101; OPP7; MINUS9; PLUSS101 |]
                    [| TREKK1FRAODDE; MINUS1; MINUS9; PLUSS1TILPAR |]
                    [| PLUSS1TILPAR; PLUSS4; PLUSS101; MINUS9 |]
                    [| MINUS9; PLUSS101; TREKK1FRAODDE; MINUS1 |]
                    [| ROTERODDE; MINUS1; PLUSS4; ROTERALLE |]
                    [| GANGEMSD; PLUSS4; MINUS9; STOPP |]
                    [| MINUS1; PLUSS4; MINUS9; PLUSS101 |]
                    [| PLUSS1TILPAR; MINUS9; TREKK1FRAODDE; DELEMSD |]
                    [| PLUSS101; REVERSERSIFFER; MINUS1; ROTERPAR |]
                    [| PLUSS4; GANGEMSD; REVERSERSIFFER; MINUS9 |]
                    |] 
                    
let automathjulIndekser = [|0;0;0;0;0;0;0;0;0;0|]

let intTilSiffer c =
    string (abs c)
    |> fun x -> x.ToCharArray()
    |> Seq.map (string >> int)
    |> Seq.rev
    |> Seq.toArray
    
let sifferTilInt (siffer: int array) =
    siffer
    |> Array.rev
    |> Array.map (fun s -> string s)
    |> String.Concat
    |> int


let inkrementerGjeldendeIndeks (indeks: int array) gjeldendeIndeks =
    indeks
    |> Array.mapi (fun i iVal -> if i = gjeldendeIndeks then  (iVal + 1) else iVal  )

let prosesserOperasjon ((operasjon, gevinst, indekser) : (Operasjon * int * int array))  : (int * int array * bool) = 
    let alleSiffer = intTilSiffer gevinst
    let sisteDigit = alleSiffer.[0]
    match operasjon with
    | PLUSS4 -> 
        (gevinst + 4, inkrementerGjeldendeIndeks indekser sisteDigit, true)
    | PLUSS101 ->
        (gevinst + 101, inkrementerGjeldendeIndeks indekser sisteDigit, true)
    | MINUS9 -> 
        (gevinst - 9, inkrementerGjeldendeIndeks indekser sisteDigit, true)
    | MINUS1 ->
        (gevinst - 1, inkrementerGjeldendeIndeks indekser sisteDigit, true)
    | REVERSERSIFFER ->
        let sign = Math.Sign(gevinst)
        let reversertGevinst =
            gevinst
            |> intTilSiffer
            |> Array.rev
            |> sifferTilInt
            |> fun x -> x * sign
        (reversertGevinst, inkrementerGjeldendeIndeks indekser sisteDigit, true)
        
    | OPP7 ->
        let rec opp7 tall =
            let siffer = intTilSiffer tall
            if siffer.[0] = 7 then
                tall
            else
                opp7 (tall + 1)
        (opp7 gevinst, inkrementerGjeldendeIndeks indekser sisteDigit, true)
        
    | GANGEMSD ->
        let oversteSiffer = Array.last alleSiffer
        (gevinst * oversteSiffer, inkrementerGjeldendeIndeks indekser sisteDigit, true)
        
    | DELEMSD ->
        let oversteSiffer = Array.last alleSiffer |> double
        let doubleGevinst = double gevinst
        let deltGevinst = doubleGevinst / oversteSiffer |> floor |> int
        (deltGevinst, inkrementerGjeldendeIndeks indekser sisteDigit, true)
        
    | PLUSS1TILPAR ->
        let sign = Math.Sign(gevinst)
        let pluss1ParGevinst =
            alleSiffer
            |> Array.map (fun x -> if x % 2 = 0 then x + 1 else x)
            |> sifferTilInt
            |> (fun x -> x * sign)
        (pluss1ParGevinst, inkrementerGjeldendeIndeks indekser sisteDigit, true)
        
    | TREKK1FRAODDE ->
        let sign = Math.Sign(gevinst)
        let trekk1FraOddeGevinst =
            alleSiffer
            |> Array.map (fun x -> if x % 2 <> 0 then x - 1 else x)
            |> sifferTilInt
            |> (fun x -> x * sign)
        (trekk1FraOddeGevinst, inkrementerGjeldendeIndeks indekser sisteDigit, true)
        
    | ROTERPAR ->
        let parIndekserRotert =
            indekser
            |> Array.mapi (fun i value -> if i % 2 = 0 then value + 1 else value)
            |> (fun ind -> inkrementerGjeldendeIndeks ind sisteDigit)
        (gevinst, parIndekserRotert, true)
        
    | ROTERODDE ->
        let oddeIndekserRotert =
            indekser
            |> Array.mapi (fun i value -> if i % 2 <> 0 then value + 1 else value)
            |> (fun ind -> inkrementerGjeldendeIndeks ind sisteDigit)
        (gevinst, oddeIndekserRotert, true)
        
    | ROTERALLE ->
        let alleIndekserRotert =
            indekser
            |> Array.map (fun value -> value + 1)
            |> (fun ind -> inkrementerGjeldendeIndeks ind sisteDigit)
        (gevinst, alleIndekserRotert, true)
        
    | STOPP ->
        (gevinst, indekser, false)

                    
let prosesserNeste ((gevinst, indekser) : (int * int array))  : (int * int array * bool) = 
    let sisteDigit = (intTilSiffer gevinst).[0]
    let operasjon = 
        automatHjul.[sisteDigit].[indekser.[sisteDigit] % 4]
    prosesserOperasjon (operasjon, gevinst, indekser)
    
    
let rec run init indekser =
    let (gevinst, nyeIndekser, fortsett) = prosesserNeste (init, indekser)
    if fortsett = true then
        run gevinst nyeIndekser
    else
        gevinst

let besteAntallMynter =
    [1..9]
    |> List.map (fun x -> (x, run x automathjulIndekser))
    |> List.sortBy (fun (_, gevinst) -> gevinst)
    |> List.rev

besteAntallMynter.Dump()
  
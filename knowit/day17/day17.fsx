open System

let million = 1000000

let trekantFormel n =
    (n * (n + 1UL) / 2UL)

let trekantTallTilMillion: seq<uint64> =
    Seq.initInfinite (uint64 >> trekantFormel)
    |> Seq.take million

let alleRotasjoner (n: uint64) =
    let strengTall = string n
    let lengde = strengTall.Length
    let dobbelStreng = strengTall |> Seq.replicate 2 |> String.Concat

    [ 0 .. lengde - 1 ]
    |> Seq.mapi (fun i _ -> dobbelStreng.Substring(i, lengde))
    |> Seq.map uint64

let sjekkKvadrattall (nInput: uint64) =
    let n = (float nInput)
    let sq = (sqrt >> floor) n
    n = (sq * sq)

let rotasjonAvTallErKvadrattall n =
            let rotasjoner = alleRotasjoner n
            let kvadrattall =
                rotasjoner
                |> Seq.map sjekkKvadrattall
                |> Seq.filter id

            not (Seq.isEmpty kvadrattall)

trekantTallTilMillion
    |> Seq.map rotasjonAvTallErKvadrattall
    |> Seq.filter id
    |> Seq.length



open System

let erLike list = Seq.forall (fun elem -> elem = Seq.head list) list

let strengOgPad nyttTall = (string nyttTall).PadLeft(4, '0')

let charsTilInt (s: char seq) = String.Concat s |> int

let kalkulerFormel (tall: string) =
    let minst = Seq.sort tall
    let storst = Seq.rev minst
    (charsTilInt storst) - (charsTilInt minst)

let finnAntallIterasjoner n =
    let rec tallProsedyre i t =
        let strengTall = strengOgPad t
        if erLike strengTall then
            None
        else
            let nyttTall = kalkulerFormel strengTall
            if nyttTall = 6174 then Some(i + 1)
            else tallProsedyre (i + 1) nyttTall
    tallProsedyre 0 n

let dagensTall() =
    [| 1000 .. 9999 |]
    |> Array.Parallel.choose finnAntallIterasjoner
    |> Array.filter (fun x -> x = 7)
    |> Array.length

dagensTall() |> printfn "%A"
//

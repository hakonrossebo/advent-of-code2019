<Query Kind="FSharpProgram" />

//Lista blei delt i to, og dei to halvdelane bytta plass.
//Første og andre bokstav bytta plass. Tredje og fjerde bokstav bytta plass. Femte og sjette bokstav bytta plass, og så videre for hele lista.
//Dei tre første bokstavane bytta plass med dei tre siste. Dei tre neste bytta plass med dei tre nest siste. Og så vidare.
//
//Lista blei til slutt slik:
//
//Eksempel
let testData0 = "PonnistallHoppeslottTrommesett"
let testData1 = "slottTrommesettPonnistallHoppe"
let testData2 = "lstoTtormmsetePtnointslaHlpoep"
let testData3 = "oepHlpslainttnotePmseormoTtlst"

let inputData = "tMlsioaplnKlflgiruKanliaebeLlkslikkpnerikTasatamkDpsdakeraBeIdaegptnuaKtmteorpuTaTtbtsesOHXxonibmksekaaoaKtrssegnveinRedlkkkroeekVtkekymmlooLnanoKtlstoepHrpeutdynfSneloietbol"

let byttTreBokstaver (input:string) : string =
    input
    |> Seq.chunkBySize 3
    |> Seq.rev
    |> Seq.concat
    |> String.Concat

let byttAnnenhverBokstav (input:string) : string =
    input
    |> Seq.chunkBySize 2
    |> Seq.map Array.rev
    |> Seq.concat
    |> String.Concat

let byttHalvdeler (input:string) : string =
    input
    |> Seq.chunkBySize (input.Length / 2)
    |> Seq.rev
    |> Seq.concat
    |> String.Concat

let reverserBlanding (input:string) : string =
    input
    |> byttTreBokstaver
    |> byttAnnenhverBokstav
    |> byttHalvdeler
    
(reverserBlanding inputData).Dump("Inputdata reversert")


(reverserBlanding testData3).Dump("Testdata reversert")
let verifisertSteg3 = byttTreBokstaver testData3
(verifisertSteg3 = testData2).Dump()
let verifisertSteg2 = byttAnnenhverBokstav testData2
(verifisertSteg2 = testData1).Dump()
let verifisertSteg1 = byttHalvdeler testData1
(verifisertSteg1 = testData0).Dump()











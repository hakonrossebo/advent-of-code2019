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
    |> Seq.toArray
    |> Array.chunkBySize 3
    |> Array.rev
    |> Array.concat
    |> String

let byttAnnenhverBokstav (input:string) : string =
    input
    |> Seq.toArray
    |> Array.chunkBySize 2
    |> Array.map Array.rev
    |> Array.concat
    |> String

let byttHalvdeler (input:string) : string =
    input
    |> Seq.toArray
    |> Array.chunkBySize (input.Length / 2)
    |> Array.rev
    |> Array.concat
    |> String

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











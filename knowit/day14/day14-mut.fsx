open System.Collections.Generic
open System.Linq

let testSluttSekvens = [2; 2; 3; 3; 2; 2; 2; 3; 3; 3; 2; 2; 3; 3; 2; 2]
let testAlfabet = [|2y; 3y|]
let dagensAlfabet = [|2y; 3y; 5y; 7y; 11y|]
let stopp = 217532235


let finnSvar stoppLengde alfabet =
    let alfabetLengde = Array.length alfabet
    let startElementer = Enumerable.Repeat(alfabet.[0], int alfabet.[0])
    let sekvens = new List<int8>(startElementer)
    let mutable sIdx = 1
    let mutable sum7 = 0
    let mutable frekvensTeller = 0
    while sekvens.Count < stoppLengde do
        let alfaNum = alfabet.[sIdx % alfabetLengde]
        frekvensTeller <- 0
        while (frekvensTeller < (int sekvens.[sIdx]) && sekvens.Count < stoppLengde) do
            frekvensTeller <- frekvensTeller + 1
            sekvens.Add(alfaNum)
            if alfaNum = 7y then 
                sum7 <- sum7 + 7
        sIdx <- sIdx + 1
    sum7

finnSvar (List.length testSluttSekvens) testAlfabet
|> printfn "Sum av alle 7: %d"

finnSvar stopp dagensAlfabet
|> printfn "Sum av alle 7: %d"

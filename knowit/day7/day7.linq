<Query Kind="FSharpProgram" />

// Med Seq

let num = 27644437L
let num2 = 5897L
let gamleKoder = [(2, 13825167);(3, 9216778); (4, 20734802)]
let rangeY' = seq {2L..num-1L}
        
// Hovedalgoritme
let finnDagensKode dag =
    let steg1Y =
            rangeY'
            |> Seq.map (fun y' -> (y',  y' * dag))
            |> Seq.map (fun (y', b') ->  (y', (b' % num)))
            |> Seq.tryFind (fun (_,r') -> r' = 1L)
        
    let steg2Z =
        steg1Y
        |> Option.map (fun (yVal, _) -> num2 * yVal)
        
    let steg3Kode =
        steg2Z
        |> Option.map (fun zVal -> zVal % num)
    (dag, steg3Kode) 

let dag2 =
    finnDagensKode 4L
    
dag2.Dump()


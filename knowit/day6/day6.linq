<Query Kind="FSharpProgram" />


let testImage = [(240, 33, 11); (205, 111, 102); (120, 96, 7); (45, 3, 202); (76, 237, 47)]
let testImageObfuscated = [(240, 33, 11); (61, 78, 109); (69, 46, 106); (104, 45, 160); (36, 192, 143)]
testImage.Dump("Original image tuples")


let xorTuples (t1r, t1g, t1b) (t2r, t2g, t2b) = 
    (t1r ^^^ t2r, t1g ^^^ t2g, t1b ^^^ t2b)
    
let newTupleBasedOnPreviousAndCurrent prev current =
    if prev = (-1, -1, -1) then
        current
    else
        xorTuples prev current
        
let r =
    testImage
    |> List.scan newTupleBasedOnPreviousAndCurrent (-1,-1,-1)
    |> List.tail
    
let rBack =
    let first = List.head r
    r
    |> List.rev
    |> List.pairwise
    |> List.map (fun (a, b) -> xorTuples a b)
    |> List.rev
    |> List.append [first]
    
r.Dump("Obfuscated test image")
rBack.Dump("Reverted back")
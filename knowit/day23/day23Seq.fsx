let sluttAntall = 98765432 
let primtall = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73] |> Set.ofList

let inline sumAvSiffer n =
    let mutable sum = 0
    let mutable i = n
    while i > 0 do
        sum <- sum + i % 10
        i <- i / 10
    sum
    
let inline erPrimtall n = Set.contains n primtall

let finnDagensSvar () =
    seq {
        for n in 1..sluttAntall do
            let sifferSum = sumAvSiffer n
            if n % sifferSum = 0 && erPrimtall sifferSum then
                yield n
    }
    |> Seq.length

finnDagensSvar()



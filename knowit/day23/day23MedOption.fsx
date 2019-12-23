let sluttAntall = 98765432 
let primtall = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73] 
            |> List.map (fun x -> x, true) 
            |> Map.ofList

let inline sumAvSiffer n =
    let mutable sum = 0
    let mutable i = n
    while i > 0 do
        sum <- sum + i % 10
        i <- i / 10
    sum

let inline erHarshadtall n =
    let sum = sumAvSiffer n
    if n % sum = 0 then
        Some sum
    else
        None
    
let inline erPrimtall n = 
    match Map.containsKey n primtall with
    | true -> Some n
    | false -> None

let inline erHarshadPrimtall n = erHarshadtall n |> Option.bind erPrimtall

erHarshadPrimtall 1730
erHarshadPrimtall 1729

erPrimtall 10

let finnDagensSvar () =
    [|1..sluttAntall|]
    |> Array.Parallel.choose erHarshadPrimtall
    |> Array.length

finnDagensSvar()



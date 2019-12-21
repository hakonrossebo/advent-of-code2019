let tallTilSiffer tall =
    let rec neste n =
        if n = 0 then []
        else (n % 10) :: neste (n / 10)
    neste tall

let sifferTilTall siffer =
  siffer
  |> List.mapi (fun i x -> (pown 10 i) * x)
  |> List.sum

let alleSifferErLike list =
    let head = List.head list
    List.forall (fun elem -> elem = head) list

let palindrome n =
  let rec recPalindrome current result = 
    match current with
      | 0 -> result
      | _ -> recPalindrome (result * 10 + current % 10) (current / 10) 
  recPalindrome n 0

let rec revdigits i =
    let tens = i / 10
    if tens = 0 then 
      [i]
    else
        let ones = i % 10
        ones :: revdigits tens

#time "on"

//From https://stackoverflow.com/questions/12014224/when-generating-primes-in-f-why-is-the-sieve-of-erosthenes-so-slow-in-this-p/12014908#12014908
// let limit = 1000000
//returns an array of all the primes up to limit
let generatePrimesArray limit =
    let table = Array.create limit true //use bools in the table to save on memory
    let tlimit = int (sqrt (float limit)) //max test no for table, ints should be fine
    let mutable curfactor = 1;
    while curfactor < tlimit-2 do
        curfactor <- curfactor+2
        if table.[curfactor]  then //simple optimisation
            let mutable v = curfactor*2
            while v < limit do
                table.[v] <- false
                v <- v + curfactor
    let out = Array.create (100000) 0 //this needs to be greater than pi(limit)
    let mutable idx = 1
    out.[0]<-2
    let mutable curx=1
    while curx < limit-2 do
        curx <- curx + 2
        if table.[curx] then
            out.[idx]<-curx
            idx <- idx+1
    out


// let p = generatePrimesArray 1000000
// p
// |> Array.rev
// |> Array.filter (fun x -> x <> 0)
// |> Array.take 10

// Array.exists (fun x -> x = 999883) p


let sieve_primes top_number = 
    let numbers = [ yield 2
                    for i in 3..2..top_number -> i ]
    let rec sieve ns = 
        match ns with
        | [] -> []
        | x::xs when x*x > top_number -> ns
        | x::xs -> x::sieve (List.filter(fun y -> y%x <> 0) xs)
    sieve numbers 

// sieve_primes 1000000



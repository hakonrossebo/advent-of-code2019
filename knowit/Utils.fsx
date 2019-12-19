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
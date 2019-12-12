let tallTilSiffer tall =
    let rec neste n =
        if n = 0 then []
        else (n % 10) :: neste (n / 10)
    neste tall

let alleSifferErLike list =
    let head = List.head list
    List.forall (fun elem -> elem = head) list

t

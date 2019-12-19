open Microsoft.FSharp.Core.Operators.Checked

let tallTilSiffer tall =
    let rec neste n =
        if n = 0 then []
        else (n % 10) :: neste (n / 10)
    neste tall 

let sifferTilTall siffer =
  siffer
  |> List.mapi (fun i x -> (pown 10 i) * x)
  |> List.sum

sifferTilTall [4;3;2;1]
tallTilSiffer 1234
tallTilSiffer 123454319 |> List.rev |> sifferTilTall

let reverserTall n =
  n |> tallTilSiffer |> List.rev |> sifferTilTall

let erSkjultePalindromer range =
  seq {
    for n in range do
      let reversert = reverserTall n
      if n <> reversert then
        let sumMedReversert = n + reversert
        let reversertSum = reverserTall sumMedReversert
        if sumMedReversert = reversertSum then
          yield uint64 n
  }

{1..123454319}
|> erSkjultePalindromer
|> Seq.sum

reverserTall 123454319 

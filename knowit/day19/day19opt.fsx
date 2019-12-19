
open System
open System.Numerics
open Microsoft.FSharp.Core.Operators.Checked

let tallTilSiffer tall =
    let rec neste n =
        if n = 0UL then []
        else (n % 10UL) :: neste (n / 10UL)
    neste tall 

let sifferTilTall siffer =
  siffer
  |> List.mapi (fun i x -> (pown 10UL i) * x)
  |> List.sum

sifferTilTall [4UL;3UL;2UL;1UL]
tallTilSiffer 1234UL
tallTilSiffer 123454319UL |> List.rev |> sifferTilTall

let reverserTall n =
  n |> tallTilSiffer |> List.rev |> sifferTilTall

let reverserTallU n =
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

let erSkjultPalindrom n =
      let reversert = reverserTall n
      if n <> reversert then
        let sumMedReversert = (n + reversert) |> uint64
        let reversertSum = (reverserTallU sumMedReversert) |> uint64
        if sumMedReversert = reversertSum then
          Some (uint64 n)
        else
          None
      else None
      
// {1..123454319}
{1UL..1000UL}
|> erSkjultePalindromer
|> Seq.toArray

{1UL..123454319UL}
|> erSkjultePalindromer
|> Seq.sum

reverserTall 123454319UL

[|1UL..123454319UL|]
|> Array.Parallel.choose erSkjultPalindrom
|> Array.sum
// |> Seq.sum
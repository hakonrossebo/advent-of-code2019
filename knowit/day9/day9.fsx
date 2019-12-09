open System.IO
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "krampus.txt" |])

let finnKrampusTall (liste: string seq) =
    seq {
        for input in liste do
            let inputNummer = int64 input
            let kvadratTall = string (inputNummer * inputNummer)
            for i in 1..kvadratTall.Length-1 do
                let part1 = int64 kvadratTall.[..i - 1]
                let part2 = int64 kvadratTall.[i..]
                if part2 <> 0L && part1 + part2 = inputNummer then
                    yield inputNummer
    }

File.ReadAllLines(filSti)
|> finnKrampusTall
|> Seq.sum

//fasit = 445372L
open System.IO
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "krampus.txt" |])

let linjer() =
    File.ReadAllLines(filSti)
    |> Array.map int64
    |> Array.sort
    |> Array.map string

let splittStreng (input: string) index =
    if index < 1 || index > input.Length - 1 then
        None
    else
        let part1 = input.[..index - 1]
        let part2 = input.[index..]
        part2
            |> (fun part2 ->
            if int64 part2 = 0L then 
                None
            else 
                Some part2)
            |> Option.map (fun part2 -> (part1, part2))

let finnStrengKombinasjoner (input: string) =
    let len = input.Length
    [ 1 .. len ] |> List.choose (splittStreng input)

let summerKombinasjoner ((p1, p2): string * string) =
    let p1Num = int p1
    let p2Num = int p2
    p1Num + p2Num

let sjekkKrampus (input: string) =
    let inputNumber = int64 input
    string (inputNumber * inputNumber)
    |> finnStrengKombinasjoner
    |> List.map summerKombinasjoner
    |> List.exists (fun x -> int64 x = inputNumber)

let sum =
    linjer()
    |> Array.Parallel.map (fun s -> (int64 s, sjekkKrampus s))
    |> Array.filter (fun (_, isKrampus) -> isKrampus)
    |> Array.sumBy (fun (d, _) -> d)

//fasit = 445372L
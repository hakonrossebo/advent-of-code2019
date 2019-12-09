open System.IO

let path = Path.Combine([| __SOURCE_DIRECTORY__; "krampus.txt" |])

let lines() =
    File.ReadAllLines(path)
    |> Array.map int64
    |> Array.sort
    |> Array.map string

let splitStringAt (input: string) index =
    if index < 1 || index > input.Length - 1 then
        None
    else
        let part1 = input.[0..index - 1]

        let combination =
            input.[index..]
            |> (fun part2 ->
            if part2 = "0" then None
            else Some part2)
            |> Option.map (fun part2 -> (part1, part2))
        combination

let getStringCombinations (input: string) =
    let len = input.Length
    [ 1 .. len ] |> List.choose (splitStringAt input)

let sumCombinations ((p1, p2): string * string) =
    let p1Num = int p1
    let p2Num = int p2
    p1Num + p2Num

let verifyKrampus (input: string) =
    let inputNumber = int64 input
    string (inputNumber * inputNumber)
    |> getStringCombinations
    |> List.map sumCombinations
    |> List.exists (fun x -> int64 x = inputNumber)


let sum =
    lines()
    |> Array.map (fun s -> (s, verifyKrampus s))
    |> Array.filter (fun (_, isKrampus) -> isKrampus)
    |> Array.map (fun (s, _) -> int64 s)
    |> Array.sum

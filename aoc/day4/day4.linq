<Query Kind="FSharpProgram" />

let inputRange = [231832..767346]
let inputRangeTest = [123..133]
let testNumbers = [122345;111123;135679;111111;223450;123789]

//facts
//It is a six-digit number.
//The value is within the range given in your puzzle input.
//Two adjacent digits are the same (like 22 in 122345).
//Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679
let intArrayFromInt n =
    n.ToString().ToCharArray()
    |> Seq.map (string >> int)
    

let validateTwoAdjacentDigitsExists n : Option<int> =
    let ints = intArrayFromInt n
    let countOfAdj = Seq.zip ints (ints.Skip 1)
                    |> Seq.filter (fun (p, n) -> p = n)
                    |> Seq.length
    if countOfAdj >= 1 then
        Some n
    else
        None

let validateDigitIsSameOrHigher n : Option<int> =
    let ints = intArrayFromInt n
    let countOfOk = Seq.zip ints (ints.Skip 1)
                    |> Seq.filter (fun (p, n) -> p = n || p < n)
                    |> Seq.length
    if countOfOk = 5 then
        Some n
    else
        None

let validList l = 
   l 
    |> List.choose validateDigitIsSameOrHigher
    |> List.choose validateTwoAdjacentDigitsExists
    |> List.length
   
(validList testNumbers).Dump()
(validList inputRange).Dump()


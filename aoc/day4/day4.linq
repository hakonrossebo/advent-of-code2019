<Query Kind="FSharpProgram" />

let inputRange = [231832..767346]
let inputRangeTest = [123..133]
let testNumbers = [122345;111123;135679;111111;223450;123789]
let testNumbersPart2 = [112233;123444;111122]

//Part1
//It is a six-digit number.
//The value is within the range given in your puzzle input.
//Two adjacent digits are the same (like 22 in 122345).
//Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679

//Part2
//An Elf just remembered one more important detail: the two adjacent matching digits are not part of a larger group of matching digits.
//Given this additional criterion, but still ignoring the range rule, the following are now true:
//    112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
//    123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
//    111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).


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
        
let validateTwoAdjacentDigitsExistsButNotInLargerGroup n : Option<int> =
    let ints = intArrayFromInt n
    let adj = Seq.zip ints (ints.Skip 1)
                    |> Seq.filter (fun (p, n) -> p = n)
                    |> Seq.map (fun (p, _) -> p)
    if Seq.length adj >= 1 then
        let adj3 =  Seq.zip3 ints (ints.Skip 1) (ints.Skip 2)
                            |> Seq.filter (fun (a, b, c) -> a = b && b = c)
                            |> Seq.map (fun (a, _, _) -> a)
        let adj2NotIn3 = 
            Set.difference (set adj) (set adj3)
        
        if Seq.length adj2NotIn3 >= 1 then
            Some n
        else
            None
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
    
let validListPart2 l = 
   l 
    |> List.choose validateDigitIsSameOrHigher
    |> List.choose validateTwoAdjacentDigitsExistsButNotInLargerGroup
   
(validList testNumbers).Dump("Part 1 test numbers count")
(validList inputRange).Dump("Part 1 count")

(validListPart2 testNumbersPart2).Dump("Test numbers part 2")
let countOfPart2 =
    (validListPart2 inputRange)
    |> List.length
    
countOfPart2.Dump("Part 2 count")


open Microsoft.FSharp.Core.Operators.Checked

let reverser n =
  let mutable n = n
  let mutable reversert = 0UL
  while n <> 0UL do
    reversert <- (reversert * 10UL) + (n % 10UL)
    n <- n / 10UL
  reversert

let erPalindrom n = 
  n = reverser n

let erSkjultPalindrom n =
    if erPalindrom n then
        None
    else
        if erPalindrom (n + reverser n) then
          Some n
        else
          None

[|1UL..123454319UL|]
|> Array.Parallel.choose erSkjultPalindrom
|> Array.sum
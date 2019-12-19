
let reverser (n: uint64) =
  let mutable (n: uint64) = n
  let mutable (reversert: uint64) = 0UL
  while n <> 0UL do
    reversert <- (reversert * 10UL) + (n % 10UL)
    n <- n / 10UL
  reversert

let erPalindrom (n:uint64) = 
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
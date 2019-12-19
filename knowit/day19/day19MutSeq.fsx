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

let skjultePalindromer range =
  seq {
    for n in range do
      if not (erPalindrom n) then
        if erPalindrom (n + reverser n) then
          yield n
  }

{1UL..123454319UL}
|> skjultePalindromer
|> Seq.sum
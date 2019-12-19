let reverser (n: uint64) =
  let mutable (n: uint64) = n
  let mutable (reversert: uint64) = 0UL
  while n <> 0UL do
    reversert <- (reversert * 10UL) + (n % 10UL)
    n <- n / 10UL
  reversert

let erPalindrom (n:uint64) = 
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
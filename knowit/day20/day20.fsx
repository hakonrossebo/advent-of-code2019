open Microsoft.FSharp.Core.Operators.Checked

let nesteIndeks retning indeks =
  let s = 5
  (retning + indeks + s) % s

// nesteIndeks -4 4

let sluttAntall = 1000740 
type Tilstand = {Retning: int; Indeks: int; AntallStegPerAlv: int array; AntallSteg:int}
let startTilstand = {Retning=1;Indeks=0;AntallStegPerAlv=[||];AntallSteg=0}

let utforOppgave (tilstand: Tilstand) : Tilstand =
  startTilstand

// Alvene rundt bordet er nummerert 1–5 med klokka. Alv 1 starter med å utføre steg 1. 
// Etter Alv 1 er ferdig med steg 1 går gaven videre med klokka til alv 2 som utfører steg 2.
// Kompliserte gaver har kompliserte regler for bygging. 
// Alv 1 utfører det første steget, deretter gjelder følgende regler for dagens gave:
//     1. Dersom neste steg er et primtall, skal alven som til nå har gjort færrest oppgaver utføre neste steg.
//     2. Dersom neste steg er delelig på 28, snur retningen gaven blir gitt videre. 
//              Dersom gjeldende retning er med klokka vil gaven nå gå mot klokka fram til retningen snur igjen. 
//              Om alv 1 skulle ha gitt gaven videre til alv 2, vil gaven nå istedenfor gå til alv 5.
//     3. Dersom neste steg er partall og neste alv har utført flest oppgaver, hopp over den alven og gi gaven til den neste.
//     4. Dersom neste steg er delelig på 7, skal alv 5 gjøre neste steg.
//     5. Gi gaven videre til neste alv i gjeldende klokkeretning.
// For hvert steg er det kun den første gyldige regelen som skal brukes. 
// Dersom det er flere kandidater for neste alv (f.eks. om flere alver har gjort like mange oppgaver) 
// dropper vi regelen og prøver neste regel.
// Gaven er ferdig etter 1 000 740 steg.
// Eksempel
// De første stegene blir som følger:
//     Alv 1 gjør steg 1. (start)
//     Alv 2 gjør steg 2. (regel 1 og regel 3 bortgår pga. flere kandidater, regel 5 gjelder)
//     Alv 3 gjør steg 3. (regel 1 bortgår, regel 5 gjelder)
//     Alv 4 gjør steg 4. (regel 3 bortgår, regel 5 gjelder)
//     Alv 5 gjør steg 5. (regel 1 gjelder)
//     Alv 1 gjør steg 6. (regel 3 bortgår, regel 5 gjelder)
//     Alv 5 gjør steg 7. (regel 4 gjelder)
//     Alv 1 gjør steg 8. (regel 3 bortgår, regel 5 gjelder)
//     osv.
// Oppgave
// Alvenes fagorganisasjon for gavemakere er bekymret for ujevn arbeidsfordeling i gavemakerprossessen. Hva er differansen i antall steg utført mellom den alven som gjorde minst og den alven som gjorde mest?
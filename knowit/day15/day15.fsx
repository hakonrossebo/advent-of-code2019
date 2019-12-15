let testPunkt =
    [
        [0;0;0;10;0;0;0;0;10]
        [0;0;0;0;0;10;0;10;0]
        [0;0;0;0;10;0;10;0;0]
        [10;0;0;0;10;0;0;0;10]
    ]

open System.IO
let filSti = Path.Combine([| __SOURCE_DIRECTORY__; "MODEL.CSV" |])

let lesFil () =
    File.ReadAllLines(filSti)
        |> Seq.map (fun str -> str.Split(',') |> Seq.map float |> Seq.toList)
        |> Seq.toList

type Koordinat = {
    X: float
    Y: float
    Z: float
}

let lagKoordinat (arr: float list) =
    {
        X = float arr.[0]
        Y = float arr.[1]
        Z = float arr.[2]
    }

let linjeTilKoordinater (arr: float list) =
        arr
        |> List.chunkBySize 3
        |> List.map lagKoordinat

let koordinaterTilTriangel (arr: Koordinat list) =
    (arr.[0], arr.[1], arr.[2])

//Fant på SO
let signedVolumeOfTriangle ((p1, p2, p3) : Koordinat * Koordinat * Koordinat) =
    let v321 = p3.X*p2.Y*p1.Z;
    let v231 = p2.X*p3.Y*p1.Z;
    let v312 = p3.X*p1.Y*p2.Z;
    let v132 = p1.X*p3.Y*p2.Z;
    let v213 = p2.X*p1.Y*p3.Z;
    let v123 = p1.X*p2.Y*p3.Z;
    (1.0/6.0) * (-v321 + v231 + v312 - v132 - v213 + v123);

let beregnVolumAvMesh (linjer: float list list) =
            linjer
            |> List.sumBy (linjeTilKoordinater >> koordinaterTilTriangel >> signedVolumeOfTriangle)
            |> fun x -> abs (x / 1000.0)

lesFil ()
    |> beregnVolumAvMesh
    |> printfn "Resultat: %.3f"

let testPunktSomFloat =
    testPunkt
    |> List.map (fun x -> List.map float x)

testPunktSomFloat
    |> beregnVolumAvMesh
    |> printfn "Resultat: %.3f"
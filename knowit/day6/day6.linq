<Query Kind="FSharpProgram">
  <NuGetReference>SkiaSharp</NuGetReference>
</Query>


let testImage = [(240, 33, 11); (205, 111, 102); (120, 96, 7); (45, 3, 202); (76, 237, 47)]
let testImageObfuscated = [(240, 33, 11); (61, 78, 109); (69, 46, 106); (104, 45, 160); (36, 192, 143)]
testImage.Dump("Original image tuples")


let xorTuples (t1r, t1g, t1b) (t2r, t2g, t2b) = 
    (t1r ^^^ t2r, t1g ^^^ t2g, t1b ^^^ t2b)
    
let newTupleBasedOnPreviousAndCurrent prev current =
    if prev = (-1, -1, -1) then
        current
    else
        xorTuples prev current
        
let r =
    testImage
    |> List.scan newTupleBasedOnPreviousAndCurrent (-1,-1,-1)
    |> List.tail
    
let rBack =
    let first = List.head r
    r
    |> List.rev
    |> List.pairwise
    |> List.map (fun (a, b) -> xorTuples a b)
    |> List.rev
    |> List.append [first]
    
let rBackImage lst =
    let first = List.head lst
    lst
    |> List.rev
    |> List.pairwise
    |> List.map (fun (a, b) -> xorTuples a b)
    |> List.rev
    |> List.append [first]
    
//r.Dump("Obfuscated test image")
//rBack.Dump("Reverted back")

open System.IO
let source_path = Path.GetDirectoryName (Util.CurrentQueryPath)
let sourceImage = Path.Combine([|source_path; "mush.png"|])
let destinationImage = Path.Combine([|source_path; "mush_fixed.png"|])

open SkiaSharp
let testLoadAndSave () =
    use fs = File.OpenRead(sourceImage)
    use ms = new SKManagedStream(fs)
    use bitmap = SKBitmap.Decode(ms)
    
    let mutable pixels = bitmap.Pixels
    pixels.Length.Dump()
    
    let chunkedPixels = 
        pixels
        |> Array.map (fun x -> (int x.Red, int x.Green, int x. Blue))
        |> Array.toList
        |> rBackImage
        |> List.toArray
        |> Array.map (fun (r, g, b) -> new SKColor(byte r, byte g, byte b))
        
    bitmap.Pixels <- chunkedPixels
    
    use imageStreamWrite = new FileStream(destinationImage,FileMode.Create, FileAccess.Write)
    use d = SKImage.FromBitmap(bitmap).Encode(SKEncodedImageFormat.Png, 100);
    d.SaveTo(imageStreamWrite)
    Dump("Write ok")
    ()
    
testLoadAndSave ()

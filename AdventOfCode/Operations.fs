module Operations

open System.IO
open System.Reflection

let inputFromResource path =
    use stream = 
        Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream path
    use reader = new StreamReader(stream)
    let input = reader.ReadToEnd()
    printfn "Input:"
    printfn "%s" input
    printfn ""
    input

open System.Security.Cryptography
open System.Text

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let inline asSecond first second = second, first
let inline asFirst first second = first, second


open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
    
    
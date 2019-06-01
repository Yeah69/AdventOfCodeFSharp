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
    
type Integer = int
type Long = int64
type Byte = byte
    
let (|Integer|_|) text =
    let (isSuccess, number) = Integer.TryParse text
    if isSuccess then Some(number)
    else None
    
let identity x = x

let Array2Dflatten (A:'a[,]) = A |> Seq.cast<'a>

let Array2DgetColumn c (A:_[,]) =
    Array2Dflatten A.[c..c,*] |> Seq.toArray

let Array2DgetRow r (A:_[,]) =
    Array2Dflatten A.[*,r..r] |> Seq.toArray  
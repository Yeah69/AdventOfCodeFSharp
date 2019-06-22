module Operations

open System
open System.IO
open System.Reflection
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions

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


let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let inline asSecond first second = second, first
let inline asFirst first second = first, second

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
    
let alwaysTrue _ = true
let alwaysFalse _ = false

let equal element0 element1 = element0 = element1

let scramble (rnd:Random) (sqn : seq<'T>) = 
    let rec scramble2 (sqn : seq<'T>) = 
        /// Removes an element from a sequence.
        let remove n sqn = sqn |> Seq.filter (fun x -> x <> n)
 
        seq {
            let x = sqn |> Seq.item (rnd.Next(0, sqn |> Seq.length))
            yield x
            let sqn' = remove x sqn
            if not (sqn' |> Seq.isEmpty) then
                yield! scramble2 sqn'
        }
    if sqn |> Seq.isEmpty then Seq.empty else scramble2 sqn

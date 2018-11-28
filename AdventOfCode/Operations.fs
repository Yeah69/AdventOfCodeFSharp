module Operations

open System.IO
open System.Reflection

let inputFromResource path =
    use stream = 
        Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(path)
    use reader = new StreamReader(stream)
    let input = reader.ReadToEnd()
    printfn "Input:"
    printfn "%s" input
    printfn ""
    input
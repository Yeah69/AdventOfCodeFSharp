module Binary
    open System.Text.RegularExpressions

    type Integer = int32

    let identity x = x

    let addr a b c (registers: int[]) =
        let a', b' = registers.[a], registers.[b]
        (c, a' + b') ||> Array.set registers

    let addi a b c (registers: int[]) =
        let a' = registers.[a]
        (c, a' + b) ||> Array.set registers

    let mulr a b c (registers: int[]) =
        let a', b' = registers.[a], registers.[b]
        (c, a' * b') ||> Array.set registers

    let muli a b c (registers: int[]) =
        let a' = registers.[a]
        (c, a' * b) ||> Array.set registers

    let banr a b c (registers: int[]) =
        let a', b' = registers.[a], registers.[b]
        (c, a' &&& b') ||> Array.set registers

    let bani a b c (registers: int[]) =
        let a' = registers.[a]
        (c, a' &&& b) ||> Array.set registers

    let borr a b c (registers: int[]) =
        let a', b' = registers.[a], registers.[b]
        (c, a' ||| b') ||> Array.set registers

    let bori a b c (registers: int[]) =
        let a' = registers.[a]
        (c, a' ||| b) ||> Array.set registers

    let setr a _ c (registers: int[]) =
        let a' = registers.[a]
        (c, a') ||> Array.set registers

    let seti a _ c (registers: int[]) =
        (c, a) ||> Array.set registers

    let gtir a b c (registers: int[]) =
        let b' = registers.[b]
        (c, if a > b' then 1 else 0) ||> Array.set registers

    let gtri a b c (registers: int[]) =
        let a' = registers.[a]
        (c, if a' > b then 1 else 0) ||> Array.set registers

    let gtrr a b c (registers: int[]) =
        let a', b' = registers.[a], registers.[b]
        (c, if a' > b' then 1 else 0) ||> Array.set registers

    let eqir a b c (registers: int[]) =
        let b' = registers.[b]
        (c, if a = b' then 1 else 0) ||> Array.set registers

    let eqri a b c (registers: int[]) =
        let a' = registers.[a]
        (c, if a' = b then 1 else 0) ||> Array.set registers

    let eqrr a b c (registers: int[]) =
        let a', b' = registers.[a], registers.[b]
        (c, if a' = b' then 1 else 0) ||> Array.set registers

    let (| Instruction | _ |) str =
        match str with
        | "addr" -> Some addr
        | "addi" -> Some addi
        | "mulr" -> Some mulr
        | "muli" -> Some muli
        | "banr" -> Some banr
        | "bani" -> Some bani
        | "borr" -> Some borr
        | "bori" -> Some bori
        | "setr" -> Some setr
        | "seti" -> Some seti
        | "gtir" -> Some gtir
        | "gtri" -> Some gtri
        | "gtrr" -> Some gtrr
        | "eqir" -> Some eqir
        | "eqri" -> Some eqri
        | "eqrr" -> Some eqrr
        | _ -> None

    let getInstructionPointerIndex (line:string) = Integer.Parse (line.Chars 4 |> string)

    let loadInstructions lines =
        lines
        |> Seq.map (fun line -> 
            let mat = Regex.Match(line, "(.+) (\d+) (\d+) (\d+)")
            let opcode, a, b, c = mat.Groups.[1].Value, Integer.Parse mat.Groups.[2].Value, Integer.Parse mat.Groups.[3].Value, Integer.Parse mat.Groups.[4].Value
            match opcode with
            | Instruction inst -> Some (inst a b c)
            | _ -> None)
        |> Seq.choose identity
        |> Seq.toArray
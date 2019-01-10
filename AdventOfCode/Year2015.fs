module Year2015

open Domain
open Operations

module Day1 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.01.txt"
        let result1 = input.ToCharArray() 
                     |> Array.fold (fun state current -> 
                        match current with 
                        | '(' -> state + 1
                        | ')' -> state - 1
                        | _ -> state) 0
        let result2 = input.ToCharArray() 
                      |> Seq.ofArray
                      |> Seq.scan (fun state current -> 
                        match current with 
                        | '(' -> state + 1
                        | ')' -> state - 1
                        | _ -> state) 0
                      |> Seq.mapi (fun index current -> match current with -1 -> index | _ -> -1)
                      |> Seq.skipWhile (fun i -> i = -1)
                      |> Seq.head
        { First = result1.ToString(); Second = result2.ToString() }

module Day2 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.02.txt"

        let sequence = input.Split('\n')
                      |> Seq.ofArray
                      |> Seq.map (fun line -> line.TrimEnd('\r'))
                      |> Seq.map (fun line -> line.Split('x')
                                              |> Array.map (fun numberAsText -> match System.Int32.TryParse(numberAsText) with
                                                                                | true, number -> number
                                                                                | _ -> 0))

        let wrappingPaper = 
            sequence
            |> Seq.fold (fun state current -> 
                let numbers = [| current.[0] * current.[1]; current.[0] * current.[2]; current.[1] * current.[2] |]
                let min = numbers |> Array.min
                let currentSum = numbers |> Array.map (fun number -> number * 2) |> Array.sum
                state + currentSum + min) 0
        let ribbons = 
            sequence
            |> Seq.fold (fun state current -> 
                let sides = current |> Array.sort |> Array.take 2 |> Array.map (fun n -> n + n) |> Array.sum
                let bow = current.[0] * current.[1] * current.[2]
                state + sides + bow) 0
        { First = wrappingPaper.ToString(); Second = ribbons.ToString() }

module Day3 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.03.txt"

        let parseDirections (x, y) c =
            match c with
            | '^' -> (x, y + 1)
            | 'v' -> (x, y - 1)
            | '<' -> (x - 1, y)
            | '>' -> (x + 1, y)
            | _ -> (x, y)

        let sequence = input.ToCharArray()
                      |> Seq.ofArray
                      |> Seq.scan parseDirections (0,0)

        let sequenceEven = input.ToCharArray()
                          |> Seq.ofArray
                          |> Seq.mapi (fun i c -> i, c)
                          |> Seq.filter (fun (i, _) -> i % 2 = 0)
                          |> Seq.map (fun (_, c) -> c)
                          |> Seq.scan parseDirections (0,0)

        let sequenceOdd = input.ToCharArray()
                          |> Seq.ofArray
                          |> Seq.mapi (fun i c -> i, c)
                          |> Seq.filter (fun (i, _) -> i % 2 = 1)
                          |> Seq.map (fun (_, c) -> c)
                          |> Seq.scan parseDirections (0,0)

        let housesSum = 
            seq { yield (0, 0) }
            |> Seq.append sequence
            |> Seq.distinct
            |> Seq.sumBy (fun _ -> 1)
            
        let housesSharedWork = 
            seq { yield (0, 0) }
            |> Seq.append sequenceEven
            |> Seq.append sequenceOdd
            |> Seq.distinct
            |> Seq.sumBy (fun _ -> 1)


        { First = housesSum.ToString(); Second = housesSharedWork.ToString() }

module Day4 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.04.txt"

        let sequence =
            seq { while true do yield input }
            |> Seq.mapi (fun i input -> i, input + (i |> string))
            |> Seq.skip 1 
            |> Seq.map (fun (i, message) -> i, System.Text.Encoding.ASCII.GetBytes(message))
            |> Seq.map (fun (i, message) -> i, Operations.md5 message)
            
        let (i1, _) = sequence 
                      |> Seq.filter (fun (_, hash) -> hash.StartsWith("00000")) 
                      |> Seq.head

        let (i2, _) = sequence 
                      |> Seq.filter (fun (_, hash) -> hash.StartsWith("000000")) 
                      |> Seq.head


        { First = i1.ToString(); Second = i2.ToString() }

module Day5 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.05.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let result1 =
            lines
            |> Seq.ofArray
            |> Seq.where (fun line ->
                let chars = line.ToCharArray()
                let doubles = 
                    chars 
                    |> Seq.ofArray 
                    |> Seq.pairwise 
                    |> Seq.exists (fun (c1, c2) -> c1 = c2) 
                let valid = 
                    chars 
                    |> Seq.ofArray 
                    |> Seq.pairwise 
                    |> Seq.map (fun (c1, c2) -> sprintf "%c%c" c1 c2) 
                    |> Seq.forall (fun pair -> pair <> "ab" && pair <> "cd" && pair <> "pq" && pair <> "xy")
                let vowels = 
                    chars 
                    |> Seq.ofArray 
                    |> Seq.where (fun c -> c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
                    |> Seq.length >= 3
                doubles && vowels && valid)
            |> Seq.length
            
        let result2 =
            lines
            |> Seq.ofArray
            |> Seq.where (fun line ->
                let chars = line.ToCharArray()
                
                let condition1 =
                    seq{ 2 .. chars.Length - 1}
                    |> Seq.exists (fun i -> chars.[i] = chars.[i - 2])
                let condition2 =
                    chars
                    |> Seq.ofArray
                    |> Seq.pairwise
                    |> Seq.map (fun (c1, c2) -> sprintf "%c%c" c1 c2)
                    |> Seq.exists (fun text ->
                        line.LastIndexOf(text) - line.IndexOf(text) >= 2)

                condition1 && condition2)
            |> Seq.length

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day6 =
    open System.Text.RegularExpressions

    type Integer = int

    type Action = 
        | TurnOn of From : int*int * To : int*int
        | TurnOff of From : int*int * To : int*int
        | Toggle of From : int*int * To : int*int

    let (| Action | _ |) text =
        let matchTurnOn = Regex.Match(text, "turn on (\d+),(\d+) through (\d+),(\d+)")
        let matchTurnOff = Regex.Match(text, "turn off (\d+),(\d+) through (\d+),(\d+)")
        let matchToggle = Regex.Match(text, "toggle (\d+),(\d+) through (\d+),(\d+)")
        seq { yield TurnOn (0,0,0,0), matchTurnOn
              yield TurnOff (0,0,0,0), matchTurnOff
              yield Toggle (0,0,0,0), matchToggle }
        |> Seq.filter (fun (_, mat) -> mat.Success)
        |> Seq.tryHead
        |> Option.map (fun (action, mat) ->
            let from = mat.Groups.[1].Value |> Integer.Parse, mat.Groups.[2].Value |> Integer.Parse
            let tot = mat.Groups.[3].Value |> Integer.Parse, mat.Groups.[4].Value |> Integer.Parse
            match action with
            | TurnOn _->  TurnOn (from |> fst, from |> snd, tot |> fst, tot |> snd)
            | TurnOff _->  TurnOff (from |> fst, from |> snd, tot |> fst, tot |> snd)
            | Toggle _->  Toggle (from |> fst, from |> snd, tot |> fst, tot |> snd))

    let doAction (turnOn, turnOff, toggle) field action =
        let action' pos =
            match action with
            | TurnOn _ -> turnOn pos field
            | TurnOff _ -> turnOff pos field
            | Toggle _ -> toggle pos field
        match action with
        | TurnOn (fromX, fromY, toX, toY) 
        | TurnOff (fromX, fromY, toX, toY) 
        | Toggle (fromX, fromY, toX, toY) ->
            seq {fromX .. toX }
            |> Seq.allPairs { fromY .. toY}
            |> Seq.iter action'

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.06.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let field1 = Array2D.create 1000 1000 false

        let funcs1 = 
            (fun (x, y) field -> (x, y, true) |||> Array2D.set field), 
            (fun (x, y) field -> (x, y, false) |||> Array2D.set field), 
            (fun (x, y) field -> (x, y, (x, y) ||> Array2D.get field |> not) |||> Array2D.set field)

        lines
        |> Seq.ofArray
        |> Seq.choose (fun line -> match line with | Action action -> Some action | _ -> None)
        |> Seq.iter (doAction funcs1 field1)

        let result1 = 
            seq { 0 .. 999} 
            |> Seq.allPairs (seq { 0 .. 999 }) 
            |> Seq.filter (fun pos -> pos ||> Array2D.get field1)
            |> Seq.length

        let field2 = Array2D.create 1000 1000 0

        let funcs2 = 
            (fun (x, y) field -> (x, y, ((x, y) ||> Array2D.get field) + 1) |||> Array2D.set field), 
            (fun (x, y) field -> 
                let value = (x, y) ||> Array2D.get field
                (x, y, if value > 0 then value - 1 else value) |||> Array2D.set field),
            (fun (x, y) field -> (x, y, ((x, y) ||> Array2D.get field) + 2) |||> Array2D.set field)

        lines
        |> Seq.ofArray
        |> Seq.choose (fun line -> match line with | Action action -> Some action | _ -> None)
        |> Seq.iter (doAction funcs2 field2)

        let result2 = 
            seq { 0 .. 999} 
            |> Seq.allPairs (seq { 0 .. 999 }) 
            |> Seq.sumBy (fun pos -> pos ||> Array2D.get field2)

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
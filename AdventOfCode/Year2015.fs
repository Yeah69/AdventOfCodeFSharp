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
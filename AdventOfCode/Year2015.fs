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
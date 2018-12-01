module Year2018

open Domain
open Operations

module Day1 =
    open System.Collections.Generic

    type Integer = int32
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.01.txt"
        let sequence = 
            input.Split("\r\n")
            |> Seq.ofArray
            |> Seq.choose (fun line -> 
                match Integer.TryParse line with
                | (true, value) -> Some value
                | _ -> None)

        let sequenceOfSequences =
            seq { while true do yield! sequence }
        let result1 = 
            sequence
            |> Seq.sum
        let result2 =
            sequenceOfSequences
            |> Seq.scan (fun ((listSoFar: HashSet<int>), sumSoFar, _) i -> 
                let sum = sumSoFar + i
                match listSoFar.Contains(sum) with
                | true ->
                    listSoFar.Add sum |> ignore
                    listSoFar, sum, true
                | false -> 
                    listSoFar.Add sum |> ignore
                    listSoFar, sum, false) (new HashSet<int>(), 0, false)
            |> Seq.filter (fun (_, _, isDuplicate) -> isDuplicate)
            |> Seq.map (fun (_, sum, _) -> sum)
            |> Seq.head
        { First = result1.ToString(); Second = result2.ToString() }
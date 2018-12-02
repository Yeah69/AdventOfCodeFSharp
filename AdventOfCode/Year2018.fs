module Year2018

open Domain
open Operations

module Day1 =
    open System.Collections.Generic

    type Integer = int32
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.01.txt"
        let frequencyChanges = 
            input.Split("\r\n")
            |> Array.choose (fun line -> 
                match Integer.TryParse line with
                | (true, value) -> Some value
                | _ -> None)
        
        let result1 = 
            frequencyChanges
            |> Array.sum
        let result2 =
            Seq.unfold (fun i -> Some (frequencyChanges.[i % frequencyChanges.Length] , i + 1)) 0
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

module Day2 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.02.txt"
        let lines = input.Split("\r\n")
        let (duplesCount, triplesCount) = 
            lines
            |> Seq.ofArray
            |> Seq.map (fun line ->
                let counts = line.ToCharArray() |> Array.countBy (fun c -> c)
                let duplicateExists = counts |> Array.exists (fun (_, i) -> i = 2)
                let triplicateExists = counts |> Array.exists (fun (_, i) -> i = 3)
                match (duplicateExists, triplicateExists) with
                | true, true -> (1, 1)
                | true, false -> (1, 0)
                | false, true -> (0, 1)
                | false, false | _ -> (0, 0))
            |> Seq.fold (fun (currentDuples, currentTriples) (duples, triples) ->
                (currentDuples + duples, currentTriples + triples)) (0, 0)
        let result = 
            lines
            |> Seq.ofArray
            |> Seq.allPairs lines
            |> Seq.map (fun (first, second) ->
                (first.ToCharArray() |> Seq.ofArray)
                |> Seq.fold2 (fun (s, i) c1 c2 ->
                    if c1 = c2 then (s + c1.ToString(), i)
                    else (s, i + 1)) ("", 0) (second.ToCharArray() |> Seq.ofArray))
            |> Seq.filter (fun (_, i) -> i = 1)
            |> Seq.map (fun (s, _) -> s)
            |> Seq.head
        { First = (duplesCount * triplesCount).ToString(); Second = result }
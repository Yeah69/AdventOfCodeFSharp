module Year2018

open Domain
open Operations

module Day1 =
    type Integer = int32
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.01.txt"
        let frequencyChanges = 
            input.Split(System.Environment.NewLine)
            |> Array.choose (fun line -> 
                match Integer.TryParse line with
                | (true, value) -> Some value
                | _ -> None)
        
        let result1 = 
            frequencyChanges
            |> Array.sum
        let result2 =
            Seq.unfold (fun i -> Some (frequencyChanges.[i % frequencyChanges.Length] , i + 1)) 0
            |> Seq.scan (fun ((setSoFar: Set<int>), sumSoFar, _) i -> 
                let sum = sumSoFar + i
                if setSoFar.Contains sum then setSoFar.Add sum, sum, true
                else setSoFar.Add sum, sum, false) (Set.empty, 0, false)
            |> Seq.filter (fun (_, _, isDuplicate) -> isDuplicate)
            |> Seq.map (fun (_, sum, _) -> sum)
            |> Seq.head
        { First = result1.ToString(); Second = result2.ToString() }

module Day2 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.02.txt"
        let lines = input.Split(System.Environment.NewLine)
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

module Day3 =
    open System.Text.RegularExpressions
    type Box = { Id: int; X: int; Y: int; Width: int; Heigth: int }
    type Integer = int
    
    let flat2Darray array2D = 
            seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                        for y in [0..(Array2D.length2 array2D) - 1] do 
                            yield array2D.[x, y] }
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.03.txt"
        let lines = input.Split(System.Environment.NewLine)
        let boxes = lines |> Array.choose (fun line -> 
            let matchResult = Regex.Match(line, "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
            match matchResult.Success with
            | true -> 
                Some ({ Id = Integer.Parse(matchResult.Groups.[1].Value); 
                        X = Integer.Parse(matchResult.Groups.[2].Value); 
                        Y = Integer.Parse(matchResult.Groups.[3].Value); 
                        Width = Integer.Parse(matchResult.Groups.[4].Value); 
                        Heigth = Integer.Parse(matchResult.Groups.[5].Value)})
            | false -> None)
        let field : int[,] = Array2D.zeroCreate 1000 1000;
        let set = Set.empty.Add(-1)
        let set = 
            boxes
            |> Array.fold (fun set box -> 
                seq { box.X .. (box.X + box.Width - 1)}
                |> Seq.allPairs (seq { box.Y .. (box.Y + box.Heigth - 1)})
                |> Seq.fold (fun (set: Set<int>) (x, y) -> 
                    if (Array2D.get field x y) = 0 then 
                        (Array2D.set field x y box.Id)
                        set
                    else 
                        let set = set |> Set.add (Array2D.get field x y)
                        (Array2D.set field x y -1)
                        set |> Set.add box.Id) set) set
        let count = flat2Darray field
                    |> Seq.countBy (fun i -> i)
                    |> Seq.filter (fun (key, _) -> key = 2)
                    |> Seq.map (fun (_, value) -> value)
                    |> Seq.head
        let id = 
            boxes 
            |> Seq.ofArray 
            |> Seq.filter (fun box -> set.Contains(box.Id) = false) 
            |> Seq.map (fun box -> box.Id) 
            |> Seq.head
        { First = count.ToString(); Second = id.ToString() }

module Day4 =
    
    open System;
    open System.Text.RegularExpressions

    type Integer = int

    type Event = 
        | BeginsDuty of int 
        | FallsAsleep
        | WakesUp

    type Log = { Timestamp: DateTime; Event: Event}

    let tryParseLog line =
        let matchResult = Regex.Match(line, "\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)")
        if matchResult.Success then
            let year = Integer.Parse(matchResult.Groups.[1].Value)
            let month = Integer.Parse(matchResult.Groups.[2].Value)
            let day = Integer.Parse(matchResult.Groups.[3].Value)
            let hour = Integer.Parse(matchResult.Groups.[4].Value)
            let minute = Integer.Parse(matchResult.Groups.[5].Value)
            let description = matchResult.Groups.[6].Value
            let timestamp = DateTime (year, month, day, hour, minute, 0)
            let matchResult = Regex.Match(description, "Guard #(\d+) begins shift")
            let event = 
                if matchResult.Success then
                    let guardNumber = Integer.Parse(matchResult.Groups.[1].Value)
                    Some(BeginsDuty guardNumber)
                else if description = "falls asleep" then Some FallsAsleep
                else if description = "wakes up" then Some WakesUp
                else None
            match event with
            | Some event -> Some({ Timestamp = timestamp; Event = event })
            | None -> None
        else None

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.04.txt"
        let lines = input.Split(System.Environment.NewLine)

        let minutesOfGuards =
            lines
            |> Seq.ofArray
            |> Seq.choose tryParseLog
            |> Seq.sortBy (fun log -> log.Timestamp)
            |> Seq.scan (fun (number, _) log -> 
                match log.Event with
                | BeginsDuty guard -> (guard, None)
                | FallsAsleep | WakesUp -> (number, Some log)) (0, None)
            |> Seq.choose (fun (number, log) ->
                match log with
                | Some log -> Some (number, log)
                | None -> None)
            |> Seq.pairwise
            |> Seq.collect (fun ((number1, log1), (number2, log2)) ->
                match number1, log1.Event, number2, log2.Event with
                | number1, FallsAsleep, number2, WakesUp when number1 = number2 ->
                    seq {for i in log1.Timestamp.Minute .. log2.Timestamp.Minute - 1 do yield (number1, i)}
                | _, _, _, _ -> Seq.empty)
            |> Seq.toArray

        let (maxGuard1, _) =
            minutesOfGuards
            |> Array.countBy (fun (guard, _) -> guard)
            |> Array.maxBy (fun (_, value) -> value)
            
        let (maxMinute1, _) =
            minutesOfGuards
            |> Seq.ofArray
            |> Seq.filter (fun (guard, _) -> guard = maxGuard1)
            |> Seq.countBy (fun (_, minute) -> minute)
            |> Seq.maxBy (fun (_, value) -> value)

        let ((maxGuard2, maxMinute2), _) =
            minutesOfGuards
            |> Seq.ofArray
            |> Seq.countBy (fun (guard, minute) -> (guard, minute))
            |> Seq.maxBy (fun (_, value) -> value)

        
        { First = (maxGuard1 * maxMinute1).ToString(); Second = (maxGuard2 * maxMinute2).ToString() }

module Day5 =

    type Character = char

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.05.txt"
        
        let firstPair (s: string) =
            s.ToCharArray() 
            |> Seq.ofArray 
            |> Seq.mapi (fun i c -> (i, c))
            |> Seq.pairwise
            |> Seq.filter (fun ((_, c1), (_, c2)) -> 
                Character.ToUpper(c1) = Character.ToUpper(c2) && ((Character.IsUpper(c1) && Character.IsLower(c2)) || (Character.IsUpper(c2) && Character.IsLower(c1))))
            |> Seq.map (fun ((i1, _), (_, _)) -> i1)
            |> Seq.tryHead

        let countOfSolution input = 
            let lastString = 
                Seq.unfold (fun s ->
                    let nextPair = firstPair s
                    match nextPair with
                    | Some i ->
                        let newS = s.Remove(i, 2)
                        Some(newS, newS)
                    | None -> None) input
                |> Seq.last
            lastString.Length

        let result1 = countOfSolution input

        let result2 = 
            seq{ 'a' .. 'z' }
            |> Seq.map (fun c -> 
                let inputWithoutChar = input.Replace(c.ToString(),"").Replace(Character.ToUpper(c).ToString(),"")
                countOfSolution inputWithoutChar)
            |> Seq.min

        { First = result1.ToString(); Second = result2.ToString() }
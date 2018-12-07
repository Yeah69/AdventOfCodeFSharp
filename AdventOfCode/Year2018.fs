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
    open System

    type Character = char

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.05.txt"
        let inputAsMemory = input.AsMemory()

        let matchCharCombo c1 c2 =
            Char.IsLower(c1) && Char.IsUpper(c2) && c1 = Char.ToLower(c2) || Char.IsUpper(c1) && Char.IsLower(c2) && c1 = Char.ToUpper(c2)

        let splitIntoMemories (memory: ReadOnlyMemory<char>) =
            Seq.unfold (fun (currentSpan: ReadOnlyMemory<char>) ->
                if currentSpan.Length = 0 then
                    None
                else
                    let maybeIndex = 
                        seq { 0 .. (currentSpan.Length - 2)}
                        |> Seq.filter (fun i -> matchCharCombo (currentSpan.Span.Item i) (currentSpan.Span.Item (i + 1)))
                        |> Seq.tryHead
                    match maybeIndex with
                    | Some i -> Some (currentSpan.Slice(0, i), currentSpan.Slice(i + 2))
                    | None -> Some (currentSpan, ReadOnlyMemory.Empty)) (memory)
            |> Seq.filter (fun m -> m.Length > 0)

        let iteration listToIterate =
            let mutable flag = false
            let newList = 
                Seq.unfold (fun (list : ReadOnlyMemory<char> list) ->
                    match list with
                    | first::second::tail -> 
                        if first.Length > 0 && second.Length > 0 && matchCharCombo (first.Span.Item (first.Length - 1)) (second.Span.Item 0) then
                            flag <- true
                            Some (first.Slice(0, first.Length - 1), second.Slice(1)::tail)
                        else
                            Some (first, second::tail)
                    | [ one ] -> Some (one, [])
                    | [] -> None) listToIterate
                |> Seq.filter (fun m -> m.Length > 0)
                |> Seq.toList
            newList, flag

        let result1 =
            Seq.unfold (fun (list : ReadOnlyMemory<char> list, flag) ->
                if flag then
                    let (resList, resFlag) = iteration list
                    Some (resList, (resList, resFlag))
                else None) ((splitIntoMemories inputAsMemory |> Seq.toList), true)
            |> Seq.last
            |> Seq.sumBy (fun mem -> mem.Length)

        let splitByChar c (memory: ReadOnlyMemory<char>) =
            let upper = Char.ToUpper(c)
            Seq.unfold (fun (state: ReadOnlyMemory<char>) ->
                if state.Length = 0 then 
                    None
                else
                    let i = state.Span.IndexOfAny(c, upper)
                    if i = -1 then
                        Some(state, ReadOnlyMemory.Empty)
                    else
                        Some(state.Slice(0, i), state.Slice(i + 1))) memory
            |> Seq.filter (fun memory -> memory.Length > 0)
        
        let result2 =
            seq { 'a' .. 'z' }
            |> Seq.map (fun c -> 
                let splitted = inputAsMemory |> splitByChar c |> Seq.collect (fun memory -> splitIntoMemories memory) |> Seq.toList
                Seq.unfold (fun (list : ReadOnlyMemory<char> list, flag) ->
                    if flag then
                        let (resList, resFlag) = iteration list
                        Some (resList, (resList, resFlag))
                    else None) (splitted, true)
                |> Seq.last
                |> Seq.sumBy (fun mem -> mem.Length))
            |> Seq.min

        { First = result1.ToString(); Second = result2.ToString() }

module Day6 =
    
    open System;
    open System.Text.RegularExpressions

    type Integer = int

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.06.txt"
        let lines = input.Split(System.Environment.NewLine)
        let points = 
            lines 
            |> Array.choose (fun line -> 
                let matchResult = Regex.Match(line, "(\d+), (\d+)")
                match matchResult.Success with
                | true -> 
                    Some ( Integer.Parse(matchResult.Groups.[1].Value), 
                           Integer.Parse(matchResult.Groups.[2].Value))
                | false -> None)
        
        let distanceMatrix =
            seq { 0 .. 500} 
            |> Seq.allPairs (seq { 0 .. 500 })
            |> Seq.map (fun (x, y) -> 
                let bestDistances = 
                    points 
                    |> Seq.ofArray 
                    |> Seq.mapi (fun i (pX, pY) -> (i, Math.Abs(pX - x) + Math.Abs(pY-y))) 
                    |> Seq.sortBy (fun (_, d) -> d) 
                    |> Seq.take 2 
                    |> Seq.toList

                match bestDistances with
                | [ (i1, d1); (_, d2) ] when d1 < d2 && (i1 = 7 || i1 = 12 || i1 = 21 || i1 = 31 || i1 = 43 || i1 = 44) -> ((x, y), i1)
                | [ (i1, d1); (_, d2) ] when d1 < d2 -> ((x, y), i1)
                | _ -> ((x, y), -1))
            |> Seq.toList

        let setOfInfinites =
            distanceMatrix 
            |> Seq.ofList 
            |> Seq.filter (fun ((x, y), _) -> x = 0 || x = 500 || y = 0 || y = 500) 
            |> Seq.map (fun (_, i) -> i) 
            |> Set.ofSeq
        let setOfInfinites = setOfInfinites |> Set.add -1

        let maxFinite =
            distanceMatrix
            |> Seq.ofList
            |> Seq.filter (fun (_, i) -> setOfInfinites |> Set.contains i |> not)
            |> Seq.countBy (fun (_, i) -> i)
            |> Seq.map (fun (_, count) -> count)
            |> Seq.max

        let result2 =
            seq { 0 .. 500} 
            |> Seq.allPairs (seq { 0 .. 500 })
            |> Seq.filter (fun (x, y) -> 
                let accumulatedDistance = 
                    points 
                    |> Seq.ofArray 
                    |> Seq.map (fun (pX, pY) -> (Math.Abs(pX - x) + Math.Abs(pY-y))) 
                    |> Seq.sum
                accumulatedDistance < 10000)
            |> Seq.length

        { First = maxFinite.ToString(); Second = result2.ToString() }

module Day7 =
    
    open System.Text.RegularExpressions
    open System.Collections.Generic

    type Character = char

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.07.txt"
        let lines = input.Split(System.Environment.NewLine)
        let instructions = 
            lines 
            |> Array.choose (fun line -> 
                let matchResult = Regex.Match(line, "Step (.) must be finished before step (.) can begin.")
                match matchResult.Success with
                | true -> 
                    Some ( Character.Parse(matchResult.Groups.[1].Value), 
                           Character.Parse(matchResult.Groups.[2].Value))
                | false -> None)

        let predecessors =
            instructions
            |> Seq.ofArray
            |> Seq.groupBy (fun (_, n2) -> n2)
            |> Seq.map (fun (key, insts) -> (key, (insts |> Seq.map (fun (n1, _) -> n1) |> Seq.toList)))
            |> Map.ofSeq 

        let allPredsFinished notFinished n =
            if predecessors.ContainsKey n |> not then
                true
            else
                predecessors.Item n 
                |> Seq.ofList 
                |> Seq.forall (fun pred -> (notFinished |> Set.contains pred |> not))
                
        let result1 =
            seq { 
                let mutable notFinished = seq { 'A' .. 'Z' } |> Set.ofSeq
                while notFinished.Count > 0 do
                    let current = 
                        notFinished 
                        |> Set.toSeq 
                        |> Seq.sort 
                        |> Seq.filter (fun n -> allPredsFinished notFinished n) 
                        |> Seq.head
                    yield current
                    notFinished <- notFinished |> Set.remove current}
            |> Seq.toArray

        let result1 = new string(result1)
            
        let workers = Array.init 5 (fun i -> '.')
        let mutable notFinished = seq { 'A' .. 'Z' } |> Set.ofSeq
        let queue = Queue<(int * int * char)>()

        queue.Enqueue (0, -1, '.')
        let mutable lastTime = 0
        while queue.Count > 0 do
            let (currentTime, idleWorker, finishedInst) = (queue.Dequeue())
            lastTime <- currentTime
            if idleWorker <> -1 then
                workers.[idleWorker] <- '.'
            if finishedInst <> '.' then
                notFinished <- notFinished |> Set.remove finishedInst
            let current = 
                notFinished 
                |> Set.toSeq 
                |> Seq.sort 
                |> Seq.filter (fun n -> (workers |> Array.contains n |> not) && allPredsFinished notFinished n)
            seq { 0 .. 4 } 
            |> Seq.filter (fun i -> workers.[i] = '.') 
            |> Seq.iter2 (fun (c: char) i -> 
                workers.[i] <- c
                queue.Enqueue (currentTime + (int c - int 'A' + 61) , i, c)) current |> ignore 

        { First = result1.ToString(); Second = lastTime.ToString() }
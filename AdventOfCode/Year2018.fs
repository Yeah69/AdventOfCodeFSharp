﻿module Year2018

open Domain
open Operations

module Day1 =
    type Integer = int32
    type String = string

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.01.txt"
        let frequencyChanges = 
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
            |> Array.choose (fun line -> 
                match Integer.TryParse line with
                | true, value -> Some value
                | _ -> None)
        let result1 = 
            frequencyChanges
            |> Array.sum
        let result2 =
            ((fun i -> Some (frequencyChanges[i % frequencyChanges.Length] , i + 1)), 0)
            ||> Seq.unfold 
            |> asFirst (Set.empty, 0, false)
            ||> Seq.scan (fun (setSoFar, sumSoFar, _) i -> 
                let sum = sumSoFar + i
                setSoFar.Add sum, sum, setSoFar.Contains sum)
            |> Seq.filter (fun (_, _, isDuplicate) -> isDuplicate)
            |> Seq.map (fun (_, sum, _) -> sum)
            |> Seq.head
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day2 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.02.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        let duplesCount, triplesCount = 
            lines
            |> Seq.ofArray
            |> Seq.map (fun line ->
                let counts = line.ToCharArray() |> Array.countBy (fun c -> c)
                let duplicateExists = counts |> Array.exists (fun (_, i) -> i = 2)
                let triplicateExists = counts |> Array.exists (fun (_, i) -> i = 3)
                match (duplicateExists, triplicateExists) with
                | true, true -> 1, 1
                | true, false -> 1, 0
                | false, true -> 0, 1
                | false, false | _ -> 0, 0)
            |> asFirst (0, 0)
            ||> Seq.fold (fun (currentDuples, currentTriples) (duples, triples) ->
                (currentDuples + duples, currentTriples + triples))
        let result1 = duplesCount * triplesCount
        let result2 = 
            lines
            |> Seq.ofArray
            |> Seq.allPairs lines
            |> Seq.map (fun (first, second) ->
                (("", 0), (first.ToCharArray() |> Seq.ofArray), (second.ToCharArray() |> Seq.ofArray))
                |||> Seq.fold2 (fun (s, i) c1 c2 ->
                    if c1 = c2 then s + c1.ToString(), i
                    else s, i + 1) )
            |> Seq.filter (fun (_, i) -> i = 1)
            |> Seq.map (fun (s, _) -> s)
            |> Seq.head
        { First = sprintf "%d" result1; Second = result2 }

module Day3 =
    open System.Text.RegularExpressions
    type Box = { Id: int; X: int; Y: int; Width: int; Heigth: int }
    type Integer = int
    
    let flat2Darray array2D = 
            seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                        for y in [0..(Array2D.length2 array2D) - 1] do 
                            yield array2D[x, y] }
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.03.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        let boxes = lines |> Array.choose (fun line -> 
            let matchResult = Regex.Match(line, "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
            match matchResult.Success with
            | true -> 
                Some { Id = Integer.Parse(matchResult.Groups[1].Value); 
                       X = Integer.Parse(matchResult.Groups[2].Value); 
                       Y = Integer.Parse(matchResult.Groups[3].Value); 
                       Width = Integer.Parse(matchResult.Groups[4].Value); 
                       Heigth = Integer.Parse(matchResult.Groups[5].Value)}
            | false -> None)
        let field : int[,] = Array2D.zeroCreate 1000 1000;
        let set = Set.empty.Add(-1)
        let set = 
            (set, boxes)
            ||> Array.fold (fun set box -> 
                seq { box.X .. (box.X + box.Width - 1)}
                |> Seq.allPairs (seq { box.Y .. (box.Y + box.Heigth - 1)})
                |> asFirst set
                ||> Seq.fold (fun set (x, y) -> 
                    if (Array2D.get field x y) = 0 then 
                        (Array2D.set field x y box.Id)
                        set
                    else 
                        let set = set |> Set.add (Array2D.get field x y)
                        (Array2D.set field x y -1)
                        set |> Set.add box.Id))
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
        { First = sprintf "%d" count; Second = sprintf "%d" id }

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
            let year = Integer.Parse(matchResult.Groups[1].Value)
            let month = Integer.Parse(matchResult.Groups[2].Value)
            let day = Integer.Parse(matchResult.Groups[3].Value)
            let hour = Integer.Parse(matchResult.Groups[4].Value)
            let minute = Integer.Parse(matchResult.Groups[5].Value)
            let description = matchResult.Groups[6].Value
            let timestamp = DateTime (year, month, day, hour, minute, 0)
            let matchResult = Regex.Match(description, "Guard #(\d+) begins shift")
            let event = 
                if matchResult.Success then
                    let guardNumber = Integer.Parse(matchResult.Groups[1].Value)
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
        let lines = input.Split([| Environment.NewLine |], StringSplitOptions.None)

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

        let maxGuard1, _ =
            minutesOfGuards
            |> Array.countBy fst
            |> Array.maxBy snd
            
        let maxMinute1, _ =
            minutesOfGuards
            |> Seq.ofArray
            |> Seq.filter (fun (guard, _) -> guard = maxGuard1)
            |> Seq.countBy snd
            |> Seq.maxBy snd

        let (maxGuard2, maxMinute2), _ =
            minutesOfGuards
            |> Seq.ofArray
            |> Seq.countBy identity
            |> Seq.maxBy snd

        let result1 = maxGuard1 * maxMinute1
        let result2 = maxGuard2 * maxMinute2
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day5 =
    open System

    type Character = char

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.05.txt"
        let inputAsMemory = input.AsMemory()

        let matchCharCombo c1 c2 =
            Char.IsLower(c1) && Char.IsUpper(c2) && c1 = Char.ToLower(c2) || Char.IsUpper(c1) && Char.IsLower(c2) && c1 = Char.ToUpper(c2)

        let splitIntoMemories (memory: ReadOnlyMemory<char>) =
            memory
            |> Seq.unfold (fun currentSpan ->
                if currentSpan.Length = 0 then
                    None
                else
                    let maybeIndex = 
                        seq { 0 .. (currentSpan.Length - 2)}
                        |> Seq.filter (fun i -> matchCharCombo (currentSpan.Span.Item i) (currentSpan.Span.Item (i + 1)))
                        |> Seq.tryHead
                    match maybeIndex with
                    | Some i -> Some (currentSpan.Slice(0, i), currentSpan.Slice(i + 2))
                    | None -> Some (currentSpan, ReadOnlyMemory.Empty))
            |> Seq.filter (fun m -> m.Length > 0)

        let iteration listToIterate =
            let mutable flag = false
            let newList = 
                listToIterate
                |> Seq.unfold (fun (list : ReadOnlyMemory<char> list) ->
                    match list with
                    | first::second::tail -> 
                        if first.Length > 0 && second.Length > 0 && matchCharCombo (first.Span.Item (first.Length - 1)) (second.Span.Item 0) then
                            flag <- true
                            Some (first.Slice(0, first.Length - 1), second.Slice(1)::tail)
                        else
                            Some (first, second::tail)
                    | [ one ] -> Some (one, [])
                    | [] -> None)
                |> Seq.filter (fun m -> m.Length > 0)
                |> Seq.toList
            newList, flag

        let result1 =
            ((splitIntoMemories inputAsMemory |> Seq.toList), true)
            |> Seq.unfold (fun (list : ReadOnlyMemory<char> list, flag) ->
                if flag then
                    let resList, resFlag = iteration list
                    Some (resList, (resList, resFlag))
                else None) 
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
                (splitted, true)
                |> Seq.unfold (fun (list, flag) ->
                    if flag then
                        let resList, resFlag = iteration list
                        Some (resList, (resList, resFlag))
                    else None)
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
        let lines = input.Split([| Environment.NewLine |], StringSplitOptions.None)
        let points = 
            lines 
            |> Array.choose (fun line -> 
                let matchResult = Regex.Match(line, "(\d+), (\d+)")
                match matchResult.Success with
                | true -> 
                    Some ( Integer.Parse(matchResult.Groups[1].Value), 
                           Integer.Parse(matchResult.Groups[2].Value))
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
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        let instructions = 
            lines 
            |> Array.choose (fun line -> 
                let matchResult = Regex.Match(line, "Step (.) must be finished before step (.) can begin.")
                match matchResult.Success with
                | true -> 
                    Some ( Character.Parse(matchResult.Groups[1].Value), 
                           Character.Parse(matchResult.Groups[2].Value))
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
            
        let workers = Array.init 5 (fun _ -> '.')
        let mutable notFinished = seq { 'A' .. 'Z' } |> Set.ofSeq
        let queue = Queue<int * int * char>()

        queue.Enqueue (0, -1, '.')
        let mutable lastTime = 0
        while queue.Count > 0 do
            let currentTime, idleWorker, finishedInst = queue.Dequeue()
            lastTime <- currentTime
            if idleWorker <> -1 then
                workers[idleWorker] <- '.'
            if finishedInst <> '.' then
                notFinished <- notFinished |> Set.remove finishedInst
            let current = 
                notFinished 
                |> Set.toSeq 
                |> Seq.sort 
                |> Seq.filter (fun n -> (workers |> Array.contains n |> not) && allPredsFinished notFinished n)
            seq { 0 .. 4 } 
            |> Seq.filter (fun i -> workers[i] = '.') 
            |> Seq.iter2 (fun (c: char) i -> 
                workers[i] <- c
                queue.Enqueue (currentTime + (int c - int 'A' + 61) , i, c)) current 

        { First = result1.ToString(); Second = lastTime.ToString() }

module Day8 =
    open System.Collections.Generic

    type StackNode = { Length: int; NodeCount: int; Start: int; MetadataCount: int; Value: int; Children: int[] }
    type Integer = int32

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.08.txt"
        let numbers = input.Split(' ') |> Array.map Integer.Parse

        let stack = Stack<StackNode>()
        stack.Push { Length = 2; NodeCount = numbers[0]; Start = 0; MetadataCount = numbers[1]; Value = 0; Children = [| |]}
        let mutable result1 = 0
        let mutable result2 = 0
        while stack.Count > 0 do
            let current = stack.Pop()
            if current.NodeCount > 0 then
                let current = { current with NodeCount = current.NodeCount - 1 }
                stack.Push current
                let nextStart = current.Start + current.Length
                let nextValue = 
                    if numbers[nextStart] = 0 then
                        seq { nextStart + 2 .. nextStart + 1 + numbers[nextStart + 1]} |> Seq.sumBy (fun i -> numbers[i])
                    else
                        0
                let next = { Length = 2; NodeCount = numbers[nextStart]; Start = nextStart; MetadataCount = numbers[nextStart + 1]; Value = nextValue; Children = [| |]}
                stack.Push next
            else
                let length = current.Length + current.MetadataCount
                result1 <- result1 + (seq { current.Start + current.Length .. current.Start + length - 1} |> Seq.sumBy (fun i -> numbers[i]))
                let currentValue =
                    if current.Length > 2 then
                        seq { current.Start + current.Length .. current.Start + length - 1} 
                        |> Seq.map (fun i -> numbers[i])
                        |> Seq.filter (fun i -> i >= 1 && i <= current.Children.Length)
                        |> Seq.sumBy (fun i -> current.Children[i - 1])
                    else
                        current.Value
                if stack.Count > 0 then
                    let previous = stack.Pop()
                    stack.Push { previous with Length = previous.Length + length; Children = (previous.Children, [| currentValue |]) ||> Array.append }
                else
                    result2 <- currentValue

            
        { First = result1.ToString(); Second = result2.ToString() }

module Day9 =
    open System.Text.RegularExpressions

    type Integer = int32

    type Node = { Value: int64; mutable Previous: Node option; mutable Next: Node option }

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.09.txt"
        
        let matchResult = Regex.Match(input, "(\d+) players; last marble is worth (\d+) points")

        let playersCount, lastMarble = Integer.Parse(matchResult.Groups[1].Value), Integer.Parse(matchResult.Groups[2].Value)

        let scores = Array.zeroCreate playersCount

        let currentNode = { Value = 0L; Previous = None; Next = None }
        currentNode.Previous <- Some currentNode
        currentNode.Next <- Some currentNode
        
        let iteration startNode marbleCount scores =
            Seq.initInfinite (fun i -> i % playersCount)
            |> Seq.take marbleCount
            |> asFirst (startNode, 1)
            ||> Seq.fold (fun (currentNode, currentMarbleNumber) playerNumber ->
                if currentMarbleNumber % 23 <> 0 then
                    match currentNode.Next with
                    | Some node -> 
                        let newNode = { Value = int64 currentMarbleNumber; Previous = Some node; Next = node.Next }
                        node.Next <- Some newNode
                        newNode.Next 
                        |> Option.iter (fun node -> node.Previous <- Some newNode)
                        (newNode, currentMarbleNumber + 1)
                    | None -> (currentNode, currentMarbleNumber + 1)
                else
                    let currentNode = 
                        (currentNode, seq { 1 .. 7 })
                        ||> Seq.fold (fun currentNode _ ->
                            currentNode.Previous |> Option.defaultValue currentNode)

                    (playerNumber, int64 (playerNumber |> Array.get scores) + int64 currentMarbleNumber + currentNode.Value)
                    ||> Array.set scores
                    
                    let next = currentNode.Next
                    currentNode.Previous 
                    |> Option.iter (fun node -> node.Next <- next)
                    
                    let currentNode = currentNode.Next |> Option.defaultValue currentNode

                    (currentNode, currentMarbleNumber + 1))
                    
        let _ = iteration currentNode lastMarble scores
        
        let maxScore1 = scores |> Array.max

        let currentNode = { Value= 0L; Previous = None; Next = None }
        currentNode.Previous <- Some currentNode
        currentNode.Next <- Some currentNode

        let scores = Array.zeroCreate playersCount
        
        let _ = iteration currentNode (lastMarble * 100) scores
        
        let maxScore2 = scores |> Array.max


        { First = maxScore1.ToString(); Second = maxScore2.ToString() }

module Day10 =
    open System.Text.RegularExpressions
    open System.Drawing
    open System.IO

    type Integer = int32

    type String = string

    type Light = { X: int; Y: int; VeloX: int; VeloY: int }

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.10.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        let instructions = 
            lines 
            |> Array.choose (fun line -> 
                let matchResult = Regex.Match(line, "position=<(.*), (.*)> velocity=<(.*), (.*)>")
                match matchResult.Success with
                | true -> 
                    Some { X = Integer.Parse(matchResult.Groups[1].Value.Trim()); 
                           Y = Integer.Parse(matchResult.Groups[2].Value.Trim()); 
                           VeloX = Integer.Parse(matchResult.Groups[3].Value.Trim()); 
                           VeloY = Integer.Parse(matchResult.Groups[4].Value.Trim()) }
                | false -> None)

        let saveFolder = @"C:\temp\adventofcode\2018\10\"

        let scaleX = 640
        let scaleY = 100
        
        let minRectangle points =
            let minX, _ = points |> Array.minBy (fun (x, _) -> x)
            let maxX, _ = points |> Array.maxBy (fun (x, _) -> x)
            let _, minY = points |> Array.minBy (fun (_, y) -> y)
            let _, maxY = points |> Array.maxBy (fun (_, y) -> y)
            minX, maxX, minY, maxY

        let drawImage points minX minY width height i =
            use bmp = new Bitmap(scaleX, scaleY)
            use g = Graphics.FromImage(bmp)

            g.Clear(Color.White)

            points
            |> Array.iter (fun (x, y) ->
                let actualX, actualY = int (double (x - minX) * double scaleX / double width), int(double (y - minY) * double scaleY / double height)
                g.FillRectangle(Brushes.Black, Rectangle(actualX, actualY, 15, 15)))


            g.Dispose()
            
            let filePath = saveFolder + i.ToString() + ".tiff"
            use stream = new FileStream(filePath, FileMode.Create)

            bmp.Save(stream, Imaging.ImageFormat.Tiff)
            
            System.Diagnostics.Process.Start(filePath) |> ignore

        let getWidthAndHeight i =
            let pointsToRender = 
                instructions 
                |> Array.map (fun inst -> (inst.X + i * inst.VeloX, inst.Y + i  * inst.VeloY))
            let minX, maxX, minY, maxY =
                minRectangle pointsToRender
            let width, height = maxX - minX + 1, maxY - minY + 1
            width, height

        let drawForIndex i =
            let pointsToRender = 
                instructions 
                |> Array.map (fun inst -> (inst.X + i * inst.VeloX, inst.Y + i  * inst.VeloY))
            let minX, maxX, minY, maxY =
                minRectangle pointsToRender
            let width, height = maxX - minX + 1, maxY - minY + 1
            drawImage pointsToRender minX minY width height i
        
        let _, _, minIndex =
            ((Integer.MaxValue, Integer.MaxValue, 0) , seq { 10000 .. 11000})
            ||> Seq.fold (fun (minWidth, minHeight, minIndex) currentIndex ->
                let currentWidth, currentHeight = getWidthAndHeight currentIndex
                if currentWidth < minWidth && currentHeight < minHeight then
                    (currentWidth, currentHeight, currentIndex)
                else (minWidth, minHeight, minIndex))
                

        drawForIndex minIndex

        { First = "".ToString(); Second = minIndex.ToString() }

module Day11 =
    type Integer = int32

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.11.txt"

        let serialNumber = Integer.Parse input

        let powerGrid = (300, 300) ||> Array2D.zeroCreate 

        (seq { 0 .. 299 }, seq { 0 .. 299 })
        ||> Seq.allPairs
        |> Seq.iter (fun (x, y) ->
            let x', y' = x + 1, y + 1
            let rackId = x' + 10
            let step2 = rackId * y'
            let step3 = step2 + serialNumber
            let step4 = step3 * rackId
            let step5 = (step4 % 1000) / 100
            (x, y, step5 - 5) |||> Array2D.set powerGrid)
        
        // https://en.wikipedia.org/wiki/Summed-area_table
        let summedAreaTable = (300, 300) ||> Array2D.zeroCreate 

        seq { 0 .. 299 }
        |> Seq.iter (fun y ->
            seq { 0 .. 299 }
            |> Seq.iter (fun x ->
                let value = (x, y) ||> Array2D.get powerGrid
                let above = if y = 0 then 0 else (x, y - 1) ||> Array2D.get summedAreaTable
                let left = if x = 0 then 0 else (x - 1, y) ||> Array2D.get summedAreaTable
                let diagonally = if x = 0 || y = 0 then 0 else (x - 1, y - 1) ||> Array2D.get summedAreaTable
                (x, y, value + above + left - diagonally) |||> Array2D.set summedAreaTable))
        
        let calculateMaxTotal subSize =
            let x', y', total = 
                (seq { 0 .. 300 - subSize }, seq { 0 .. 300 - subSize })
                ||> Seq.allPairs
                |> Seq.map (fun (x, y) -> 
                    let topLeft = if x = 0 || y = 0 then 0 else (x - 1, y - 1) ||> Array2D.get summedAreaTable
                    let bottomRight = (x + subSize - 1, y + subSize - 1) ||> Array2D.get summedAreaTable
                    let topRight = if x = 0 then 0 else (x - 1, y + subSize - 1) ||> Array2D.get summedAreaTable
                    let bottomLeft = if y = 0 then 0 else (x + subSize - 1, y - 1) ||> Array2D.get summedAreaTable
                    x, y, topLeft + bottomRight - topRight - bottomLeft)
                |> Seq.maxBy (fun (_, _, total) -> total)
            (x' + 1, y' + 1, total)
        
        let x1, y1, _ = calculateMaxTotal 3
        let result1 = x1.ToString() + "," + y1.ToString()

        let x2, y2, subSize2, _ =
            seq { 1 .. 300 }
            |> Seq.map (fun subSize ->
                let x, y, total = calculateMaxTotal subSize
                (x, y, subSize, total))
            |> Seq.maxBy (fun (_, _, _, total) -> total)
        let result2 = x2.ToString() + "," + y2.ToString() + "," + subSize2.ToString()

        { First = result1; Second = result2.ToString() }

module Day12 =
    open System.Text.RegularExpressions

    type Integer = int32

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.12.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        
        let matchResult = Regex.Match(lines[0], "initial state: (.+)")
        let initialState = matchResult.Groups[1].Value

        let map =
            lines
            |> Seq.ofArray
            |> Seq.skip 2
            |> Seq.map (fun line ->
                let matchResult = Regex.Match(line, "(.+) => (.)")
                let key = matchResult.Groups[1].Value
                let value = matchResult.Groups[2].Value.ToCharArray() |> Array.head
                key, value)
            |> Map.ofSeq

        let iteration currentState = 
            let _, firstIndex = currentState |> Array.head
            let currentState =
                ([| '.', firstIndex - 4; '.', firstIndex - 3; '.', firstIndex - 2; '.', firstIndex - 1 |], currentState)
                ||> Array.append 
                |> asSecond [| '.', firstIndex + currentState.Length; '.', firstIndex + currentState.Length + 1; '.', firstIndex + currentState.Length + 2; '.', firstIndex + currentState.Length + 3 |]
                ||> Array.append 
            let result = 
                seq { 2 .. currentState.Length - 3 }
                |> Seq.map (fun i ->
                    let currentNeighborhood =
                        [| currentState[i - 2] |> fst
                           currentState[i - 1] |> fst
                           currentState[i] |> fst
                           currentState[i + 1] |> fst
                           currentState[i + 2] |> fst |]
                    let currentNeighborhood = new string(currentNeighborhood)
                    Map.find currentNeighborhood map, currentState[i] |> snd)
                |> Seq.skipWhile (fun (c, _) -> c = '.')
                |> Seq.toArray
            let lastIndex = result |> Array.findIndexBack (fun (c, _) -> c = '#')
            let result = result |> Array.take (lastIndex + 1)
            printfn "%s" (new string(result |> Array.map(fun (c, _) -> c)))
            result
        
        let afterGenerations generations =
            ((initialState.ToCharArray() |> Array.mapi (fun i c -> c, i)) ,seq { 1 .. generations })
            ||> Seq.fold (fun currentState _ ->
                iteration currentState)

        let sumAfter20Generations = 
            afterGenerations 20 
            |> Array.filter (fun (c, _) -> c = '#')
            |> Array.map (fun (_, number) -> int64 number)
            |> Array.sum

        let state, _, i =
            ((initialState.ToCharArray() |> Array.mapi (fun i c -> c, i), false, 0) , Seq.initInfinite (fun i -> i + 1))
            ||> Seq.scan (fun (currentState, _, _) i ->
                let nextState = iteration currentState
                let sameStructure = 
                    (nextState |> Seq.ofArray, currentState |> Seq.ofArray) 
                    ||> Seq.forall2 (fun (c1, _) (c2, _) -> c1 = c2)
                (nextState, sameStructure, i))
            |> Seq.skipWhile (fun (_, b, _) -> not b)
            |> Seq.head


        let sumAfter50000000000Generations = 
            state
            |> Array.filter (fun (c, _) -> c = '#')
            |> Array.map (fun (_, number) -> int64 number + 50000000000L - int64 i)
            |> Array.sum
        { First = sumAfter20Generations.ToString(); Second = sumAfter50000000000Generations.ToString() }

module Day13 =

    type Direction = | Up | Down | Right | Left

    type Cart = { Id: int; Position : int*int; Direction : Direction; IntersectionCount : int; Dead : bool }

    let turnLeft prevDirection =
        match prevDirection with
        | Up -> Left
        | Right -> Up
        | Down -> Right
        | Left -> Down

    let turnRight prevDirection =
        match prevDirection with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    let goStraight prevDirection =
        prevDirection

    let moveCart curves1 curves2 intersections previousCart =
        let newPosition =
            match previousCart.Direction with
            | Up -> previousCart.Position |> fst, (previousCart.Position |> snd) - 1
            | Down -> previousCart.Position |> fst, (previousCart.Position |> snd) + 1
            | Left -> (previousCart.Position |> fst) - 1, previousCart.Position |> snd
            | Right -> (previousCart.Position |> fst) + 1, previousCart.Position |> snd
        let newDirection = 
            if curves1 |> Set.contains newPosition then
                match previousCart.Direction with
                | Up -> Right
                | Down -> Left
                | Left -> Down
                | Right -> Up
            elif curves2 |> Set.contains newPosition then
                match previousCart.Direction with
                | Up -> Left
                | Down -> Right
                | Left -> Up
                | Right -> Down
            elif intersections |> Set.contains newPosition then
                match previousCart.IntersectionCount % 3 with
                | 0 -> turnLeft previousCart.Direction
                | 2 -> turnRight previousCart.Direction
                | 1 | _ -> goStraight previousCart.Direction
            else previousCart.Direction
        let newIntersectionCount =
            if intersections |> Set.contains newPosition then
                previousCart.IntersectionCount + 1
            else previousCart.IntersectionCount
        { Id = previousCart.Id; Position = newPosition; Direction = newDirection; IntersectionCount = newIntersectionCount; Dead = false}

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.13.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let curves1, curves2, intersections, carts =
            lines 
            |> Seq.ofArray 
            |> Seq.mapi (fun i line -> i, line)
            |> Seq.collect (fun (y, line) ->
                line.ToCharArray()
                |> Seq.ofArray
                |> Seq.mapi (fun x c -> (x, y, c)))
            |> Seq.filter (fun (_, _, c) -> c <> '-' && c <> '|' && c <> ' ')
            |> asFirst (Set.empty, Set.empty, Set.empty, Array.empty)
            ||> Seq.fold (fun (curves1, curves2, intersections, carts) (x, y, c) ->
                match c with
                | '/'  -> 
                    let curves1 = curves1 |> Set.add (x, y)
                    curves1, curves2, intersections, carts
                | '\\' -> 
                    let curves2 = curves2 |> Set.add (x, y)
                    curves1, curves2, intersections, carts
                | '+' -> 
                    let intersections = intersections |> Set.add (x, y)
                    curves1, curves2, intersections, carts
                | '^' -> 
                    let carts = carts |> Array.append [| { Id = carts.Length; Position = (x,y); Direction = Up; IntersectionCount = 0; Dead = false } |]
                    curves1, curves2, intersections, carts
                | '<' -> 
                    let carts = carts |> Array.append [| { Id = carts.Length; Position = (x,y); Direction = Left; IntersectionCount = 0; Dead = false } |]
                    curves1, curves2, intersections, carts
                | 'v' -> 
                    let carts = carts |> Array.append [| { Id = carts.Length; Position = (x,y); Direction = Down; IntersectionCount = 0; Dead = false } |]
                    curves1, curves2, intersections, carts
                | '>' -> 
                    let carts = carts |> Array.append [| { Id = carts.Length; Position = (x,y); Direction = Right; IntersectionCount = 0; Dead = false } |]
                    curves1, curves2, intersections, carts
                | _ -> curves1, curves2, intersections, carts)

        let moveCartWithCurrentData = moveCart curves1 curves2 intersections
        
        let cartsForFirstTask = carts |> Array.copy

        let x1,y1 = 
            Seq.initInfinite (fun i -> i + 1)
            |> Seq.collect (fun _ -> 
                cartsForFirstTask
                |> Seq.ofArray 
                |> Seq.mapi asSecond
                |> Seq.sortWith (fun (x, _) (y, _) -> 
                    match (x.Position, y.Position) with
                    | (x1, y1),(x2, y2) when y1 < y2 || y1 = y2 && x1 < x2 -> -1
                    | (x1, y1),(x2, y2) when y1 = y2 && x1 = x2 -> 0
                    | _ -> 1))
            |> Seq.choose (fun (previousCart, i) ->
                let newCart = previousCart |> moveCartWithCurrentData
                let doesCrashHappen = cartsForFirstTask |> Array.exists (fun cart -> cart.Position = newCart.Position)
                (i, newCart) 
                ||> Array.set cartsForFirstTask
                if doesCrashHappen then
                    Some newCart.Position
                else
                    None)
            |> Seq.head
        
        let cartsForSecondTask = carts |> Array.copy

        let x2,y2 = 
            Seq.initInfinite (fun i -> i + 1)
            |> Seq.collect (fun _ -> 
                cartsForSecondTask 
                |> Seq.ofArray 
                |> Seq.mapi asSecond
                |> Seq.sortWith (fun (x, _) (y, _) -> 
                    match (x.Position, y.Position) with
                    | (x1, y1),(x2, y2) when y1 < y2 || y1 = y2 && x1 < x2 -> -1
                    | (x1, y1),(x2, y2) when y1 = y2 && x1 = x2 -> 0
                    | _ -> 1))
            |> Seq.choose (fun (previousCart, i) ->
                if (cartsForSecondTask |> Array.find (fun c -> c.Id = previousCart.Id)).Dead then
                    None
                else
                    let newCart = previousCart |> moveCartWithCurrentData
                    let maybeCrashHappenedTo = 
                        cartsForSecondTask 
                        |> Array.filter (fun cart -> not cart.Dead && cart.Position = newCart.Position)
                        |> Array.tryHead
                    match maybeCrashHappenedTo with
                    | Some cart -> 
                        let index = cartsForSecondTask |> Array.findIndex (fun c -> c.Id = cart.Id)
                        (index, { cart with Dead = true }) 
                        ||> Array.set cartsForSecondTask
                        (i, { newCart with Dead = true }) 
                        ||> Array.set cartsForSecondTask
                    | None ->
                        (i, newCart) 
                        ||> Array.set cartsForSecondTask
                
                    let remainingCarts = cartsForSecondTask |> Array.filter (fun c -> c.Dead |> not)

                    if remainingCarts.Length = 1 then
                        let newCart = (remainingCarts |> Array.head) |> moveCartWithCurrentData 
                        Some newCart.Position
                    else
                        None)
            |> Seq.head

        { First = sprintf "%d,%d" x1 y1; Second = sprintf "%d,%d" x2 y2 }

module Day14 =
    type Integer = int32
    type String = string

    type Node = { Value: int; mutable Next: Node option}

    let lastNode head node =
        let mutable current = node
        while LanguagePrimitives.PhysicalEquality current.Next.Value head |> not do
            match current.Next with
            | Some node -> current <- node
            | None -> current <- current
        current

    let moveForward i node =
        (node, seq { 1 .. i })
        ||> Seq.fold (fun prevNode _ -> prevNode.Next |> Option.defaultValue prevNode)

    let getSolution1 node = 
        ((node, ""), seq { 1 .. 10 })
        ||> Seq.fold (fun (prevNode, text) _ -> (prevNode.Next |> Option.defaultValue prevNode, sprintf "%s%d" text prevNode.Value))
        |> snd

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.14.txt"
        let numberOfRecipes = Integer.Parse input

        let last = { Value = 7; Next = None } 
        let head = { Value = 3; Next = Some last }

        let lastNodeForCurrentHead = lastNode head

        last.Next <- Some head
        let count = 2
        let first = head
        let second = last

        let mutable result2 = None

        let _ = 
            (last, first, second, count, "")
            |> Seq.unfold (fun (last, first, second, count, text) ->
                if count >= numberOfRecipes + 10 && result2.IsSome then None
                else
                    let newRecipe = first.Value + second.Value
                    let newRecipeAsCharArray = 
                        (sprintf "%d" newRecipe).ToCharArray() 
                    let newNodeToAppend = 
                        newRecipeAsCharArray
                        |> Seq.ofArray
                        |> Seq.map string
                        |> Seq.map Integer.Parse 
                        |> Seq.rev
                        |> asFirst head
                        ||> Seq.fold (fun prevNode number -> { Value = number; Next = Some prevNode })
            
                    last.Next <- Some newNodeToAppend

                    let newCount = count + newRecipeAsCharArray.Length
                    let newFirst = first |> moveForward (first.Value + 1)
                    let newSecond = second |> moveForward (second.Value + 1)

                    let newLast = last |> lastNodeForCurrentHead

                    let newText = sprintf "%s%d" text newRecipe
                    let maybePos = newText.IndexOf input
                    if maybePos >= 0 && result2.IsNone then 
                        result2 <- Some (newCount - newText.Length + maybePos)
                    let newText = 
                        if newText.Length > input.Length then
                            newText.Substring(newText.Length - input.Length)
                        else
                            newText

                    Some(1, (newLast, newFirst, newSecond, newCount, newText)))
            |> Seq.last


        let result1 =
            head 
            |> moveForward numberOfRecipes
            |> getSolution1

        let result2 = result2 |> Option.defaultValue 0

        { First = result1; Second = sprintf "%d" result2 }

module Day15 =
    type Integer = int32
    type String = string

    type Unit = { Position: int*int; HP: int }

    type Player = | Elve of Unit | Goblin of Unit
    
    let unitOfPlayer player = match player with | Elve unit | Goblin unit -> unit
    
    let positionOfPlayer player = (player |> unitOfPlayer).Position
    
    let hpOfPlayer player = (player |> unitOfPlayer).HP
    
    let isGoblin player = match player with | Elve _ -> false | Goblin _ -> true
    
    let isElve player = match player with | Elve _ -> true | Goblin _ -> false

    let isAlive player = player |> hpOfPlayer > 0

    let isDead player = player |> isAlive |> not

    let adjacentPositions (x, y) = 
        seq { yield x, y - 1
              yield x - 1, y
              yield x + 1, y
              yield x, y + 1 } 
        |> Seq.filter (fun (x,y) -> x >= 0 && x <= 32 && y >= 0 && y <= 32)

    let adjacentFreePositions walls elves goblins position = 
        position
        |> adjacentPositions 
        |> Seq.filter (fun position ->
            walls |> Set.contains position |> not
            && elves 
               |> Seq.ofArray 
               |> Seq.filter isAlive 
               |> Seq.map positionOfPlayer 
               |> Seq.forall (fun pos -> pos <> position)
            && goblins
               |> Seq.ofArray 
               |> Seq.filter isAlive
               |> Seq.map positionOfPlayer 
               |> Seq.forall (fun pos -> pos <> position))

    let comparePositions pos1 pos2 =
        let (x1, y1), (x2, y2) = pos1, pos2
        if y1 = y2 && x1 = x2 then 0
        elif y1 < y2 || y1 = y2 && x1 < x2 then -1
        else 1

    let tryAdjacentEnemy elves goblins player = 
        let fromEnemies enemies unit =
            adjacentPositions unit 
            |> Seq.map (fun position -> enemies |> Array.tryFind (fun enemy -> enemy |> isAlive && enemy |> positionOfPlayer = position))
            |> Seq.choose identity
            |> Seq.sortWith (fun player1 player2 ->
                let hp1, hp2 = player1 |> hpOfPlayer, player2 |> hpOfPlayer
                if hp1 < hp2 then -1
                elif hp1 > hp2 then 1
                else
                    (player1 |> positionOfPlayer, player2 |> positionOfPlayer)||>comparePositions)
            |> Seq.tryHead
        match player with
        | Elve unit -> unit.Position |> fromEnemies goblins
        | Goblin unit -> unit.Position |> fromEnemies elves

    let getFieldValues (lines: string[]) =
        lines 
        |> Seq.ofArray
        |> Seq.mapi (fun y line -> y, line)
        |> Seq.collect (fun (y, line) -> line.ToCharArray() |> Seq.ofArray |> Seq.mapi (fun x c -> x, y, c))
    
    let extractWalls getFieldValues =
        getFieldValues()
        |> Seq.filter (fun (_, _, c) -> c = '#')
        |> Seq.map (fun (x, y, _) -> x, y)
        |> Set.ofSeq
    
    let extractElves getFieldValues =
        getFieldValues()
        |> Seq.filter (fun (_, _, c) -> c = 'E')
        |> Seq.map (fun (x, y, _) -> Elve({ Position = x, y; HP = 200 }))
        |> Array.ofSeq
    
    let extractGoblins getFieldValues =
        getFieldValues()
        |> Seq.filter (fun (_, _, c) -> c = 'G')
        |> Seq.map (fun (x, y, _) -> Goblin({ Position = x, y; HP = 200 }))
        |> Array.ofSeq

    let createTurnOrder elves goblins =
        elves 
        |> Seq.ofArray 
        |> Seq.mapi (fun i _ ->
            let f() = i |> Array.get elves
            f)
        |> Seq.filter (fun f -> f() |> isAlive)
        |> Seq.append (
            goblins 
            |> Seq.ofArray 
            |> Seq.mapi (fun i _ ->
                let f() = i |> Array.get goblins
                f))
            |> Seq.filter (fun f -> f() |> isAlive)
        |> Seq.sortWith (fun x' y' -> 
            (x'() |> positionOfPlayer, y'() |> positionOfPlayer)
            ||> comparePositions)
        |> Seq.toArray

    let noParamReturns f = f()

    let attackPlayer power player  = 
        match player with 
        | Elve unit -> Elve { unit with HP = unit.HP - 3 } 
        | Goblin unit -> Goblin { unit with HP = unit.HP - power } 

    let getFloodedMap walls elves goblins position = 
        let visited = Set.empty |> Set.add position

        (visited, Map.empty, [ position ], 1)
        |> Seq.unfold (fun (visited, map, lastPositions, step) ->
            if lastPositions.IsEmpty then
                None
            else
                let nextPositions =
                    lastPositions 
                    |> Seq.collect (fun position -> position |> adjacentFreePositions walls elves goblins)
                    |> Seq.distinct
                    |> Seq.filter (fun position -> visited |> Set.contains position |> not)
                    |> Seq.toList

                let map = (map, nextPositions |> Seq.ofList) ||> Seq.fold (fun map position -> map |> Map.add position step)
                let visited = (visited, nextPositions |> Seq.ofList) ||> Seq.fold (fun visited position -> visited |> Set.add position)
            
                Some(map, (visited, map, nextPositions, step + 1)))
        |> Seq.last

    let getSuitableEnemyPositions walls elves goblins map player = 
        let enemies = if player |> isElve then goblins else elves
        enemies 
        |> Seq.ofArray 
        |> Seq.filter isAlive
        |> Seq.collect (fun enemy -> enemy |> positionOfPlayer |> adjacentFreePositions walls elves goblins)
        |> Seq.distinct
        |> Seq.filter (fun position -> map |> Map.containsKey position)
        |> Seq.groupBy (fun position -> map[position])
        |> Seq.sortBy (fun (key, _) -> key)
        |> Seq.map (fun (_, seq) -> seq)
        |> Seq.tryHead
        |> Option.defaultValue Seq.empty
        |> Seq.sortWith comparePositions
        |> Seq.tryHead

    let retraceToOrigin walls elves goblins (map: Map<int*int, int>) enemyPosition playerPosition =
        if map[enemyPosition] = 1 then
            enemyPosition
        else
            let map = getFloodedMap walls elves goblins enemyPosition
            playerPosition 
            |> adjacentFreePositions walls elves goblins 
            |> Seq.filter (fun pos -> map |> Map.containsKey pos)
            |> Seq.groupBy (fun pos -> map[pos])
            |> Seq.sortBy (fun (key, _) -> key)
            |> Seq.map (fun (_, seq) -> seq |> Seq.sortWith comparePositions |> Seq.head)
            |> Seq.head

    let movePlayer walls elves goblins player =
        let maybeAdjacentEnemy = player |> tryAdjacentEnemy elves goblins
        match maybeAdjacentEnemy with
        | Some _ -> player
        | None ->
            let map = player |> positionOfPlayer |> getFloodedMap walls elves goblins
            let maybePos = (map, player) ||> getSuitableEnemyPositions walls elves goblins
            match maybePos with
            | Some pos -> 
                let newPosition = (map, pos, player |> positionOfPlayer) |||> retraceToOrigin walls elves goblins
                match player with
                | Elve unit -> Elve { unit with Position = newPosition }
                | Goblin unit -> Goblin { unit with Position = newPosition }
            | None -> player

    let go() =
        let iteration power = 
            let input = inputFromResource "AdventOfCode.Inputs._2018.15.txt"
            let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        
            let getFieldValues() = getFieldValues lines

            let walls = getFieldValues |> extractWalls

            let elves = getFieldValues |> extractElves

            let goblins = getFieldValues |> extractGoblins

            let myTryAdjacentEnemy = tryAdjacentEnemy elves goblins

            let myMovePlayer = movePlayer walls elves goblins

            let setPlayer oldPlayer newPlayer =
                if oldPlayer |> isElve then
                    let index = elves |> Array.findIndex (fun elve -> LanguagePrimitives.PhysicalEquality elve oldPlayer)
                    (index, newPlayer) ||> Array.set elves
                else
                    let index = goblins |> Array.findIndex (fun goblin -> LanguagePrimitives.PhysicalEquality goblin oldPlayer)
                    (index, newPlayer) ||> Array.set goblins

            let lastFullRound =
                1
                |> Seq.unfold (fun round ->
                    let allDead() = elves |> Array.forall isDead || goblins |> Array.forall isDead
                    if allDead() then 
                        None
                    else
                        let turnOrder = (elves, goblins) ||> createTurnOrder
                        let mutable didntTurn = false
                        turnOrder 
                        |> Seq.ofArray 
                        |> Seq.filter (fun player -> player() |> isAlive)
                        |> Seq.iter (fun player ->
                            let player = player()
                            if player |> isAlive then
                                if allDead() then didntTurn <- true
                                let movedPlayer = player |> myMovePlayer
                                (player, movedPlayer) ||> setPlayer
                                match movedPlayer |> myTryAdjacentEnemy with
                                | Some enemy -> (enemy, enemy |> attackPlayer power) ||> setPlayer
                                | None -> movePlayer |> ignore
                            else player |> ignore)
                        //renderField round
                        if allDead() && didntTurn then 
                            None
                        else
                            Some (round, round + 1))
                |> Seq.last
            
            let sumOfElves = elves |> Array.filter (fun elve -> elve |> hpOfPlayer > 0) |> Array.sumBy hpOfPlayer
            let sumOfGoblins = goblins |> Array.filter (fun goblin -> goblin |> hpOfPlayer > 0) |> Array.sumBy hpOfPlayer

            let result1 = sprintf "%d" ((lastFullRound - 0) * (sumOfElves + sumOfGoblins))
            (result1, elves |> Array.forall isAlive)

        let result1 = iteration 3 |> fst

        let result2 =
            Seq.initInfinite (fun i -> iteration (i + 4)) 
            |> Seq.skipWhile (fun (_, b) -> not b) 
            |> Seq.map (fun (res, _) -> res)
            |> Seq.head

        { First = result1; Second = result2 }
module Day16 =
    open System.Text.RegularExpressions

    open Binary

    type Integer = int32

    type Sample = { Before: int[]; Instruction: int*int*int*int; After: int[]}
    
    let go() =

        let insts = [| addr; addi; mulr; muli; banr; bani; borr; bori; setr; seti; gtir; gtri; gtrr; eqir; eqri; eqrr |]

        let input_a = inputFromResource "AdventOfCode.Inputs._2018.16_a.txt"

        let matches = Regex.Matches(input_a, "Before: \[(\d+), (\d+), (\d+), (\d+)\](\r\n|\r|\n)(\d+) (\d+) (\d+) (\d+)(\r\n|\r|\n)After:  \[(\d+), (\d+), (\d+), (\d+)\]")
        
        let samples =
            matches
            |> Seq.cast
            |> Seq.map (fun (mat: Match) -> 
                let before = 
                    [| Integer.Parse mat.Groups[1].Value
                       Integer.Parse mat.Groups[2].Value
                       Integer.Parse mat.Groups[3].Value
                       Integer.Parse mat.Groups[4].Value |]
                let inst = Integer.Parse mat.Groups[6].Value, Integer.Parse mat.Groups[7].Value, Integer.Parse mat.Groups[8].Value, Integer.Parse mat.Groups[9].Value
                let after = 
                    [| Integer.Parse mat.Groups[11].Value
                       Integer.Parse mat.Groups[12].Value
                       Integer.Parse mat.Groups[13].Value
                       Integer.Parse mat.Groups[14].Value |]

                { Before = before; Instruction = inst; After = after })
            |> Seq.toArray

        let count = 
            (0, samples |> Seq.ofArray)
            ||> Seq.fold (fun count sample ->
                let matches, _ = 
                    insts |> Array.partition (fun inst ->
                        let copyOfBefore = sample.Before |> Array.copy
                        let _, a, b, c = sample.Instruction
                        inst a b c copyOfBefore 
                        copyOfBefore |> Array.zip sample.After |> Array.forall (fun (x, y) -> x = y))
                if matches.Length > 2 then count + 1 else count)
                
        let candidates = 
            samples 
            |> Seq.ofArray
            |> Seq.collect (fun sample ->
                insts
                |> Seq.ofArray
                |> Seq.mapi (fun i inst -> i, inst)
                |> Seq.where (fun (_, inst) ->
                    let copyOfBefore = sample.Before |> Array.copy
                    let _, a, b, c = sample.Instruction
                    inst a b c copyOfBefore
                    copyOfBefore |> Array.zip sample.After |> Array.forall (fun (x, y) -> x = y))
                |> Seq.map (fun (i, _) -> 
                    let opcode, _, _, _ = sample.Instruction
                    opcode, i))
            |> Seq.groupBy (fun (opcode, _) -> opcode)
            |> Seq.map (fun (opcode, seq) -> opcode, seq |> Seq.map snd |> Seq.distinct |> Seq.toArray)
            |> Seq.toArray

        let map = 
            Map.empty
            |> Seq.unfold (fun map -> 
                if seq { 0 .. insts.Length - 1} |> Seq.forall (fun i -> map |> Map.containsKey i) then
                    None
                else
                    let map =
                        candidates 
                        |> Seq.filter (fun (opcode, _) -> map |> Map.containsKey opcode |> not)
                        |> Seq.map (fun (opcode, indeces) -> 
                            let filtered, _ = indeces |> Array.partition (fun i -> map |> Map.forall (fun _ index -> index <> i))
                            opcode, filtered)
                        |> Seq.filter (fun (_, filtered) -> filtered.Length = 1)
                        |> Seq.map (fun (opcode, inArray) -> opcode, inArray[0])
                        |> asFirst map
                        ||> Seq.fold (fun map (opcode, index) -> map |> Map.add opcode index)
                    Some (map, map))
            |> Seq.last

        let input_b = inputFromResource "AdventOfCode.Inputs._2018.16_b.txt"
        let lines = input_b.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let registers = [| 0; 0; 0; 0 |]

        lines 
        |> Seq.map (fun line -> 
            let mat = Regex.Match(line, "(\d+) (\d+) (\d+) (\d+)")
            Integer.Parse mat.Groups[1].Value, Integer.Parse mat.Groups[2].Value, Integer.Parse mat.Groups[3].Value, Integer.Parse mat.Groups[4].Value)
        |> Seq.iter (fun (opcode, a, b, c) ->
            insts[map[opcode]] a b c registers)

        { First = sprintf "%d" count; Second = sprintf "%d" registers[0] }

module Day17 =

    open System.Text.RegularExpressions
    open System.Collections.Generic

    type Integer = int32

    type Event = | FlowDown of int*int | BottomHit of int*int

    let isNotEmpty clay resting floating pos =
       clay |> Set.contains pos || resting |> Set.contains pos || floating |> Set.contains pos

    let isEmpty clay resting floating pos =
        pos |> isNotEmpty clay resting floating |> not

    let isClayOrResting clay resting pos =
       clay |> Set.contains pos || resting |> Set.contains pos

    let isClay clay pos =
       clay |> Set.contains pos

    let isResting resting pos =
       resting |> Set.contains pos

    let isFloating floating pos =
       floating |> Set.contains pos
    
    let go() =

        let input = inputFromResource "AdventOfCode.Inputs._2018.17.txt"

        let y_lines = Regex.Matches(input, "x=(\d+), y=(\d+)..(\d+)")

        let clay =
            y_lines
            |> Seq.cast
            |> Seq.map (fun (mat: Match) -> 
                Integer.Parse mat.Groups[1].Value, Integer.Parse mat.Groups[2].Value, Integer.Parse mat.Groups[3].Value)
            |> Seq.collect (fun (x, y1, y2) -> seq { y1 .. y2 } |> Seq.map (fun y -> x, y))
            |> asFirst Set.empty
            ||> Seq.fold (fun clay point -> clay |> Set.add point)

        let x_lines = Regex.Matches(input, "y=(\d+), x=(\d+)..(\d+)")

        let clay =
            x_lines
            |> Seq.cast
            |> Seq.map (fun (mat: Match) -> 
                Integer.Parse mat.Groups[1].Value, Integer.Parse mat.Groups[2].Value, Integer.Parse mat.Groups[3].Value)
            |> Seq.collect (fun (y, x1, x2) -> seq { x1 .. x2 } |> Seq.map (fun x -> x, y))
            |> asFirst clay
            ||> Seq.fold (fun clay point -> clay |> Set.add point)
        
        let minY, maxY =
            ((Integer.MaxValue, Integer.MinValue), clay) 
            ||> Set.fold (fun (minY, maxY) (_, y) ->
                let minY = if y < minY then y else minY
                let maxY = if y > maxY then y else maxY
                minY, maxY)

        let events = Queue<Event>()
        let firstEvent = FlowDown(500, 0)
        events.Enqueue firstEvent
        
        let resting, floating = 
            (Set.empty, Set.empty)
            |> Seq.unfold(fun (resting, floating) ->
                if events.Count <= 0 then None
                else 
                    let event = events.Dequeue()
                    match event with
                    | FlowDown (x, y) ->
                        let floating, _ =
                            if (x, y + 1) |> isEmpty clay resting floating then
                            
                                let floating, y = 
                                    (floating, y + 1)
                                    |> Seq.unfold (fun (floating, y) ->
                                        if y > maxY || (x, y) |> isNotEmpty clay resting floating then
                                            None
                                        else
                                            let floating = floating |> Set.add (x, y)
                                            Some ((floating, y), (floating, y + 1)))
                                    |> Seq.last

                                if y <= maxY && (x, y + 1) |> isClayOrResting clay resting then
                                    let event = BottomHit(x, y)
                                    events.Enqueue event
                                (floating, y)
                            else (floating, y)
                
                        Some ((resting, floating), (resting, floating))
                    | BottomHit (x, y) -> 
                        let floating = 
                            if (x, y) |> isEmpty clay resting floating then floating |> Set.add (x, y) 
                            else floating
                        let floating, xLeft, hitWallLeft =
                            (floating, x - 1, false)
                            |> Seq.unfold (fun (floating, x, abort) ->
                                if abort then
                                    None
                                elif (x, y) |> isClay clay then
                                    Some ((floating, x, true), (floating, x - 1, true))
                                elif (x, y + 1) |> isEmpty clay resting floating then
                                    let floating = floating |> Set.add (x, y)
                                    Some ((floating, x, false), (floating, x - 1, true))
                                else
                                    let floating = floating |> Set.add (x, y)
                                    Some ((floating, x, false), (floating, x - 1, false)))
                            |> Seq.last
                        let floating, xRight, hitWallRight =
                            (floating, x + 1, false)
                            |> Seq.unfold (fun (floating, x, abort) ->
                                if abort then
                                    None
                                elif (x, y) |> isClay clay then
                                    Some ((floating, x, true), (floating, x + 1, true))
                                elif (x, y + 1) |> isEmpty clay resting floating then
                                    let floating = floating |> Set.add (x, y)
                                    Some ((floating, x, false), (floating, x + 1, true))
                                else
                                    let floating = floating |> Set.add (x, y)
                                    Some ((floating, x, false), (floating, x + 1, false)))
                            |> Seq.last
                        
                        let resting =
                            match hitWallLeft, hitWallRight with
                            | false, false -> 
                                let event = FlowDown(xLeft, y)
                                events.Enqueue event
                                let event = FlowDown(xRight, y)
                                events.Enqueue event
                                resting
                            | true, false -> 
                                let event = FlowDown(xRight, y)
                                events.Enqueue event
                                resting
                            | false, true -> 
                                let event = FlowDown(xLeft, y)
                                events.Enqueue event
                                resting
                            | true, true -> 
                                let event = BottomHit(x, y - 1)
                                events.Enqueue event
                                seq { xLeft + 1 .. xRight - 1 }
                                |> asFirst resting
                                ||> Seq.fold (fun resting x -> resting |> Set.add (x, y))
                
                        Some ((resting, floating), (resting, floating)))
            |> Seq.last

        let result1 = sprintf "%d" (resting |> Set.filter (fun (_, y) -> y >= minY && y <= maxY) |> Set.union (floating  |> Set.filter (fun (_, y) -> y >= minY && y <= maxY))).Count
        let result2 = sprintf "%d" (resting |> Set.filter (fun (_, y) -> y >= minY && y <= maxY)).Count

        { First = result1; Second = result2}

module Day18 =

    let iteration size previous =
            let next = Array2D.create size size '.'
            seq { 0 .. size - 1 }
            |> Seq.allPairs (seq { 0 .. size - 1 })
            |> Seq.iter (fun (x, y) ->
                let state = (x, y) ||> Array2D.get previous
                let countedElements =
                    seq { (if y = 0 then 0 else y - 1) .. (if y = size - 1 then size - 1 else y + 1) }
                    |> Seq.allPairs (seq { (if x = 0 then 0 else x - 1) .. (if x = size - 1 then size - 1 else x + 1) })
                    |> Seq.except (seq { yield (x, y) })
                    |> Seq.map (fun (x, y) -> (x, y) ||> Array2D.get previous)
                let newState =
                    if state = '.' then
                        let count = countedElements |> Seq.filter (fun e -> e = '|') |> Seq.length
                        if count >= 3 then '|' else '.'
                    elif state = '|' then
                        let count = countedElements |> Seq.filter (fun e -> e = '#') |> Seq.length
                        if count >= 3 then '#' else '|'
                    else
                        let containsLumberyards = countedElements |> Seq.contains '#'
                        let containsTrees = countedElements |> Seq.contains '|'
                        if containsLumberyards && containsTrees then '#' else '.'
                (x, y, newState) |||> Array2D.set next)
            next
            
    let countTiles t size field =
        seq { 0 .. size - 1 }
        |> Seq.allPairs (seq { 0 .. size - 1 })
        |> asFirst 0
        ||> Seq.fold (fun count (x, y) -> 
            let tile = (x, y) ||> Array2D.get field
            if tile = t then count + 1 else count)
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.18.txt"
        let lines = 
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None) 
            |> Array.map (fun line -> line.ToCharArray())
        let size = lines.Length

        let myIteration = iteration size

        let initialField = Array2D.create size size '.'
        seq { 0 .. size - 1 }
        |> Seq.allPairs (seq { 0 .. size - 1 })
        |> Seq.iter (fun (x, y) ->
            (x, y, lines[y][x]) |||> Array2D.set initialField)

        let field1 = 
            (initialField, seq { 1 .. 10})
            ||> Seq.fold (fun previous _ -> myIteration previous)
                
        let countLumbers1 = (size, field1) ||> countTiles '#'
        let countTrees1 = (size, field1) ||> countTiles '|'

        let results = 
            (1, initialField, [], false)
            |> Seq.unfold (fun (round, previous, previousResults, abort) ->
                if abort then None
                else 
                    let next = myIteration previous
                    let countLumbers2 = (size, next) ||> countTiles '#'
                    let countTrees2 = (size, next) ||> countTiles '|'
                    let res = countLumbers2 * countTrees2
                    let nextResults = res::previousResults
                    Some (nextResults, (round + 1, next, nextResults, round = 1000)))
            |> Seq.last
            |> List.rev
            |> List.toArray

        let lastResult = results |> Array.last

        let firstIndex =  (0, (results.Length - 1)) ||> Array.sub results |> Array.findIndexBack (fun res -> res = lastResult)

        let moduloBase = results.Length - firstIndex - 1

        let indexFromStart = (1000000000 - firstIndex - 1) % moduloBase

        let result2 = results[firstIndex + indexFromStart]

        { First = sprintf "%d" (countLumbers1 * countTrees1); Second = sprintf "%d" result2 }

module Day19 =

    open Binary
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.19.txt"
        let lines = 
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let instIdx = getInstructionPointerIndex lines[0]

        let insts =
            lines
            |> Seq.ofArray
            |> Seq.skip 1
            |> loadInstructions
        let doFor (registers: int[]) =
            let _ =
                registers[instIdx]
                |> Seq.unfold (fun curInst ->
                    insts[curInst] registers
                    (instIdx, registers[instIdx] + 1) ||> Array.set registers
                    let nextInst = registers[instIdx]
                    if nextInst < 0 || nextInst >= insts.Length then
                        None
                    else
                        Some (1, nextInst))
                |> Seq.last
            0
            
        let disassemblePart2() =
            seq { 1 .. 10551418 }
            |> Seq.map (fun i -> if 10551418 % i = 0 then i else 0)
            |> Seq.sum
            
        let registers1 = Array.zeroCreate 6

        doFor registers1 |> ignore

        let result2 = disassemblePart2()

        { First = sprintf "%d" registers1[0]; Second = sprintf "%d" result2 }

module Day20 =

    open System.Collections.Generic

    type Node = { 
        Position: int*int
        mutable North: Node option 
        mutable South: Node option 
        mutable West:  Node option
        mutable East:  Node option }
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.20.txt"
        let directions = input.ToCharArray()

        let initialNode = { Position = 0, 0; North = None; South = None; West = None; East = None }
        let stack = Stack<Node>()
        let map = Map.empty |> Map.add initialNode.Position initialNode

        let _, map = 
            ((initialNode, map), directions)
            ||> Seq.fold (fun (prevNode, map) c ->
                match c with
                | 'N' | 'S' | 'W' | 'E' -> 
                    let newPosition = 
                        match c with
                        | 'N' -> prevNode.Position |> fst, (prevNode.Position |> snd) - 1
                        | 'S' -> prevNode.Position |> fst, (prevNode.Position |> snd) + 1
                        | 'W' -> (prevNode.Position |> fst) - 1, prevNode.Position |> snd
                        | 'E' -> (prevNode.Position |> fst) + 1, prevNode.Position |> snd
                        | _ -> (prevNode.Position |> fst) + 1, prevNode.Position |> snd
                    let nextNode =
                        match (newPosition, map) ||> Map.tryFind with
                        | Some node -> node
                        | None -> { Position = newPosition; North = None; South = None; West = None; East = None }
                    let map = map |> Map.add nextNode.Position nextNode
                    match c with
                        | 'N' -> prevNode.North <- Some nextNode; nextNode.South <- Some prevNode
                        | 'S' -> prevNode.South <- Some nextNode; nextNode.North <- Some prevNode
                        | 'W' -> prevNode.West <- Some nextNode; nextNode.East <- Some prevNode
                        | 'E' -> prevNode.East <- Some nextNode; nextNode.West <- Some prevNode
                        | _ -> prevNode.East <- Some nextNode; nextNode.West <- Some prevNode
                    (nextNode, map)
                | '(' -> 
                    stack.Push prevNode
                    (prevNode, map)
                | '|' -> 
                    let nodeToJumpBackTo = stack.Peek()
                    (nodeToJumpBackTo, map)
                | ')' -> 
                    stack.Pop() |> ignore
                    (prevNode, map)
                | '^' | '$' | _ ->
                    (prevNode, map))

        let distanceMap = Map.empty
        let visited = Set.empty
        let needVisit = [ initialNode.Position ]

        let distanceMap =
            (distanceMap, visited, needVisit, 0)
            |> Seq.unfold (fun (distanceMap, visited, needVisit, currentDistance) ->
                if needVisit = [] then
                    None
                else
                    let distanceMap, visited =
                        ((distanceMap, visited), needVisit)
                        ||> Seq.fold (fun (map, set) position ->
                            map |> Map.add position currentDistance, set |> Set.add position)
                    let needVisit =
                        needVisit 
                        |> Seq.ofList 
                        |> Seq.collect (fun position -> 
                            let node = map[position]
                            seq { yield node.North; yield node.South; yield node.West; yield node.East })
                        |> Seq.choose identity
                        |> Seq.map (fun node -> node.Position)
                        |> Seq.where (fun position -> visited |> Set.contains position |> not)
                        |> Seq.distinct
                        |> Seq.toList
                    Some (distanceMap, (distanceMap, visited, needVisit, currentDistance + 1)))
            |> Seq.last

        let result1 = distanceMap |> Map.toSeq |> Seq.map (fun (_, distance) -> distance) |> Seq.max

        let result2 = distanceMap |> Map.toSeq |> Seq.map (fun (_, distance) -> distance) |> Seq.where (fun distance -> distance >= 1000) |> Seq.length

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day21 =

    open Binary
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.21.txt"
        let lines = 
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let instIdx = getInstructionPointerIndex lines[0]

        let insts =
            lines
            |> Seq.ofArray
            |> Seq.skip 1
            |> loadInstructions
        let doForResult1 (registers: int[]) =
            (registers[instIdx], false)
            |> Seq.unfold (fun (curInst, abort) ->
                if abort then 
                    None
                else
                    insts[curInst] registers
                    (instIdx, registers[instIdx] + 1) ||> Array.set registers
                    let nextInst = registers[instIdx]
                    Some (registers[2], (nextInst, nextInst = 28)))
            |> Seq.last

        let disassemblePart2() =
            let mutable _2, _5 = 0, 0

            let mutable loop_0 = true 
            let mutable last_2 = 0;
            let mutable set = Set.empty
            _5 <- _2 ||| 65536
            _2 <- 16123384
            while loop_0 do
                _2 <- (((_2 + (_5 &&& 255)) &&& 16777215) * 65899) &&& 16777215
                if 256 > _5 then
                    if set |> Set.contains _2 then
                        loop_0 <- false
                    else
                        last_2 <- _2
                        set <- set |> Set.add _2
                        _5 <- _2 ||| 65536
                        _2 <- 16123384
                else
                    _5 <- _5 / 256
            last_2
            
        let registers1 = Array.zeroCreate 6
        let result1 = doForResult1 registers1
        
        //let registers2 = Array.zeroCreate 6
        //let result2 = doForResult2 registers2

        // Same result as doForResult2 would have but much faster
        let result2 = disassemblePart2()

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day22 =
    open System.Text.RegularExpressions
    open FSharpx.Collections
    open System

    type Integer = int32

    type Terrain = | Rocky | Wet | Narrow

    type Gadget = | ClimbingGear | Torch | Neither

    [<CustomEquality;CustomComparison>]
    type Step = { TargetPosition: int*int; Value: int; Gadget: Gadget } with
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? Step as other ->
                    if this.Value > other.Value then 1
                    elif this.Value = other.Value then 0
                    else -1
                | _ -> -1
        interface IComparable<Step> with
            member this.CompareTo other =
                if this.Value > other.Value then 1
                elif this.Value = other.Value then 0
                else -1
        interface IEquatable<Step> with
            member this.Equals other =
                this.TargetPosition = other.TargetPosition && this.Value = other.Value && this.Gadget = other.Gadget
        override this.Equals other =
            match other with
                | :? Step as other ->
                    this.TargetPosition = other.TargetPosition && this.Value = other.Value && this.Gadget = other.Gadget
                | _ -> false
        override this.GetHashCode() =
            this.Value * 17 + (this.TargetPosition |> fst) * 17 + (this.TargetPosition |> snd) * 17

    let adjacent' lX lY (x, y) =
        seq {
            if x > 0 then yield x - 1, y
            if y > 0 then yield x, y - 1
            if x < lX - 1 then yield x + 1, y
            if y < lY - 1 then yield x, y + 1
            }

    let switchGadget' field prevPos nextPos gadget = 
        let prevTerrain = prevPos ||> Array2D.get field
        let nextTerrain = nextPos ||> Array2D.get field
        match prevTerrain, nextTerrain with
        | Rocky, Wet -> ClimbingGear
        | Rocky, Narrow -> Torch
        | Wet, Rocky -> ClimbingGear
        | Wet, Narrow -> Neither
        | Narrow, Rocky -> Torch
        | Narrow, Wet -> Neither
        | _ -> gadget

    let otherGadget terrain gadget =
        match terrain, gadget with
        | Rocky, ClimbingGear -> Torch
        | Rocky, Torch -> ClimbingGear
        | Wet, ClimbingGear -> Neither
        | Wet, Neither -> ClimbingGear
        | Narrow, Torch -> Neither
        | Narrow, Neither -> Torch
        | _ -> gadget

    let next' lX lY field targetPosition map position value gadget =
        position 
        |> adjacent' lX lY
        |> Seq.map (fun pos -> 
            let switchedGadget = (position, pos, gadget) |||> switchGadget' field
            let value = if gadget <> switchedGadget then value + 8 else value + 1
            let value = if pos = targetPosition && switchedGadget <> Torch then value + 7 else value
            pos, switchedGadget, value)
        |> Seq.filter (fun (position, gadget, value) -> 
            ((position, gadget), map) ||> Map.containsKey |> not || ((position, gadget), map) ||> Map.find > value)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.22.txt"
        let matchDepth = Regex.Match(input, "depth: (\d+)")
        let matchTarget = Regex.Match(input, "target: (\d+),(\d+)")
        
        let depth = matchDepth.Groups[1].Value |> Integer.Parse
        let depthModulo = depth % 20183
        let tX, tY = matchTarget.Groups[1].Value |> Integer.Parse, matchTarget.Groups[2].Value |> Integer.Parse
        let moduloBase = 20183
        let moduloTypeBase = 3
        let mulXModulo = 16807 % moduloBase
        let mulYModulo = 48271 % moduloBase
        let margin = 15
        let lX, lY = tX + margin, tY + margin

        let field = (lX + 1 , lY + 1) ||> Array2D.zeroCreate
        
        seq { 0 .. lX }
        |> Seq.iter (fun x -> (x, 0 , (((x * mulXModulo) % moduloBase) + depthModulo) % moduloBase) |||> Array2D.set field)
        seq { 0 .. lY }
        |> Seq.iter (fun y -> (0, y , (((y * mulYModulo) % moduloBase) + depthModulo) % moduloBase) |||> Array2D.set field)

        seq { 1 .. lX }
        |> Seq.iter (fun x -> 
            seq { 1 .. lY }
            |> Seq.iter (fun y ->
                if x = tX && y = tY then
                    (x, y, depthModulo % moduloBase) |||> Array2D.set field
                else
                    (x, y, ((((x - 1, y) ||> Array2D.get field) * ((x, y - 1) ||> Array2D.get field)) + depthModulo) % moduloBase) |||> Array2D.set field))
            
        let fieldWithTiles = 
            (lX + 1, lY + 1,  (fun x y -> 
                let discriminator = ((x, y) ||> Array2D.get field) % moduloTypeBase
                match discriminator with
                | 0 -> Rocky
                | 1 -> Wet
                | 2 | _ -> Narrow))
            |||> Array2D.init
        
        let result1 = 
            (seq { 0 .. tX }, seq { 0 .. tY }) 
            ||> Seq.allPairs
            |> Seq.map (fun (x, y) ->
                let terrain = (x, y) ||> Array2D.get fieldWithTiles
                match terrain with
                | Rocky -> 0
                | Wet -> 1
                | Narrow -> 2)
            |> Seq.sum

        let next = next' lX lY fieldWithTiles (tX, tY)
        let queue = PriorityQueue.empty false |> PriorityQueue.insert { TargetPosition = (0, 0); Value = 0; Gadget = Torch }
        let initialState = Integer.MaxValue, Map.empty, queue

        let result2 =
            initialState
            |> Seq.unfold(fun (currentMin, map, queue) ->
                if queue |> PriorityQueue.isEmpty then
                    None
                else
                    let step, queue = queue |> PriorityQueue.pop
                    match ((step.TargetPosition, step.Gadget), map) ||> Map.tryFind with
                    | Some found when step.Value >= found -> Some(currentMin, (currentMin, map, queue))
                    | _ -> 
                        let map = ((step.TargetPosition, step.Gadget), step.Value, map) |||> Map.add
                        let currentMin = if step.TargetPosition = (tX, tY) && step.Value < currentMin then step.Value else currentMin
                        let queue = 
                            (queue, next map step.TargetPosition step.Value step.Gadget)
                            ||> Seq.fold(fun queue (pos, gadget, value) -> 
                                queue |> PriorityQueue.insert {TargetPosition = pos; Value = value; Gadget = gadget })
                        Some(currentMin, (currentMin, map, queue)))
            |> Seq.last

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day23 =
    open System.Text.RegularExpressions
    open System
    open FSharpx.Collections

    [<CustomEquality;CustomComparison>]
    type Step = { Position: int64*int64*int64; Count: int; } with
        interface IComparable<Step> with
            member this.CompareTo other =
                if this.Count > other.Count then 1
                elif this.Count = other.Count then 0
                else -1
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? Step as other ->
                    if this.Count > other.Count then 1
                    elif this.Count = other.Count then 0
                    else -1
                | _ -> -1
        interface IEquatable<Step> with
            member this.Equals other =
                this.Position = other.Position && this.Count = other.Count
        override this.Equals other =
            match other with
                | :? Step as other -> this.Position = other.Position && this.Count = other.Count
                | _ -> false
        override this.GetHashCode() =
            let x, y, z = this.Position
            (x * 17L + y * 17L + z * 17L) |> int32

    let manHatDist (x1:int64, y1:int64, z1:int64) (x2, y2, z2) =
        Math.Abs(x1 - x2) + Math.Abs(y1 -  y2) + Math.Abs(z1 - z2)

    [<CustomEquality;CustomComparison>]
    type OctreeArea = { MinPosition: int64*int64*int64; MaxPosition: int64*int64*int64; Depth: int; Value: int } with
        interface IComparable<OctreeArea> with
            member this.CompareTo other =
                if this.Value > other.Value then 1
                elif this.Value < other.Value then -1
                else 
                    if this.Depth > other.Depth then -1
                    elif this.Depth < other.Depth then 1
                    else 0
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? OctreeArea as other ->
                    if this.Value > other.Value then 1
                    elif this.Value < other.Value then -1
                    else 
                        if this.Depth > other.Depth then -1
                        elif this.Depth < other.Depth then 1
                        else 0
                | _ -> 0
        interface IEquatable<OctreeArea> with
            member this.Equals other =
                this.MinPosition = other.MinPosition && this.MaxPosition = other.MaxPosition
        override this.Equals other =
            match other with
                | :? OctreeArea as other -> this.MinPosition = other.MinPosition && this.MaxPosition = other.MaxPosition
                | _ -> false
        override this.GetHashCode() =
            let x, y, z = this.MinPosition
            let x1, y1, z1 = this.MaxPosition
            (x * 17L + y * 17L + z * 17L + x1 * 17L + y1 * 17L + z1 * 17L) |> int32

    let middlePoint (minX, minY, minZ) (maxX, maxY, maxZ) =
        minX + (maxX - minX) / 2L, minY + (maxY - minY) / 2L, minZ + (maxZ - minZ) / 2L

    let splitIntoSubOctrees octree =
        let middle = (octree.MinPosition, octree.MaxPosition) ||> middlePoint
        let minX, minY, minZ = octree.MinPosition
        let maxX, maxY, maxZ = octree.MaxPosition
        let middleX, middleY, middleZ = middle
        let middleX', middleY', middleZ' = middleX + 1L, middleY + 1L, middleZ + 1L
        seq { yield { MinPosition = octree.MinPosition; MaxPosition = middle; Depth = octree.Depth + 1; Value = 0 }
              yield { MinPosition = (minX, minY, middleZ'); MaxPosition = (middleX, middleY, maxZ); Depth = octree.Depth + 1; Value = 0 }
              yield { MinPosition = (minX, middleY', minZ); MaxPosition = (middleX, maxY, middleZ); Depth = octree.Depth + 1; Value = 0 }
              yield { MinPosition = (minX, middleY', middleZ'); MaxPosition = (middleX, maxY, maxZ); Depth = octree.Depth + 1; Value = 0 }
              yield { MinPosition = (middleX', minY, minZ); MaxPosition = (maxX, middleY, middleZ); Depth = octree.Depth + 1; Value = 0 }
              yield { MinPosition = (middleX', minY, middleZ'); MaxPosition = (maxX, middleY, maxZ); Depth = octree.Depth + 1; Value = 0 }
              yield { MinPosition = (middleX', middleY', minZ); MaxPosition = (maxX, maxY, middleZ); Depth = octree.Depth + 1; Value = 0 }
              yield { MinPosition = (middleX', middleY', middleZ'); MaxPosition = (maxX, maxY, maxZ); Depth = octree.Depth + 1; Value = 0 } }

    let getAvgOfCorners' (getValue:int64*int64*int64->int) octree =
        let minX, minY, minZ = octree.MinPosition
        let maxX, maxY, maxZ = octree.MaxPosition
        seq { yield octree.MinPosition
              yield (minX, minY, maxZ)
              yield (minX, maxY, minZ)
              yield (minX, maxY, maxZ)
              yield (maxX, minY, minZ)
              yield (maxX, maxY, minZ)
              yield (maxX, minY, maxZ)
              yield octree.MaxPosition }
        |> Seq.map (getValue >> double)
        |> Seq.average
        |> int

    type Long = int64

    type Integer = int

    type Nanobot = { Position : int64*int64*int64; Radius : int64}

    type SetAction = | Add | Remove

    let getRadius nanobot = nanobot.Radius

    let getPosition nanobot = nanobot.Position

    let getX nanobot = 
        let x, _, _ = nanobot.Position
        x

    let getY nanobot = 
        let _, y, _ = nanobot.Position
        y

    let getZ nanobot = 
        let _, _, z = nanobot.Position
        z

    let getXPersp nanobot = 
        let _, y, z = nanobot.Position
        y + z

    let getYPersp nanobot = 
        let x, _, z = nanobot.Position
        x + z

    let getZPersp nanobot = 
        let x, y, _ = nanobot.Position
        x + y

    let dist (x1:int64, y1:int64, z1:int64) (x2, y2, z2) =
        Math.Sqrt(Math.Pow(double (x1 - x2), 2.) + Math.Pow(double (y1 - y2), 2.) + Math.Pow(double (z1 - z2), 2.))

    let inRadiusOf referenceNanobot (x, y, z) =
        let distance = manHatDist referenceNanobot.Position (x, y, z)
        distance <= referenceNanobot.Radius

    let countInRadius' bots pos =
        let ret = bots |> Seq.ofArray |> Seq.filter (fun bot -> pos |> inRadiusOf bot) |> Seq.length
        ret

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.23.txt"
        let matches = Regex.Matches(input, "pos=<(.+),(.+),(.+)>, r=(\d+)")

        let bots =
            matches
            |> Seq.cast
            |> Seq.map (fun (data : Match) -> 
                let position = Long.Parse data.Groups[1].Value, Long.Parse data.Groups[2].Value, Long.Parse data.Groups[3].Value
                let radius = Long.Parse data.Groups[4].Value
                { Position = position; Radius = radius})
            |> Seq.toArray

        let countInRadius = countInRadius' bots

        let maxRadiusNanobot =
            bots
            |> Seq.ofArray
            |> Seq.maxBy getRadius

        let inRadiusOfMaxRadiusNanobot = inRadiusOf maxRadiusNanobot

        let result1 =
            bots
            |> Seq.ofArray
            |> Seq.filter (fun bot -> bot.Position |> inRadiusOfMaxRadiusNanobot)
            |> Seq.length
            
        let clique = 
            (Set.empty, bots |> Seq.ofArray)
            ||> Seq.fold (fun set bot1 ->
                let set = 
                    if set |> Set.toSeq |> Seq.forall (fun bot2 -> (bot1.Position, bot2.Position) ||> manHatDist < (bot1.Radius + bot2.Radius)) then
                        (bot1, set) ||> Set.add
                    else set
                set)

        let cliqueAsArray = clique |> Set.toArray

        let cliqueLength = cliqueAsArray.Length

        let minX, minY, minZ = int64 Integer.MinValue, int64  Integer.MinValue, int64  Integer.MinValue
        let maxX, maxY, maxZ = int64 Integer.MaxValue, int64  Integer.MaxValue, int64  Integer.MaxValue

        let value = (-1L, -1L, -1L) |> countInRadius

        let pq = 
            PriorityQueue.empty true 
            |> PriorityQueue.insert 
                { MinPosition = minX, minY, minZ
                  MaxPosition = maxX, maxY, maxZ
                  Depth = 0
                  Value = value }

        let getAvgOfCorners = getAvgOfCorners' countInRadius

        let result2 =
            (pq, value)
            |> Seq.unfold (fun (pq, value)  ->
                if pq.IsEmpty || value = cliqueLength then
                    None
                else
                    let current, pq = pq |> PriorityQueue.pop
                    let pq =
                        current
                        |> splitIntoSubOctrees
                        |> Seq.map(fun octree ->
                            { octree with Value = octree |> getAvgOfCorners })
                        |> asFirst pq
                        ||> Seq.fold(fun pq octree ->
                            (octree, pq) ||>PriorityQueue.insert)
                    let pos = (current.MinPosition, current.MaxPosition) ||> middlePoint
                    Some(pos, (pq, current.Value)))
            |> Seq.tryLast

        let result2 = ((0L, 0L, 0L), result2 |> Option.defaultValue (0L, 0L, 0L)) ||> manHatDist
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day24 =
    
    type DamageType = | Fire | Cold | Slashing | Radiation | Bludgeoning

    type Group = { Units: int32; HP: int32; Immunities: Set<DamageType>; Weaknesses: Set<DamageType>; Damage: int32; DamageType: DamageType; Initiative: int32 }

    let ImmuneSystem =
        [| 
        { Units = 76;   HP = 3032;  Immunities = Set.empty;                            Weaknesses = Set.empty;                      Damage = 334;  DamageType = Radiation;   Initiative = 7  }
        { Units = 4749; HP = 8117;  Immunities = Set.empty;                            Weaknesses = Set.empty;                      Damage = 16;   DamageType = Bludgeoning; Initiative = 16 }
        { Units = 4044; HP = 1287;  Immunities = [Radiation; Fire] |> Set.ofList;      Weaknesses = Set.empty;                      Damage = 2;    DamageType = Fire;        Initiative = 20 }
        { Units = 1130; HP = 11883; Immunities = Set.empty;                            Weaknesses = [Radiation] |> Set.ofList;      Damage = 78;   DamageType = Radiation;   Initiative = 14 }
        { Units = 1698; HP = 2171;  Immunities = Set.empty;                            Weaknesses = [Slashing; Fire] |> Set.ofList; Damage = 11;   DamageType = Bludgeoning; Initiative = 12 }
        { Units = 527;  HP = 1485;  Immunities = Set.empty;                            Weaknesses = Set.empty;                      Damage = 26;   DamageType = Bludgeoning; Initiative = 17 }
        { Units = 2415; HP = 4291;  Immunities = [Radiation] |> Set.ofList;            Weaknesses = Set.empty;                      Damage = 17;   DamageType = Cold;        Initiative = 5  }
        { Units = 3266; HP = 6166;  Immunities = [Cold; Slashing] |> Set.ofList;       Weaknesses = [Radiation] |> Set.ofList;      Damage = 17;   DamageType = Bludgeoning; Initiative = 18 }
        { Units = 34;   HP = 8390;  Immunities = [Cold; Fire; Slashing] |> Set.ofList; Weaknesses = Set.empty;                      Damage = 2311; DamageType = Cold;        Initiative = 10 }
        { Units = 3592; HP = 5129;  Immunities = [Cold; Fire] |> Set.ofList;           Weaknesses = [Radiation] |> Set.ofList;      Damage = 14;   DamageType = Radiation;   Initiative = 11 }
        |]

    let Infection =
        [| 
        { Units = 3748; HP = 11022; Immunities = Set.empty;                             Weaknesses = [Bludgeoning] |> Set.ofList;       Damage = 4;  DamageType = Bludgeoning; Initiative = 6  }
        { Units = 2026; HP = 11288; Immunities = Set.empty;                             Weaknesses = [Fire; Slashing] |> Set.ofList;    Damage = 10; DamageType = Slashing;    Initiative = 13 }
        { Units = 4076; HP = 23997; Immunities = [Cold] |> Set.ofList;                  Weaknesses = Set.empty;                         Damage = 11; DamageType = Bludgeoning; Initiative = 19 }
        { Units = 4068; HP = 40237; Immunities = [Cold] |> Set.ofList;                  Weaknesses = [Slashing] |> Set.ofList;          Damage = 18; DamageType = Slashing;    Initiative = 4  }
        { Units = 3758; HP = 16737; Immunities = Set.empty;                             Weaknesses = [Slashing] |> Set.ofList;          Damage = 6;  DamageType = Radiation;   Initiative = 2  }
        { Units = 1184; HP = 36234; Immunities = [Cold] |> Set.ofList;                  Weaknesses = [Bludgeoning; Fire] |> Set.ofList; Damage = 60; DamageType = Radiation;   Initiative = 1  }
        { Units = 1297; HP = 36710; Immunities = [Cold] |> Set.ofList;                  Weaknesses = Set.empty;                         Damage = 47; DamageType = Fire;        Initiative = 3  }
        { Units =  781; HP = 18035; Immunities = [Bludgeoning; Slashing] |> Set.ofList; Weaknesses = Set.empty;                         Damage = 36; DamageType = Fire;        Initiative = 15 }
        { Units = 1491; HP = 46329; Immunities = [Bludgeoning; Slashing] |> Set.ofList; Weaknesses = Set.empty;                         Damage = 56; DamageType = Fire;        Initiative = 8  }
        { Units = 1267; HP = 34832; Immunities = [Cold] |> Set.ofList;                  Weaknesses = Set.empty;                         Damage = 49; DamageType = Radiation;   Initiative = 9  }
        |]
    
    let ImmuneSystem_0 =
        [| 
        { Units = 17;  HP = 5390; Immunities = Set.empty;            Weaknesses = [Radiation; Bludgeoning] |> Set.ofList; Damage = 4507; DamageType = Fire;     Initiative = 2 }
        { Units = 989; HP = 1274; Immunities = [Fire] |> Set.ofList; Weaknesses = [Bludgeoning; Slashing] |> Set.ofList;  Damage = 25;   DamageType = Slashing; Initiative = 3 }
        |]

    let Infection_0 =
        [| 
        { Units = 801;  HP = 4706; Immunities = Set.empty;                 Weaknesses = [Radiation] |> Set.ofList;  Damage = 116; DamageType = Bludgeoning; Initiative = 1 }
        { Units = 4485; HP = 2961; Immunities = [Radiation] |> Set.ofList; Weaknesses = [Fire; Cold] |> Set.ofList; Damage = 12;  DamageType = Slashing;    Initiative = 4 }
        |]

    let effectivePower group = group.Units * group.Damage

    let isRemoved group = group.Units <= 0

    let isImmuneSystem immuneSystem group =
        (group, immuneSystem) ||> Array.contains

    let isInfection infection group =
        (group, infection) ||> Array.contains

    let getsDamageFrom group opponent = 
        if (group.DamageType, opponent.Immunities) ||> Set.contains then
            0
        else
            let baseDamage = group |> effectivePower
            if (group.DamageType, opponent.Weaknesses) ||> Set.contains then
                baseDamage * 2
            else
                baseDamage

    let sortWithInitiative x y =
        if x.Initiative > y.Initiative then -1
        elif x.Initiative < y.Initiative then 1
        else 0

    let sortWithEffectivePowerThanInitiative x y =
        let xEffectivePower, yEffectivePower = x |> effectivePower, y |> effectivePower
        if xEffectivePower > yEffectivePower then -1
        elif xEffectivePower < yEffectivePower then 1
        else (x, y) ||> sortWithInitiative

    let chooseTarget opponents selected group =
        let maybeTarget =
            opponents
            |> Seq.ofArray
            |> Seq.filter (isRemoved >> not)
            |> Seq.filter (fun opponent -> (opponent, selected) ||> Set.contains |> not)
            |> Seq.sortWith (fun x y ->
                let xDamage, yDamage = x |> getsDamageFrom group, y |> getsDamageFrom group
                if xDamage > yDamage then -1
                elif xDamage < yDamage then 1
                else (x, y) ||> sortWithEffectivePowerThanInitiative)
            |> Seq.tryHead
        match maybeTarget with
        | Some target when target |> getsDamageFrom group <= 0 -> None
        | Some target -> Some target
        | None -> None


    let targetSelectionPhase immuneSystem infection =
        let immuneSystemMap = 
            immuneSystem 
            |> Seq.ofArray 
            |> Seq.mapi (fun i group -> group, i)
            |> Map.ofSeq
        let infectionMap = 
            infection 
            |> Seq.ofArray 
            |> Seq.mapi (fun i group -> group, i)
            |> Map.ofSeq
        let seq, _ =
            immuneSystem |> Seq.ofArray
            |> Seq.append (infection |> Seq.ofArray)
            |> Seq.sortWith sortWithEffectivePowerThanInitiative
            |> asFirst Set.empty
            ||> Seq.mapFold (fun set group ->
                let isImmuneSystem = (immuneSystem, group) ||> isImmuneSystem
                let opponents = if isImmuneSystem then infection else immuneSystem
                let maybeSelected = group |> chooseTarget opponents set
                match maybeSelected, isImmuneSystem with
                | Some opponent, true ->
                    let set = (opponent, set) ||> Set.add
                    let getAttacker() = (immuneSystem, immuneSystemMap[group]) ||> Array.get
                    let getOpponent() = (infection, infectionMap[opponent]) ||> Array.get
                    let setOpponent newOpponent = (infection, infectionMap[opponent], newOpponent) |||> Array.set
                    Some (getAttacker, getOpponent, setOpponent), set
                | Some opponent, false ->
                    let set = (opponent, set) ||> Set.add
                    let getAttacker() = (infection, infectionMap[group]) ||> Array.get
                    let getOpponent() = (immuneSystem, immuneSystemMap[opponent]) ||> Array.get
                    let setOpponent newOpponent = (immuneSystem, immuneSystemMap[opponent], newOpponent) |||> Array.set
                    Some (getAttacker, getOpponent, setOpponent), set
                | None, _ -> None, set)
        seq |> Seq.choose identity

    let takeDamage damage group =
        { group with Units = group.Units - damage / group.HP }
        
    let attackPhase orders =
        orders
        |> Seq.sortByDescending (fun (getAttacker, _, _) -> getAttacker().Initiative)
        |> Seq.iter(fun (getAttacker, getOpponent, setOpponent) ->
            let attacker, opponent = getAttacker(), getOpponent()
            if attacker |> isRemoved |> not then
                let damage = opponent |> getsDamageFrom attacker
                let updatedOpponent = opponent |> takeDamage damage
                setOpponent updatedOpponent)

    let countRemainingUnits groups =
        groups 
        |> Seq.ofArray
        |> Seq.filter (isRemoved >> not)
        |> Seq.sumBy (fun group -> group.Units)

    let go() =

        let war boost =
            let immuneSystem = ImmuneSystem |> Array.map (fun group -> { group with Damage = group.Damage + boost })
            let infection = Infection |> Array.copy
            let _ =
                (-1, -1)
                |> Seq.unfold(fun (lastImmuneSystemCount, lastInfectionCount) ->
                    let isImmuneSystemDown = immuneSystem |> Array.forall isRemoved
                    let isInfectionDown = infection |> Array.forall isRemoved
                    if isImmuneSystemDown || isInfectionDown then
                        None
                    else
                        let orders = (immuneSystem, infection) ||> targetSelectionPhase
                        orders |> attackPhase
                        let countImmuneSystem, countInfection = immuneSystem |> countRemainingUnits, infection |> countRemainingUnits
                        if countImmuneSystem = lastImmuneSystemCount && countInfection = lastInfectionCount then
                            None
                        else
                            Some (1, (countImmuneSystem, countInfection)))
                |> Seq.last
            immuneSystem, infection

        let immuneSystem1, infection1 = war 0
        
        let isImmuneSystemDown = immuneSystem1 |> Array.forall isRemoved
        let winners = if isImmuneSystemDown then infection1 else immuneSystem1

        let result1 = winners |> countRemainingUnits

        let immuneSystem2 =
            (110, false)
            |> Seq.unfold (fun (boost, abort) ->
                if abort then
                    None
                else
                    let immuneSystem, infection = war boost
                    Some (immuneSystem, (boost + 1, infection |> Array.forall isRemoved)))
            |> Seq.last

        let result2 = immuneSystem2  |> countRemainingUnits

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day25 =
    open System
    open FSharpx.Collections

    type Integer = int

    let manHatDist (x1:int, y1:int, z1:int, t1:int) (x2, y2, z2, t2) =
        Math.Abs(x1 - x2) + Math.Abs(y1 - y2) + Math.Abs(z1 - z2) + Math.Abs(t1 - t2)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2018.25.txt"
        let lines = input.Split([| Environment.NewLine |], StringSplitOptions.None)

        let fixedPoints =
            lines
            |> Array.map (fun line ->
                let arr =
                    line.Split ([| ',' |], StringSplitOptions.None)
                    |> Array.map Integer.Parse
                arr[0], arr[1], arr[2], arr[3])

        let edgeMap =
            fixedPoints
            |> Seq.ofArray
            |> Seq.map (fun point ->
                let adjacentNodes =
                    fixedPoints
                    |> Seq.ofArray
                    |> Seq.filter (fun p -> p <> point)
                    |> Seq.filter (fun p -> (point, p) ||> manHatDist <= 3)
                    |> Seq.toList
                point, adjacentNodes)
            |> Map.ofSeq

        let result1 =
            (1, Set.empty)
            |> Seq.unfold (fun (i, set) ->
                if set |> Set.count = fixedPoints.Length then
                    None
                else
                    let pickedPoint = fixedPoints |> Seq.ofArray |> Seq.filter (fun p -> (p, set) ||> Set.contains |> not) |> Seq.head
                    let queue = Queue.empty |> Queue.conj pickedPoint
                    let set = 
                        (set, queue)
                        |> Seq.unfold (fun (set, queue) -> 
                            if queue |> Queue.isEmpty then
                                None
                            else
                                let currentPoint, queue = queue |> Queue.uncons
                                let set = (currentPoint, set) ||> Set.add

                                let queue =
                                    (queue, edgeMap[currentPoint] |> Seq.ofList |> Seq.filter (fun p -> (p, set) ||> Set.contains |> not))
                                    ||> Seq.fold (fun queue point ->
                                        (point, queue) ||> Queue.conj)

                                Some (set, (set, queue)))
                        |> Seq.last
                    Some(i, (i + 1, set)))
            |> Seq.last

        let result2 = 2

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
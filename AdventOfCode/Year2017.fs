﻿module Year2017

open Domain
open Operations

module Day1 =
    let calculateCaptcha (input:string) =
        input
        |> Seq.append (seq { yield (input |> Seq.last) })
        |> Seq.pairwise
        |> Seq.filter (fun (c1, c2) -> c1 = c2)
        |> Seq.map fst
        |> Seq.map (fun c -> Integer.Parse (sprintf "%c" c))
        |> Seq.sum

    let calculateNextCaptcha (input:string) =
        let half = input.Length / 2
        let firstArray = input |> Seq.take half |> Seq.toArray
        let secondArray = input |> Seq.skip half |> Seq.toArray
        firstArray 
        |> Seq.mapi (fun i c -> i,c) 
        |> Seq.filter (fun (i, c) -> c = ((secondArray, i) ||> Array.get))
        |> Seq.map (fun (_, c) -> Integer.Parse (sprintf "%c" c) * 2)
        |> Seq.sum
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.01.txt"

        let result1 = input |> calculateCaptcha

        let result2 = input |> calculateNextCaptcha

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day2 =
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.map(fun line -> line.Split([| '\t' |], System.StringSplitOptions.None) |> Seq.map Integer.Parse |> Seq.toList)
        |> Seq.toList

    let sumOfSpans lines =
        lines
        |> Seq.map (fun line -> (line |> List.max) - (line |> List.min))
        |> Seq.sum
        
    let sumOfDivisions lines =
        lines
        |> Seq.map (fun line -> 
            let numbers = line |> Seq.mapi (fun i n -> i, n)
            numbers 
            |> Seq.allPairs numbers 
            |> Seq.filter (fun ((i1, n1), (i2, n2)) -> i1 <> i2 && n1 % n2 = 0) 
            |> Seq.map (fun ((_, n1), (_, n2)) -> n1 / n2) 
            |> Seq.head)
        |> Seq.sum

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.02.txt"

        let result1 = input |> parse |> sumOfSpans

        let result2 = input |> parse |> sumOfDivisions

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day3 =    
    open System

    let parse input = input |> Integer.Parse
    
    let calculateFirst number =        
        let sideLength = 
            let temp = Math.Ceiling(Math.Sqrt(number |> double)) |> int
            if temp % 2 = 0 then temp + 1 else temp 
        let prevSideLength = sideLength - 2
        let lengthToNumber = number - prevSideLength * prevSideLength
        let halfSideLength = (sideLength - 1) / 2
        Math.Abs(lengthToNumber % (sideLength - 1) - halfSideLength) + halfSideLength
            
    let calculateSecond number =        
        [| 1; 2; 4; 5; 10; 11; 23; 25 |] 
        |> Seq.unfold (fun prevArray ->
            let prevSideLength = prevArray.Length / 4
            let sideLength = prevSideLength + 2
            let array = 
                seq { 0 .. 3 }
                |> Seq.collect (fun side -> seq { 0 .. (sideLength - 1) } |> Seq.map (fun i -> side, i))
                |> asFirst (prevArray |> Array.last, (prevArray, prevArray.Length - 2) ||> Array.get)
                ||> Seq.scan (fun (prevValue, prePrevValue) (side, i) ->
                    let nextValue =
                        if i = sideLength - 1 then
                            prevValue + ((prevArray, prevSideLength - 1 + side * prevSideLength) ||> Array.get)
                        elif i = 0 then 
                            if side = 0 then prevValue + (prevArray |> Array.head)
                            else 
                                let cornerIndex = (side * prevSideLength - 1)
                                prevValue + ((prevArray, cornerIndex) ||> Array.get) + ((prevArray, cornerIndex + 1) ||> Array.get) + prePrevValue
                        elif i = sideLength - 2 then
                            let cornerIndex = prevSideLength - 1 + side * prevSideLength
                            prevValue + ((prevArray, cornerIndex) ||> Array.get) + ((prevArray, cornerIndex - 1) ||> Array.get)
                        else
                            let index = i - 1 + side * prevSideLength
                            let specialValue = if index = 0 then prevArray |> Array.last else (prevArray, index - 1) ||> Array.get
                            prevValue + ((prevArray, index) ||> Array.get) + ((prevArray, index + 1) ||> Array.get) + specialValue
                    nextValue, prevValue)
                |> Seq.skip 1
                |> Seq.map fst
                |> Seq.toArray
            Array.set array (array.Length - 2) (((array, (array.Length - 2)) ||> Array.get) + (array |> Array.head))
            Array.set array (array.Length - 1) (((array |> Array.last) + (array |> Array.head) * 2))
            Some(prevArray, array))
        |> Seq.collect identity
        |> Seq.filter (fun currentNumber -> currentNumber > number)
        |> Seq.head

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.03.txt"
        
        let result1 = input |> parse |> calculateFirst

        let result2 = input |> parse |> calculateSecond

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day4 =
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.map (fun line -> line.Split([| ' ' |], System.StringSplitOptions.None) |> Seq.toList)
        |> Seq.toList
        
    let solve wordPreprocessor listOfWords =
        listOfWords
        |> Seq.filter (fun words -> 
            words 
            |> wordPreprocessor
            |> Seq.countBy identity 
            |> Seq.exists (fun (_, count) -> count > 1) 
            |> not)
        |> Seq.length

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.04.txt"

        let listOfWords = input |> parse

        let result1 = listOfWords |> solve identity

        let result2 = listOfWords |> solve (Seq.map(fun word -> word |> Seq.sort |> asFirst "" ||> Seq.fold (fun text c -> sprintf "%s%c" text c)))

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day5 =
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Array.map Integer.Parse

    let solve decreasePredicate instructions =
        0
        |> Seq.unfold (fun i ->
            if i < 0 || i >= (instructions |> Array.length) then None
            else
                let value = ((instructions, i) ||> Array.get)
                (instructions, i, if decreasePredicate value then value - 1 else value + 1) |||> Array.set 
                let nextI = i + value
                Some (nextI, nextI))
        |> Seq.length

    let solveFirst = solve (fun _ -> false)

    let solveSecond = solve (fun i -> i >= 3)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.05.txt"

        let result1 = input |> parse |> solveFirst

        let result2 = input |> parse |> solveSecond

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day6 =
    let parse (input:string) =
        input.Split([| '\t' |], System.StringSplitOptions.None)
        |> Array.map Integer.Parse

    let memoryReallocation memory =
        let map = Map.empty |> Map.add (memory |> Array.toList) 0
        (0, map, false)
        |> Seq.unfold (fun (prevStep, map, abort) ->
            if abort then None
            else
                let max = memory |> Array.max
                let index = 
                    memory 
                    |> Seq.mapi (fun i m -> i, m) 
                    |> Seq.filter (fun (_, m) -> m = max) 
                    |> Seq.head 
                    |> fst
                let portion = max / memory.Length
                let modulo = max % memory.Length
                (index, 0) ||> Array.set memory
                seq { 0 .. (modulo - 1) }
                |> Seq.map (fun i -> (index + i + 1) % memory.Length)
                |> Seq.iter (fun i -> (i, (i |> Array.get memory) + portion + 1) ||> Array.set memory)
                seq { modulo .. memory.Length - 1 }
                |> Seq.map (fun i -> (index + i + 1) % memory.Length)
                |> Seq.iter (fun i -> (i, (i |> Array.get memory) + portion) ||> Array.set memory)
                let thisState = memory |> Array.toList
                let step = prevStep + 1
                match map |> Map.tryFind thisState with
                | Some firstOccurence -> Some ((step, step - firstOccurence), (step, map, true))
                | None -> 
                    let map = map |> Map.add thisState step
                    Some ((step, step), (step, map, false)))
        |> Seq.last
        

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.06.txt"

        let (result1, result2) = input |> parse |> memoryReallocation

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day7 =
    type Node = { Label:string; Value:int; Children: Node list }
    
    let parse (input:string) =
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        let nodeMap =
            lines
            |> Seq.choose (fun line ->
                match line with
                | Regex "(.+) \((\d+)\)" (label::textValue::[]) -> Some { Label = label; Value = Integer.Parse textValue; Children = [] }
                | _ -> None)
            |> Seq.map (fun node -> node.Label, node)
            |> Map.ofSeq
        let childrenMap =
            lines
            |> Seq.choose (fun line ->
                match line with
                | Regex "(.+) \(\d+\) \-\> (.*)" (label::textChildren::[]) -> Some (label, textChildren.Split([| ", " |], System.StringSplitOptions.None) |> Array.toList)
                | Regex "(.+) \(\d+\)" (label::[]) -> Some (label, [])
                | _ -> None)
            |> Map.ofSeq
        nodeMap, childrenMap

    let getRoot nodeMap childrenMap =
        nodeMap 
        |> Map.toSeq
        |> Seq.map (fun (key, _) -> key)
        |> Seq.filter (fun label -> 
            childrenMap 
            |> Map.toSeq 
            |> Seq.exists(fun (_, value) -> value |> Seq.exists (fun l -> l = label)) 
            |> not)
        |> Seq.exactlyOne

    let rec calculateWeightOrDetermineError label nodeMap childrenMap =
        let children = childrenMap |> Map.find label
        let value = (nodeMap |> Map.find label).Value
        if children |> List.isEmpty then
            value, None
        else
            let childrenResults =
                children
                |> Seq.map (fun childLabel -> childLabel, (nodeMap, childrenMap) ||> calculateWeightOrDetermineError childLabel)
                |> Seq.toList
            match childrenResults |> List.tryPick (snd >> snd) with
            | Some x -> value, Some x
            | None ->
                let counts =
                    childrenResults
                    |> Seq.map (snd >> fst)
                    |> Seq.countBy identity
                    |> Seq.toArray
                if counts.Length = 1 then value + (counts.[0] |> fst) * (counts.[0] |> snd), None
                else
                    let min = counts |> Seq.minBy snd |> fst
                    let max = counts |> Seq.maxBy snd |> fst
                    let childLabel = childrenResults |> Seq.filter (fun (_, (v, _)) -> v = min) |> Seq.map fst |> Seq.head
                    value, Some ((nodeMap |> Map.find childLabel).Value + max - min)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.07.txt"

        let maps = input |> parse

        let result1 = maps ||> getRoot

        let result2 = maps ||> calculateWeightOrDetermineError result1 |> snd |> Option.defaultValue 0

        { First = result1; Second = sprintf "%d" result2 }

module Day8 =
    open System
    type Operator = | Increase | Decrease
    type Comparator = | Equal | Unequal | Greater | GreaterOrEqual | Less | LessOrEqual 
    type Execution = { Identifier:string; Operator:Operator; Value:int }
    type Condition = { Identifier:string; Comparator:Comparator; Value:int }
    
    type Instruction = { Execution:Execution; Condition:Condition }
    
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line -> 
            match line with
            | Regex "(.+) (inc|dec) (.+) if (.+) (==|!=|\>|\<|\<=|\>=) (.+)" (exeId::op::exeVal::condId::comp::condVal::[]) ->
                let operator = match op with | "inc" -> Increase | _ -> Decrease
                let exeValue = Integer.Parse exeVal
                let comparator = match comp with | "==" -> Equal | "!=" -> Unequal | ">" -> Greater | ">=" -> GreaterOrEqual | "<" -> Less | _ -> LessOrEqual
                let condValue = Integer.Parse condVal
                Some { Execution = { Identifier = exeId; Operator = operator; Value = exeValue }; Condition = { Identifier = condId; Comparator = comparator; Value = condValue } }
            | _ -> None)
        |> Seq.toList

    let initialize instructions =
        instructions
        |> Seq.collect (fun instruction -> seq { yield instruction.Execution.Identifier; yield instruction.Condition.Identifier })
        |> Seq.distinct
        |> Seq.map (fun identifier -> identifier, 0)
        |> Map.ofSeq

    let execute map (execution:Execution) = 
        let currentValue = map |> Map.find execution.Identifier
        let nextValue =
            match execution.Operator with
            | Increase -> currentValue + execution.Value
            | Decrease -> currentValue - execution.Value
        map |> Map.remove execution.Identifier |> Map.add execution.Identifier nextValue

    let checkCondition map condition =
        let value = map  |> Map.find condition.Identifier
        match condition.Comparator with
        | Equal -> value = condition.Value
        | Unequal -> value <> condition.Value
        | Greater -> value > condition.Value
        | Less -> value < condition.Value
        | GreaterOrEqual -> value >= condition.Value
        | LessOrEqual -> value <= condition.Value

    let iterateInstructions instructions =
        let getMaxOf m = m |> Map.toSeq |> Seq.map snd |> Seq.max

        let map = instructions |> initialize
        let (map, globalMax) =
            ((map, Integer.MinValue), instructions)
            ||> Seq.fold (fun (map, currentMax) instruction ->
                let map = if instruction.Condition |> checkCondition map then instruction.Execution |> execute map else map
                (map, Math.Max(map |> getMaxOf, currentMax)))
        map |> getMaxOf, globalMax

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.08.txt"

        let (result1, result2) = input |> parse |> iterateInstructions

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day9 =
    type State = | OutsideOfGarbage | InsideOfGarbage | InsideOfGarbageAndIgnoreNext

    let traverse (input:string) =
        let (_, score, letters, _) =
            ((OutsideOfGarbage, 0, 0, 1), input)
            ||> Seq.fold (fun (state, score, letters, level) c ->
                match state, c with
                | OutsideOfGarbage, '{' -> state, score + level, letters, level + 1
                | OutsideOfGarbage, '}' -> state, score, letters, level - 1
                | OutsideOfGarbage, '<' -> InsideOfGarbage, score, letters, level
                | OutsideOfGarbage, _ -> state, score, letters, level
                | InsideOfGarbage, '>' -> OutsideOfGarbage, score, letters, level
                | InsideOfGarbage, '!' -> InsideOfGarbageAndIgnoreNext, score, letters, level
                | InsideOfGarbage, _ -> InsideOfGarbage, score, letters + 1, level
                | InsideOfGarbageAndIgnoreNext, _ -> InsideOfGarbage, score, letters, level)
        score, letters

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.09.txt"

        let (result1, result2) = input |> traverse

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day10 =
    let parseFirst (input:string) =
        input.Split([| "," |], System.StringSplitOptions.None)
        |> Array.map Integer.Parse

    let parseSecond (input:string) =
        ([| for c in input do yield c |> int |], [| 17; 31; 73; 47; 23 |]) ||> Array.append

    let round pos skip (lengths: int array) (field: byte array) =
        ((pos, skip), lengths)
        ||> Array.fold (fun (pos, skip) length ->
            let reversedSection = Array.init length (fun i -> field.[(pos + i) % field.Length]) |> Array.rev
            seq { 0 .. length - 1} 
            |> Seq.iter (fun i -> 
                field.[(pos + i) % field.Length] <- reversedSection.[i])
            (pos + length + skip) % field.Length, skip + 1)
            
    let solveFirst input =
        let field = [| 0uy .. 255uy |]
        let lengths = input |> parseFirst
        let _ = round 0 0 lengths field
        (field.[0] |> int) * (field.[1] |> int)
                    
    let solveSecond input =
        let field = [| 0uy .. 255uy |]
        let lengths = input |> parseSecond
        let _ = 
            ((0, 0), seq { 0 .. 63 }) 
            ||> Seq.fold (fun (pos, skip) _ -> round pos skip lengths field)
        field 
        |> Seq.chunkBySize 16 
        |> Seq.map (fun bytes -> (0uy, bytes) ||> Seq.fold (^^^))
        |> Seq.map (fun byte -> System.String.Format("{0:X2}", byte).ToLower())
        |> String.concat ""
        
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.10.txt"

        let result1 = input |> solveFirst

        let result2 = input |> solveSecond

        { First = sprintf "%d" result1; Second = result2 }

module Day11 =
    open System
    
    let parse (input:string) =
        input.Split([| "," |], System.StringSplitOptions.None)

    let round instructions = 
        let compareOpposites firstSide secondSide map =
            let (firstValue, secondValue) = map |> Map.tryFind firstSide, map |> Map.tryFind secondSide
            let (firstValue, secondValue) =
                match firstValue, secondValue with
                | Some fv, Some sv when fv > sv -> fv - sv, 0
                | Some fv, Some sv when sv >= fv -> 0, sv - fv
                | Some fv, None -> fv, 0
                | None, Some sv -> 0, sv
                | _ -> 0, 0
            map 
            |> Map.remove firstSide 
            |> Map.remove secondSide
            |> Map.add firstSide firstValue
            |> Map.add secondSide secondValue

        let adjustDirection counterclockwise middle clockwise map =
            let (ccValue, cValue) = map |> Map.find counterclockwise, map |> Map.find clockwise
            if ccValue > 0 && cValue > 0 then
                let diff = Math.Min(ccValue, cValue)
                let mValue = map |> Map.find middle
                let map = 
                    map
                    |> Map.remove counterclockwise
                    |> Map.remove clockwise
                    |> Map.remove middle
                    |> Map.add middle (mValue + diff)
                    |> Map.add counterclockwise (ccValue - diff)
                    |> Map.add clockwise (cValue - diff)
                true, map
            else false, map
            
        instructions 
        |> Seq.countBy identity 
        |> Map.ofSeq
        |> compareOpposites "n" "s"
        |> compareOpposites "ne" "sw"
        |> compareOpposites "nw" "se"
        |> asFirst false
        |> asSecond (seq { 
                            yield "se", "s", "sw";
                            yield "s", "sw", "nw";
                            yield "sw", "nw", "n";
                            yield "nw", "n", "ne";
                            yield "n", "ne", "se";
                            yield "ne", "se", "s"})
        ||> Seq.fold (fun (abort, map) (cc, m, c) ->
            if abort then true, map 
            else map |> adjustDirection cc m c)
        |> snd
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sum


    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.11.txt"

        let instructions = input |> parse

        let result1 = instructions |> round

        let result2 = 
            seq { 1 .. instructions.Length } 
            |> Seq.map (fun i -> instructions |> Seq.take i |> round) 
            |> Seq.max

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day12 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.12.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day13 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.13.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day14 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.14.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day15 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.15.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day16 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.16.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day17 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.17.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day18 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.18.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day19 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.19.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day20 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.20.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day21 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.21.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day22 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.22.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day23 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.23.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day24 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.24.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day25 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.25.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

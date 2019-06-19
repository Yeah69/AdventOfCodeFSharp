module Year2017

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
                    
    let knotHash input =
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

        let result2 = input |> knotHash

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
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line ->
            match line with
            | Regex "(\d+) <-> (.+)" (textKey::textAdjacents::[]) -> Some (Integer.Parse textKey, textAdjacents.Split([| ", " |], System.StringSplitOptions.None) |> Seq.map Integer.Parse |> Seq.toList)
            | _ -> None)
        |> Map.ofSeq

    let rec visit map current visited remaining =
        let visited = visited |> Set.add current
        let remaining = remaining |> Set.remove current
        let toIterate = map |> Map.find current |> List.where (fun key -> remaining |> Set.contains key)
        if toIterate |> List.length = 0 then
            visited, remaining
        else
            ((visited, remaining), toIterate)
            ||> Seq.fold (fun (visited, remaining) key ->
                visit map key visited remaining)
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.12.txt"

        let map = input |> parse

        let visit = visit map

        let (visited, remaining) = visit 0 Set.empty (seq { 0 .. 1999 } |> Set.ofSeq)

        let result1 = visited |> Set.count

        let result2 = 
            (visited, remaining) 
            |> Seq.unfold (fun (visited, remaining) -> 
               if remaining |> Set.count = 0 then None
               else Some (1, visit (remaining |> Seq.head) visited remaining))
            |> Seq.sum
        let result2 = result2 + 1

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day13 =
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line -> 
            match line with 
            | Regex "(\d+): (\d+)" (textDepth::textRange::[]) -> Some(textDepth |> Integer.Parse, textRange |> Integer.Parse)
            | _ -> None)
        |> Seq.toList
        
    let solveFirst firewall =
        (0, firewall) 
        ||> Seq.fold (fun severity (depth, range) ->
            if depth % (2 * range - 2) = 0 then severity + depth * range
            else severity)
                    
    let solveSecond firewall =
        seq { 0 .. Integer.MaxValue }
        |> Seq.where (fun delay ->
            firewall
            |> Seq.exists (fun (depth, range) ->
                (depth + delay) % (2 * range - 2) = 0)
            |> not)
        |> Seq.head

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.13.txt"

        let firewall = input |> parse

        let result1 = firewall |> solveFirst

        let result2 = firewall |> solveSecond

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day14 =
    let parse input =
        seq { 0 .. 127 }
        |> Seq.map (fun i -> sprintf "%s-%d" input i)
        |> Seq.map Day10.knotHash
        |> Seq.map (fun hash -> 
            hash 
            |> Seq.collect (fun c -> 
                let number = 
                    if c >= '0' && c <= '9' then ((c |> int) - ('0' |> int))
                    else ((c |> int) - ('a' |> int)) + 10
                seq { yield (number &&& 8) > 0; yield (number &&& 4) > 0; yield (number &&& 2) > 0; yield (number &&& 1) > 0; })
            |> Seq.toArray)
        |> Seq.toArray

    let countTurnedOn field =
        field |> Seq.collect identity |> Seq.filter identity |> Seq.length

    let turnedOnPositions field =
        field 
        |> Seq.mapi (fun y arr -> y, arr) 
        |> Seq.collect (fun (y, arr) -> 
            arr 
            |> Seq.mapi (fun x value -> x, value) 
            |> Seq.filter snd 
            |> Seq.map (fun (x, _) -> x, y))
        |> Set.ofSeq

    let countRegions field =
        let turnedOnRegionSet = field |> turnedOnPositions
        let rec inner availableSet seedPoint =
            let availableSet = 
                (availableSet, [ seedPoint ]) 
                |> Seq.unfold (fun (availableSet, points) ->
                    if points |> List.isEmpty then None
                    else
                        let availableSet = (availableSet, points) ||> Seq.fold (fun set point -> set |> Set.remove point)
                        let nextPoints =
                            points 
                            |> Seq.collect (fun (x, y) -> seq { yield x - 1, y; yield x + 1, y; yield x, y - 1; yield x, y + 1 })
                            |> Seq.filter (fun point -> availableSet |> Set.contains point)
                            |> Seq.toList
                        Some(availableSet, (availableSet, nextPoints)))
                |> Seq.last
            if availableSet |> Seq.length > 0 then 1 + inner availableSet (availableSet |> Seq.head)
            else 1
        inner turnedOnRegionSet (turnedOnRegionSet |> Seq.head)
            
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.14.txt"

        let field = input |> parse

        let result1 = field |> countTurnedOn

        let result2 = field |> countRegions

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day15 =
    let modFactor = Integer.MaxValue |> int64

    let parse (input:string) =
        let result =
            match input with
            | Regex "Generator A starts with (\d+)(?:\\r\\n|\\r|\\n)Generator B starts with (\d+)" (textA::textB::[]) -> Some (Long.Parse textA, Long.Parse textB)
            | _ -> None
        result |> Option.defaultValue (0L, 0L)

    let countMatches filterA filterB initialA initialB countOfRounds =
        let seq initial factor filter =
            initial 
            |> Seq.unfold (fun value -> 
                let next = (value * factor) % modFactor
                Some(next, next))
            |> Seq.filter filter
            |> Seq.map (fun value -> value <<< 48)

        (seq initialA 16_807L filterA, seq initialB 48_271L filterB)
        ||> Seq.map2 (=)
        |> Seq.take countOfRounds
        |> Seq.filter identity
        |> Seq.length


    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.15.txt"

        let (initialA, initialB) = input |> parse

        let result1 = (initialA, initialB, 40_000_000) |||> countMatches alwaysTrue alwaysTrue

        let result2 = (initialA, initialB, 5_000_000) |||> countMatches (fun a -> a % 4L = 0L) (fun b -> b % 8L = 0L)

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day16 =
    type Instruction = | Spin of int | Exchange of int * int | Partner of char * char

    let aAsInt = 'a' |> int

    let parse (input:string) =
        input.Split([| "," |], System.StringSplitOptions.None)
        |> Array.choose (fun line ->
            match line with
            | Regex "s(\d+)" (textX::[]) -> Some (Spin (textX |> Integer.Parse))
            | Regex "x(\d+)/(\d+)" (textX::textY::[]) -> Some (Exchange (textX |> Integer.Parse, textY |> Integer.Parse))
            | Regex "p(.)/(.)" (textA::textB::[]) -> Some (Partner (textA.Chars 0, textB.Chars 0))
            | _ -> None)

    let toOffsetAndMaps instructions =
        ((0, [| 0 .. 15 |], [| 0 .. 15 |]), instructions)
        ||> Seq.fold (fun (offset, xMap, pMap) instruction ->
            match instruction with
            | Spin x -> offset + x, xMap, pMap
            | Exchange (x, y) ->
                let prepareIndex index =
                    let index = (index - offset) % 16
                    if index < 0 then index + 16 else index
                let (x, y) = prepareIndex x, prepareIndex y
                let (one, two) = xMap.[x], xMap.[y]
                xMap.[x] <- two
                xMap.[y] <- one
                offset, xMap, pMap
            | Partner (a, b) ->
                let (x, y) = (a |> int) - aAsInt, (b |> int) - aAsInt
                let (o, t) = pMap.[x], pMap.[y]
                pMap.[x] <- t
                pMap.[y] <- o
                offset, xMap, pMap)

    let dance (poses: char array) offset xMap pMap =
        let poses = xMap |> Array.map (fun i -> poses.[i])
        let pArr = Array.create 16 0
        poses |> Array.iteri (fun i c -> pArr.[(c |> int) - aAsInt] <- i)
        let pArr = pMap |> Array.map (fun i -> pArr.[i])
        pArr |> Array.iteri (fun i c -> poses.[c] <- (i + aAsInt) |> char)
        let result = Array.create 16 'a'
        seq { 0 .. 15 } |> Seq.iter (fun i -> result.[(i + offset - 1) % 16] <- poses.[(i + offset) % 16])
        result

    let billionDances (positions: char array) offset xMap pMap =
        let (offset, positions, i) =
            ((offset, positions, 2), seq { 2 .. 1_000_000_000 })
            ||> Seq.scan (fun (offset, positions, _) i -> 
                offset % 16 + offset, (offset, xMap, pMap) |||> dance positions, i)
            |> Seq.filter (fun (_, positions, i) -> 
                (positions |> Seq.mapi (fun i c -> i, c) |> Seq.exists (fun (i, c) -> i + aAsInt <> (c |> int)) |> not) || i = 1_000_000_000)
            |> Seq.head
        if i = 1_000_000_000 then positions
        else 
            let remainder = 1_000_000_000 % i
            (positions, seq { 0 .. remainder - 1})
            ||> Seq.fold (fun positions _ -> (offset, xMap, pMap) |||> dance positions)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.16.txt"

        let (offset, xMap, yMap) = input |> parse |> toOffsetAndMaps

        let firstDance = (offset, xMap, yMap) |||> dance [| 'a' .. 'p' |]

        let result1 = firstDance |> Seq.map (fun c -> c.ToString()) |> String.concat ""

        let afterOneBillionDances = (offset, xMap, yMap) |||> billionDances firstDance

        let result2 = afterOneBillionDances |> Seq.map (fun c -> c.ToString()) |> String.concat ""

        { First = result1; Second = result2 }

module Day17 =
    let parse (input:string) = input |> Integer.Parse

    type Node = { Value: int; mutable Next: Node option}

    let runFirst skipCount =
        let initialNode = { Value = 0; Next = None }
        initialNode.Next <- Some initialNode
        let resultNode =
            (initialNode, seq { 1 .. 2017 })
            ||> Seq.fold (fun node i ->
                let nextNode = 
                    (node, seq  { 1 .. skipCount - 1 })
                    ||> Seq.fold (fun node _ -> match node.Next with | Some node -> node | None -> node)
                let newNode = { Value = i; Next = nextNode.Next }
                nextNode.Next <- Some newNode
                newNode.Next |> Option.defaultValue nextNode)
        resultNode.Value

    let runSecond skipCount =
        ((1, 1), seq { 2 .. 50_000_000 })
        ||> Seq.fold (fun (currentNextToZero, currentPosition) i ->
            let nextPosition = (currentPosition + skipCount) % i
            (if nextPosition = 0 then i else currentNextToZero), nextPosition + 1)
        |> fst
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.17.txt"

        let result1 = input |> parse |> runFirst

        let result2 = input |> parse |> runSecond

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day18 =
    open FSharpx.Collections
    type Program = 
        { 
            InstructionPointer:int
            Registers:Map<char, int64>
            Mailbox:Queue<int64>
            IsWaiting:bool
            IsTerminated:bool
            Count:int64
            FirstRcv:int64 option
        }

    let myMapSet key value map =
        let map = if map |> Map.containsKey key then map |> Map.remove key else map
        map |> Map.add key value

    let myMapFind key map =
        if map |> Map.containsKey key then map |> Map.find key, map else 0L, map |> Map.add key 0L
        
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line -> 
            let op1 regX (textValue:string) logic = 
                match Long.TryParse textValue with
                | true, valueY -> Some (fun prog foreign ->
                    let (valueX, registers) = prog.Registers |> myMapFind regX
                    { prog with Registers = registers |> myMapSet regX ((valueX, valueY) ||> logic) }, foreign)
                | false, _ -> 
                    let regY = textValue.Chars 0
                    Some (fun prog foreign ->
                        let (valueX, registers) = prog.Registers |> myMapFind regX
                        let (valueY, registers) = registers |> myMapFind regY
                        { prog with Registers = registers |> myMapSet regX ((valueX, valueY) ||> logic) }, foreign)
            match line with
            | Regex "snd (.+)" (textReg::[]) ->
                let reg = textReg.Chars 0
                Some (fun prog foreign ->
                    let (value, registers) = prog.Registers |> myMapFind reg
                    { prog with Count = prog.Count + 1L; Registers = registers }, { foreign with Mailbox = foreign.Mailbox |> Queue.conj value; IsWaiting = false })
            | Regex "set (.+) (.+)" (textReg::textValue::[]) ->
                op1 (textReg.Chars 0) textValue (fun _ valueY -> valueY)
            | Regex "add (.+) (.+)" (textReg::textValue::[]) ->
                op1 (textReg.Chars 0) textValue (fun valueX valueY -> valueX + valueY)
            | Regex "mul (.+) (.+)" (textReg::textValue::[]) ->
                op1 (textReg.Chars 0) textValue (fun valueX valueY -> valueX * valueY)
            | Regex "mod (.+) (.+)" (textReg::textValue::[]) ->
                op1 (textReg.Chars 0) textValue (fun valueX valueY -> valueX % valueY)
            | Regex "rcv (.+)" (textReg::[]) ->
                let reg = textReg.Chars 0
                Some (fun prog foreign ->
                    let firstRcv =
                        match prog.FirstRcv with
                        | Some _ -> prog.FirstRcv
                        | None -> prog.Mailbox |> Queue.toSeq |> Seq.tryLast
                    if prog.Mailbox |> Queue.isEmpty then
                        { prog with IsWaiting = true; InstructionPointer = prog.InstructionPointer - 1; FirstRcv = firstRcv }, foreign
                    else
                        let (value, mailbox) = prog.Mailbox |> Queue.uncons
                        { prog with Registers = prog.Registers |> myMapSet reg value; Mailbox = mailbox; FirstRcv = firstRcv }, foreign)
            | Regex "jgz (.+) (.+)" (textReg::textValue::[]) ->
                match Long.TryParse textReg, Long.TryParse textValue with
                | (true, value1), (true, value2) ->
                    if value1 > 0L then
                        Some (fun prog foreign ->
                            { prog with InstructionPointer = prog.InstructionPointer + (value2 |> int) - 1 }, foreign)
                    else None
                | (true, value1), (false, _) ->
                    if value1 > 0L then
                        let reg = textValue.Chars 0
                        Some (fun prog foreign ->
                            let (value, registers) = prog.Registers |> myMapFind reg
                            { prog with InstructionPointer = prog.InstructionPointer + (value |> int) - 1; Registers = registers }, foreign)
                    else None
                | (false, _), (true, value2) ->
                    let reg = textReg.Chars 0
                    Some (fun prog foreign ->
                        let (value, registers) = prog.Registers |> myMapFind reg
                        if value > 0L then
                            { prog with InstructionPointer = prog.InstructionPointer + (value2 |> int) - 1; Registers = registers }, foreign
                        else
                            { prog with Registers = registers }, foreign)
                | (false, _), (false, _) ->
                    let regX = textReg.Chars 0
                    let regY = textReg.Chars 0
                    Some (fun prog foreign ->
                        let (valueX, registers) = prog.Registers |> myMapFind regX
                        let (valueY, registers) = registers |> myMapFind regY
                        if valueX > 0L then
                            { prog with InstructionPointer = prog.InstructionPointer + (valueY |> int) - 1; Registers = registers }, foreign
                        else
                            { prog with Registers = registers }, foreign)
            | _ -> None)
        |> Seq.toArray

    let run (instructions: (Program -> Program -> Program * Program) array) =
        let prog value =
            { 
                InstructionPointer = 0
                Registers = seq { yield 'p', value } |> Map.ofSeq
                Mailbox = Queue.empty
                IsWaiting = false
                IsTerminated = false
                Count = 0L
                FirstRcv = None
            }
        let progs = 0L |> prog, 1L |> prog

        let (prog0, prog1) =
            progs
            |> Seq.unfold (fun (prog0, prog1) ->
                if prog0.IsTerminated && prog1.IsTerminated then
                    None
                else
                    let (prog0, prog1) =
                        if prog0.IsTerminated |> not then
                            instructions.[prog0.InstructionPointer] prog0 prog1
                        else prog0, prog1
                    let (prog0, prog1) =
                        if prog1.IsTerminated |> not then   
                            let (prog1, prog0) = instructions.[prog1.InstructionPointer] prog1 prog0
                            prog0, prog1
                        else prog0, prog1
                    let (prog0, prog1) =
                        if prog0.IsWaiting && prog1.IsWaiting then
                            { prog0 with IsTerminated = true }, { prog1 with IsTerminated = true }
                        else prog0, prog1
                    let (prog0, prog1) =
                        { prog0 with InstructionPointer = prog0.InstructionPointer + 1 }, { prog1 with InstructionPointer = prog1.InstructionPointer + 1 }
                    let prog0 =
                        if prog0.InstructionPointer < 0 || prog0.InstructionPointer >= (instructions |> Array.length) then
                            { prog0 with IsTerminated = true }
                        else prog0
                    let prog1 =
                        if prog1.InstructionPointer < 0 || prog1.InstructionPointer >= (instructions |> Array.length) then
                            { prog1 with IsTerminated = true }
                        else prog1
                    Some ((prog0, prog1), (prog0, prog1)))
            |> Seq.last
        prog0.FirstRcv |> Option.defaultValue 0L, prog1.Count


    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.18.txt"
        
        let (result1, result2) = input |> parse |> run

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day19 =
    let parse (input:string) =
        let lines =
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        Array2D.init lines.[0].Length lines.Length (fun x y -> lines.[y].Chars x)

    let getStartingPoint map =
        map 
        |> Array2DgetRow 0 
        |> Seq.mapi (fun i c -> i, c)
        |> Seq.filter (fun (_, c) -> c = '|')
        |> Seq.map fst
        |> Seq.head

    type Direction = | Down | Up | Left | Right

    let run map =
        let startingPoint = map |> getStartingPoint, 0
        let startingDirection = Down
        let startingSteps = 0
        let startingText = ""

        (startingPoint, startingDirection, startingSteps, startingText)
        |> Seq.unfold (fun ((x, y), direction, steps, text) ->
            if x < 0 || y < 0 || x >= (map |> Array2D.length1) || y >= (map |> Array2D.length2) || (map, x, y) |||> Array2D.get = ' ' then
                None
            else
                let steps = steps + 1
                let (x, y) =
                    match direction with
                    | Down -> x, y + 1
                    | Up -> x, y - 1
                    | Left -> x - 1, y
                    | Right -> x + 1, y
                let currentChar = (map, x, y) |||> Array2D.get
                let text = if currentChar >= 'A' && currentChar <= 'Z' then sprintf "%s%c" text currentChar else text
                let direction =
                    if currentChar = '+' then
                        if direction = Down || direction = Up then
                            if x <> 0 && (map, x - 1, y) |||> Array2D.get = '-' then Left
                            else Right
                        else
                            if y <> 0 && (map, x, y - 1) |||> Array2D.get = '|' then Up
                            else Down
                    else direction
                Some ((text, steps), ((x, y), direction, steps, text)))
        |> Seq.last

        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.19.txt"

        let (result1, result2) = input |> parse |> run

        { First = result1; Second = sprintf "%d" result2 }

module Day20 =
    open System
    type Coordinates = { X:int; Y:int; Z:int }
    type Particle = { Position:Coordinates; Velocity:Coordinates; Acceleration:Coordinates }

    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line ->
            match line with
            | Regex "p=<(.+),(.+),(.+)>, v=<(.+),(.+),(.+)>, a=<(.+),(.+),(.+)>" (pX::pY::pZ::vX::vY::vZ::aX::aY::aZ::[]) ->
                Some {
                        Position= { X=Integer.Parse pX; Y=Integer.Parse pY; Z=Integer.Parse pZ }
                        Velocity= { X=Integer.Parse vX; Y=Integer.Parse vY; Z=Integer.Parse vZ }
                        Acceleration= { X=Integer.Parse aX; Y=Integer.Parse aY; Z=Integer.Parse aZ }
                     }
            | _ -> None)
        |> Seq.toArray

    let getNearestFromZero particles =
        particles
        |> Seq.mapi (fun i particle ->
            i, Math.Abs(particle.Acceleration.X) + Math.Abs(particle.Acceleration.Y) + Math.Abs(particle.Acceleration.Z))
        |> Seq.minBy snd
        |> fst

    let getCollisions particles =
        (particles, seq { 0 .. 999 })
        ||> Seq.fold (fun particles _ ->
            let newParticles = 
                particles 
                |> Seq.map (fun particle ->
                    { particle with Velocity = { particle.Velocity with X = particle.Velocity.X + particle.Acceleration.X; 
                                                                        Y = particle.Velocity.Y + particle.Acceleration.Y; 
                                                                        Z = particle.Velocity.Z + particle.Acceleration.Z }})
                |> Seq.map (fun particle ->
                    { particle with Position = { particle.Position with X = particle.Position.X + particle.Velocity.X; 
                                                                        Y = particle.Position.Y + particle.Velocity.Y; 
                                                                        Z = particle.Position.Z + particle.Velocity.Z }})
                |> Seq.toArray
            newParticles
            |> Seq.groupBy (fun particle -> particle.Position)
            |> Seq.filter (fun (_, elements) -> elements |> Seq.skip 1 |> Seq.isEmpty)
            |> Seq.collect (fun (_, elements) -> elements)
            |> Seq.toArray)
        |> Array.length

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.20.txt"

        let particles = input |> parse

        let result1 = particles |> getNearestFromZero

        let result2 = particles |> getCollisions

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day21 =
    open System

    let signsToNumber text = text |> Seq.rev |> Seq.mapi (fun i c -> if c = '#' then Math.Pow(2., i |> double) |> int else 0) |> Seq.sum

    let parse (input:string) =
        let twoMap =
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
            |> Seq.choose (fun line -> 
                match line with
                | Regex "([\.#][\.#])/([\.#][\.#]) => ([\.#][\.#][\.#])/([\.#][\.#][\.#])/([\.#][\.#][\.#])" (in0::in1::out0::out1::out2::[]) ->
                    let input = seq { yield in0; yield in1 } |> String.concat ""
                    let output = seq { yield out0; yield out1; yield out2 } |> String.concat ""
                    Some (input, output)
                | _ -> None)
            |> Seq.map (fun (input, output) ->
                input, output |> signsToNumber)
            |> Seq.collect (fun (input, output) ->
                let rotate (text:string) =
                    sprintf "%c%c%c%c" (text.Chars 2) (text.Chars 0) (text.Chars 3) (text.Chars 1)
                let flip (text:string) = 
                    sprintf "%c%c%c%c" (text.Chars 1) (text.Chars 0) (text.Chars 3) (text.Chars 2)
            
                seq { yield input }
                |> Seq.append(
                    (input, seq { 0 .. 6 })
                    ||> Seq.scan (fun text i -> if i = 3 then input |> flip else text |> rotate))
                |> Seq.distinct
                |> Seq.map signsToNumber
                |> Seq.map (fun input -> input, output))
            |> Map.ofSeq

        let threeMap =
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
            |> Seq.choose (fun line -> 
                match line with
                | Regex "([\.#][\.#][\.#])/([\.#][\.#][\.#])/([\.#][\.#][\.#]) => ([\.#][\.#][\.#][\.#])/([\.#][\.#][\.#][\.#])/([\.#][\.#][\.#][\.#])/([\.#][\.#][\.#][\.#])" (in0::in1::in2::out0::out1::out2::out3::[]) ->
                    let input = seq { yield in0; yield in1; yield in2 } |> String.concat ""
                    let output = seq { yield out0; yield out1; yield out2; yield out3 } |> String.concat ""
                    Some (input, output)
                | _ -> None)
            |> Seq.map (fun (input, output) ->
                input, output |> signsToNumber)
            |> Seq.collect (fun (input, output) ->
                let rotate (text:string) =
                    sprintf "%c%c%c%c%c%c%c%c%c" (text.Chars 6) (text.Chars 3) (text.Chars 0) (text.Chars 7) (text.Chars 4) (text.Chars 1) (text.Chars 8) (text.Chars 5) (text.Chars 2)
                let flipVertically (text:string) = 
                    sprintf "%c%c%c%c%c%c%c%c%c" (text.Chars 2) (text.Chars 1) (text.Chars 0) (text.Chars 5) (text.Chars 4) (text.Chars 3) (text.Chars 8) (text.Chars 7) (text.Chars 6)
                let flipHoricontally (text:string) = 
                    sprintf "%c%c%c%c%c%c%c%c%c" (text.Chars 6) (text.Chars 7) (text.Chars 8) (text.Chars 3) (text.Chars 4) (text.Chars 5) (text.Chars 0) (text.Chars 1) (text.Chars 2)
                
                seq { yield input }
                |> Seq.append(
                    (input, seq { 0 .. 10 })
                    ||> Seq.scan (fun text i -> 
                        if i = 3 then input |> flipVertically
                        elif i = 7 then input |> flipHoricontally
                        else text |> rotate))
                |> Seq.distinct
                |> Seq.map signsToNumber
                |> Seq.map (fun input -> input, output))
            |> Map.ofSeq

        twoMap, threeMap

    let iterate currentArray dimension startIteration endIteration twoMap threeMap =
        ((currentArray, dimension), seq { startIteration .. endIteration })
        ||> Seq.fold (fun (prevArray, prevDimension) _ ->
            let fourTransform arr = 
                let length = (arr |> Array2D.length1) * 2
                Array2D.init length length (fun y x -> 
                    let prevValue = Array2D.get arr (y / 2) (x / 2)
                    match y % 2, x % 2 with
                    | 0, 0 -> ((0b1100_0000_0000_0000 &&& prevValue) >>> 12) ||| ((0b0000_1100_0000_0000 &&& prevValue) >>> 10)
                    | 0, 1 -> ((0b0011_0000_0000_0000 &&& prevValue) >>> 10) ||| ((0b0000_0011_0000_0000 &&& prevValue) >>>  8)
                    | 1, 0 -> ((0b0000_0000_1100_0000 &&& prevValue) >>>  4) ||| ((0b0000_0000_0000_1100 &&& prevValue) >>>  2)
                    | 1, 1 -> ((0b0000_0000_0011_0000 &&& prevValue) >>>  2) ||| ((0b0000_0000_0000_0011 &&& prevValue) >>>  0)
                    | _ -> 0)

            let threeTransform arr = 
                let length = (arr |> Array2D.length1) + (arr |> Array2D.length1) / 2 
                Array2D.init length length (fun y x -> 
                    let (y', x') = ((y |> double) * 2. / 3.) |> int, ((x |> double) * 2. / 3.) |> int
                    match y % 3, x % 3 with
                    | 0, 0 ->
                        let prevValue = Array2D.get arr y' x'
                        ((0b110_000_000 &&& prevValue) >>> 5) ||| ((0b000_110_000 &&& prevValue) >>> 4)
                    | 0, 1 -> 
                        let (prevValue_0, prevValue_1) = Array2D.get arr y' x', Array2D.get arr y' (x' + 1)
                        ((0b001_000_000 &&& prevValue_0) >>> 3) ||| ((0b100_000_000 &&& prevValue_1) >>> 6) ||| ((0b000_001_000 &&& prevValue_0) >>> 2) ||| ((0b000_100_000 &&& prevValue_1) >>> 5)
                    | 0, 2 -> 
                        let prevValue = Array2D.get arr y' x'
                        ((0b011_000_000 &&& prevValue) >>> 4) ||| ((0b000_011_000 &&& prevValue) >>> 3)
                    | 1, 0 -> 
                        let (prevValue_0, prevValue_1) = Array2D.get arr y' x', Array2D.get arr (y' + 1) x'
                        ((0b000_000_110 &&& prevValue_0) <<< 1) ||| ((0b110_000_000 &&& prevValue_1) >>> 7)
                    | 1, 1 -> 
                        let (prevValue_0, prevValue_1, prevValue_2, prevValue_3) =Array2D.get arr y' x', Array2D.get arr y' (x' + 1), Array2D.get arr (y' + 1) x', Array2D.get arr (y' + 1) (x' + 1)
                        ((0b000_000_001 &&& prevValue_0) <<< 3) ||| ((0b000_000_100 &&& prevValue_1) >>> 0) ||| ((0b001_000_000 &&& prevValue_2) >>> 5) ||| ((0b100_000_000 &&& prevValue_3) >>> 8)
                    | 1, 2 -> 
                        let (prevValue_0, prevValue_1) = Array2D.get arr y' x', Array2D.get arr (y' + 1) x'
                        ((0b000_000_011 &&& prevValue_0) <<< 2) ||| ((0b011_000_000 &&& prevValue_1) >>> 6)
                    | 2, 0 -> 
                        let prevValue = Array2D.get arr y' x'
                        ((0b000_110_000 &&& prevValue) >>> 2) ||| ((0b000_000_110 &&& prevValue) >>> 1)
                    | 2, 1 -> 
                        let (prevValue_0, prevValue_1) = Array2D.get arr y' x', Array2D.get arr y' (x' + 1)
                        ((0b000_001_000 &&& prevValue_0) >>> 0) ||| ((0b000_100_000 &&& prevValue_1) >>> 3) ||| ((0b000_000_001 &&& prevValue_0) <<< 1) ||| ((0b000_000_100 &&& prevValue_1) >>> 2)
                    | 2, 2 -> 
                        let prevValue = Array2D.get arr y' x'
                        ((0b000_011_000 &&& prevValue) >>> 1) ||| ((0b000_000_011 &&& prevValue) >>> 0)
                    | _ -> 0) 
            
            let map = if prevDimension = 2 then twoMap else threeMap
            let currArray =
                prevArray 
                |> Array2D.map (fun number -> map |> Map.find number)
            let currDimension = prevDimension + 1
            
            if currDimension = 4 then
                (currArray |> fourTransform), 2
            elif (3 * (currArray |> Array2D.length1)) % 2 = 1 then
                currArray, 3
            else
                (currArray |> threeTransform), 2)

    let countLights array =
        array
        |> Array2Dflatten
        |> Seq.collect (fun number ->
            seq {
                    yield number &&& 0b1000_0000_0000_0000 > 0
                    yield number &&& 0b0100_0000_0000_0000 > 0
                    yield number &&& 0b0010_0000_0000_0000 > 0
                    yield number &&& 0b0001_0000_0000_0000 > 0
                    yield number &&& 0b0000_1000_0000_0000 > 0
                    yield number &&& 0b0000_0100_0000_0000 > 0
                    yield number &&& 0b0000_0010_0000_0000 > 0
                    yield number &&& 0b0000_0001_0000_0000 > 0
                    yield number &&& 0b0000_0000_1000_0000 > 0
                    yield number &&& 0b0000_0000_0100_0000 > 0
                    yield number &&& 0b0000_0000_0010_0000 > 0
                    yield number &&& 0b0000_0000_0001_0000 > 0
                    yield number &&& 0b0000_0000_0000_1000 > 0
                    yield number &&& 0b0000_0000_0000_0100 > 0
                    yield number &&& 0b0000_0000_0000_0010 > 0
                    yield number &&& 0b0000_0000_0000_0001 > 0
                })
        |> Seq.filter identity
        |> Seq.length

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.21.txt"

        let maps = input |> parse

        let first = maps ||> iterate (Array2D.create 1 1 143) 3 1 5

        let result1 = first |> fst |> countLights

        let second = maps ||> iterate (first |> fst) (first |> snd) 6 18

        let result2 = second |> fst |> countLights

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day22 =
    open System.Collections.Generic
    type State = | Clean | Infected | Weakened | Flagged
    let parse (input:string) =
        let textRows = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        let countRows = textRows.Length
        let countColumns = textRows |> Seq.map String.length |> Seq.max
        let rowOffset = countRows / 2
        let columnOffset = countColumns / 2

        let dict = new Dictionary<(int * int), State>()

        textRows
        |> Seq.mapi (fun y row -> columnOffset - y, row)
        |> Seq.collect (fun (y, row) -> 
            row 
            |> Seq.mapi (fun x c -> x - rowOffset, c)
            |> Seq.where (fun (_, c) -> c = '#') 
            |> Seq.map (fun (x, _) -> y, x))
        |> Seq.map (fun pos -> pos, Infected)
        |> Seq.iter (fun (pos, state) -> dict.[pos] <- state)

        dict

    let wormRun getDirection getState (map:IDictionary<(int*int),State>) iterationCount =
        let getPos (y, x) dir =
            match dir with
            | Up -> (y + 1, x)
            | Down -> (y - 1, x)
            | Left -> (y, x - 1)
            | Right -> (y, x + 1)

        (((0, 0), Up, Clean), seq { 1 .. iterationCount })
        ||> Seq.scan (fun (pos, direction, _) _ ->
            let state =
                match map.TryGetValue pos with
                | true, state -> state
                | false, _ -> Clean
            let direction = (direction, state) ||> getDirection
            let state = state |> getState
            if state = Clean then map.Remove pos |> ignore else map.[pos] <- state
            let pos = (pos, direction) ||> getPos
            pos, direction, state)
        |> Seq.filter (fun (_, _, state) -> state = Infected)
        |> Seq.length

    let getDirectionFirst direction state =
        match direction, state with
        | Up,    Infected | Down,  Clean -> Right
        | Down,  Infected | Up,    Clean -> Left
        | Left,  Infected | Right, Clean -> Up
        | Right, Infected | Left,  Clean -> Down
        | _                              -> direction
        
    let getStateFirst state =
        match state with
        | Clean    -> Infected
        | Infected -> Clean
        | _        -> state

    let wormRunFirst = wormRun getDirectionFirst getStateFirst
    
    let getDirectionSecond direction state =
        match direction, state with
        | Up,    Infected | Down,  Clean | Left,  Flagged -> Right
        | Down,  Infected | Up,    Clean | Right, Flagged -> Left
        | Left,  Infected | Right, Clean | Down,  Flagged -> Up
        | Right, Infected | Left,  Clean | Up,    Flagged -> Down
        | _,     Weakened                                 -> direction
            
    let getStateSecond state =
        match state with
        | Clean    -> Weakened
        | Weakened -> Infected
        | Infected -> Flagged
        | Flagged  -> Clean
    
    let wormRunSecond = wormRun getDirectionSecond getStateSecond

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.22.txt"
        
        let result1 = (input |> parse, 10_000) ||> wormRunFirst

        let result2 = (input |> parse, 10_000_000) ||> wormRunSecond

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

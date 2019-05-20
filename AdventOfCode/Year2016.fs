module Year2016

open Domain
open Operations

module Day1 =
    open System

    type CardinalPoint = | North | West | South | East

    type Direction = | Left | Right

    type Instruction = { Direction:Direction; Steps:int }

    let getInstructions (input:string) =
        let textInstructions = input.Split([| ", " |], System.StringSplitOptions.None)
        textInstructions 
        |> Seq.map (fun textInstruction -> 
            match textInstruction with
            | Regex "R(\d+)" (steps::_) -> Some { Direction = Right; Steps = Integer.Parse steps }
            | Regex "L(\d+)" (steps::_) -> Some { Direction = Left; Steps = Integer.Parse steps }
            | _ -> None)
        |> Seq.choose identity
        |> Seq.toList
        
    let changeCardinalPoint cardinalPoint instruction =
        match cardinalPoint, instruction.Direction with
        | North, Left | South, Right -> West
        | North, Right | South, Left -> East
        | West, Left | East, Right -> South
        | West, Right | East, Left -> North
        
    let changePosition (x, y) cardinalPoint steps =
        match cardinalPoint with
        | North -> x, y + steps
        | South -> x, y - steps
        | West -> x - steps, y
        | East -> x + steps, y

    let changePositions (x, y) cardinalPoint steps =
        match cardinalPoint with
        | North -> seq { y + 1 .. 1 .. y + steps } |> Seq.map (fun y -> x, y) |> Seq.toList
        | South -> seq { y - 1 .. -1 .. y - steps } |> Seq.map (fun y -> x, y) |> Seq.toList
        | West -> seq { x - 1 .. -1 .. x - steps } |> Seq.map (fun x -> x, y) |> Seq.toList
        | East -> seq { x + 1 .. 1 .. x + steps } |> Seq.map (fun x -> x, y) |> Seq.toList
        
    let calculate instructions =
        let (_, (x, y)) = 
            ((North, (0, 0)), instructions)
            ||> List.fold (fun (cardinalPoint, position) instruction -> 
                let cardinalPoint = (cardinalPoint, instruction) ||> changeCardinalPoint
                cardinalPoint, (position, cardinalPoint, instruction.Steps) |||> changePosition) 
        
        (x |> Math.Abs) + (y |> Math.Abs)
                
    let calculate2 instructions =
        ((North, [ (0, 0) ], Set.empty), seq { while true do yield! instructions })
        ||> Seq.scan (fun (cardinalPoint, positions, set) instruction -> 
            let cardinalPoint = (cardinalPoint, instruction) ||> changeCardinalPoint
            let set = (set, positions) ||> List.fold (fun set position -> set |> Set.add position)
            let positions = (positions |> List.last, cardinalPoint, instruction.Steps) |||> changePositions
            cardinalPoint, positions, set)
        |> Seq.collect (fun (_, positions, set) -> positions |> Seq.map (fun position -> position, set))
        |> Seq.where (fun (position, set) -> set |> Set.contains position)
        |> Seq.map (fun ((x, y), _) -> (x |> Math.Abs) + (y |> Math.Abs))
        |> Seq.head

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.01.txt"
        
        let instructions = input |> getInstructions

        let result1 = instructions |> calculate

        let result2 = instructions |> calculate2

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day2 =
    type Direction = | Up | Down | Left | Right

    let mapCharToDirection c =
        match c with
        | 'U' -> Some Up
        | 'D' -> Some Down
        | 'L' -> Some Left
        | 'R' -> Some Right
        | _ -> None

    let step1 currentNumber direction =
        match currentNumber, direction with
        | '2', Left  | '4', Up                          -> '1'
        | '1', Right | '3', Left  | '5', Up             -> '2'
        | '2', Right | '6', Up                          -> '3'
        | '1', Down  | '5', Left  | '7', Up             -> '4'
        | '2', Down  | '4', Right | '6', Left | '8', Up -> '5'
        | '3', Down  | '5', Right | '9', Up             -> '6'
        | '4', Down  | '8', Left                        -> '7'
        | '5', Down  | '7', Right | '9', Left           -> '8'
        | '6', Down  | '8', Right                       -> '9'
        | _ -> currentNumber

    let step2 currentNumber direction =
        match currentNumber, direction with
        | '3', Up                                       -> '1'
        | '3', Left  | '6', Up                          -> '2'
        | '1', Down  | '2', Right | '4', Left | '7', Up -> '3'
        | '3', Right | '8', Up                          -> '4'
        | '6', Left                                     -> '5'
        | '2', Down  | '5', Right | '7', Left | 'A', Up -> '6'
        | '3', Down  | '6', Right | '8', Left | 'B', Up -> '7'
        | '4', Down  | '7', Right | '9', Left | 'C', Up -> '8'
        | '8', Right                                    -> '9'
        | '6', Down  | 'B', Left                        -> 'A'
        | '7', Down  | 'A', Right | 'C', Left | 'D', Up -> 'B'
        | '8', Down  | 'B', Right                       -> 'C'
        | 'B', Down                                     -> 'D'
        | _ -> currentNumber

    let calculate lines step =
        lines
        |> Seq.mapi (fun i line -> line, i)
        |> asFirst ('5', "")
        ||> Seq.fold (fun (lastNumber, currentSum) ((line: string), i) ->
            let nextNumber =
                line
                |> Seq.map mapCharToDirection
                |> Seq.choose identity
                |> asFirst lastNumber 
                ||> Seq.fold step
            nextNumber, sprintf "%s%c" currentSum nextNumber)
        |> snd

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.02.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let result1 = calculate lines step1

        let result2 = calculate lines step2

        { First = result1; Second = result2 }

module Day3 =
    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.map (fun line ->
            Integer.Parse(line.Substring(0, 5)), Integer.Parse(line.Substring(5, 5)), Integer.Parse(line.Substring(10, 5)))
        |> Seq.toArray

    let getCountOfValidTriangles seq =
        seq
        |> Seq.filter (fun (first, second, third) -> (first + second > third) && (second + third > first) && (first + third > second))
        |> Seq.length

    let trueTriangularSides triangularSides =
        triangularSides
        |> Seq.chunkBySize 3
        |> Seq.collect (fun threeTriangles ->
            let (first1, first2, first3) = (threeTriangles, 0) ||> Array.get
            let (second1, second2, second3) = (threeTriangles, 1) ||> Array.get
            let (third1, third2, third3) = (threeTriangles, 2) ||> Array.get
            seq { yield first1, second1, third1; yield first2, second2, third2; yield first3, second3, third3})
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.03.txt"
        
        let triangularSides = input |> parse

        let result1 = triangularSides |> getCountOfValidTriangles

        let result2 = triangularSides |> trueTriangularSides |> getCountOfValidTriangles

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day4 =
    type EncryptedName = { Name:string; Id:int; Checksum:string }

    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.map (fun line ->
            match line with
            | Regex "(.*)\-(\d+)\[(.*)\]" (name::textId::checksum::[]) -> 
                Some { Name = name; Id = Integer.Parse textId; Checksum = checksum}
            | _ -> None)
        |> Seq.choose identity
        |> Seq.toArray

    let getValidNames encryptedNames =
        encryptedNames
        |> Seq.filter (fun encryptedName ->
            let sum =
                encryptedName.Name
                |> Seq.countBy identity
                |> Seq.sortWith (fun (char1, count1) (char2, count2) ->
                    if char1 = '-' then 1
                    elif char2 = '-' then -1
                    elif count1 > count2 then -1
                    elif count2 > count1 then 1
                    elif char1 < char2 then -1
                    elif char2 < char1 then 1
                    else 0)
                |> Seq.take 5
                |> Seq.map fst
                |> asFirst ""
                ||> Seq.fold (fun text char -> sprintf "%s%c" text char)
            encryptedName.Checksum = sum)

    let getSumOfIdsForValidNames encryptedNames =
        encryptedNames
        |> getValidNames
        |> Seq.sumBy (fun encryptedName -> encryptedName.Id)

    let getRoomId encryptedNames =
        encryptedNames
        |> getValidNames
        |> Seq.map (fun encryptedName ->
            let decryptedName =
                encryptedName.Name
                |> Seq.map (fun c ->
                    if c = '-' then ' '
                    else
                        (((c |> int) - ('a' |> int) + encryptedName.Id) % 26 + ('a' |> int)) |> char)
                |> asFirst ""
                ||> Seq.fold (fun text char -> sprintf "%s%c" text char)
            decryptedName, encryptedName.Id)
        |> Seq.filter (fun (decryptedName, _) -> 
            decryptedName.Contains("northpole object storage"))
        |> Seq.map snd
        |> Seq.head


    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.04.txt"

        let encryptedNames = input |> parse

        let result1 = encryptedNames |> getSumOfIdsForValidNames

        let result2 = encryptedNames |> getRoomId

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day5 =
    let generateHash input =
        seq { 0 .. Integer.MaxValue }
        |> Seq.map (fun i -> sprintf "%s%d" input i)
        |> Seq.map (fun source -> System.Text.Encoding.ASCII.GetBytes(source) |> md5)
        |> Seq.filter (fun hash -> hash.StartsWith("00000"))

    let getPassword input = 
        input
        |> generateHash
        |> Seq.take 8
        |> Seq.map (fun hash -> hash.Chars 5)
        |> asFirst ""
        ||> Seq.fold (fun text char -> sprintf "%s%c" text char)
        
    let getSecondPassword input = 
        let map =
            input
            |> generateHash
            |> Seq.where (fun hash ->
                let pos = hash.Chars 5
                pos >= '0' && pos <= '7')
            |> Seq.map (fun hash -> hash.Chars 5, hash.Chars 6)
            |> asFirst Map.empty
            ||> Seq.scan (fun map (pos, char) -> 
                if map |> Map.containsKey pos then map else map |> Map.add pos char)
            |> Seq.filter (fun map -> map |> Map.count = 8)
            |> Seq.head
        sprintf "%c%c%c%c%c%c%c%c" (map |> Map.find '0') (map |> Map.find '1') (map |> Map.find '2') (map |> Map.find '3') (map |> Map.find '4') (map |> Map.find '5') (map |> Map.find '6') (map |> Map.find '7')

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.05.txt"

        let result1 = input |> getPassword

        let result2 = input |> getSecondPassword

        { First = result1; Second = result2 }

module Day6 =
    let parse (input:string) =
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        let min =
            lines
            |> Seq.map (fun line -> line.Length)
            |> Seq.min
        lines, min

    let decode picker lines min =
        seq { 0 .. (min - 1) }
        |> Seq.map (fun i -> lines |> Seq.map (fun (line:string) -> line.Chars i) |> Seq.toList)
        |> Seq.map (fun column -> (column |> Seq.countBy identity |> picker snd) |> fst)
        |> asFirst ""
        ||> Seq.fold (fun text char -> sprintf "%s%c" text char)
        

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.06.txt"
        let linesAndMin = input |> parse
            
        let result1 = linesAndMin ||> decode Seq.maxBy

        let result2 = linesAndMin ||> decode Seq.minBy

        { First = result1; Second = result2 }

module Day7 =
    type IPv7 = { Ips:string list; Hypernets:string list; }

    let parse (input:string) =
        let getLists (input:string) =
            let firstSplit = input.Split([| '[' |], System.StringSplitOptions.None)
            let firstIp = (firstSplit |> Array.head)::[]
            firstSplit
            |> Seq.skip 1
            |> asFirst (firstIp, [])
            ||> Seq.fold (fun (ips, hypernets) segment ->
                let parts = segment.Split([| ']' |], System.StringSplitOptions.None)
                parts.[1]::ips, parts.[0]::hypernets)

        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.map (fun line -> 
            let (ips, hypernets) = getLists line
            { Ips = ips; Hypernets = hypernets})

    let doesSupportTLS ipv7 =
        let containsABBA (text:string) =
            seq { 0 .. text.Length - 4 }
            |> Seq.exists (fun i -> text.Chars i <> text.Chars (i + 1) && text.Chars i = text.Chars (i + 3) && text.Chars (i + 1) = text.Chars (i + 2))

        ipv7.Ips |> List.exists containsABBA && ipv7.Hypernets |> List.exists containsABBA |> not
                
    let doesSupportSSL ipv7 =
        let getABAs (text:string) =
            seq { 0 .. text.Length - 3 }
            |> Seq.filter (fun i -> text.Chars i <> text.Chars (i + 1) && text.Chars i = text.Chars (i + 2))
            |> Seq.map (fun i -> text.Chars i, text.Chars (i + 1))

        ipv7.Ips 
        |> Seq.collect (fun ip -> ip |> getABAs) 
        |> Seq.allPairs (ipv7.Hypernets |> Seq.collect (fun ip -> ip |> getABAs))
        |> Seq.exists (fun ((a1, b1), (a2, b2)) -> a1 = b2 && b1 = a2)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.07.txt"

        let result1 = input |> parse |> Seq.filter doesSupportTLS |> Seq.length

        let result2 = input |> parse |> Seq.filter doesSupportSSL |> Seq.length

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day8 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.08.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day9 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.09.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day10 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.10.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day11 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.11.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day12 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.12.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day13 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.13.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day14 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.14.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day15 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.15.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day16 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.16.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day17 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.17.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day18 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.18.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day19 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.19.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day20 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.20.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day21 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.21.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day22 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.22.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day23 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.23.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day24 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.24.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day25 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.25.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

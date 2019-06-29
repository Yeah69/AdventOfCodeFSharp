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
    type Instruction = | Rect of Width:int * Height:int| RotateRow of Row:int * By:int | RotateColumn of Column:int * By:int

    let parse (input:string) =
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line -> 
            match line with
            | Regex "rect (\d+)x(\d+)" (textWidth::textHeight::[]) -> Some(Rect(Integer.Parse textWidth, Integer.Parse textHeight))
            | Regex "rotate row y=(\d+) by (\d+)" (textRow::textBy::[]) -> Some(RotateRow(Integer.Parse textRow, Integer.Parse textBy))
            | Regex "rotate column x=(\d+) by (\d+)" (textColumn::textBy::[]) -> Some(RotateColumn(Integer.Parse textColumn, Integer.Parse textBy))
            | _ -> None)
        |> Seq.toList

    let execute instruction array =
        match instruction with
        | Rect (width, height) -> 
            seq { 0 .. height - 1} 
            |> Seq.allPairs (seq { 0 .. width - 1 }) 
            |> Seq.iter (fun (x,y) -> (x, y, true) |||> Array2D.set array)
        | RotateRow (row, by) ->
            let slice =
                array 
                |> Array2DgetRow row 
            seq { 0 .. slice.Length - 1 }
            |> Seq.iter (fun i ->
                ((i + by) % slice.Length, row, slice.[i]) |||> Array2D.set array)
        | RotateColumn (column, by) ->
            let slice =
                array 
                |> Array2DgetColumn column 
            seq { 0 .. slice.Length - 1 }
            |> Seq.iter (fun i ->
                (column, (i + by) % slice.Length, slice.[i]) |||> Array2D.set array)
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.08.txt"

        let field = Array2D.init 50 6 (fun _ _ -> false)

        input |> parse |> Seq.iter (fun instruction -> field |> execute instruction)

        seq { 0 .. 5 }
        |> Seq.iter (fun row ->
            printfn "%s" (field |> Array2DgetRow row |> asFirst "" ||> Seq.fold (fun text b -> if b then sprintf "%s%c" text '#' else sprintf "%s%c" text '.')))
        printfn ""

        let result1 = field |> Seq.cast |> Seq.filter identity |> Seq.length

        let result2 = "EOARGPHYAO" // is printed to console above

        { First = sprintf "%d" result1; Second = result2 }

module Day9 =
    open System
    let getDecompressedLength (text:string) furtherDecompression =
        let rec inner (currentSpan: ReadOnlyMemory<char>) currentSize =
            let index = currentSpan.Span.IndexOf '('
            if index = -1 then currentSize + (currentSpan.Length |> int64)
            else 
                let currentSize = currentSize + (index |> int64)
                let currentSpan = currentSpan.Slice index
                let xIndex = currentSpan.Span.IndexOf 'x'
                let closeIndex = currentSpan.Span.IndexOf ')'
                if xIndex > -1 && closeIndex > -1 && xIndex < closeIndex then 
                    let getNumber (span: ReadOnlyMemory<char>) i1 i2 =
                        let check =
                            seq { i1 .. i2} 
                            |> Seq.map (fun i -> 
                                let c = span.Span.Item i
                                c |> Char.IsDigit |> not)
                            |> Seq.exists identity
                            |> not
                        if check then Some (Long.Parse (span.Slice(i1, i2 - i1 + 1).ToString()))
                        else None
                    match getNumber currentSpan 1 (xIndex - 1), getNumber currentSpan (xIndex + 1) (closeIndex - 1) with
                    | Some length, Some times ->
                        let currentSize = 
                            if furtherDecompression then currentSize + (inner (currentSpan.Slice(closeIndex + 1, length |> int)) 0L) * times
                            else currentSize + length * times
                        let currentSpan = currentSpan.Slice (closeIndex + (length |> int) + 1)
                        inner currentSpan currentSize
                    | _ -> inner (currentSpan.Slice 1) currentSize + 1L
                else inner (currentSpan.Slice 1) currentSize + 1L
                
        
        let span = text.AsMemory()
        inner span 0L
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.09.txt"

        let result1 = (input, false) ||> getDecompressedLength

        let result2 = (input, true) ||> getDecompressedLength

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day10 =
    type Bot = { First:int option; Second:int option; Instruction:Bot->Map<int,Bot>->Map<int,int>->Map<int,Bot>*Map<int,int>*int option }

    let parse (input:string) =
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let bots =
            lines
            |> Seq.choose (fun line -> 
                match line with
                | Regex "bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" (indexText::lowKind::lowIndexText::highKind::highIndexText::[]) ->
                    let (index, lowIndex, highIndex) = Integer.Parse indexText, Integer.Parse lowIndexText, Integer.Parse highIndexText
                    let instruction =
                        (fun bot bots output -> 
                            let checkValidAction() =
                                let valuesCheck = bot.First |> Option.isSome && bot.Second |> Option.isSome
                                let botCheck kind index = 
                                    kind = "output" || 
                                            (let otherBot = bots |> Map.tryFind index;
                                             match otherBot with
                                             | Some oBot -> oBot.First |> Option.isNone || oBot.Second |> Option.isNone
                                             | _ -> false)
                                valuesCheck && (botCheck lowKind lowIndex) && (botCheck highKind highIndex)
                            
                            if checkValidAction() then
                                // path where bot has two values
                                let setOtherBot index value =
                                    let otherBot = bots |> Map.find index
                                    let otherBot =
                                        match otherBot.First with 
                                        | Some _ -> { otherBot with Second = Some value }
                                        | _ -> { otherBot with First = Some value }
                                    otherBot
                                let (low, high) = 
                                    match bot.First, bot.Second with
                                    | Some f, Some s -> if f < s then f, s else s, f
                                    | _ -> 0, 1
                                let (bots, output) =
                                    if lowKind = "output" then bots, output |> Map.add lowIndex low
                                    else bots |> Map.add lowIndex (setOtherBot lowIndex low), output
                                let (bots, output) =
                                    if highKind = "output" then bots, output |> Map.add highIndex high
                                    else bots |> Map.add highIndex (setOtherBot highIndex high), output
                                let bots = bots |> Map.add index { bot with First = None; Second = None }
                                bots, output, if low = 17 && high = 61 then Some index else None
                            else
                                // can only operate if has exactly two values
                                bots, output, None)
                    Some (index, { First = None; Second = None; Instruction = instruction})
                | _ -> None)
            |> Map.ofSeq

        let bots =
            lines
            |> Seq.choose (fun line -> 
                match line with
                | Regex "value (\d+) goes to bot (\d+)" (valueText::indexText::[]) ->
                    Some (Integer.Parse valueText, Integer.Parse indexText)
                | _ -> None)
            |> asFirst bots
            ||> Seq.fold (fun bots (value, index) ->
                let bot = bots |> Map.find index
                let bot = match bot.First with
                          | Some _ -> { bot with Second = Some value }
                          | _ -> { bot with First = Some value }
                bots |> Map.add index bot)

        bots

    let rec runBots bots output firstAnswer =
        let filledBots =
            bots
            |> Map.toSeq
            |> Seq.where (fun (_, bot) -> bot.First |> Option.isSome && bot.Second |> Option.isSome)
            |> Seq.map snd
            |> Seq.toList
        if filledBots |> List.isEmpty then bots, output, firstAnswer
        else
            let (bots, output, firstAnswer) =
                ((bots, output, firstAnswer), filledBots)
                ||> Seq.fold (fun (bots, output, firstAnswer) bot ->
                    let (bots, output, otherFirstAnswer) = bot.Instruction bot bots output
                    bots, output, if firstAnswer |> Option.isNone then otherFirstAnswer else firstAnswer)
            runBots bots output firstAnswer

        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.10.txt"

        let bots = input |> parse

        let (_, output, firstAnswer) = (bots, Map.empty, None) |||> runBots

        let result1 = firstAnswer |> Option.defaultValue -1

        let result2 = (output |> Map.find 0) * (output |> Map.find 1) * (output |> Map.find 2)

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day11 =
    open System
    open System.Collections.Generic
    type Floor = | F1 | F2 | F3 | F4

    let floorRank floor =
        match floor with
        | F1 -> 0
        | F2 -> 1
        | F3 -> 2
        | F4 -> 3

    type Component = | Microchip of string * Floor | Generator of string * Floor
    
    let getFloor ``component`` = match ``component`` with | Microchip (_, floor) | Generator (_, floor) -> floor
    
    let getLabel ``component`` = match ``component`` with | Microchip (label, _) | Generator (label, _) -> label
    
    let compWithFloor floor ``component`` = 
        match ``component`` with 
        | Microchip (label, _) -> Microchip (label, floor)
        | Generator (label, _) -> Generator (label, floor)
        
    let isMicrochip ``component`` = 
        match ``component`` with 
        | Microchip _ -> true
        | Generator _ -> false
        
    let isGenerator ``component`` = 
        match ``component`` with 
        | Microchip _ -> false
        | Generator _ -> true

    let getNeighboringFloors floor = match floor with | F1 -> seq { yield F2 } | F2 -> seq { yield F1; yield F3 } | F3 -> seq { yield F2; yield F4 } | F4 -> seq { yield F3 }

    let parse (input:string) = 
        input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line ->
            match line with
            | Regex "The (?:.+) floor contains nothing relevant\." _ -> None
            | Regex "The (first|second|third|fourth) floor contains(.*)\." (textFloor::textList::[]) ->
                let floor =
                    match textFloor with
                    | "first" -> F1
                    | "second" -> F2
                    | "third" -> F3
                    | _ -> F4
                Some (textList.Split([| " a " |], System.StringSplitOptions.None)
                      |> Seq.choose (fun textComponent -> 
                        match textComponent with
                        | Regex "(.+)-compatible microchip" (label::[]) -> Some(Microchip(label, floor))
                        | Regex "(.+) generator" (label::[]) -> Some(Generator(label, floor))
                        | _ -> None))
            | _ -> None)
        |> Seq.collect identity
        |> Seq.toArray

    let rec getAllPairs seq =
        if seq |> Seq.isEmpty then Seq.empty
        else
            let first = seq |> Seq.head
            seq
            |> Seq.skip 1
            |> Seq.map (fun element -> first, element)
            |> Seq.append (getAllPairs (seq |> Seq.skip 1))

    let fourBaseArray = seq { 0. .. 14. } |> Seq.map (fun i -> Math.Pow(4., i) |> int64) |> Array.ofSeq

    let rec findSolution components =
        let queue = Queue<int*Floor*Component array>()
        queue.Enqueue((0, F1, components))
        let hashedStateToSteps = Dictionary<int64,int>()

        Integer.MaxValue
        |> Seq.unfold (fun bestSofar ->
            if queue.Count = 0 then
                None
            else 
                let (stepsSofar, elevator, components) = queue.Dequeue()
                // get hash of current state
                let hash = 
                    seq { yield (elevator |> floorRank) } 
                    |> Seq.append (components |> Seq.map (getFloor >> floorRank)) 
                    |> Seq.zip fourBaseArray 
                    |> Seq.map (fun (fourBase, floorRank) -> (floorRank |> int64) * fourBase)
                    |> Seq.sum
                let ``continue`` =
                    match hashedStateToSteps.TryGetValue hash with
                    // state already happened worse result
                    | true, value when value > stepsSofar ->
                        hashedStateToSteps.[hash] <- stepsSofar
                        true
                    // state already happened with at least as good result
                    | true, value when value <= stepsSofar -> false
                    // yet unknown state
                    | _ ->
                        hashedStateToSteps.[hash] <- stepsSofar
                        true
                // check wether not yet in final state
                let anyComponentsNotOnF4 = components |> Seq.exists (fun comp -> (comp |> getFloor) <> F4)
                match bestSofar with
                // abort if this state was already handled
                | _ when ``continue`` |> not -> Some (bestSofar, bestSofar)
                // if final state return steps so far, because it is the best result so far
                | _ when anyComponentsNotOnF4 |> not -> 
                    let bestSofar = 
                        if bestSofar > stepsSofar then stepsSofar 
                        else bestSofar
                    Some (bestSofar, bestSofar)
                // if steps so far are equal or greater to the best result then abort
                | steps when stepsSofar >= steps -> Some (bestSofar, bestSofar)
                // else 
                | _ -> 
                    let neighboringFloors = elevator |> getNeighboringFloors
                    let componentsOnSameFloor =
                        components
                        |> Seq.filter (fun comp -> (comp |> getFloor) = elevator)
                        |> Seq.toList

                    let validation (targetFloor, movedComponents) =
                        let checkPreviewFloor comps =
                            (comps |> Seq.exists isGenerator |> not) ||
                                (comps
                                 |> Seq.groupBy getLabel
                                 |> Seq.exists (fun (_, group) -> (group |> Seq.length) = 1 && (group |> Seq.head |> isMicrochip))
                                 |> not)
                        let previewTargetFloor =
                            components
                            |> Seq.filter (fun comp -> (comp |> getFloor) = targetFloor)
                            |> Seq.append movedComponents

                        let previewSourceFloor = 
                            componentsOnSameFloor
                            |> Seq.except movedComponents

                        (checkPreviewFloor previewTargetFloor) && (checkPreviewFloor previewSourceFloor)

                    let nextMoves =
                        componentsOnSameFloor
                        // get move candidates with one component
                        |> Seq.collect (fun comp -> neighboringFloors |> Seq.map (fun floor -> floor, [ comp ]))
                        // get move candidates with two components
                        |> Seq.append (componentsOnSameFloor |> getAllPairs |> Seq.allPairs neighboringFloors |> Seq.map (fun (floor, (comp0, comp1)) -> floor, [ comp0; comp1 ]))
                        // validate collected move candidates
                        |> Seq.filter validation

                    if nextMoves |> Seq.isEmpty then Some (bestSofar, bestSofar)
                    else
                        let stepsSofar = stepsSofar + 1
                        nextMoves
                        |> Seq.iter (fun (targetFloor, compList) -> 
                            let components = components |> Array.map (fun comp -> if compList |> List.exists (equal comp) then comp |> compWithFloor targetFloor else comp)
                            queue.Enqueue((stepsSofar, targetFloor, components)))
                        Some (bestSofar,bestSofar))
        |> Seq.last

    let getResults components =
        // strip all matching pairs on F1
        let pairedElementsOnF1 = components |> Seq.filter (fun comp -> (comp |> getFloor) = F1) |> Seq.groupBy getLabel |> Seq.filter (fun (_, group) -> group |> Seq.length = 2) |> Seq.collect snd
        let initialComponents =  components |> Array.except pairedElementsOnF1
                           
        
        let (firstLength, firstSteps, stableDiff) =
            seq { yield (initialComponents, (initialComponents |> findSolution)) }
            |> Seq.append ((initialComponents, seq { 'a' .. 'z'}) 
                            ||> Seq.scan (fun components c -> components |> Array.append ([| Microchip (c.ToString(), F1); Generator (c.ToString(), F1) |])) 
                            |> Seq.map (fun components -> components, (components |> findSolution)))

            // track difference of outcome if successively adding matching pairs to F1
            |> Seq.pairwise
            |> Seq.map (fun ((comps1, count1), (_, count2)) -> comps1.Length, count1, count2 - count1)
            // get the data of the first occurence which lead to a stable difference
            |> Seq.pairwise
            |> Seq.filter (fun ((_, _, diff1), (_, _, diff2)) -> diff1 = diff2)
            |> Seq.map (fun ((length, steps, diff), _) -> length, steps, diff)
            |> Seq.head

        firstSteps + (10 - firstLength) / 2 * stableDiff, firstSteps + (14 - firstLength) / 2 * stableDiff
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.11.txt"

        let components = input |> parse
            
        let (result1, result2) = components |> getResults

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day12 =
    type Program = { InstructionPointer:int; Registers:Map<char,int>; Instructions:(Program->Program) array}

    let nop prog = { prog with InstructionPointer = prog.InstructionPointer + 1 }

    let parse (input:string) =
        let instructions =
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
            |> Seq.choose (fun line -> 
                match line with
                | Regex "cpy (.+) (.)" (textValue::textReg::[]) ->
                    let regToCopyTo = textReg.Chars 0
                    match Integer.TryParse textValue with
                    | true, value -> Some (fun prog ->
                        { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToCopyTo value})
                    | false, _ -> 
                        let regToCopyFrom = textValue.Chars 0
                        Some (fun prog ->
                            let value = prog.Registers |> Map.find regToCopyFrom
                            { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToCopyTo value})
                | Regex "inc (.)" (textReg::[]) ->
                    let regToIncreace = textReg.Chars 0
                    Some (fun prog ->
                        let value = (prog.Registers |> Map.find regToIncreace) + 1
                        { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToIncreace value})
                | Regex "dec (.)" (textReg::[]) ->
                    let regToDecreace = textReg.Chars 0
                    Some (fun prog ->
                        let value = (prog.Registers |> Map.find regToDecreace) - 1
                        { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToDecreace value})
                | Regex "jnz (.+) (.+)" (textValue::textOffset::[]) ->
                    match Integer.TryParse textValue, Integer.TryParse textOffset with
                    | (true, value), (true, offset) ->
                        if value = 0 then Some nop 
                        else Some (fun prog -> { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
                    | (true, value), (false, _) ->
                        if value = 0 then Some nop 
                        else 
                            let offsetReg = textOffset.Chars 0
                            Some (fun prog -> 
                                    let offset = prog.Registers |> Map.find offsetReg
                                    { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
                    | (false, _), (true, offset) ->
                        let valueReg = textValue.Chars 0
                        Some (fun prog ->
                                let value = prog.Registers |> Map.find valueReg
                                if value = 0 then prog |> nop
                                else { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
                    | (false, _), (false, _) ->
                        let valueReg = textValue.Chars 0
                        let offsetReg = textOffset.Chars 0
                        Some (fun prog ->
                                let value = prog.Registers |> Map.find valueReg
                                let offset = prog.Registers |> Map.find offsetReg
                                if value = 0 then prog |> nop
                                else { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
                | _ -> None)
            |> Seq.toArray
        { InstructionPointer = 0; Registers = (seq { for c in 'a' .. 'd' do yield (c, 0)} |> Map.ofSeq); Instructions = instructions }

    let runProgram prog =
        prog
        |> Seq.unfold (fun prog ->
            if prog.InstructionPointer < 0 || prog.InstructionPointer >= prog.Instructions.Length then
                None
            else
                let prog = prog.Instructions.[prog.InstructionPointer] prog
                Some (prog, prog))
        |> Seq.last

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.12.txt"

        let prog = input |> parse

        let result1 = (prog |> runProgram).Registers |> Map.find 'a'

        let result2 = ({ prog with Registers = prog.Registers |> Map.add 'c' 1 } |> runProgram).Registers |> Map.find 'a'

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day13 =
    open FSharpx.Collections
    open System
    let parse (input:string) = Integer.Parse input

    type Coordinate = Coordinate of X:int*Y:int

    [<CustomEquality>]
    [<CustomComparison>]
    type PrioCoordinate = 
        PrioCoordinate of Coordinate*int
            interface IComparable<PrioCoordinate> with
                member this.CompareTo other =
                    let distance coord = match coord with | PrioCoordinate (Coordinate(x, y), _) -> Math.Abs(x - 31) + Math.Abs(y - 39)
                    let thisDistance = this |> distance
                    let otherDistance = other |> distance
                    compare thisDistance otherDistance
            interface IComparable with
                member this.CompareTo obj =
                    match obj with
                        | null                 -> 1
                        | :? PrioCoordinate as other -> (this :> IComparable<_>).CompareTo other
                        | _                    -> invalidArg "obj" "not a PrioCoordinate"
            interface IEquatable<PrioCoordinate> with
                member this.Equals other =
                    let (myCoord, mySteps) = match this with | PrioCoordinate (coord, steps) -> coord, steps
                    let (theirCoord, theirSteps) = match other with | PrioCoordinate (coord, steps) -> coord, steps
                    myCoord = theirCoord && mySteps = theirSteps
            override x.Equals(obj) =
                match obj with
                    | null                 -> false
                    | :? PrioCoordinate as other -> (x :> IEquatable<PrioCoordinate>).Equals other
                    | _                    -> invalidArg "obj" "not a PrioCoordinate"
            override x.GetHashCode() =
                match x with | PrioCoordinate (coord, _) -> coord.GetHashCode()

    
    let isOpenSpace favNumber coordinate =
        let (x, y) = match coordinate with | Coordinate (x, y) -> x, y
        let countOnes x =                            
            let rec count b acc = if b = 0 then acc else count (b &&& (b-1)) (acc+1) 
            count x 0
        ((x*x + 3*x + 2*x*y + y + y*y + favNumber) |> countOnes) % 2 = 0

    let getOpenSpaceAndWallCandits favNumber openSpaces walls x y =
        seq { yield x - 1, y; yield x + 1, y; yield x, y - 1; yield x, y + 1}
        |> Seq.filter (fun (x,y) -> x >= 0 && y >= 0)
        |> Seq.map Coordinate
        |> Seq.filter (fun coord -> (openSpaces |> Set.contains coord |> not) && (walls |> Set.contains coord |> not))
        |> Seq.toList
        |> List.partition (isOpenSpace favNumber)

    let getSteps favNumber =

        let initialPoint = Coordinate (1, 1)
        let initialPrioCoord = PrioCoordinate (initialPoint, 0)
        let walls = Set.empty
        let openSpaces = Set.empty |> Set.add initialPoint
        let queue = PriorityQueue.empty false |> PriorityQueue.insert initialPrioCoord

        (queue, openSpaces, walls, false)
        |> Seq.unfold (fun (queue, openSpaces, walls, abort) ->
            if abort then None
            else
                let (prioCoord, queue) = queue |> PriorityQueue.pop
                let (x, y, steps) = match prioCoord with | PrioCoordinate (Coordinate(x, y), steps) -> x, y, steps
                if x = 31 && y = 39 then
                    Some (steps, (queue, openSpaces, walls, true))
                else
                    let (openSpaceCandits, wallCandits) = (x, y) ||> getOpenSpaceAndWallCandits favNumber openSpaces walls
                    let walls = (walls, wallCandits) ||> Seq.fold (fun walls wall -> walls |> Set.add wall)
                    let openSpaces = (openSpaces, openSpaceCandits) ||> Seq.fold (fun openSpaces openSpace -> openSpaces |> Set.add openSpace)
                    let queue = (queue, openSpaceCandits) ||> Seq.fold (fun queue openSpace -> queue |> PriorityQueue.insert (PrioCoordinate(openSpace, steps + 1)))
                    Some (steps, (queue, openSpaces, walls, false)))
        |> Seq.last

    let getLocations favNumber =

        let initialPoint = Coordinate (1, 1)
        let initialPrioCoord = PrioCoordinate (initialPoint, 0)
        let walls = Set.empty
        let openSpaces = Set.empty |> Set.add initialPoint
        let queue = Queue.empty |> Queue.conj initialPrioCoord

        (queue, openSpaces, walls)
        |> Seq.unfold (fun (queue, openSpaces, walls) ->
            if queue |> Queue.isEmpty then None
            else
                let (prioCoord, queue) = queue |> Queue.uncons
                let (x, y, steps) = match prioCoord with | PrioCoordinate (Coordinate(x, y), steps) -> x, y, steps
                let (openSpaceCandits, wallCandits) = (x, y) ||> getOpenSpaceAndWallCandits favNumber openSpaces walls
                let walls = (walls, wallCandits) ||> Seq.fold (fun walls wall -> walls |> Set.add wall)
                let openSpaces = (openSpaces, openSpaceCandits) ||> Seq.fold (fun openSpaces openSpace -> openSpaces |> Set.add openSpace)
                let queue = if steps >= 50 then queue else (queue, openSpaceCandits) ||> Seq.fold (fun queue openSpace -> queue |> Queue.conj (PrioCoordinate(openSpace, steps + 1)))
                Some (1, (queue, openSpaces, walls)))
        |> Seq.sum

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.13.txt"

        let result1 = input |> parse |> getSteps

        let result2 = input |> parse |> getLocations

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day14 =
    let stringToMd5 text =
        text |> Seq.map (fun c -> c |> byte) |> Seq.toArray |> md5

    let parse f (input:string) =
        let initialArray = 
            seq { for i in 0 .. 1000 do yield sprintf "%s%d" input i }
            |> Seq.map f 
            |> Seq.toArray
        input, 0, initialArray

    let getSearchedIndex f salt index (array:string array) =
        let getIndexAndCharsOfTriplets i = 
            i
            |> Seq.unfold (fun i ->
                let currentHash = array.[(i % 1001)]
                let triplets = 
                    seq { 0 .. currentHash.Length - 3} 
                    |> Seq.filter (fun i ->
                        let c = currentHash.Chars i
                        currentHash.Chars (i + 1) = c && currentHash.Chars (i + 2) = c)
                    |> Seq.map (fun i -> currentHash.Chars i)
                    |> Seq.distinct
                    |> Seq.toArray
                array.[(i % 1001)] <- sprintf "%s%d" salt (i + 1001) |> f
                Some ((i, triplets), i + 1))
            |> Seq.filter (fun (_, triplets) -> triplets |> Seq.isEmpty |> not)
            |> Seq.head

        index
        |> Seq.unfold (fun i ->
            let (tripletIndex, tripletChars) = i |> getIndexAndCharsOfTriplets
            let quintupletCheck = 
                seq { for ind in tripletIndex + 1 .. tripletIndex + 1000 do yield ind % 1001 }
                |> Seq.exists (fun subI ->
                    let hash = array.[subI]
                    tripletChars
                    |> Seq.exists (fun c ->
                        seq { 0 .. hash.Length - 5} 
                        |> Seq.exists (fun charI ->
                            hash.Chars charI = c 
                            && hash.Chars (charI + 1) = c 
                            && hash.Chars (charI + 2) = c
                            && hash.Chars (charI + 3) = c
                            && hash.Chars (charI + 4) = c)))
            Some ((quintupletCheck, tripletIndex), tripletIndex + 1))
        |> Seq.filter fst
        |> Seq.skip 64
        |> Seq.map snd
        |> Seq.head

    let getHashForSecond (text:string) =
        (text, { 0 .. 2016 }) ||> Seq.fold (fun prev _ -> prev |> stringToMd5)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.14.txt"
        
        printfn "This solution could take approximately five minutes!!!"
        printfn ""

        let result1 = input |> parse stringToMd5 |||> getSearchedIndex stringToMd5

        let result2 = input |> parse getHashForSecond |||> getSearchedIndex getHashForSecond

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day15 =
    type Disc = { TimeOffset:int; PositionOffset:int; PositionCount:int }

    let parse (input:string) =
        input.Split( [| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line ->
            match line with
            | Regex "Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)." (textTimeOffset::textPositonCount::textPositionOffset::[]) ->
                Some { TimeOffset = Integer.Parse textTimeOffset; PositionOffset = Integer.Parse textPositionOffset; PositionCount = Integer.Parse textPositonCount }
            | _ -> None)
        |> Seq.toArray

    let findFirstValidButtonClick discs =
        let maxPositionCountDisc = discs |> Array.maxBy (fun disc -> disc.PositionCount)
        
        let initialTime = maxPositionCountDisc.PositionCount - (maxPositionCountDisc.TimeOffset + maxPositionCountDisc.PositionOffset)
        let distance = maxPositionCountDisc.PositionCount

        initialTime
        |> Seq.unfold (fun time ->
            Some (time, time + distance))
        |> Seq.filter (fun time -> 
            discs
            |> Array.exists (fun disc -> (time + disc.PositionOffset + disc.TimeOffset) % disc.PositionCount <> 0)
            |> not)
        |> Seq.head

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.15.txt"

        let discs = input |> parse

        let result1 = discs |> findFirstValidButtonClick

        let maxTimeOffset = discs |> Seq.map (fun disc -> disc.TimeOffset) |> Seq.max

        let result2 = (discs, [| { TimeOffset = maxTimeOffset + 1; PositionOffset = 0; PositionCount = 11 } |]) ||> Array.append |> findFirstValidButtonClick

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day16 =
    let getChecksum dataLength (input:string)  =
        let data =
            input
            |> Seq.unfold (fun a ->
                let b = a |> Seq.rev |> Seq.map(fun c -> if c = '1' then "0" else "1") |> String.concat ""
                let out = sprintf "%s0%s" a b
                Some (out, out))
            |> Seq.filter (fun d -> d.Length >= dataLength)
            |> Seq.map (fun d -> d.Substring(0, dataLength))
            |> Seq.head

        data
        |> Seq.unfold (fun d -> 
            let checksum =
                d
                |> Seq.chunkBySize 2
                |> Seq.map (fun chars -> 
                    match chars.[0], chars.[1] with
                    | '1', '1' | '0', '0' -> "1"
                    | _ -> "0")
                |> String.concat ""
            Some (checksum, checksum))
        |> Seq.filter (fun cs -> cs.Length % 2 = 1)
        |> Seq.head

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.16.txt"

        let result1 = input |> getChecksum 272

        let result2 = input |> getChecksum 35651584

        { First = result1; Second = result2 }

module Day17 =
    open System.Collections.Generic
    let getShortestPath (password:string) =
        let queue = Queue<int*int*string>()

        queue.Enqueue(0, 0, "")

        ()
        |> Seq.unfold (fun _ ->
            if queue.Count = 0 then None
            else
                let (x, y, suffix) = queue.Dequeue()
                let hash = (sprintf "%s%s" password suffix) |> Seq.map (fun c -> c |> byte) |> Seq.toArray |> md5
                let validNextMoves =
                    seq {
                            if hash.[0] >= 'b' then yield x, y - 1, sprintf "%s%c" suffix 'U'
                            if hash.[1] >= 'b' then yield x, y + 1, sprintf "%s%c" suffix 'D'
                            if hash.[2] >= 'b' then yield x - 1, y, sprintf "%s%c" suffix 'L'
                            if hash.[3] >= 'b' then yield x + 1, y, sprintf "%s%c" suffix 'R'
                        }
                    |> Seq.filter (fun (x, y, _) -> 
                        x >= 0 && x <= 3 && y >= 0 && y <= 3)
                    |> Seq.toList
                let (goals, continues) =
                    validNextMoves
                    |> List.partition (fun (x, y, _) -> x = 3 && y = 3)
                continues |> Seq.iter (fun t -> queue.Enqueue t)
                Some ((if goals |> List.isEmpty |> not then goals |> List.head else (x, y, suffix)), ()))
        |> Seq.filter (fun (x, y, _) -> x = 3 && y = 3)
        |> Seq.map (fun (_, _, path) -> path)
        |> Seq.toArray


    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.17.txt"

        let solutions = input |> getShortestPath

        let result1 = solutions.[0]

        let result2 = solutions |> Array.last |> String.length

        { First = result1; Second = sprintf "%d" result2 }

module Day18 =
    type Tile = | Safe | Trap

    let parse (input:string) =
        input
        |> Seq.choose (fun c -> 
            match c with
            | '.' -> Some Safe
            | '^' -> Some Trap
            | _ -> None)
        |> Seq.toArray

    let getSafeCount rowCount firstRow =
        (firstRow, seq { 1 .. rowCount - 1 })
        ||> Seq.scan (fun firstRow _ ->
            let completeRow = seq { yield [| Safe |]; yield firstRow; yield [| Safe |] } |> Array.concat
            seq { 1 .. completeRow.Length - 2 }
            |> Seq.map (fun i -> 
                match completeRow.[i - 1], completeRow.[i], completeRow.[i + 1] with
                | Trap, Trap, Safe | Safe, Trap, Trap | Trap, Safe, Safe | Safe, Safe, Trap -> Trap
                | _ -> Safe)
            |> Seq.toArray)
        |> Seq.collect identity
        |> Seq.filter (fun tile -> tile = Safe)
        |> Seq.length

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.18.txt"

        let result1 = input |> parse |> getSafeCount 40

        let result2 = input |> parse |> getSafeCount 400_000

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day19 =
    type Node = { Value:int; mutable Next:Node option }
        
    let parse (input:string) =
        let count = Integer.Parse input
        let last = { Value = count; Next = None }
        let first =
            (last, seq { count - 1 .. -1 .. 1 })
            ||> Seq.fold (fun prev i -> { Value = i; Next = Some prev })
        last.Next <- Some first
        let half = count / 2
        let beforeHalfNode = (first, seq { 1 .. half - 1 }) ||> Seq.fold (fun prev _ -> prev.Next |> Option.defaultValue prev)
        first, beforeHalfNode, count

    let determineLastElfFirst (firstNode:Node) count =
        let last =
            (firstNode, seq { count .. -1 .. 2 })
            ||> Seq.fold (fun node _ ->
                node.Next <- (node.Next |> Option.defaultValue node).Next
                let node = node.Next |> Option.defaultValue node
                node)
        last.Value

    let determineLastElfSecond (beforeHalfNode:Node) count =
        let last =
            (beforeHalfNode, seq { count .. -1 .. 2 })
            ||> Seq.fold (fun node count ->
                let isOdd = count % 2 = 1
                node.Next <- (node.Next |> Option.defaultValue node).Next
                let node = 
                    if isOdd then node.Next |> Option.defaultValue node
                    else node
                node)
        last.Value

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.19.txt"

        let (first, _, count) = input |> parse

        let result1 = (first, count) ||> determineLastElfFirst
        
        let (_, beforeHalfNode, count) = input |> parse

        let result2 = (beforeHalfNode, count) ||> determineLastElfSecond

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day20 =
    let parse (input:string) =
        let sort ranges =
            ranges
            |> Seq.sortWith (fun (start1, end1) (start2, end2) ->
                if start1 < start2 then -1
                elif start2 < start1 then 1
                else
                    let range1 = end1 - start1
                    let range2 = end2 - start2
                    if range1 > range2 then -1 
                    elif range2 > range1 then 1
                    else 0)

        let ranges =
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
            |> Seq.choose (fun line ->
                match line with
                | Regex "(\d+)\-(\d+)" (textStart::textEnd::[]) -> Some(Long.Parse textStart, Long.Parse textEnd)
                | _ -> None)
            |> Seq.toArray

        ([], ranges |> sort)
        ||> Seq.fold (fun currRangeList (rangeStart, rangeEnd) ->
            match currRangeList with 
            | (currStart, currEnd)::currRemainder when rangeStart <= currEnd + 1L ->
                let currEnd = if rangeEnd > currEnd then rangeEnd else currEnd
                (currStart, currEnd)::currRemainder
            | _ -> (rangeStart, rangeEnd)::currRangeList)
        |> Seq.rev
        |> Seq.toArray

    let getFirstValidIP ranges =        
        let (startPoint, endPoint) = ranges |> Seq.head
        if startPoint = 0L then endPoint + 1L else 0L

    let getCountOfValidIPs ranges =
        let beginning = if (ranges |> Array.head |> fst) > 0L then (ranges |> Array.head |> fst) - 1L else 0L
        let finish = if (ranges |> Array.last |> snd) < 4294967295L then 4294967295L - (ranges |> Array.last |> snd) else 0L
        let sumOfRanges = 
            ranges
            |> Seq.pairwise 
            |> Seq.map (fun ((_, end1), (start2, _)) -> start2 - end1 - 1L)
            |> Seq.sum
        beginning + sumOfRanges + finish

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.20.txt"

        let ranges = input |> parse

        let result1 = ranges |> getFirstValidIP

        let result2 = ranges |> getCountOfValidIPs

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day21 =
    type Direction = | Left | Right
    type Instruction = | SwapPositions of int*int 
                       | SwapLetters of char*char
                       | RotateSteps of Direction*int
                       | RotateBasedOnLetter of char
                       | ReversePositions of int*int
                       | MovePosition of int*int

    let parse (input:string) =
        input.Split ([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.choose (fun line ->
            match line with
            | Regex "swap position (\d) with position (\d)" (textFirst::textSecond::[]) -> Some (SwapPositions (Integer.Parse textFirst, Integer.Parse textSecond))
            | Regex "swap letter (.) with letter (.)" (textFirst::textSecond::[]) -> Some (SwapLetters (textFirst.Chars 0, textSecond.Chars 0))
            | Regex "rotate (left|right) (\d) steps*" (textDirection::textSteps::[]) -> Some (RotateSteps ((if textDirection = "left" then Left else Right), Integer.Parse textSteps))
            | Regex "rotate based on position of letter (.)" (textLetter::[]) -> Some (RotateBasedOnLetter (textLetter.Chars 0))
            | Regex "reverse positions (\d) through (\d)" (textFirst::textSecond::[]) -> Some (ReversePositions (Integer.Parse textFirst, Integer.Parse textSecond))
            | Regex "move position (\d) to position (\d)" (textFirst::textSecond::[]) -> Some (MovePosition (Integer.Parse textFirst, Integer.Parse textSecond))
            | _ -> None)
        |> Seq.toArray

    let swapPositions first second (currText:string) =
        let (firstChar, secondChar) = currText.Chars first, currText.Chars second
        currText
            .Remove(first, 1)
            .Insert(first, secondChar.ToString())
            .Remove(second, 1)
            .Insert(second, firstChar.ToString())

    let rotate direction steps (currText:string) =
        let steps = if direction = Left then currText.Length - steps else steps
        let offset = currText.Length - steps
        new System.String(
            seq { 0 .. currText.Length - 1 }
            |> Seq.map (fun i -> currText.Chars ((i + offset) % currText.Length))
            |> Seq.toArray)

    let reverse start finish (currText:string) =
        let length = finish - start + 1
        let subString = currText.Substring (start, length)
        let reversed = new System.String(subString |> Seq.rev |> Seq.toArray)
        currText.Replace(subString, reversed)

    let reverseDirection direction = match direction with | Left -> Right | Right -> Left

    let scramble (text:string) instructions =
        (text, instructions)
        ||> Seq.fold (fun currText instruction ->
            match instruction with
            | SwapPositions (first, second) -> currText |> swapPositions first second
            | SwapLetters (first, second) ->
                let (firstPos, secondPos) = currText.IndexOf first, currText.IndexOf second
                currText |> swapPositions firstPos secondPos
            | RotateSteps (direction, steps) -> currText |> rotate direction (steps % currText.Length)
            | RotateBasedOnLetter (letter) ->
                let indexOfLetter = currText.IndexOf letter
                let steps = if indexOfLetter < 4 then indexOfLetter + 1 else indexOfLetter + 2
                currText |> rotate Right (steps % currText.Length)
            | ReversePositions (start, finish) -> currText |> reverse start finish
            | MovePosition (first, second) -> 
                let char = currText.Chars first
                currText.Remove(first, 1).Insert(second, char.ToString()))
                

    let unscramble (text:string) instructions =
        let reverseRotateBasedOnLetterMap =
            seq { 0 .. text.Length - 1 }
            |> Seq.map (fun i -> 
                let steps = if i < 4 then i + 1 else i + 2
                let pos = (i + steps) % text.Length
                pos, steps)
            |> Map.ofSeq
        (text, instructions |> Seq.rev)
        ||> Seq.fold (fun currText instruction ->
            match instruction with
            | SwapPositions (first, second) -> currText |> swapPositions first second
            | SwapLetters (first, second) ->
                let (firstPos, secondPos) = currText.IndexOf first, currText.IndexOf second
                currText |> swapPositions firstPos secondPos
            | RotateSteps (direction, steps) -> currText |> rotate (direction |> reverseDirection) (steps % currText.Length)
            | RotateBasedOnLetter (letter) ->
                let indexOfLetter = currText.IndexOf letter
                let steps = reverseRotateBasedOnLetterMap |> Map.find indexOfLetter
                currText |> rotate Left (steps % currText.Length)
            | ReversePositions (start, finish) -> currText |> reverse start finish
            | MovePosition (first, second) -> 
                let char = currText.Chars second
                currText.Remove(second, 1).Insert(first, char.ToString()))

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.21.txt"

        let instructions = input |> parse

        let result1 = instructions |> scramble "abcdefgh"

        let result2 = instructions |> unscramble "fbgdceah"

        { First = result1; Second = result2 }

module Day22 =
    open System
    open System.Collections.Generic
    type FromDirection = | Left | Right | Below
    let parse (input:string) =
        input.Split ([| System.Environment.NewLine |], System.StringSplitOptions.None)
        |> Seq.skip 2
        |> Seq.choose (fun line ->
            match line with
            | Regex "/dev/grid/node\-x(\d+)\-y(\d+)\s*\d+T\s*(\d+)T\s*(\d+)T\s*\d+%" (textX::textY::textUsed::textAvail::[]) -> 
                Some ((Integer.Parse textX, Integer.Parse textY), (Integer.Parse textUsed, Integer.Parse textAvail))
            | _ -> None)
        |> Map.ofSeq

    let getCountOfViablePairs map =
        map
        |> Map.toSeq
        |> Seq.allPairs (map |> Map.toSeq)
        |> Seq.filter (fun (((xA, yA), (usedA, _)), ((xB, yB), (_, availB))) ->
            (xA <> xB || yA <> yB) && usedA <> 0 && usedA <= availB)
        |> Seq.length

    let getMinStepsToGoal map =
        let maxY = map |> Map.toSeq |> Seq.map (fst >> snd) |> Seq.max
        let goal = (0, maxY)
        let goalZero = (0, maxY - 1)

        let zeroStatesDict = Dictionary<(int*int), int>()

        let rec stepsToZeroGoal currZero currSteps fromDir currSolution =
            zeroStatesDict.[currZero] <- currSteps
            let (currZeroX, currZeroY) = currZero
            match currSolution with
            | Some steps when steps < currSteps || goal = currZero || currZeroX < 0 || currZeroY < 0 || currZeroY > maxY -> None
            | _ ->
                if currZero = goalZero then Some currSteps
                else
                    let currAvail = 
                        let (used, avail) = map |> Map.find currZero
                        used + avail
                    let nextMoves =
                        match fromDir with
                        | Left -> seq { yield (currZeroX - 1, currZeroY), Below; yield (currZeroX, currZeroY + 1), Left }
                        | Right -> seq { yield (currZeroX - 1, currZeroY), Below; yield (currZeroX, currZeroY - 1), Right }
                        | Below -> seq { yield (currZeroX - 1, currZeroY), Below; yield (currZeroX, currZeroY - 1), Right; yield (currZeroX, currZeroY + 1), Left }

                    let filterMoves =
                        nextMoves 
                        |> Seq.filter (fun (point, _) -> 
                            let checkPrevStates =
                                match zeroStatesDict.TryGetValue point with
                                | true, prevSteps when prevSteps < currSteps + 1 -> false
                                | _ -> true
                            let checkAvail =
                                ((map |> Map.tryFind point |> Option.defaultValue (Integer.MaxValue, Integer.MaxValue)) |> fst) <= currAvail
                            checkPrevStates && checkAvail)
                    (currSolution, filterMoves)
                    ||> Seq.fold (fun currSolution (nextPoint, fromDir) ->
                        let solution = stepsToZeroGoal nextPoint (currSteps + 1) fromDir currSolution
                        let currSolution =
                            match solution, currSolution with
                            | None, _ -> currSolution
                            | Some newSteps, Some currSteps when currSteps < newSteps -> currSolution
                            | _ -> solution
                        currSolution)




        let isZero i = i = 0
        let zeroPoint = map |> Map.toSeq |> Seq.filter (snd >> fst >> isZero) |> Seq.map fst |> Seq.head
        
        let steps = stepsToZeroGoal zeroPoint 0 Below None

        let queue = Queue<Map<(int*int),(int*int)>*(int*int)*int>()
        queue.Enqueue((map, (0, maxY), 0))
        ()
        |> Seq.unfold (fun _ ->
            if queue.Count = 0 then None
            else
                let (currMap, (currGX, currGY), currSteps) = queue.Dequeue()
                let possibleMoves =
                    currMap
                    |> Map.toSeq
                    |> Seq.allPairs (currMap |> Map.toSeq)
                    |> Seq.filter (fun (((xA, yA), (usedA, _)), ((xB, yB), (usedB, availB))) ->
                        usedB = 0
                        && usedA <> 0 
                        && (xB < 3 || xB <= xA)
                        && (xA <> xB || yA <> yB) 
                        && (xA = xB || yA = yB) 
                        && (Math.Abs(xA - xB) = 1 || Math.Abs(yA - yB) = 1) 
                        && usedA <= availB)
                    |> Seq.toList
                if possibleMoves |> Seq.exists (fun (((xA, yA), _), ((xB, yB), _)) -> xB = 0 && yB = 0 && xA = currGX && yA = currGX) then Some (((0, 0), currSteps + 1), ())
                else
                    let currSteps = currSteps + 1
                    possibleMoves
                    |> Seq.iter (fun (((xA, yA), (usedA, availA)), ((xB, yB), (usedB, availB))) ->
                        let nextMap = currMap |> Map.add (xA, yA) (0, availA + usedA) |> Map.add (xB, yB) (usedB + usedA, availB - usedA)
                        let (nextGX, nextGY) = if xA = currGX && yA = currGY then xB, yB else currGX, currGY
                        queue.Enqueue((nextMap, (nextGX, nextGY), currSteps)))
                    Some (((currGX, currGY), currSteps), ()))
        |> Seq.filter (fun ((gX, gY), _) -> gX = 0 && gY = 0)
        |> Seq.map snd
        |> Seq.head
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.22.txt"

        let nodes = input |> parse 

        let result1 = nodes |> getCountOfViablePairs

        let result2 = nodes |> getMinStepsToGoal

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day23 =
    type Program = { InstructionPointer:int; Registers:Map<char,int>; Instructions:(Program->Program) array}
    
    let nop prog = { prog with InstructionPointer = prog.InstructionPointer + 1 }
    
    let parse (input:string) =
        let textInstructions =
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let rec compileInstructionToFunc line =
            match line with
            | Regex "cpy (.+) (.)" (textValue::textReg::[]) ->
                let regToCopyTo = textReg.Chars 0
                match Integer.TryParse textValue, Integer.TryParse textReg with
                | _, (true, _) -> nop
                | (true, value), _ -> (fun prog ->
                    { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToCopyTo value})
                | (false, _), _ -> 
                    let regToCopyFrom = textValue.Chars 0
                    (fun prog ->
                        let value = prog.Registers |> Map.find regToCopyFrom
                        { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToCopyTo value})
            | Regex "inc (.)" (textReg::[]) ->
                let regToIncreace = textReg.Chars 0
                (fun prog ->
                    let value = (prog.Registers |> Map.find regToIncreace) + 1
                    { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToIncreace value})
            | Regex "dec (.)" (textReg::[]) ->
                let regToDecreace = textReg.Chars 0
                (fun prog ->
                    let value = (prog.Registers |> Map.find regToDecreace) - 1
                    { prog with InstructionPointer = prog.InstructionPointer + 1; Registers = prog.Registers |> Map.add regToDecreace value})
            | Regex "jnz (.+) (.+)" (textValue::textOffset::[]) ->
                match Integer.TryParse textValue, Integer.TryParse textOffset with
                | (true, value), (true, offset) ->
                    let offset = if offset = 0 then 1 else offset
                    if value = 0 then nop 
                    else (fun prog -> { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
                | (true, value), (false, _) ->
                    if value = 0 then nop 
                    else 
                        let offsetReg = textOffset.Chars 0
                        (fun prog -> 
                                let offset = prog.Registers |> Map.find offsetReg
                                let offset = if offset = 0 then 1 else offset
                                { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
                | (false, _), (true, offset) ->
                    let offset = if offset = 0 then 1 else offset
                    let valueReg = textValue.Chars 0
                    (fun prog ->
                            let value = prog.Registers |> Map.find valueReg
                            if value = 0 then prog |> nop
                            else { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
                | (false, _), (false, _) ->
                    let valueReg = textValue.Chars 0
                    let offsetReg = textOffset.Chars 0
                    (fun prog ->
                            let value = prog.Registers |> Map.find valueReg
                            let offset = prog.Registers |> Map.find offsetReg
                            let offset = if offset = 0 then 1 else offset
                            if value = 0 then prog |> nop
                            else { prog with InstructionPointer = prog.InstructionPointer + (offset |> int) })
            | Regex "tgl (.)" (textReg::[]) ->
                let reg = textReg.Chars 0
                (fun prog ->
                    let value = prog.InstructionPointer + (prog.Registers |> Map.find reg)
                    if value < 0 || value >= textInstructions.Length then { prog with InstructionPointer = prog.InstructionPointer + 1 }
                    else
                        let textInstruction = textInstructions.[value]
                        let toggledInstruction =
                            match textInstruction.Substring(0, 3) with
                            | "inc" -> textInstruction.Remove(0, 3).Insert(0, "dec") |> compileInstructionToFunc
                            | "dec" | "tgl" -> textInstruction.Remove(0, 3).Insert(0, "inc") |> compileInstructionToFunc
                            | "cpy" -> textInstruction.Remove(0, 3).Insert(0, "jnz") |> compileInstructionToFunc
                            | "jnz" -> textInstruction.Remove(0, 3).Insert(0, "cpy") |> compileInstructionToFunc
                            | _ -> nop
                        prog.Instructions.[value] <- toggledInstruction
                        { prog with InstructionPointer = prog.InstructionPointer + 1 })
            | _ -> nop
        let instructions =
            textInstructions
            |> Seq.map compileInstructionToFunc
            |> Seq.toArray
        { InstructionPointer = 0; Registers = (seq { for c in 'a' .. 'd' do yield (c, 0)} |> Map.ofSeq); Instructions = instructions }
    
    let runProgram prog =
        prog
        |> Seq.unfold (fun prog ->
            if prog.InstructionPointer < 0 || prog.InstructionPointer >= prog.Instructions.Length then
                None
            else
                let prog = 
                    if prog.InstructionPointer = 4 then
                        // the task description hinted to shortcut the multiplication
                        let (a, b, d) = prog.Registers |> Map.find 'a', prog.Registers |> Map.find 'b', prog.Registers |> Map.find 'd'
                        let registers = prog.Registers |> Map.add 'a' (a + b * d) |> Map.add 'd' 0 |> Map.add '0' 0
                        { prog with Registers = registers; InstructionPointer = 9}
                    else prog.Instructions.[prog.InstructionPointer] prog
                Some (prog, prog))
        |> Seq.last
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.23.txt"
    
        let prog = input |> parse
    
        let result1 = ({ prog with Registers = prog.Registers |> Map.add 'a' 7 } |> runProgram).Registers |> Map.find 'a'
    
        let prog = input |> parse

        let result2 = ({ prog with Registers = prog.Registers |> Map.add 'a' 12 } |> runProgram).Registers |> Map.find 'a'
    
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day24 =
    type Node = { Position:int*int; mutable Up:(Node*int) option; mutable Down:(Node*int) option; mutable Left:(Node*int) option; mutable Right:(Node*int) option; Value: int option}
    let isSameObject = LanguagePrimitives.PhysicalEquality
    let isSame opt proxyNode =
        match opt with
        | Some (n, _) when isSameObject n proxyNode -> true
        | _ -> false

    let parse (input:string) =
        let map =
            input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
            |> Seq.mapi (fun y line -> y, line)
            |> Seq.collect (fun (y, line) -> 
                line 
                |> Seq.mapi(fun x c -> x, c) 
                |> Seq.filter (snd >> (unequal '#')) 
                |> Seq.map (fun (x, c) -> x, y, c))
            |> Seq.map (fun (x, y, c) -> 
                let value = if c = '.' then None else Some (Integer.Parse (c.ToString()))
                (x, y), { Position=(x, y); Up=None; Down=None; Left=None; Right=None; Value=value })
            |> Map.ofSeq
        map
        |> Map.toSeq
        |> Seq.iter (fun ((x, y), node) ->
            let setupBranch set pos =
                let node = map |> Map.tryFind pos
                match node with
                | Some node -> set node
                | None -> ()
            (x, y - 1) |> setupBranch (fun otherNode -> node.Up <- Some (otherNode, 1))
            (x, y + 1) |> setupBranch (fun otherNode -> node.Down <- Some (otherNode, 1))
            (x - 1, y) |> setupBranch (fun otherNode -> node.Left <- Some (otherNode, 1))
            (x + 1, y) |> setupBranch (fun otherNode -> node.Right <- Some (otherNode, 1)))

        let removingCriteria endpointCount node =
            if node.Value |> Option.isSome then false
            else
                let countOfSome = 
                    seq { yield node.Up; yield node.Down; yield node.Left; yield node.Right }
                    |> Seq.filter (fun opt -> opt |> Option.isSome)
                    |> Seq.length
                countOfSome = endpointCount

        // remove with only two endpoints
        let binaryNodes =
            map
            |> Map.toSeq
            |> Seq.filter (snd >> (removingCriteria 2))
            |> Seq.toList
        binaryNodes
        |> Seq.iter (fun (_, node) ->
            let getDistance opt = match opt with | Some (_, dist) -> dist | _ -> Integer.MaxValue
            let setter node proxyNode =
                if isSame node.Up proxyNode then (fun otherNode dist -> node.Up <- Some (otherNode, dist))
                elif isSame node.Down proxyNode then (fun otherNode dist -> node.Down <- Some (otherNode, dist))
                elif isSame node.Left proxyNode then (fun otherNode dist -> node.Left <- Some (otherNode, dist))
                elif isSame node.Right proxyNode then (fun otherNode dist -> node.Right <- Some (otherNode, dist))
                else (fun _ _ -> ())

            match node.Up, node.Down, node.Left, node.Right with
            | Some (node1, dist1), Some (node2, dist2), None, None
            | Some (node1, dist1), None, Some (node2, dist2), None
            | Some (node1, dist1), None, None, Some (node2, dist2)
            | None, Some (node1, dist1), Some (node2, dist2), None
            | None, Some (node1, dist1), None, Some (node2, dist2)
            | None, None, Some (node1, dist1), Some (node2, dist2) -> 
                let (setter1, setter2) = (setter node1 node), (setter node2 node)
                let dist = dist1 + dist2
                setter1 node2 dist
                setter2 node1 dist
            | _ -> ())
        let map =
            (map, binaryNodes)
            ||> Seq.fold (fun map (pos, _) -> map |> Map.remove pos)

        // remove deadlocks
        let deadlocks =
            map
            |> Map.toSeq
            |> Seq.filter (snd >> (removingCriteria 1))
            |> Seq.toList
        deadlocks
        |> Seq.iter (fun (_, deadLockNode) ->
            match deadLockNode.Up, deadLockNode.Down, deadLockNode.Left, deadLockNode.Right with
            | Some (node, _), None, None, None
            | None, Some (node, _), None, None
            | None, None, Some (node, _), None
            | None, None, None, Some (node, _) -> 
                if isSame node.Up deadLockNode then node.Up <- None
                elif isSame node.Down deadLockNode then node.Down <- None
                elif isSame node.Left deadLockNode then node.Left <- None
                elif isSame node.Right deadLockNode then node.Right <- None
                else ()
            // This case may happen with unreachable segments
            | _ -> ())
        let map =
            (map, deadlocks)
            ||> Seq.fold (fun map (pos, _) -> map |> Map.remove pos)

        map

    let determineShortestRoundTrip map =
        let rec shortestPaths distanceSofar node spMap =
            let checkPath opt spMap =
                match opt with 
                | Some (nextNode, distance) -> 
                    let distanceSofar = distanceSofar + distance
                    match spMap |> Map.tryFind nextNode.Position with
                    | Some dist when dist > distanceSofar -> shortestPaths distanceSofar nextNode spMap
                    | None -> shortestPaths distanceSofar nextNode spMap
                    | _ -> spMap
                | _ -> spMap
            let spMap = 
                spMap 
                |> Map.add node.Position distanceSofar 
                |> checkPath node.Up 
                |> checkPath node.Down 
                |> checkPath node.Left 
                |> checkPath node.Right
            spMap

        let rec shortestRoute f number distanceSofar numbersToGo distances =
            if numbersToGo |> Set.isEmpty then f number distanceSofar
            else
                numbersToGo
                |> Seq.map (fun i ->
                    let dist = distances |> Map.find (number, i)
                    shortestRoute f i (dist + distanceSofar) (numbersToGo |> Set.remove i) distances)
                |> Seq.min

        let numberMap =
            map
            |> Map.toSeq
            |> Seq.choose (fun (_, node) -> node.Value |> Option.bind (fun value -> Some (value, node)))
            |> Map.ofSeq

        let distancesToTheNumbers =
            numberMap
            |> Map.toSeq
            |> asFirst Map.empty
            ||> Seq.fold (fun map (number, node) ->
                let shortestPathsMap = shortestPaths 0 node Map.empty
                let shortPathSeq =
                    numberMap
                    |> Map.toSeq
                    |> Seq.filter (fst >> (unequal number))
                    |> Seq.map (fun (otherNumber, otherNode) -> 
                        otherNumber, shortestPathsMap |> Map.find otherNode.Position)
                (map, shortPathSeq)
                ||> Seq.fold (fun map (otherNumber, distance) ->
                    map |> Map.add (number, otherNumber) distance |> Map.add (otherNumber, number) distance))

        let first =
            shortestRoute (fun _ dist -> dist) 0 0 (numberMap |> Map.toSeq |> Seq.map fst |> Seq.filter (unequal 0) |> Set.ofSeq) distancesToTheNumbers
        let second =
            shortestRoute (fun number dist -> dist + (distancesToTheNumbers |> Map.find (number, 0))) 0 0 (numberMap |> Map.toSeq |> Seq.map fst |> Seq.filter (unequal 0) |> Set.ofSeq) distancesToTheNumbers
        first, second
    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.24.txt"

        let map = input |> parse

        let (result1, result2) = map |> determineShortestRoundTrip

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day25 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.25.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

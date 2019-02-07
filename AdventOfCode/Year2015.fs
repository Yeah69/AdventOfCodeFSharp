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

module Day2 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.02.txt"

        let sequence = input.Split('\n')
                      |> Seq.ofArray
                      |> Seq.map (fun line -> line.TrimEnd('\r'))
                      |> Seq.map (fun line -> line.Split('x')
                                              |> Array.map (fun numberAsText -> match System.Int32.TryParse(numberAsText) with
                                                                                | true, number -> number
                                                                                | _ -> 0))

        let wrappingPaper = 
            sequence
            |> Seq.fold (fun state current -> 
                let numbers = [| current.[0] * current.[1]; current.[0] * current.[2]; current.[1] * current.[2] |]
                let min = numbers |> Array.min
                let currentSum = numbers |> Array.map (fun number -> number * 2) |> Array.sum
                state + currentSum + min) 0
        let ribbons = 
            sequence
            |> Seq.fold (fun state current -> 
                let sides = current |> Array.sort |> Array.take 2 |> Array.map (fun n -> n + n) |> Array.sum
                let bow = current.[0] * current.[1] * current.[2]
                state + sides + bow) 0
        { First = wrappingPaper.ToString(); Second = ribbons.ToString() }

module Day3 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.03.txt"

        let parseDirections (x, y) c =
            match c with
            | '^' -> (x, y + 1)
            | 'v' -> (x, y - 1)
            | '<' -> (x - 1, y)
            | '>' -> (x + 1, y)
            | _ -> (x, y)

        let sequence = input.ToCharArray()
                      |> Seq.ofArray
                      |> Seq.scan parseDirections (0,0)

        let sequenceEven = input.ToCharArray()
                          |> Seq.ofArray
                          |> Seq.mapi (fun i c -> i, c)
                          |> Seq.filter (fun (i, _) -> i % 2 = 0)
                          |> Seq.map (fun (_, c) -> c)
                          |> Seq.scan parseDirections (0,0)

        let sequenceOdd = input.ToCharArray()
                          |> Seq.ofArray
                          |> Seq.mapi (fun i c -> i, c)
                          |> Seq.filter (fun (i, _) -> i % 2 = 1)
                          |> Seq.map (fun (_, c) -> c)
                          |> Seq.scan parseDirections (0,0)

        let housesSum = 
            seq { yield (0, 0) }
            |> Seq.append sequence
            |> Seq.distinct
            |> Seq.sumBy (fun _ -> 1)
            
        let housesSharedWork = 
            seq { yield (0, 0) }
            |> Seq.append sequenceEven
            |> Seq.append sequenceOdd
            |> Seq.distinct
            |> Seq.sumBy (fun _ -> 1)


        { First = housesSum.ToString(); Second = housesSharedWork.ToString() }

module Day4 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.04.txt"

        let sequence =
            seq { while true do yield input }
            |> Seq.mapi (fun i input -> i, input + (i |> string))
            |> Seq.skip 1 
            |> Seq.map (fun (i, message) -> i, System.Text.Encoding.ASCII.GetBytes(message))
            |> Seq.map (fun (i, message) -> i, Operations.md5 message)
            
        let (i1, _) = sequence 
                      |> Seq.filter (fun (_, hash) -> hash.StartsWith("00000")) 
                      |> Seq.head

        let (i2, _) = sequence 
                      |> Seq.filter (fun (_, hash) -> hash.StartsWith("000000")) 
                      |> Seq.head


        { First = i1.ToString(); Second = i2.ToString() }

module Day5 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.05.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let result1 =
            lines
            |> Seq.ofArray
            |> Seq.where (fun line ->
                let chars = line.ToCharArray()
                let doubles = 
                    chars 
                    |> Seq.ofArray 
                    |> Seq.pairwise 
                    |> Seq.exists (fun (c1, c2) -> c1 = c2) 
                let valid = 
                    chars 
                    |> Seq.ofArray 
                    |> Seq.pairwise 
                    |> Seq.map (fun (c1, c2) -> sprintf "%c%c" c1 c2) 
                    |> Seq.forall (fun pair -> pair <> "ab" && pair <> "cd" && pair <> "pq" && pair <> "xy")
                let vowels = 
                    chars 
                    |> Seq.ofArray 
                    |> Seq.where (fun c -> c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
                    |> Seq.length >= 3
                doubles && vowels && valid)
            |> Seq.length
            
        let result2 =
            lines
            |> Seq.ofArray
            |> Seq.where (fun line ->
                let chars = line.ToCharArray()
                
                let condition1 =
                    seq{ 2 .. chars.Length - 1}
                    |> Seq.exists (fun i -> chars.[i] = chars.[i - 2])
                let condition2 =
                    chars
                    |> Seq.ofArray
                    |> Seq.pairwise
                    |> Seq.map (fun (c1, c2) -> sprintf "%c%c" c1 c2)
                    |> Seq.exists (fun text ->
                        line.LastIndexOf(text) - line.IndexOf(text) >= 2)

                condition1 && condition2)
            |> Seq.length

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day6 =
    open System.Text.RegularExpressions

    type Integer = int

    type Action = 
        | TurnOn of From : int*int * To : int*int
        | TurnOff of From : int*int * To : int*int
        | Toggle of From : int*int * To : int*int

    let (| Action | _ |) text =
        let matchTurnOn = Regex.Match(text, "turn on (\d+),(\d+) through (\d+),(\d+)")
        let matchTurnOff = Regex.Match(text, "turn off (\d+),(\d+) through (\d+),(\d+)")
        let matchToggle = Regex.Match(text, "toggle (\d+),(\d+) through (\d+),(\d+)")
        seq { yield TurnOn (0,0,0,0), matchTurnOn
              yield TurnOff (0,0,0,0), matchTurnOff
              yield Toggle (0,0,0,0), matchToggle }
        |> Seq.filter (fun (_, mat) -> mat.Success)
        |> Seq.tryHead
        |> Option.map (fun (action, mat) ->
            let from = mat.Groups.[1].Value |> Integer.Parse, mat.Groups.[2].Value |> Integer.Parse
            let tot = mat.Groups.[3].Value |> Integer.Parse, mat.Groups.[4].Value |> Integer.Parse
            match action with
            | TurnOn _->  TurnOn (from |> fst, from |> snd, tot |> fst, tot |> snd)
            | TurnOff _->  TurnOff (from |> fst, from |> snd, tot |> fst, tot |> snd)
            | Toggle _->  Toggle (from |> fst, from |> snd, tot |> fst, tot |> snd))

    let doAction (turnOn, turnOff, toggle) field action =
        let action' pos =
            match action with
            | TurnOn _ -> turnOn pos field
            | TurnOff _ -> turnOff pos field
            | Toggle _ -> toggle pos field
        match action with
        | TurnOn (fromX, fromY, toX, toY) 
        | TurnOff (fromX, fromY, toX, toY) 
        | Toggle (fromX, fromY, toX, toY) ->
            seq {fromX .. toX }
            |> Seq.allPairs { fromY .. toY}
            |> Seq.iter action'

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.06.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let field1 = Array2D.create 1000 1000 false

        let funcs1 = 
            (fun (x, y) field -> (x, y, true) |||> Array2D.set field), 
            (fun (x, y) field -> (x, y, false) |||> Array2D.set field), 
            (fun (x, y) field -> (x, y, (x, y) ||> Array2D.get field |> not) |||> Array2D.set field)

        lines
        |> Seq.ofArray
        |> Seq.choose (fun line -> match line with | Action action -> Some action | _ -> None)
        |> Seq.iter (doAction funcs1 field1)

        let result1 = 
            seq { 0 .. 999} 
            |> Seq.allPairs (seq { 0 .. 999 }) 
            |> Seq.filter (fun pos -> pos ||> Array2D.get field1)
            |> Seq.length

        let field2 = Array2D.create 1000 1000 0

        let funcs2 = 
            (fun (x, y) field -> (x, y, ((x, y) ||> Array2D.get field) + 1) |||> Array2D.set field), 
            (fun (x, y) field -> 
                let value = (x, y) ||> Array2D.get field
                (x, y, if value > 0 then value - 1 else value) |||> Array2D.set field),
            (fun (x, y) field -> (x, y, ((x, y) ||> Array2D.get field) + 2) |||> Array2D.set field)

        lines
        |> Seq.ofArray
        |> Seq.choose (fun line -> match line with | Action action -> Some action | _ -> None)
        |> Seq.iter (doAction funcs2 field2)

        let result2 = 
            seq { 0 .. 999} 
            |> Seq.allPairs (seq { 0 .. 999 }) 
            |> Seq.sumBy (fun pos -> pos ||> Array2D.get field2)

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day7 =
    open System.Text.RegularExpressions

    type UInt16 = uint16
    type Integer = int

    type Operation = 
        | Assignment of Operand : string * Assignee : string
        | And of Operand1 : string * Operand2 : string * Assignee : string
        | Or of Operand1 : string * Operand2 : string * Assignee : string
        | LeftShift of Operand1 : string * Operand2 : string * Assignee : string
        | RightShift of Operand1 : string * Operand2 : string * Assignee : string
        | Not of Operand : string * Assignee : string

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let (|Operation|_|) text =
        match text with
        | Regex "(.+) -> (.+)" [ operation; assignee ] -> 
            match operation with
            | Regex "(.+) AND (.+)" [op1; op2]      -> Some(And(op1, op2, assignee))
            | Regex "(.+) OR (.+)" [op1; op2]       -> Some(Or(op1, op2, assignee))
            | Regex "(.+) LSHIFT (.+)" [op1; op2]   -> Some(LeftShift(op1, op2, assignee))
            | Regex "(.+) RSHIFT (.+)" [op1; op2]   -> Some(RightShift(op1, op2, assignee))
            | Regex "NOT (.+)" [op]                 -> Some(Not(op, assignee))
            | _                                     -> Some(Assignment(operation, assignee))
        | _ -> None

    let doOperation map operation =
        let insertIfNotExisting map operand =
            if (operand, map) ||> Map.containsKey then
                map
            else
                let success, number = Integer.TryParse operand
                let number = if success then number else 0
                (operand, uint16 number, map) |||> Map.add
        match operation with
        | Assignment (op, ass) ->
            let map = (map, op) ||> insertIfNotExisting
            (ass, (op, map) ||> Map.find, map) |||> Map.add
        | And (op1, op2, ass) ->
            let map = (map, op1) ||> insertIfNotExisting
            let map = (map, op2) ||> insertIfNotExisting
            let result = ((op1, map) ||> Map.find) &&& ((op2, map) ||> Map.find)
            (ass, result, map) |||> Map.add
        | Or (op1, op2, ass) ->
            let map = (map, op1) ||> insertIfNotExisting
            let map = (map, op2) ||> insertIfNotExisting
            let result = ((op1, map) ||> Map.find) ||| ((op2, map) ||> Map.find)
            (ass, result, map) |||> Map.add
        | LeftShift (op1, op2, ass) ->
            let map = (map, op1) ||> insertIfNotExisting
            let map = (map, op2) ||> insertIfNotExisting
            let result = ((op1, map) ||> Map.find) <<< int32 ((op2, map) ||> Map.find)
            (ass, result, map) |||> Map.add
        | RightShift (op1, op2, ass) ->
            let map = (map, op1) ||> insertIfNotExisting
            let map = (map, op2) ||> insertIfNotExisting
            let result = ((op1, map) ||> Map.find) >>> int32 ((op2, map) ||> Map.find)
            (ass, result, map) |||> Map.add
        | Not (op, ass) ->
            let map = (map, op) ||> insertIfNotExisting
            let result = ~~~ ((op, map) ||> Map.find)
            (ass, result, map) |||> Map.add

    let getAssignee operation =
        match operation with
        | Assignment (_, ass) 
        | And (_, _, ass)
        | Or (_, _, ass)
        | And (_, _, ass)
        | LeftShift (_, _, ass)
        | RightShift (_, _, ass)
        | Not (_, ass) -> ass

    let getOperands operation =
        match operation with
        | Assignment (op, _) 
        | Not (op, _) -> [ op ]
        | And (op1, op2, _)
        | Or (op1, op2, _)
        | LeftShift (op1, op2, _)
        | RightShift (op1, op2, _) -> [ op1; op2 ]


    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.07.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let map = Map.empty

        let assigneeToOperation =
            lines
            |> Seq.ofArray
            |> Seq.choose (fun line -> 
                match line with
                | Operation op -> Some op
                | _ -> None)
            |> Seq.map (fun op -> op |> getAssignee, op)
            |> Map.ofSeq
        
        let iteration map assigneeToOperation =
            (map, assigneeToOperation)
            |> Seq.unfold (fun (map, assigneeToOperation) ->
                if assigneeToOperation |> Map.isEmpty then
                    None
                else
                    let (map, assigneeToOperation) =
                        assigneeToOperation
                        |> Map.toSeq
                        |> Seq.filter(fun (_, op) -> 
                            op 
                            |> getOperands 
                            |> List.forall (fun operand -> 
                                let (isNumber, _) = operand |> Integer.TryParse
                                isNumber || ((operand, assigneeToOperation) ||> Map.containsKey |> not)))
                        |> Operations.asFirst (map, assigneeToOperation)
                        ||> Seq.fold (fun (map, assingeeToOperation) (assignee, operation) -> 
                            let map = (map, operation) ||> doOperation
                            let assigneeToOperation = (assignee, assingeeToOperation) ||> Map.remove
                            (map, assigneeToOperation))
                    Some(map, (map, assigneeToOperation)))
            |> Seq.last

        let map = (map, assigneeToOperation) ||> iteration

        let result1 = ("a", map) ||> Map.find

        let map = Map.empty
        let map = ("b", result1, map) |||> Map.add

        let assigneeToOperation =
            lines
            |> Seq.ofArray
            |> Seq.choose (fun line -> 
                match line with
                | Operation op -> Some op
                | _ -> None)
            |> Seq.map (fun op -> op |> getAssignee, op)
            |> Map.ofSeq
        let assigneeToOperation = ("b", assigneeToOperation) ||> Map.remove

        let map = (map, assigneeToOperation) ||> iteration
            
        let result2 = ("a", map) ||> Map.find

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day8 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.08.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let (inCode, inMemory) =
            lines
            |> Seq.ofArray
            |> Seq.map (fun line ->
                let inCode = line.Length
                let inMemory = 
                    1
                    |> Seq.unfold (fun curIdx ->
                        if curIdx >= line.Length - 1 then
                            None
                        else
                            let nextIdx =
                                match line.Chars curIdx, line.Chars (curIdx + 1) with
                                | ('\\', '\\') | ('\\', '"') -> curIdx + 2
                                | ('\\', 'x') -> curIdx + 4
                                | _ -> curIdx + 1

                            Some(1, nextIdx))
                    |> Seq.sum
                inCode, inMemory)
            |> Operations.asFirst (0, 0)
            ||> Seq.fold (fun (aggInCode, aggInMemory) (inCode, inMemory) ->
                aggInCode + inCode, aggInMemory + inMemory)
        
        let result1 = inCode - inMemory

        let increase =
            lines
            |> Seq.ofArray
            |> Seq.map (fun line ->
                (line.ToCharArray() |> Seq.ofArray |> Seq.filter (fun c -> c = '"' || c = '\\') |> Seq.length) + 2)
            |> Seq.sum

        let result2 = increase

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day9 =
    open System.Text.RegularExpressions

    type Integer = int

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let identity x = x

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.09.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let map =
            lines
            |> Seq.ofArray
            |> Seq.map(fun line ->
                match line with
                | Regex "(.+) to (.+) = (\d+)" mat -> Some mat
                | _ -> None)
            |> Seq.choose identity
            |> Seq.collect (fun mat ->
                match mat with
                | firstTown::secondTown::distText::_ ->
                    let distance = distText |> Integer.Parse
                    seq { yield (firstTown, secondTown), distance
                          yield (secondTown, firstTown), distance }
                | _ -> Seq.empty)
            |> Map.ofSeq

        let minDist' f =
            let rec minDist set currentTown (currentDistance : int) =
                if set |> Set.isEmpty then
                    currentDistance
                else
                    set
                    |> Set.toSeq
                    |> Seq.map (fun nextTown ->
                        minDist (set |> Set.remove nextTown) nextTown (currentDistance + map.[(currentTown, nextTown)]))
                    |> f

            let set = map |> Map.toSeq |> Seq.map (fun ((town, _), _ ) -> town) |> Set.ofSeq

            set |> Set.toSeq |> Seq.map (fun town -> minDist (set |> Set.remove town) town 0) |> f

        let result1 = minDist' Seq.min

        let result2 = minDist' Seq.max

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day10 =
    type Integer = int

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.10.txt"

        let inputAsNodes =
            input.ToCharArray()
            |> Seq.ofArray
            |> Operations.asFirst []
            ||> Seq.fold (fun currentList nextChar ->
                match currentList with
                | [] -> [(1, nextChar.ToString() |> Integer.Parse)]
                | (number, lastChar)::tail when lastChar = (nextChar.ToString() |> Integer.Parse) -> (number + 1, lastChar)::tail
                | _ -> (1, nextChar.ToString() |> Integer.Parse)::currentList)
            |> List.rev

        let iteration currentNodes =
            ([], currentNodes)
            ||> List.fold(fun currentList currentNode ->
                match currentList, currentNode with
                | [], (count, number) when count = number -> [(2, count)]
                | [], (count, number) -> [(1, number); (1,count)]
                | (headCount, headNumber)::tail, (count, number) when count = headNumber && count = number -> (headCount + 2, headNumber)::tail
                | (headCount, headNumber)::tail, (count, number) when count = headNumber -> (1, number)::(headCount + 1, headNumber)::tail
                | currentList, (count, number) when count = number -> (2, number)::currentList
                | currentList, (count, number) -> (1, number)::(1, count)::currentList
                | _ -> failwith "something wrong here")
            |> List.rev
            
        let forResult1 = (inputAsNodes, seq { 1 .. 39 }) ||> Seq.fold (fun curNodes _ -> iteration curNodes)
        let result1 = (forResult1 |> List.length) * 2
        
        let forResult2 = (forResult1, seq { 1 .. 10 }) ||> Seq.fold (fun curNodes _ -> iteration curNodes)
        let result2 = (forResult2 |> List.length) * 2

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day11 =
    type Integer = int

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.11.txt"
        
        let iteration (input : string) =
            (input.ToCharArray() |> Array.map (fun c -> (c |> int) - 96))
            |> Seq.unfold (fun previous ->
                let next = previous |> Array.copy
                let _ =
                    ((next.Length - 1), false)
                    |> Seq.unfold (fun (i, abort) ->
                        if abort || i < 0 then None
                        else
                            let x = (next, i) ||> Array.get
                            (next, i, if x >= 26 then 1 else x + 1) |||> Array.set
                            Some(1, (i - 1, x <> 26)))
                    |> Seq.last
                Some(next, next))
            |> Seq.filter (fun current ->
                let rejectNumber number = (number, current) ||> Array.contains |> not
                let twoDoubles() = (current |> Seq.pairwise |> Seq.filter (fun (i1, i2) -> i1 = i2) |> Seq.distinct |> Seq.length) >= 2
                let straight() = 
                    seq { 0 .. current.Length - 3 } 
                    |> Seq.map (fun i -> (current, i) ||> Array.get, (current, i + 1) ||> Array.get, (current, i + 2) ||> Array.get) 
                    |> Seq.filter (fun (i0, i1, i2) -> i1 = i0 + 1 && i2 = i0 + 2) 
                    |> Seq.tryHead
                    |> Option.map (fun _ -> true)
                    |> Option.defaultValue false
                rejectNumber 9 && rejectNumber 15 && rejectNumber 12 && twoDoubles() && straight())
            |> Seq.map (fun currentArr -> new string(currentArr |> Array.map(fun i -> (i + 96) |> char)))
            |> Seq.head

        let result1 = iteration input
            
        let result2 = iteration result1

        { First = result1; Second = result2 }

module Day12 =
    open System.Text.RegularExpressions
    open Newtonsoft.Json.Linq
    
    type Integer = int

    let solution2Abort (jProp : JProperty) = jProp.Value.Type = JTokenType.String && jProp.Value.ToObject<string>() = "red"

    let rec solution2 (jObject : JObject) =
        let rec solution2Tokens (jToken : JToken) =
            match jToken.Type with
            | JTokenType.Property -> jToken.ToObject<JProperty>().Value |> solution2Tokens
            | JTokenType.Integer -> jToken.ToObject<int>()
            | JTokenType.Object -> jToken.ToObject<JObject>() |> solution2
            | JTokenType.Array -> jToken.ToObject<JArray>() |> Seq.sumBy solution2Tokens
            | _ -> 0

        if jObject.Properties() |> Seq.exists solution2Abort then
            0
        else
            jObject.Properties()
            |> Seq.sumBy (fun jProperty -> solution2Tokens jProperty)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.12.txt"
        
        let result1 =
            Regex.Matches(input, "(-?\d+)")
            |> Seq.cast
            |> Seq.map (fun (mat : Match) -> mat.Value |> Integer.Parse)
            |> Seq.sum

        let result2 = JObject.Parse(input) |> solution2

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day13 =

    type Integer = int

    let identity x = x

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.13.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let map =
            lines
            |> Seq.map (fun line ->
                match line with
                | Regex "(.+) would (.+) (\d+) happiness units by sitting next to (.+)\." matches -> 
                    match matches with
                    | name1::modifier::number::name2::_ -> Some((name1, name2), Integer.Parse(number) * (if modifier = "lose" then -1 else 1))
                    | _ -> None
                | _ -> None)
            |> Seq.choose identity
            |> Map.ofSeq

        let setOfNames = map |> Map.toSeq |> Seq.map (fst >> fst) |> Set.ofSeq

        let rec permutations currentlyChosen remaining =
            if remaining |> Set.isEmpty then
                seq { yield currentlyChosen }
            else
                remaining
                |> Set.toSeq
                |> Seq.collect (fun next -> permutations (next::currentlyChosen) (remaining |> Set.remove next))
                
        let evaluate assignement =
            if assignement |> List.isEmpty then (0, 0)
            else
                let last = assignement |> List.last
                let mutable min = Integer.MaxValue
                let sum =  
                    last::assignement 
                    |> List.pairwise 
                    |> List.map (fun (name1, name2) -> 
                        let partSum = (map |> Map.find (name1, name2)) + (map |> Map.find (name2, name1))
                        min <- if partSum < min then partSum else min
                        partSum)
                    |> List.sum 
                sum, (sum - min)

        let (result1, result2) =
            ([], setOfNames) 
            ||> permutations
            |> Operations.asFirst (Integer.MinValue, Integer.MinValue)
            ||> Seq.fold (fun (max1, max2) assignment ->
                let (curr1, curr2) = evaluate assignment
                (if curr1 > max1 then curr1 else max1), (if curr2 > max2 then curr2 else max2))

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
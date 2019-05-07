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

module Day14 =

    type Integer = int

    type Reindeer = { Speed : int; Duration : int; Rest : int; CurrentDistance : int; CurrentPoints : int }

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.14.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let reindeers =
            lines
            |> Seq.map (fun line ->
                match line with
                | Regex "(.+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." matches -> 
                    match matches with
                    | _::speed::duration::restDuration::_ -> Some({ Speed = Integer.Parse(speed)
                                                                    Duration = Integer.Parse(duration)
                                                                    Rest = Integer.Parse(restDuration)
                                                                    CurrentDistance = 0 
                                                                    CurrentPoints = 0 })
                    | _ -> None
                | _ -> None)
            |> Seq.choose identity
            |> Seq.toArray

        let runDistance reindeer runDuration =
            let blockDistance = reindeer.Speed * reindeer.Duration
            let blockSpan = reindeer.Duration + reindeer.Rest
            let totalBlockDistance = blockDistance * (runDuration / blockSpan)
            let currentModulo = runDuration % blockSpan
            let distanceCurrentBlock = if currentModulo >= reindeer.Duration then blockDistance else currentModulo * reindeer.Speed
            totalBlockDistance + distanceCurrentBlock

        let result1 = reindeers |> Array.toSeq |> Seq.map (fun reindeer -> runDistance reindeer 2503) |> Seq.max
        
        let finalReindeers =
            (reindeers, seq{ 1 .. 2503 })
            ||> Seq.fold (fun reindeers i ->
                let reindeers = 
                    reindeers 
                    |> Array.map (fun reindeer -> 
                        let modulo = i % (reindeer.Duration + reindeer.Rest)
                        if modulo <> 0 && modulo <= reindeer.Duration then
                            { reindeer with CurrentDistance = reindeer.CurrentDistance + reindeer.Speed }
                        else reindeer)
                let maxDistance = (reindeers |> Array.maxBy (fun reindeer -> reindeer.CurrentDistance)).CurrentDistance
                let reindeers = 
                    reindeers 
                    |> Array.map (fun reindeer -> 
                        if reindeer.CurrentDistance = maxDistance then
                            { reindeer with CurrentPoints = reindeer.CurrentPoints + 1 }
                        else reindeer)
                reindeers)

        let result2 = finalReindeers |> Array.map (fun reindeer -> reindeer.CurrentPoints) |> Array.max

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
        
module Day15 =
    
    open System
    
    type Integer = int
    
    type Ingredient = { Capacity : int; Durability : int; Flavor : int; Texture : int; Calories : int }

    let rec getCombinations ingredientSet pieceCount =
        if (ingredientSet |> Set.count) = 1 then seq { yield seq { yield (ingredientSet |> Set.toSeq |> Seq.head), pieceCount}}
        else
            let ingredient = ingredientSet |> Set.toSeq |> Seq.head
            let ingredientSet = ingredientSet |> Set.remove ingredient
            seq { 0 .. pieceCount }
            |> Seq.collect (fun i -> 
                let others = getCombinations ingredientSet (pieceCount - i)
                others |> Seq.map (fun combination -> combination |> Seq.append (seq { yield ingredient, i })))
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.15.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let ingredients =
            lines
            |> Seq.map (fun line ->
                match line with
                | Regex "(.+): capacity (.+), durability (.+), flavor (.+), texture (.+), calories (.+)" matches -> 
                    match matches with
                    | name::capacity::durability::flavor::texture::calories::_ -> 
                        Some(name, 
                             { Capacity = Integer.Parse(capacity)
                               Durability = Integer.Parse(durability)
                               Flavor = Integer.Parse(flavor)
                               Texture = Integer.Parse(texture)
                               Calories = Integer.Parse(calories) })
                    | _ -> None
                | _ -> None)
            |> Seq.choose identity
            |> Map.ofSeq
            
        let ingredientNames = 
            ingredients 
            |> Map.toSeq 
            |> Seq.map (fun (key, _) -> key) 
            |> Set.ofSeq

        let combinations = getCombinations ingredientNames 100
        
        let result1 = 
            combinations
            |> Seq.map (fun combination ->
                let calculateProperty getProperty =
                    Math.Max (0, 
                              combination 
                              |> Seq.map (fun (ingredient, pieceCount) -> 
                                ((ingredients |> Map.find ingredient) |> getProperty) * pieceCount) 
                              |> Seq.sum)
                let capacity = calculateProperty (fun ing -> ing.Capacity)
                let durability = calculateProperty (fun ing -> ing.Durability)
                let flavor = calculateProperty (fun ing -> ing.Flavor)
                let texture = calculateProperty (fun ing -> ing.Texture)
                capacity * durability * flavor * texture)
            |> Seq.max
        
        let result2 =
            combinations
            |> Seq.map (fun combination ->
                let calculateProperty getProperty =
                    Math.Max (0, 
                              combination 
                              |> Seq.map (fun (ingredient, pieceCount) -> 
                                ((ingredients |> Map.find ingredient) |> getProperty) * pieceCount) 
                              |> Seq.sum)
                let capacity = calculateProperty (fun ing -> ing.Capacity)
                let durability = calculateProperty (fun ing -> ing.Durability)
                let flavor = calculateProperty (fun ing -> ing.Flavor)
                let texture = calculateProperty (fun ing -> ing.Texture)
                let calories = calculateProperty (fun ing -> ing.Calories)
                calories, capacity * durability * flavor * texture)
            |> Seq.filter (fun (calories, _) -> calories = 500)
            |> Seq.map snd
            |> Seq.max
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
        
module Day16 =

    type Integer = int
    
    type Sue = { Number: int; 
                 Children : int option; 
                 Cats : int option; 
                 Samoyeds : int option;
                 Pomerians : int option;
                 Akitas : int option;
                 Vizslas : int option;
                 Goldfish : int option;
                 Trees : int option;
                 Cars : int option;
                 Perfumes : int option; }
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.16.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let aunts =
            lines
            |> Seq.map (fun line ->
                match line with
                | Regex "Sue (\d+): (.+)" matches -> 
                    match matches with
                    | number::remainder::_ -> 
                        let children = match remainder with 
                                       | Regex "children: (\d+)" found -> Some(Integer.Parse found.Head)
                                       | _ -> None
                        let cats = match remainder with 
                                   | Regex "cats: (\d+)" found -> Some(Integer.Parse found.Head)
                                   | _ -> None
                        let samoyeds = match remainder with 
                                       | Regex "samoyeds: (\d+)" found -> Some(Integer.Parse found.Head)
                                       | _ -> None
                        let pomerians = match remainder with 
                                        | Regex "pomerians: (\d+)" found -> Some(Integer.Parse found.Head)
                                        | _ -> None
                        let akitas = match remainder with 
                                     | Regex "akitas: (\d+)" found -> Some(Integer.Parse found.Head)
                                     | _ -> None
                        let vizslas = match remainder with 
                                      | Regex "vizslas: (\d+)" found -> Some(Integer.Parse found.Head)
                                      | _ -> None
                        let goldfish = match remainder with 
                                       | Regex "goldfish: (\d+)" found -> Some(Integer.Parse found.Head)
                                       | _ -> None
                        let trees = match remainder with 
                                    | Regex "trees: (\d+)" found -> Some(Integer.Parse found.Head)
                                    | _ -> None
                        let cars = match remainder with 
                                   | Regex "cars: (\d+)" found -> Some(Integer.Parse found.Head)
                                   | _ -> None
                        let perfumes = match remainder with 
                                       | Regex "perfumes: (\d+)" found -> Some(Integer.Parse found.Head)
                                       | _ -> None
                        Some({ Number = Integer.Parse number; 
                               Children = children;
                               Cats = cats; 
                               Samoyeds = samoyeds; 
                               Pomerians = pomerians; 
                               Akitas = akitas;
                               Vizslas = vizslas;
                               Goldfish = goldfish;
                               Trees = trees;
                               Cars = cars;
                               Perfumes = perfumes;})
                    | _ -> None
                | _ -> None)
            |> Seq.choose identity

        let matches = 
            aunts 
            |> Seq.filter (fun aunt ->
                let children = match aunt.Children with | Some x -> x = 3 | None -> true
                let cats = match aunt.Cats with | Some x -> x = 7 | None -> true
                let samoyeds = match aunt.Samoyeds with | Some x -> x = 2 | None -> true
                let pomerians = match aunt.Pomerians with | Some x -> x = 3 | None -> true
                let akitas = match aunt.Akitas with | Some x -> x = 0 | None -> true
                let vizslas = match aunt.Vizslas with | Some x -> x = 0 | None -> true
                let goldfish = match aunt.Goldfish with | Some x -> x = 5 | None -> true
                let trees = match aunt.Trees with | Some x -> x = 3 | None -> true
                let cars = match aunt.Cars with | Some x -> x = 2 | None -> true
                let perfumes = match aunt.Perfumes with | Some x -> x = 1 | None -> true
                children && cats && samoyeds && pomerians && akitas && vizslas && goldfish && trees && cars && perfumes)

        let result1 = (matches |> Seq.head).Number
        
        let matches = 
            aunts 
            |> Seq.filter (fun aunt ->
                let children = match aunt.Children with | Some x -> x = 3 | None -> true
                let cats = match aunt.Cats with | Some x -> x > 7 | None -> true
                let samoyeds = match aunt.Samoyeds with | Some x -> x = 2 | None -> true
                let pomerians = match aunt.Pomerians with | Some x -> x < 3 | None -> true
                let akitas = match aunt.Akitas with | Some x -> x = 0 | None -> true
                let vizslas = match aunt.Vizslas with | Some x -> x = 0 | None -> true
                let goldfish = match aunt.Goldfish with | Some x -> x < 5 | None -> true
                let trees = match aunt.Trees with | Some x -> x > 3 | None -> true
                let cars = match aunt.Cars with | Some x -> x = 2 | None -> true
                let perfumes = match aunt.Perfumes with | Some x -> x = 1 | None -> true
                children && cats && samoyeds && pomerians && akitas && vizslas && goldfish && trees && cars && perfumes)
        
        let result2 = (matches |> Seq.head).Number
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
        
module Day17 =

    type Integer = int

    let rec checkCombinations availableContainers value chosenContainers returnedList =
        match availableContainers with
        | (i, capacity)::remainder ->
            let returnedList = checkCombinations remainder value chosenContainers returnedList
            let value = value + capacity
            if value > 150 then returnedList
            else
                let chosenContainers = (i, capacity)::chosenContainers
                if value = 150 then
                    chosenContainers::returnedList
                else
                    checkCombinations remainder value chosenContainers returnedList
        | _ -> returnedList
            
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.17.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let containers =
            lines
            |> Seq.mapi (fun i line ->
                match line with
                | Integer mat -> Some (i, mat)
                | _ -> None)
            |> Seq.choose identity
            |> Seq.toList

        let combinations = (checkCombinations containers 0 List.empty List.empty)

        let result1 = combinations |> List.length

        let minContainers = combinations |> Seq.ofList |> Seq.map (fun combination -> combination |> List.length) |> Seq.min
        
        let result2 = combinations |> Seq.ofList |> Seq.filter (fun combination -> combination |> List.length = minContainers) |> Seq.length
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
        
module Day18 =

    open System

    type Integer = int
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.18.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let onPositions =
            lines
            |> Seq.mapi (fun y line ->
                y, line)
            |> Seq.collect (fun (y, line) ->
                line |> Seq.mapi (fun x char -> x, y, char))
            |> Seq.filter (fun (_, _, char) -> char = '#')
            |> Seq.map (fun (x, y, _) -> x, y)
            |> Set.ofSeq

        let initialField = 
            Array2D.create 100 100 false
            |> Array2D.mapi (fun x y _ -> onPositions |> Set.contains (x, y))

        let run field prepStep =
            (field, seq { 0 .. 99})
            ||> Seq.fold (fun field _ -> 
                prepStep field
                field |> Array2D.mapi (fun x y value ->
                    let count = 
                        (seq { Math.Max(0, x - 1) .. Math.Min(99, x + 1) }, seq { Math.Max(0, y - 1) .. Math.Min(99, y + 1) }) 
                        ||> Seq.allPairs
                        |> Seq.map (fun (x, y) -> (x, y) ||> Array2D.get field)
                        |> Seq.filter identity
                        |> Seq.length
                    value && (count = 3 || count = 4) || count = 3))

        let result1 = run initialField ignore |> Seq.cast |> Seq.filter identity |> Seq.length
        
        let stuckOn field =
            (0, 0, true) |||> Array2D.set field 
            (0, 99, true) |||> Array2D.set field 
            (99, 0, true) |||> Array2D.set field 
            (99, 99, true) |||> Array2D.set field
        
        let fieldForSolution2 = run initialField stuckOn
        
        stuckOn fieldForSolution2

        let result2 = fieldForSolution2 |> Seq.cast |> Seq.filter identity |> Seq.length
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
        
module Day19 =

    open System
    open System.Threading

    type Integer = int
        
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.19.txt"
        let lines = input.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)

        let rules =
            lines
            |> Seq.map (fun line ->
                match line with
                | Regex "(.*) => (.*)" (find::replace::[]) -> Some(find, replace)
                | _ -> None)
            |> Seq.choose identity
            |> Seq.groupBy fst
            |> Seq.map (fun (find, replacements) ->
                find, replacements |> Seq.map snd |> Seq.toArray)
            |> Map.ofSeq

        let medMolecule = lines |> Seq.last

        let molecules = 
            rules
            |> Map.toSeq
            |> Seq.collect (fun (find, replacements) -> 
                if medMolecule.IndexOf(find) < 0 then Seq.empty
                else
                    0
                    |> Seq.unfold (fun i ->
                        let index = medMolecule.IndexOf(find, i)
                        if index < 0 then None
                        else Some(index, index + 1))
                    |> Seq.collect (fun i ->
                        replacements |> Seq.map (fun replace -> 
                            let removed = medMolecule.Remove(i, find.Length)
                            removed.Insert(i, replace))))
            |> Seq.distinct

        let result1 = molecules |> Seq.length

        let rules =
            lines
            |> Seq.map (fun line ->
                match line with
                | Regex "(.*) => (.*)" (replace::find::[]) -> Some(find, replace)
                | _ -> None)
            |> Seq.choose identity
            |> Array.ofSeq

        let mutable min = Integer.MaxValue
        let mutable map = Map.empty

        let rec findSteps (molecule:string) steps =
            let doIt() =
                rules 
                |> Seq.cast
                |> Seq.collect (fun ((find:string), replace) -> 
                    if molecule.IndexOf(find) < 0 then 
                        if molecule = "e" then 
                            Thread.Sleep(250)
                            min <- if steps < min then
                                       Console.WriteLine(steps)
                                       steps 
                                   else min
                            seq { yield steps }
                        else Seq.empty
                    else
                        0
                        |> Seq.unfold (fun i ->
                            let index = molecule.IndexOf(find, i)
                            if index < 0 then None
                            else Some(index, index + 1))
                        |> Seq.map (fun i ->
                            let removed = molecule.Remove(i, find.Length)
                            removed.Insert(i, replace))
                        |> Seq.collect (fun molecule -> findSteps molecule (steps + 1)))
            if map |> Map.containsKey molecule then
                if map |> Map.find molecule <= steps then Seq.empty
                else
                    map <- map |> Map.remove molecule
                    map <- map |> Map.add molecule steps
                    Console.WriteLine(molecule)
                    doIt()
            else
                map <- map |> Map.add molecule steps
                if steps > min then Seq.empty
                else
                    Console.WriteLine(molecule)
                    doIt()

        let result2 = (findSteps medMolecule 0) |> Seq.min
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
        
module Day20 =

    open System

    type Integer = int
    
    let getDividers (number:int) =
        seq { 1 .. Math.Sqrt(double number) |> int } 
        |> Seq.filter (fun i -> number % i = 0)
        |> Seq.collect (fun i -> if number / i = i then seq { yield i } else seq { yield i; yield number / i})
                
    let getDividers2 (number:int) =
        getDividers number
        |> Seq.filter (fun i -> i * 50 >= number)
            
    let getPresentsCount number =
        getDividers number |> Seq.map (fun i -> i * 10) |> Seq.sum
                    
    let getPresentsCount2 number =
        getDividers2 number |> Seq.map (fun i -> i * 11) |> Seq.sum

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.20.txt"

        let numberOfPresents = Integer.Parse input

        let initialHouseNumber = 1

        let (houseNumber, _) =
            (initialHouseNumber, getPresentsCount initialHouseNumber)
            |> Seq.unfold (fun (houseNumber, presents) ->
                if presents >= numberOfPresents then None
                else
                    let houseNumber = houseNumber + 1
                    let presents = getPresentsCount houseNumber
                    Some((houseNumber, presents), (houseNumber, presents)))
            |> Seq.last

        let result1 = houseNumber
        
        let (houseNumber, _) =
            (initialHouseNumber, getPresentsCount2 initialHouseNumber)
            |> Seq.unfold (fun (houseNumber, presents) ->
                if presents >= numberOfPresents then None
                else
                    let houseNumber = houseNumber + 1
                    let presents = getPresentsCount2 houseNumber
                    Some((houseNumber, presents), (houseNumber, presents)))
            |> Seq.last

        let result2 = houseNumber
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
        
module Day21 =
    
    open System

    type Item = { Cost:int; Damage:int; Armor:int }
    
    let Weapons = [| 
                     { Cost = 8; Damage = 4; Armor = 0 }
                     { Cost = 10; Damage = 5; Armor = 0 }
                     { Cost = 25; Damage = 6; Armor = 0 }
                     { Cost = 40; Damage = 7; Armor = 0 }
                     { Cost = 74; Damage = 8; Armor = 0 }
                  |]

    let Armor = [| 
                   { Cost = 13; Damage = 0; Armor = 1 }
                   { Cost = 31; Damage = 0; Armor = 2 }
                   { Cost = 53; Damage = 0; Armor = 3 }
                   { Cost = 75; Damage = 0; Armor = 4 }
                   { Cost = 102; Damage = 0; Armor = 5 }
                |]
    
    let Rings = [| 
                   { Cost = 25; Damage = 1; Armor = 0 }
                   { Cost = 50; Damage = 2; Armor = 0 }
                   { Cost = 100; Damage = 3; Armor = 0 }
                   { Cost = 20; Damage = 0; Armor = 1 }
                   { Cost = 40; Damage = 0; Armor = 2 }
                   { Cost = 80; Damage = 0; Armor = 3 }
                |]

    type Player = { HP:int; Damage:int; Armor:int }

    let generatePlayerWithItems items hp =
        ({HP=hp; Damage=0; Armor=0 },items) ||> Seq.fold (fun currentPlayer (item:Item) -> { currentPlayer with Damage = currentPlayer.Damage + item.Damage; Armor = currentPlayer.Armor + item.Armor })

    let fight (player:Player) (boss:Player) =
        (player, boss) 
        |> Seq.unfold (fun (player, boss) ->
            let newHP p1 p2 = p1.HP - Math.Max(1, p2.Damage - p1.Armor)
            if player.HP <= 0 || boss.HP <= 0 then None
            else
                let boss = { boss with HP = newHP boss player}
                let player =
                    if boss.HP > 0 then { player with HP = newHP player boss }
                    else player
                Some(boss.HP <= 0 && player.HP > 0, (player, boss)))
        |> Seq.last

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.21.txt"

        let boss =
            match input with
            | Regex "Hit Points: (\d+)\r\nDamage: (\d+)\r\nArmor: (\d+)" (hp::damage::armor::_) -> { HP = Integer.Parse hp; Damage = Integer.Parse damage; Armor = Integer.Parse armor }
            | _ -> { HP = 0; Damage = 0; Armor = 0 }

        let run fightFunc resultFunc =
            Weapons 
            |> Seq.collect (fun weapon ->
                seq { 
                        yield [ weapon ]; 
                        yield! Armor |> Seq.map (fun armor -> weapon::armor::[])
                    })
            |> Seq.collect (fun items -> 
                seq {
                        yield items;
                        yield! Rings |> Seq.map (fun ring -> ring::items);
                        yield! Rings |> Seq.pairwise |> Seq.where (fun (ring1, ring2) -> ring1 <> ring2) |> Seq.map (fun (ring1, ring2) -> ring1::ring2::items)
                    })
            |> Seq.map (fun items -> (generatePlayerWithItems items 100, items))
            |> Seq.filter (fun (player, _) -> fightFunc player boss)
            |> Seq.map (fun (_, items) -> items |> List.sumBy (fun item -> item.Cost))
            |> resultFunc

        let result1 = run (fight) (Seq.min)

        let result2 = run (fun player boss -> fight player boss |> not) (Seq.max)
        
        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day22 =

    open System

    type Player = { HP:int; Mana:int; Armor:int }
        
    type Boss = { HP:int; Damage:int; }

    type Spell =
        | MagicMissile
        | Drain
        | Shield
        | Poison
        | Recharge

    type BattleResult =
        | Won
        | Lost

    let Spells = [ MagicMissile; Drain; Shield; Poison; Recharge ]

    let cost spell =
        match spell with
        | MagicMissile -> 53
        | Drain -> 73
        | Shield -> 113
        | Poison -> 173
        | Recharge -> 229

    let isInstant spell =
        match spell with
        | MagicMissile | Drain -> true
        | Shield | Poison | Recharge -> false

    let isEffect spell =
        match spell with
        | MagicMissile | Drain -> false
        | Shield | Poison | Recharge -> true

    let applySpellOnPlayer spell player =
        match spell with
        | MagicMissile | Poison -> player
        | Shield -> { player with Armor = player.Armor + 7 }
        | Drain -> { player with HP = Math.Min(50, player.HP + 2) }
        | Recharge -> { player with Mana = player.Mana + 101 }

    let applySpellOnBoss spell (boss:Boss) =
        match spell with
        | Shield | Recharge -> boss
        | MagicMissile -> { boss with HP = boss.HP - 4}
        | Drain -> { boss with HP = boss.HP - 2}
        | Poison -> { boss with HP = boss.HP - 3}

    let takeMana spell player =
        { player with Mana = player.Mana - cost spell }

        
    let effectTurns spell =
        match spell with
        | MagicMissile | Drain -> 0
        | Shield -> 6
        | Poison -> 6 
        | Recharge -> 5

    let startEffect effects spell =
        if isEffect spell then effects |> Set.add (spell, effectTurns spell)
        else effects
    
    let updateEffects effects filter =
        effects 
        |> Seq.filter filter
        |> Operations.asFirst effects
        ||> Seq.fold (fun effects (spell, duration) ->
            if duration = 1 then
                effects |> Set.remove (spell, duration)
            else
                effects |> Set.remove (spell, duration) |> Set.add (spell, duration - 1))

    let updatePlayerEffects effects =
        updateEffects effects (fun (spell, _) -> match spell with |Shield|Recharge -> true |MagicMissile|Drain|Poison -> false)
    
    let updateBossEffects effects =
        updateEffects effects (fun (spell, _) -> match spell with |Poison -> true |Shield|Recharge|MagicMissile|Drain|Poison -> false)

    let castableSpells player effects =
        Spells 
        |> Seq.filter (fun spell -> player.Mana >= cost spell) 
        |> Seq.filter (fun spell -> effects |> Seq.exists (fun (spellOfEffect, duration) -> spell = spellOfEffect && duration > 1) |> not)

    let doEffectsStuff player boss effects = 
        let player = ({player with Armor = 0}, effects) ||> Seq.fold(fun player spell ->  applySpellOnPlayer (spell |> fst) player) 
        let effects = updatePlayerEffects effects
        let boss = (boss, effects) ||> Seq.fold(fun boss spell ->  applySpellOnBoss (spell |> fst) boss) 
        let effects = updateBossEffects effects
        player, boss, effects

    let rec round player boss effects manaUsed difficultyMode =
        let availableSpells = castableSpells player effects
        if availableSpells |> Seq.isEmpty then Lost, manaUsed
        else
            // player turn
            let player = difficultyMode player
            let (player, boss, effects) = doEffectsStuff player boss effects
            if boss.HP <= 0 then 
                Won, manaUsed
            else 
                let bestOutcome =
                    availableSpells
                    |> Seq.sortBy (fun spell -> cost spell)
                    |> Seq.map (fun spell ->
                        let player = player |> takeMana spell
                        let effects = if spell |> isEffect then spell |> startEffect effects else effects
                        let manaUsed = manaUsed + (spell |> cost)
                        let player = if spell |> isInstant then applySpellOnPlayer spell player else player
                        let boss = if spell |> isInstant then applySpellOnBoss spell boss else boss

                        // boss turn
                        let (player, boss, effects) = doEffectsStuff player boss effects
                        if boss.HP <= 0 then 
                            Won, manaUsed
                        else 
                            let player = { player with HP = player.HP - (boss.Damage - player.Armor)}
                            if player.HP <= 0 then Lost, manaUsed
                            else round player boss effects manaUsed difficultyMode)
                    |> Seq.filter (fun (result, _) -> result = Won)
                    |> Seq.sortBy snd
                    |> Seq.tryHead
                bestOutcome |> Option.defaultValue (Lost, manaUsed)

    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2015.22.txt"

        let boss =
            match input with
            | Regex "Hit Points: (\d+)\r\nDamage: (\d+)" (hp::damage::_) -> { HP = Integer.Parse hp; Damage = Integer.Parse damage; }
            | _ -> { HP = 0; Damage = 0; }

        let player = { HP = 50; Mana = 500; Armor = 0}
            
        let (_, result1) = round player boss Set.empty 0 identity

        let (_, result2) = round player boss Set.empty 0 (fun player -> { player with HP = player.HP - 1 })

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }
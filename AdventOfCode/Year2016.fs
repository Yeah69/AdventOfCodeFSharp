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
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.02.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day3 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.03.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day4 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.04.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day5 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.05.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day6 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.06.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day7 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2016.07.txt"

        let result1 = 0

        let result2 = 0

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

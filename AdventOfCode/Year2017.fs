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
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.04.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day5 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.05.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day6 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.06.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day7 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.07.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day8 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.08.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day9 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.09.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day10 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.10.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day11 =
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.11.txt"

        let result1 = 0

        let result2 = 0

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

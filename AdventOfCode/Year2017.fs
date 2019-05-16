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
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.02.txt"

        let result1 = 0

        let result2 = 0

        { First = sprintf "%d" result1; Second = sprintf "%d" result2 }

module Day3 =    
    let go() =
        let input = inputFromResource "AdventOfCode.Inputs._2017.03.txt"

        let result1 = 0

        let result2 = 0

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

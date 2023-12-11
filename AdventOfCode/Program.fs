﻿// Learn more about F# at http://fsharp.org

open System
open Domain

[<AutoOpen>]
module RiddleChoices =
    let yearChoices = seq {
        while true do
            printfn "Please type the year (2015 - 2018):"
            let line = Console.ReadLine()
            printfn ""
            yield line }
    let dayChoices = seq {
        while true do
            printfn "Please type the day (1 - 25):"
            let line = Console.ReadLine()
            printfn ""
            yield line }

[<EntryPoint>]
let main _ =
    printfn "Welcome to Yeah's Advent of Code solutions!"
    printfn ""

    let trackDayPerformance dayFunction =
        let start = DateTime.Now
        let result = dayFunction()
        let finish = DateTime.Now
        
        printfn "1. Solution:"
        printfn "%s" result.First
        printfn ""
        printfn "2. Solution:"
        printfn "%s" result.Second
        printfn ""
        printfn "Time elapsed:"
        printfn "%A" (finish - start)
        printfn ""

    let year =
        yearChoices
        |> Seq.choose Years.tryParseYear
        |> Seq.head
    
    let day =
        dayChoices
        |> Seq.choose Days.tryParseDay
        |> Seq.head

    match (year, day) with
    | Year 2015, Day 1 -> trackDayPerformance(Year2015.Day1.go)
    | Year 2015, Day 2 -> trackDayPerformance(Year2015.Day2.go)
    | Year 2015, Day 3 -> trackDayPerformance(Year2015.Day3.go)
    | Year 2015, Day 4 -> trackDayPerformance(Year2015.Day4.go)
    | Year 2015, Day 5 -> trackDayPerformance(Year2015.Day5.go)
    | Year 2015, Day 6 -> trackDayPerformance(Year2015.Day6.go)
    | Year 2015, Day 7 -> trackDayPerformance(Year2015.Day7.go)
    | Year 2015, Day 8 -> trackDayPerformance(Year2015.Day8.go)
    | Year 2015, Day 9 -> trackDayPerformance(Year2015.Day9.go)
    | Year 2015, Day 10 -> trackDayPerformance(Year2015.Day10.go)
    | Year 2015, Day 11 -> trackDayPerformance(Year2015.Day11.go)
    | Year 2015, Day 12 -> trackDayPerformance(Year2015.Day12.go)
    | Year 2015, Day 13 -> trackDayPerformance(Year2015.Day13.go)
    | Year 2015, Day 14 -> trackDayPerformance(Year2015.Day14.go)
    | Year 2015, Day 15 -> trackDayPerformance(Year2015.Day15.go)
    | Year 2015, Day 16 -> trackDayPerformance(Year2015.Day16.go)
    | Year 2015, Day 17 -> trackDayPerformance(Year2015.Day17.go)
    | Year 2015, Day 18 -> trackDayPerformance(Year2015.Day18.go)
    | Year 2015, Day 19 -> trackDayPerformance(Year2015.Day19.go)
    | Year 2015, Day 20 -> trackDayPerformance(Year2015.Day20.go)
    | Year 2015, Day 21 -> trackDayPerformance(Year2015.Day21.go)
    | Year 2015, Day 22 -> trackDayPerformance(Year2015.Day22.go)
    | Year 2015, Day 23 -> trackDayPerformance(Year2015.Day23.go)
    | Year 2015, Day 24 -> trackDayPerformance(Year2015.Day24.go)
    | Year 2015, Day 25 -> trackDayPerformance(Year2015.Day25.go)
    
    | Year 2016, Day 1 -> trackDayPerformance(Year2016.Day1.go)
    | Year 2016, Day 2 -> trackDayPerformance(Year2016.Day2.go)
    | Year 2016, Day 3 -> trackDayPerformance(Year2016.Day3.go)
    | Year 2016, Day 4 -> trackDayPerformance(Year2016.Day4.go)
    | Year 2016, Day 5 -> trackDayPerformance(Year2016.Day5.go)
    | Year 2016, Day 6 -> trackDayPerformance(Year2016.Day6.go)
    | Year 2016, Day 7 -> trackDayPerformance(Year2016.Day7.go)
    | Year 2016, Day 8 -> trackDayPerformance(Year2016.Day8.go)
    | Year 2016, Day 9 -> trackDayPerformance(Year2016.Day9.go)
    | Year 2016, Day 10 -> trackDayPerformance(Year2016.Day10.go)
    | Year 2016, Day 11 -> trackDayPerformance(Year2016.Day11.go)
    | Year 2016, Day 12 -> trackDayPerformance(Year2016.Day12.go)
    | Year 2016, Day 13 -> trackDayPerformance(Year2016.Day13.go)
    | Year 2016, Day 14 -> trackDayPerformance(Year2016.Day14.go)
    | Year 2016, Day 15 -> trackDayPerformance(Year2016.Day15.go)
    | Year 2016, Day 16 -> trackDayPerformance(Year2016.Day16.go)
    | Year 2016, Day 17 -> trackDayPerformance(Year2016.Day17.go)
    | Year 2016, Day 18 -> trackDayPerformance(Year2016.Day18.go)
    | Year 2016, Day 19 -> trackDayPerformance(Year2016.Day19.go)
    | Year 2016, Day 20 -> trackDayPerformance(Year2016.Day20.go)
    | Year 2016, Day 21 -> trackDayPerformance(Year2016.Day21.go)
    | Year 2016, Day 22 -> trackDayPerformance(Year2016.Day22.go)
    | Year 2016, Day 23 -> trackDayPerformance(Year2016.Day23.go)
    | Year 2016, Day 24 -> trackDayPerformance(Year2016.Day24.go)
    | Year 2016, Day 25 -> trackDayPerformance(Year2016.Day25.go)
    
    | Year 2017, Day 1 -> trackDayPerformance(Year2017.Day1.go)
    | Year 2017, Day 2 -> trackDayPerformance(Year2017.Day2.go)
    | Year 2017, Day 3 -> trackDayPerformance(Year2017.Day3.go)
    | Year 2017, Day 4 -> trackDayPerformance(Year2017.Day4.go)
    | Year 2017, Day 5 -> trackDayPerformance(Year2017.Day5.go)
    | Year 2017, Day 6 -> trackDayPerformance(Year2017.Day6.go)
    | Year 2017, Day 7 -> trackDayPerformance(Year2017.Day7.go)
    | Year 2017, Day 8 -> trackDayPerformance(Year2017.Day8.go)
    | Year 2017, Day 9 -> trackDayPerformance(Year2017.Day9.go)
    | Year 2017, Day 10 -> trackDayPerformance(Year2017.Day10.go)
    | Year 2017, Day 11 -> trackDayPerformance(Year2017.Day11.go)
    | Year 2017, Day 12 -> trackDayPerformance(Year2017.Day12.go)
    | Year 2017, Day 13 -> trackDayPerformance(Year2017.Day13.go)
    | Year 2017, Day 14 -> trackDayPerformance(Year2017.Day14.go)
    | Year 2017, Day 15 -> trackDayPerformance(Year2017.Day15.go)
    | Year 2017, Day 16 -> trackDayPerformance(Year2017.Day16.go)
    | Year 2017, Day 17 -> trackDayPerformance(Year2017.Day17.go)
    | Year 2017, Day 18 -> trackDayPerformance(Year2017.Day18.go)
    | Year 2017, Day 19 -> trackDayPerformance(Year2017.Day19.go)
    | Year 2017, Day 20 -> trackDayPerformance(Year2017.Day20.go)
    | Year 2017, Day 21 -> trackDayPerformance(Year2017.Day21.go)
    | Year 2017, Day 22 -> trackDayPerformance(Year2017.Day22.go)
    | Year 2017, Day 23 -> trackDayPerformance(Year2017.Day23.go)
    | Year 2017, Day 24 -> trackDayPerformance(Year2017.Day24.go)
    | Year 2017, Day 25 -> trackDayPerformance(Year2017.Day25.go)

    | Year 2018, Day 1 -> trackDayPerformance(Year2018.Day1.go)
    | Year 2018, Day 2 -> trackDayPerformance(Year2018.Day2.go)
    | Year 2018, Day 3 -> trackDayPerformance(Year2018.Day3.go)
    | Year 2018, Day 4 -> trackDayPerformance(Year2018.Day4.go)
    | Year 2018, Day 5 -> trackDayPerformance(Year2018.Day5.go)
    | Year 2018, Day 6 -> trackDayPerformance(Year2018.Day6.go)
    | Year 2018, Day 7 -> trackDayPerformance(Year2018.Day7.go)
    | Year 2018, Day 8 -> trackDayPerformance(Year2018.Day8.go)
    | Year 2018, Day 9 -> trackDayPerformance(Year2018.Day9.go)
    | Year 2018, Day 10 -> trackDayPerformance(Year2018.Day10.go)
    | Year 2018, Day 11 -> trackDayPerformance(Year2018.Day11.go)
    | Year 2018, Day 12 -> trackDayPerformance(Year2018.Day12.go)
    | Year 2018, Day 13 -> trackDayPerformance(Year2018.Day13.go)
    | Year 2018, Day 14 -> trackDayPerformance(Year2018.Day14.go)
    | Year 2018, Day 15 -> trackDayPerformance(Year2018.Day15.go)
    | Year 2018, Day 16 -> trackDayPerformance(Year2018.Day16.go)
    | Year 2018, Day 17 -> trackDayPerformance(Year2018.Day17.go)
    | Year 2018, Day 18 -> trackDayPerformance(Year2018.Day18.go)
    | Year 2018, Day 19 -> trackDayPerformance(Year2018.Day19.go)
    | Year 2018, Day 20 -> trackDayPerformance(Year2018.Day20.go)
    | Year 2018, Day 21 -> trackDayPerformance(Year2018.Day21.go)
    | Year 2018, Day 22 -> trackDayPerformance(Year2018.Day22.go)
    | Year 2018, Day 23 -> trackDayPerformance(Year2018.Day23.go)
    | Year 2018, Day 24 -> trackDayPerformance(Year2018.Day24.go)
    | Year 2018, Day 25 -> trackDayPerformance(Year2018.Day25.go)

    | _ -> "" |> ignore
    
    
    Console.ReadLine() |> ignore
    0 // return an integer exit code

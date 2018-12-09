// Learn more about F# at http://fsharp.org

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

open System;

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
    | Year 2018, Day 1 -> trackDayPerformance(Year2018.Day1.go)
    | Year 2018, Day 2 -> trackDayPerformance(Year2018.Day2.go)
    | Year 2018, Day 3 -> trackDayPerformance(Year2018.Day3.go)
    | Year 2018, Day 4 -> trackDayPerformance(Year2018.Day4.go)
    | Year 2018, Day 5 -> trackDayPerformance(Year2018.Day5.go)
    | Year 2018, Day 6 -> trackDayPerformance(Year2018.Day6.go)
    | Year 2018, Day 7 -> trackDayPerformance(Year2018.Day7.go)
    | Year 2018, Day 8 -> trackDayPerformance(Year2018.Day8.go)
    | Year 2018, Day 9 -> trackDayPerformance(Year2018.Day9.go)
    | _ -> "" |> ignore
    
    
    Console.ReadLine() |> ignore
    0 // return an integer exit code

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

[<EntryPoint>]
let main _ =
    printfn "Welcome to Yeah's Advent of Code solutions!"
    printfn ""

    let year =
        yearChoices
        |> Seq.choose Years.tryParseYear
        |> Seq.head
    
    let day =
        dayChoices
        |> Seq.choose Days.tryParseDay
        |> Seq.head

    let solution = match (year, day) with
                   | Year 2015, Day 1 -> Some(Year2015.Day1.go())
                   | Year 2015, Day 2 -> Some(Year2015.Day2.go())
                   | _ -> None
    
    match solution with
    | Some solution ->
        printfn "1. Solution:"
        printfn "%s" solution.First
        printfn ""
        printfn "2. Solution:"
        printfn "%s" solution.Second
        printfn ""
    | None -> 
        printfn "No solution provided yet. Go implement ist"
        printfn ""
    Console.ReadLine() |> ignore
    0 // return an integer exit code

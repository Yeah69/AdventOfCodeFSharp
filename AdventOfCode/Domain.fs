module Domain

open System

type Year = Year of int

type Day = Day of int

type Solution =
    { First : string
      Second : string }

module Years =
    let tryParseYear (s: string) = 
        match Int32.TryParse(s) with
        | (true, year) when year >= 2015 && year <= 2018 -> Some(Year year)
        | _ -> None
module Days =
    let tryParseDay (s: string) = 
        match Int32.TryParse(s) with
        | (true, day) when day >= 1 && day <= 25 -> Some(Day day)
        | _ -> None
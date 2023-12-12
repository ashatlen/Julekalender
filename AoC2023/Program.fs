namespace AoC2023

module Hoved =
    open System
    open AoC2023.Luke11

    [<EntryPoint>]
    let main argv = 
        puzzle
        Console.Read() |> ignore
        0 // return an integer exit code

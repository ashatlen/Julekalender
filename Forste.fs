namespace Julekalender

module Forste=

    open System
    open System.Numerics
    open System.IO

    let rotateRight = fun (value:int) ->
        let sv= sprintf "%d" value
        let (rest, last)= (sv.[0..(sv.Length-2)], sv.[(sv.Length - 1)..(sv.Length-1)])
        last + rest 
        |> System.Int32.Parse
 
    let rec searchSolution = fun (value) ->
        if value < 1000000 then
            let rValue= rotateRight value
            if value * 4 = rValue then
                Some (value, rValue)
            else
                searchSolution (value + 10)
        else
            None

    let FørsteDesember= fun () ->
        let solText= 
            match (searchSolution 6) with
            | Some (v, r) -> sprintf "%d er en løsning (%d)!" v r
            | None -> sprintf "Måtte gi opp å finne en løsning"

        printfn "1 desember: %s" solText

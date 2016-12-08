module Test

open System
open System.Numerics
open System.IO

let rec readAllValues= fun (xs:int list) -> 
    Console.ReadLine() 
    |> System.Int32.TryParse
    |> (fun (isInt, tall) -> 
            if not isInt then
                    xs 
                else
                    xs |> List.append [tall] |> readAllValues
       )
let getAndParseValues= fun () ->
    printfn "Skriv inn tallene, avslutt alle tall med ny linje. Når du er ferdig, trykk enter"
    readAllValues []
    |> (fun vs -> 
            let x1= vs |> List.fold (fun s v -> s + v) 0
            let x2= vs |> List.fold (fun s v -> s * v) 1
            (x1, x2)
        )
    |> fun (x1,x2) -> printfn "Summen er %A, produktet er %A" x1 x2
    
    printfn "Trykk en tast for å avslutte"
    Console.ReadKey() |> ignore

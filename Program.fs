namespace Julekalender

module Hoved=
    open System
    open System.IO

    type Ladder = {
        fromCell:int
        toCell:int
        }


    [<EntryPoint>]
    let main argv = 
        //FørsteDesember()
        //andreDesember()
        //TredjeDesember() // Feilet, Lizette var ikke korrekt svar... (Nicolle var korrekt - vet ikke hvorfor...) Oppgavetolkning...?
        //Fjerde.FjerdeDesemberStateless()
        // Femte.ParseRomerskeTall "KingOfIndonesia.txt" 
        // Sjette.springerFlytting()
        // Sjuende.summereAlvesteg "Alvesteg.txt"
                            
        Åttende.snakeAndLadder "DieThrows.txt"

        Console.ReadKey() |> ignore
        0 // return an integer exit code

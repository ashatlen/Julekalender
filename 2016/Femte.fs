namespace Julekalender
module Femte=

    open System
    open System.Numerics
    open System.IO

    let parseNumber (roman:string) =
//        if roman.Length = 0 
//        then 0
//        else
//            let number= 
        match roman with
                        | "I" -> 1
                        | "II" -> 2
                        | "III" -> 3
                        | "IV" -> 4
                        | "V" -> 5
                        | "VI" -> 6
                        | "VII" -> 7
                        | "VIII" -> 8
                        | "IX" -> 9
                        | "X" -> 10
                        | "XI" -> 11
                        | "XII" -> 12
                        | "XIII" -> 13
                        | "XIV" -> 14
                        | "XV" -> 15
                        | "XVI" -> 16
                        | "XVII" -> 17
                        | "XVIII" -> 18
                        | "XIX" -> 19
                        | "XX" -> 20
                        | x   -> failwith(sprintf "Don't know this Roman number letter %s" x)

//            ((if prevLetter = 0 || next <= prevLetter then sum + next else next - sum))

    let ParseRomerskeTall filename= 
//        let streamReader= File.OpenText(filename)
//        let values= 
//            (
//            streamReader.ReadToEnd()
//                        |> string.Replace "[" ' '
//                        |> (fun line -> line.Split ',')
//                        |> List.iter (fun token -> (parseNumber token.Trim()))
//            )
//        printfn "Roman nummer %s equals %d" roman num 
        printfn "5. Roman nummer - skal settes - TBD"

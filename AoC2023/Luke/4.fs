
namespace AoC2023

module Luke4 =
    open System
    open System.IO
    open System.Text.RegularExpressions

// Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53

    let rec chop (input : string) = 
        if input.Length < 3 then 
            []
        else
            (chop input[3..]) |> List.append [input[0..2]]

    let parseCard (card:string) =
        let parts = card.Split '|'
        let noWin = parts[0].Split ':'
        let noS= noWin[0]
        let no = Int32.Parse(noS[4..].Trim())
        let wins = 
            (chop noWin[1])
            |> List.map (fun no -> 
                Int32.Parse(no.Trim())
                )
    
        let nos = 
            (chop parts[1])
            |> List.map (fun no -> 
                    Int32.Parse(no.Trim())
                    )
        (no, wins, nos)

    let calculateScore (wins : int list) (lots : int list) =
        let matches =
            wins
            |> List.filter (fun w -> 
                let result = List.tryFind (fun e -> w = e) lots 
                result.IsSome 
                )
        let points = 
            matches 
            |> List.fold (fun state _ -> state * 2) 1
        (points / 2, matches.Length)

    // let rec duplicate idx (lots:(int*int*int) list)  =
    //     if idx = 5400 then lots
    //     else
    //         if idx >= lots.Length then
    //             []
    //         else
    //             let (no, _, matches) = lots[idx]

    //             //let dupTo = if no + matches < lots.Length then no + match

    //             let cardsToAdd =
    //                 if matches > 0 then lots[no..no+matches-1]
    //                 else [] 

    //             let lots' = List.append lots cardsToAdd

    //             printf "Idx %i Checking card %i, adding %i cards. Current cnt %i\n" idx no cardsToAdd.Length lots'.Length

    //             let n = (duplicate (idx + 1) lots') 
    //             List.append [lots[0]] n

    let rec duplicate (lots:int list) (inspect: int List)  =
        printf "Checking %i cards\n" inspect.Length 
        let toInspect =
            inspect
            |> List.map (fun idx -> 
                [for i in 1..lots[idx] -> idx+i]
                )
            |> List.concat

        inspect.Length + if toInspect = [] then 0 else (duplicate lots toInspect)

    let _1 puzzle =
        let result =
            puzzle
            |> List.map (fun card ->
                let (_, wins, lots) = parseCard card
                let (p, ms) = calculateScore wins lots
                p
            )
            |> List.sum

        printf "4.desember 1 %i" result

    let _2 puzzle =
        let result =
            puzzle
            |> List.map (fun card ->
                let (no, wins, lots) = parseCard card
                let (score, ms) = calculateScore wins lots // -> (card * matches)
//                printf "card %i %i %A\n" no ms lots
                ms
            )

        let inspect = [for i in 0..result.Length-1-> i] 
        let cards = (duplicate result inspect)

        printf "4.desember 2 %i" cards

    let puzzle =
        let puzzleInput= File.ReadAllLines(@"dec4.txt") |> List.ofArray 
        _2 puzzleInput

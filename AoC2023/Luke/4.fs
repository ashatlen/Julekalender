
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

        (wins, nos)

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
        points / 2

    let _1 puzzle =
        let result =
            puzzle
            |> List.map (fun card ->
                let (wins, lots) = parseCard card
                calculateScore wins lots
            )
            |> List.sum

        printf "4.desember 1 %i" result


    let _2 puzzle =
        printf "4.desember 2 %i" -1


    let puzzle =
        let puzzleInput= File.ReadAllLines(@"dec4.txt") |> List.ofArray 
        _1 puzzleInput

namespace Aoc2022

module Hoved =
    open System
    open System.IO


    let forstedesember= fun (items: string list) ->
        let rec chunksum = fun (s : int) (orig :string list) -> 
            
            if orig = [] then
                let r1= [s]
                r1
            elif orig.Head = "" then
                let r = chunksum 0 orig.Tail
                s :: r
            else
                let elt = Int32.Parse orig.Head
                chunksum (s+elt) orig.Tail 
                          
        chunksum 0 items 
        |> List.sortDescending 

    let forstedesember1= fun (puzzle: string list) ->
        forstedesember puzzle
        |> List.head

    let forstedesember2= fun (puzzle: string list) ->
        forstedesember puzzle 
        |> List.take 3 
        |> List.sum

    let game1 = fun opp resp ->
        let oppValue= 
            match opp with
            | "A" -> 1
            | "B" -> 2
            | "C" -> 3
            | x -> failwithf "value not handled %s" x

        let respValue=
            match resp with
            | "X" -> 1
            | "Y" -> 2
            | "Z" -> 3
            | x -> failwithf "value not handled %s" x
    
        match (oppValue, respValue) with
        // Paper
        | (1,1) -> 3 + 1
        | (1,2) -> 6 + 2
        | (1,3) -> 3
        // Rock
        | (2,1) -> 1
        | (2,2) -> 3 + 2
        | (2,3) -> 6 + 3
        // Scissors
        | (3,1) -> 6 + 1
        | (3,2) -> 2
        | (3,3) -> 3 + 3
        | _ -> 0

    let game2 = fun opp resp ->
        let oppValue= 
            match opp with
            | "A" -> 1
            | "B" -> 2
            | "C" -> 3
            | x -> failwithf "value not handled %s" x
    
        // A = ROck, B = Paper, C = scissors
        // X = loose, Y = draw, Z = win
        match (oppValue, resp) with
        // Rock
        | (1,"X") -> 0 + 3 // Loose with Scissors
        | (1,"Y") -> 3 + 1 
        | (1,"Z") -> 6 + 2 // Win with Paper
        // Paper
        | (2,"X") -> 0 + 1 // Loose with Rock
        | (2,"Y") -> 3 + 2 
        | (2,"Z") -> 6 + 3 // Win with Scissors
        // Scissors
        | (3,"X") -> 0 + 2 // Loose with Paper
        | (3,"Y") -> 3 + 3 
        | (3,"Z") -> 6 + 1 // Win with Rock
        | _ -> 0


    let andredesember = fun (puzzle: string list) game ->        
        puzzle 
        |> List.sumBy (fun g -> 
            let items = g.Split ' ' 
            let opp = items |> Array.head
            let resp = items |> Array.skip 1 |> Array.head
            game opp resp
            )

    [<EntryPoint>]
    let main argv = 
                           
        // let puzzle= File.ReadAllLines(@"dec1.txt") |> List.ofSeq
        // printf "1.desember 1 top elf %i\n" (forstedesember1 puzzle)
        // printf "1.desember 2 top 3 elfs %i\n" (forstedesember2 puzzle)

        let puzzle= File.ReadAllLines(@"dec2.txt") |> List.ofSeq
        printf "2.desember 1 Rock Paper Scissors %i\n" (andredesember puzzle game1)
        printf "2.desember 1 Rock Paper Scissors %i\n" (andredesember puzzle game2)

        Console.Read() |> ignore
        0 // return an integer exit code



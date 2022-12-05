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

    let andredesember = fun (puzzle: string list) (game: string -> string -> int) ->        
        puzzle 
        |> List.sumBy (fun g -> 

            let items = g.Split ' ' 
            let opp = items |> Array.head
            let resp = items |> Array.skip 1 |> Array.head

            game opp resp
            )


    let splitEqual = fun (s: string) ->
        let s1 = s.Substring (0, s.Length / 2)
        let s2 = s.Substring (s.Length / 2)
        (s1,s2) 

    let rec IsInString = fun (s:string) (cs:char list) ->

        if cs = List.empty then
            None
        else
            let c::r = cs
            if s.Contains(c) then
                Some c
            else
                IsInString s r

    let rec IsInStrings = fun (s1:string) (s2:string) (cs:char list) ->

        if cs = List.empty then
            None
        else
            let c::r = cs
            if s1.Contains(c) && s2.Contains(c) then
                Some c
            else
                IsInStrings s1 s2 r


    let mapToPriority= fun x ->
        match x with
        | (Some c) when c > 'Z' -> (c |> int) - ('a' |> int) + 1
        | (Some c) -> (c |> int) - ('A' |> int)  + 27

    let tredjedesember = fun (puzzle: string list) ->

        let findSingle = fun (s1: string, s2: string) ->
                
            s1 
            |> Seq.toList
            |> IsInString s2
            |> mapToPriority

        puzzle
        |> List.map (fun rucksack -> 
            rucksack 
            |> splitEqual 
            |> findSingle 
            )
        |> List.sum

    let tredjedesember2 = fun (puzzle: string list) ->

        let findCommon = fun (s1: string) (s2: string) (s3: string) ->                
            s1 
            |> Seq.toList
            |> IsInStrings s2 s3

        let parts = puzzle.Length / 3

        puzzle
        |> List.splitInto parts            
        |> List.map  (fun g -> 
            let a= g |> List.toArray
            findCommon a[0] a[1] a[2]
            |> mapToPriority
            )
        |> List.sum


    let parseSectionPair = fun (secs: string) ->  
        
        let vals= 
            secs.Split ',' 
            |> Array.map (fun ssl -> 
                ssl.Split '-'
                |> Array.map (fun sr -> sr |> int) 
            )
        ((vals[0][0], vals[0][1]), (vals[1][0], vals[1][1]))

    let isWithin = fun (from1:int, to1:int) (from2:int, to2:int) ->
        from1 >= from2 
        && from1 <= to2 
        && to1 >= from2 
        && to1 <= to2

    let isOverlap = fun (from1:int, to1:int) (from2:int, to2:int) ->
           from1 >= from2 && from1 <= to2 
        || to1 >= from2 && to1 <= to2

    let fjerdedesember = fun puzzle overlap->

        puzzle 
        |> List.map parseSectionPair
        |> List.map (fun (p1, p2) -> 
            if overlap p1 p2 || overlap p2 p1 then
                1
            else
                0
            )
        |> List.sum


    let initialStackInput = 
        ["        [F] [Q]         [Q]        " ]
        ["[B]     [Q] [V] [D]     [S]        " ]
        ["[S] [P] [T] [R] [M]     [D]        " ]
        ["[J] [V] [W] [M] [F]     [J]     [J]" ]
        ["[Z] [G] [S] [W] [N] [D] [R]     [T]" ]
        ["[V] [M] [B] [G] [S] [C] [T] [V] [S]" ]
        ["[D] [S] [L] [J] [L] [G] [G] [F] [R]" ]
        ["[G] [Z] [C] [H] [C] [R] [H] [P] [D]" ] 
       // 1   2   3   4   5   6   7   8   9 

    let parseStack = fun (initialStack: string list) ->
 
        let stacks= [
            for i = 0 to initialStack.Length do
                let row= [ 
                    for j = 0 to initialStack.Head.Length do
                        let from = j * 4 + 1
                        initialStack.[i].[from]                        
                        ]
                row
                ]
        stacks

    let parseInstruction= fun (instructionString: string) ->
        //Eg. move 14 from 3 to 9
        let numbersOnly= 
            instructionString
                .Replace("move", "")
                .Replace(" from", "")
                .Replace(" to", "") 
                .Split ' ' 
        numbersOnly |> Array.map (fun elt -> elt |> int)

    let femtedesember = fun puzzle ->
        let instructions = puzzle |> parseInstruction
        let stack = 
            parseStack initialStackInput
            |>

    [<EntryPoint>]
    let main argv = 
                           
        // let puzzle= File.ReadAllLines(@"dec1.txt") |> List.ofSeq
        // printf "1.desember 1 top elf %i\n" (forstedesember1 puzzle)
        // printf "1.desember 2 top 3 elfs %i\n" (forstedesember2 puzzle)

        // let puzzle= File.ReadAllLines(@"dec2.txt") |> List.ofSeq
        // printf "2.desember 1 Rock Paper Scissors %i\n" (andredesember puzzle game1)
        // printf "2.desember 2 Rock Paper Scissors %i\n" (andredesember puzzle game2)

        // let puzzle= File.ReadAllLines(@"dec3.txt") |> List.ofSeq
        // printf "3.desember 1 Rucksack %i\n" (tredjedesember puzzle)
        // printf "3.desember 2 Rucksack %i\n" (tredjedesember2 puzzle)

        // let puzzle= File.ReadAllLines(@"dec4.txt") |> List.ofSeq
        // printf "4.desember 1 section overlap %i\n" (fjerdedesember puzzle isWithin)
        // printf "4.desember 2 section overlap %i\n" (fjerdedesember puzzle isOverlap)

        let puzzle= File.ReadAllLines(@"dec5.txt") |> List.ofSeq
        printf "5.desember 1 section overlap %i\n" (femtedesember puzzle)
        //printf "5.desember 2 section overlap %i\n" (fjerdedesember puzzle isOverlap)

        

        Console.Read() |> ignore
        0 // return an integer exit code



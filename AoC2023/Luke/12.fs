namespace AoC2023.Luke

module Luke12 =
    open System
    open System.IO
    open System.Text.RegularExpressions

    let parseInput (line:string) = 
        let parts = line.Split " " 
        let numsizes =
            parts[1].Split ","
            |> Array.map Int32.Parse 
            |> List.ofArray
        let missing = 
            parts[0] 
            |> List.ofSeq
            |> List.filter (fun c -> c = '?')
            |> List.length 
        let pattern = (parts[0], numsizes.Length, missing)

        // (pattern*commas*<wildcards>), numsizes
        (pattern, numsizes)

    let isPossible (pattern:string) (numsizes) =
        let rx = 
            numsizes 
            |> List.fold 
                (fun sofar size -> sofar + (sprintf "([#]{%i})[\.]*" size))
                "[\.]*"

        Regex.IsMatch(pattern, rx) 


    let findPossible (pattern:(string*int*int), numsizes: int list) =
        let (resPatt, commas, wildcards)= pattern 
        let ok = isPossible resPatt numsizes
        if ok then 1 else 0 // Skal være 1, siden disse testpatternene ikke har wildcards...

    let _1 (input:string) =
        let lines = input.Split "\r\n"
        lines
        |> Array.map parseInput
        |> Array.map findPossible
        |> Array.sum
    let _2 input =
        //solve input (1000000L-1L) 
        -1

    let puzzle =
        let puzzleInput= File.ReadAllText(@"inputs/dec12.txt")
        let puzzleInputt= File.ReadAllText(@"inputs/dec12t.txt")

        let expected1 = -1
        let actual1 = _1 puzzleInputt
        if  actual1 = expected1 then
            printf "Test 1 OK %i\n" actual1
            let result = _1 puzzleInput
            printf "12. desember 1 %i\n " result
        else 
            printf "12. desember 1 - Test did not match. Expected %i but was %i\n" expected1 actual1

        // let expected2 = 8410L
        // let actual2 = _2 puzzleInputt

        // if  actual2 = expected2 then
        //     printf "Test 2 OK %i\n" actual2
        // let result = _2 puzzleInput
        // printf "12. desember 2 %i\n" result
            // 553224968560 
            // trekker fra 82... 553224968478
            // Minste var for høyt...
            // 553224415344
        // else
        //     printf "11. desember 2 - Test did not match. Expected %i but was %i\n" expected2 actual2

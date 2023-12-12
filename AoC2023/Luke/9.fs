namespace AoC2023

module Luke9 =
    open System
    open System.IO

    let parseInput (s:string) = 
        s.Split("\r\n")
        |> Array.map (fun line -> 
            line.Split(" ")
            |> Array.map Int32.Parse
            |> List.ofArray
            )
        |> List.ofArray

    let rec diffLine (prev:int option) (l:int list) =
        if l.IsEmpty then
            []
        else
            let nl = diffLine  (Some l.Head) l.Tail
            if prev.IsNone then
                nl
            else 
                (l.Head - prev.Value) :: nl

    let allIdentical n l = l |> List.fold (fun isN i -> isN && (i = n)) true

    let rec diffLineSeries (l: int list) =

        if l.IsEmpty then 
            []
        else
//            printf "%A\n" l

            if allIdentical 0 l then
                // fra l.Head til l, skal være like lang?
                [l |> List.map (fun _ -> 0)] // a list with a list of zeroes, one item shorter
            else
                let nx = (diffLine None l)
                (l :: diffLineSeries nx)

    let rec extrapolate (ls:int list list) =
        if ls.IsEmpty then
            failwith "Cannot solve, last line must be all zeros"
        elif (allIdentical 0 ls.Head) then // can solve
            [List.concat [ls.Head; [0]]]
        else
            let subLevels= (extrapolate ls.Tail) 
            let lastLevel = subLevels.Head

            let lastCarry = lastLevel[lastLevel.Length-1]
            let thisLast = ls.Head[ls.Head.Length-1]
            let extendedSeries = List.concat [ls.Head; [thisLast + lastCarry]] 
            extendedSeries :: subLevels

    let rec extrapolateLeft (ls:int list list) =
        if ls.IsEmpty then
            failwith "Cannot solve, last line must be all zeros"
        elif (allIdentical 0 ls.Head) then // can solve
            [List.concat [[0];ls.Head]]
        else
            let subLevels= extrapolateLeft ls.Tail // all but first list
            
            let prevCarry = subLevels.Head[0]  // First element of first list
            let thisFirst = ls.Head[0]
            let extendedSeries = List.concat [[thisFirst - prevCarry];ls.Head]
            extendedSeries :: subLevels

    let _1 (input: string) =
        let parsed = parseInput input 
        let difflines = 
            parsed |> List.map diffLineSeries 

        let extrapolated = 
            difflines 
            |> List.map (fun dl -> extrapolate dl) 

//        printf "\n\n\ndiff line = %A\n" extrapolated

        extrapolated 
        |> List.map (fun  lss ->
            let firstList= lss[0]
            firstList[firstList.Length-1] 
            )
        |> List.sum

    let _2 (input: string) =
        let parsed = parseInput input 
        let difflines = 
            parsed |> List.map diffLineSeries 

//        printf "\n\n\ndiff line = %A\n" difflines

        let extrapolated = 
            difflines 
            |> List.map (fun dl -> extrapolateLeft dl) 

//        printf "\n\n\ndiff line = %A\n" extrapolated

        extrapolated 
        |> List.map (fun  lss ->
            let firstList= lss[0]
            firstList[0] 
            )
        |> List.sum

    let puzzle =
        let puzzleInput= File.ReadAllText(@"dec9.txt")
        let puzzleInputt= File.ReadAllText(@"dec9t.txt")
        let expected1 = 114
        let actual1 = _1 puzzleInputt
        // if  actual1 = expected1 then
        //     printf "Test 1 OK"
        //     let result = _1 puzzleInput
        //     printf "9. desember 1 %i" result
        // else 
        //     printf "9. desember 1 - Test did not match. Expected %i but was %i" expected1 actual1


        let expected2 = 2
        let actual2 = _2 puzzleInputt
        if  actual1 = expected1 then
            let result = _2 puzzleInput

            //1993300041 too high
            //1059458062
            printf "9. desember 2 %i" result
        else
        printf "9. desember 2 - Test did not match. Expected %i but was %i" expected2 actual2
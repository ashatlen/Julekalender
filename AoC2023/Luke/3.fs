namespace AoC2023

module Luke3 =
    open System
    open System.IO
    open System.Text.RegularExpressions

    let findNumbersInLine line=
        
        let mx = Regex.Matches(line, "([\d]+)") 
        if mx.Count = 0 then
            []
        else
            mx
            |> Seq.cast 
            |> Seq.map (fun (m:Match) ->
                (m.Index, m.Value)
            )
            |> List.ofSeq

    let findSymbolsInLine pattern line=        
        let mx = Regex.Matches(line, pattern) 
        if mx.Count = 0 then
            []
        else
            mx
            |> Seq.cast 
            |> Seq.map (fun (m:Match) ->
                m.Index
            )
            |> List.ofSeq

    let linesToCheck (lines:string list) lineNo =
        if lineNo = 0 then 
            [lines[lineNo];lines[lineNo+1]]
        elif lineNo = lines.Length - 1 then 
            [lines[lineNo-1]; lines[lineNo]]
        else
            [lines[lineNo-1]; lines[lineNo]; lines[lineNo+1]]

    let isAdjacentTo symbolposs fromIdx toIdx =
        symbolposs 
        |> List.filter (fun pos -> 
            (pos >= fromIdx && pos <= toIdx)    // symbol is adjacent
        )

    let checkAdjacent 
        (lines:string list) 
        lineNo 
        pattern 
        (condition: int list -> bool)
        =

        let chkLines = linesToCheck lines lineNo
        let numbers = findNumbersInLine lines[lineNo]
        let symbolposs = 
            chkLines 
            |> List.map (findSymbolsInLine pattern)
            |> List.concat  // the actual line does not matter

        let lastIdx = lines[lineNo].Length - 1

        numbers
        |> List.map (fun numpos ->
            let (startIdx, numLit) = numpos
            let endIdx = startIdx + numLit.Length - 1

            let fromIdx = 
                if startIdx = 0 
                then 0
                else startIdx - 1
            let toIdx = 
                if lastIdx <= endIdx 
                then lastIdx
                else endIdx + 1 // one more than the last digit number if not eol
//            printf "Check for number %s %i %i max= %i\n" numLit fromIdx toIdx lastIdx 
            let adjTo = isAdjacentTo symbolposs fromIdx toIdx
            if (condition adjTo) then
                [Int32.Parse(numLit)]
            else
                printf "Number not used, adjacent cnt = %i\n" adjTo.Length
                []
            )  
        |> List.concat

    let checkForNumbersAdjacent (lines: string list) pattern condition =
        let idxs = [for i in 0..lines.Length-1 -> i]
        idxs 
        |> List.map (fun idx ->
            checkAdjacent 
                lines 
                idx 
                pattern
                condition 
            )
        |> List.concat

    let checkStarAdjacent 
        (lines:string list) 
        lineNo 
        pattern 
        =

        let chkLines = linesToCheck lines lineNo
        let numbers =
            chkLines
            |> List.map findNumbersInLine 
            |> List.concat  // the actual line does not matter

        let lastIdx = lines[lineNo].Length - 1

        // Kan være flere * på samme linje...
        let symbolposs = findSymbolsInLine pattern lines[lineNo] 
        symbolposs
        |> List.map (fun sympos ->
            let adjList =
                numbers
                |> List.map (fun numpos ->
                    let (startIdx, numLit) = numpos
                    let endIdx = startIdx + numLit.Length - 1

                    let fromIdx = 
                        if startIdx = 0 
                        then 0
                        else startIdx - 1
                    let toIdx = 
                        if lastIdx <= endIdx 
                        then lastIdx
                        else endIdx + 1 // one more than the last digit number if not eol
                    let isA = isAdjacentTo [sympos] fromIdx toIdx
                    if isA.Length > 0 then
                        [(numpos, Int32.Parse (snd numpos))]
                    else
                        []
                )  
                |> List.concat
            // adjList 
            // |> List.groupBy (fun itm -> itm)
            // |> 
            if adjList.Length = 2 then 
//                printf "Check for * in line %i, pos %i, %A\n" lineNo sympos adjList 
                [snd adjList[0] * snd adjList[1]]                
            else
                []
        )
        |> List.concat

    let checkForStarAdjacent (lines: string list) pattern =
        let idxs = [for i in 0..lines.Length-1 -> i]
        idxs 
        |> List.map (fun idx ->
            checkStarAdjacent 
                lines 
                idx 
                pattern
            )
        |> List.concat

    let _1 (lines: string list) =

        let condition = (fun (adjTo: int list) -> adjTo.Length > 0)
        let numTot = 
            checkForNumbersAdjacent lines  "[^(0-9|\.)]" condition
            |> List.map (fun number ->
//                printf "Use number: %i\n" number
                number
                )
            |> List.sum
        printf "3. desember 1: %i" numTot  

    let _2 (lines: string list) =

        let condition = (fun (adjTo: int list) -> adjTo.Length = 2)
        let numTot = 
            checkForStarAdjacent lines "[\*]"
            |> List.map (fun number ->
//                printf "Use number: %i\n" number
                number
                )
            |> List.sum
        printf "3. desember 2: %i" numTot  

    let puzzle =
        let puzzleInput= File.ReadAllLines(@"dec3.txt") |> List.ofArray 
        let testpuzzle = @"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

//        let puzzleInput = testpuzzle.Split "\n" |> List.ofArray
        _2 puzzleInput

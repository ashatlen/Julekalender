namespace AoC2023

module Luke2 =
    open System
    open System.IO

    let parseCubeGroup (sample : string) =
        let c_c = sample.Trim().Split " "
        let count = Int32.Parse c_c[0]
        let color = c_c[1]
        (color, count)

    let sampleIsPossible (sample : string) =
        let (color, count) = parseCubeGroup sample
        match color with
        | "red"  -> count <= 12
        | "green"  -> count <= 13
        | "blue"  -> count <= 14
        | x -> failwith (sprintf "Wrong color %s\n" sample)

    let _2_1 (lines: string seq) =
        let oneIsPossible (smpls : string ) =
            smpls.Split ","
            |> Array.map (fun s -> sampleIsPossible s)
            |> Array.fold (fun acc elt -> acc && elt = true) true

        let result=
            lines
            |> Seq.map (fun l ->
                let g_s = l.Split ":"
                let g = g_s[0].Split " "
                (Int32.Parse(g[1]), g_s[1])
                )
            |> Seq.map (fun (game, sampleset) ->
                let samples = (sampleset.Split ";") 
                let allPossible =
                    samples
                    |> Array.map oneIsPossible
                    |> Array.fold (fun acc elt -> acc && elt = true) true
                (game, allPossible)
                )
            |> Seq.filter (fun (g, possible) -> possible)
            |> Seq.map (fun (g, _) -> g)
            |> Seq.sum

        printf "2. desember 1: %i\n" result

    let maxOfColor targetColor (sample : (string * int) seq)  =
        let toCount =
            sample 
            |> Seq.filter (fun (color, count) -> (color.Equals(targetColor)))
            |> Seq.toArray

        printf "targetcolor %s - matches: %i \n" targetColor toCount.Length

        if toCount.Length = 0 then
            0
        else
            toCount
            |> Seq.map (fun (color, count) -> count)
            |> Seq.max

    let _2_2 (lines: string seq) =

        let result=
            lines
            |> Seq.map (fun l ->
                let g_s = l.Split ":"
                let g = g_s[0].Split " "
                g_s[1]
                )
            |> Seq.map (fun sampleset ->                

                printf "sampleset: %s\n" sampleset
                let samples = 
                    (sampleset.Split ";")
                    |> Array.map (fun sett -> 
                        sett.Split ","
                        |> Array.map parseCubeGroup
                    )                
                    |> Array.concat
                let maxBlue = maxOfColor "blue" samples
                let maxRed = maxOfColor "red" samples 
                let maxGreen = maxOfColor "green" samples 

                printf "blue %i red %i green %i\n" maxBlue maxRed maxGreen

                let pwr = maxBlue * maxRed * maxGreen
                pwr
                )
            |> Seq.sum

        printf "2. desember 2: %i" result

    let _2 =
        let testinput= "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

        let lines = 
            testinput.Split "\n" 
            |> Seq.ofArray

        let lines= File.ReadAllLines(@"dec2.txt") |> Seq.ofArray 

        _2_2 lines

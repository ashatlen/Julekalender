
namespace AoC2023

module Luke5 =

    open System
    open System.IO
    open System.Text.RegularExpressions

    let parseSeed (section:string) =
        let seedLine= section.Split (":")
        seedLine[1].Trim().Split " "
        |> Array.map( fun seed -> Int64.Parse seed )

    let parseSeed2 (section:string) =
        let sl= section.Split (":")
        let seedLine = 
            sl[1].Trim().Split " "
            |> Array.map( fun seed -> Int64.Parse seed )
//        printf "Seed line contains %i elements\n" seedLine.Length
        [for i in 0..(seedLine.Length / 2 - 1) -> i * 2]
        |> List.map (fun idx -> 
            (seedLine[idx], seedLine[idx+1])
            ) // Seed*Range        

    let parseMapCategory (section:string) =
        let lines= section.Split "\r\n"
        let categoryLine=lines[0].Split " "
        let source_target = categoryLine[0].Split "-to-"

        let accept = source_target[0]
        let give = source_target[1]

        let parseMap (map:string) = 
            let mp = 
                map.Split " "
                |> Array.map (fun v -> Int64.Parse v)
            (mp[0], mp[1], mp[2])

        let table = 
            lines[1..] 
            |> Array.map parseMap

//        printf "Map: %s %s %A\n " accept give table

        (accept, give, table)

        // let findMap from maps =
        //     let r =
        //         maps 
        //         |> Array.find (fun (accept, gives, table) -> accept = from )

    let xlat (seed:int64) (tbl: (int64*int64*int64) array) =
        let factor =
            tbl 
            |> Array.tryFind (fun (dest, source, range) -> 
            seed >= source && seed < source + range
            )
        match factor with
        | Some f -> 
            let (dest, source, range)= f    
            let result= dest + seed - source
//            printf "Finds range %A for seed %i, output %i\n" f seed result
            result
        | None ->
            seed
        
    let _1 (input:string) =
        let sections = input.Split "\r\n\r\n"
        let seeds = parseSeed sections[0]

        let maps = 
            sections[1..]
            |> Array.map (fun (s:string) -> parseMapCategory s)

        let location = 
            seeds 
            // Skal sluse seed gjennom alle maps til output => FOLD med Seed som state
            |> Array.map (fun seed ->     
                printf "Seed %i -> " seed
                let r =
                    maps
                    |> Array.fold (fun s map ->                      
                        let (_,dest,m) = map
                        let t =xlat s m        //State = seed =>
//                        printf " %s\n -> %i " dest t                        
                        t
                    ) seed
//                printf "== %i\n" r
                r
             )
            |> Array.min

        printf "5. Desember 1 %A " location

    let _2 (input:string) =

        // Jepp, dette er brute force.
        // Har ikke tenkt ut hvordan en kan forbedre dette.
        // Det er 10 seed ranges, og 8 transformatorer som
        // ikke er sortert på "source" en gang. Dvs at vi kan
        // redusere antall gjennomløp veldig lett ved sortering
        // og å gå til default når vi er utenom en range.
        //
        //Seed base 2276375722, range 160148132 -> lowest for this range:  593974860
        //Seed base 3424292843, range  82110297 -> lowest for this range:   46294175
        //Seed base 1692203766, range 342813967 -> lowest for this range:  322357427
        //Seed base 3289792522, range 103516087 -> lowest for this range:  250131305
        //Seed base 2590548294, range 590357761 -> lowest for this range:  642813586
        //Seed base 1365412380, range  80084180 -> lowest for this range: 2444964298
        //Seed base 3574751516, range 584781136 -> lowest for this range:  146637026
        //Seed base 4207087048, range  36194356 -> lowest for this range:  859731507
        //Seed base 1515742281, range 174009980 -> lowest for this range:  421684844
        //Seed base    6434225, range 291842774 -> lowest for this range:  228039921
        //5. Desember 2 46294175L

        let sections = input.Split "\r\n\r\n"
        let seeds = parseSeed2 sections[0]

        let maps = 
            sections[1..]
            |> Array.map (fun (s:string) -> parseMapCategory s)

        let location = 
            seeds 
            // Skal sluse seed gjennom alle maps til output => FOLD med Seed som state
            |> List.map (fun seedrange ->                
                let (seedstart, range) = seedrange
                printf "Seed base %i, range %i -> " seedstart range
                let mutable result = Int64.MaxValue
                let r = 
                    [for seed in seedstart..(seedstart + range) -> seed]
                    |> List.map (fun seed ->
                        let r =
                            maps
                            |> Array.fold (fun s map ->
                                let (_,dest,m) = map
                                let t =xlat s m        //State = seed =>
        //                        printf " %s\n -> %i " dest t
                                t
                            ) seed
//                        printf "== %i\n" r
                        r
                        )
                    |> List.min
                printf "lowest for this range: %i\n" r
                r
                )
            |> List.min

        printf "5. Desember 2 %A " location

    let puzzle =
        let puzzleInput= File.ReadAllText(@"dec5.txt")
        // _1 puzzleInput
        _2 puzzleInput

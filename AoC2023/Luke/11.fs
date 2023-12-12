namespace AoC2023

module Luke11 =
    open System
    open System.IO

    let parseInput (t:string) =
        let lines = t.Split("\r\n")
        lines |> List.ofArray

    let getCoordinates (map : string list) =
        let gs =    
            map
            |> List.map (fun line ->
                [for i in 0..line.Length - 1 -> i]
                |> List.map (fun x ->
                    if line[x] = '#' then
                        [int64 x]
                    else
                        []
                    )
                |> List.concat
                )
        [for i in 0..gs.Length - 1 -> i]
        |> List.map (fun y -> 
            let g = gs[y]
            g 
            |> List.map (fun x -> 
                [(x,int64 y)]
                )
            |> List.concat
            )
        |> List.concat

    let rec expandHoriz (times:int64) (x:int64) (maxx:int64) (poss: (int64*int64) list) =
        if x > maxx then
            poss
        else
            let notAnyX = 
                poss 
                |> List.filter (fun i -> fst i = x)
                |> List.isEmpty

            // printf "Map is %i wide. Are there any Galaxies at x = %i? %b\n" maxx x (not notAnyX)

            if notAnyX then
                poss 
                |> List.map (fun p ->
                    let (px, py) = p
                    if (px > x) then
                        (px+times, py)
                    else
                        p
                    )
                |> expandHoriz times (x+times+1L) (maxx + times)
            else
                expandHoriz times (x+1L) maxx poss 

    let rec expandVert (times:int64) (y:int64) (maxy:int64) (poss: (int64*int64) list) =
        if y > maxy then
            poss
        else
            let notAnyY = 
                poss 
                |> List.filter (fun i -> snd i = y)
                |> List.isEmpty

            // printf "Map is %i high. Are there any Galaxies at y = %i? %b\n" maxy y (not notAnyY)
 
            if notAnyY then
                poss 
                |> List.map (fun p ->
                    let (px, py) = p
                    if (py > y) then
                        (px, py + times)
                    else
                        p
                    )
                |> expandVert times (y+times+1L) (maxy + times)
            else
                expandVert times (y+1L) maxy poss

    let expandUniverse (times:int64) (poss:(int64*int64) list) (maxx:int) (maxy:int) =
        poss 
        |> expandHoriz times 0L maxx
        |> expandVert times 0L maxy

    let rec calculateDistances (xmap:(int64*int64) list) =
        if xmap.Length <= 1 then
            0L
        else
            let (px, py) = xmap.Head
            let s =
                xmap.Tail 
                |> List.map (fun (x,y) ->
                    let d= (Math.Abs (px-x)) + (Math.Abs(py-y))
                    d
                    )
                |> List.sum 
            
            s + (calculateDistances xmap.Tail)

    let solve input (times:int64) =
        let map = parseInput input
        let galaxies = getCoordinates map

        // printf "There are %i galaxies in this universe with dimension (%i,%i) \n" galaxies.Length map[0].Length map.Length
        printf "Distances %A\n" galaxies

        let xmap = 
            expandUniverse times
                galaxies 
                map[0].Length // first line is sample
                map.Length    // all lines

        printf "Expanded  %A\n\n\n" xmap
        let dists = calculateDistances xmap

        dists

    let _1 input =
        solve input 1
    let _2 input =
        solve input 100 //0000

    let puzzle =
        let puzzleInput= File.ReadAllText(@"dec11.txt")
        let puzzleInputt= File.ReadAllText(@"dec11t.txt")

        let expected1 = 374L
        let actual1 = _1 puzzleInputt

        if  actual1 = expected1 then
            printf "Test 1 OK %i\n" actual1
            let result = _1 puzzleInput
            printf "11. desember 1 %i\n " result
        else 
            printf "11. desember 1 - Test did not match. Expected %i but was %i\n" expected1 actual1

        let expected2 = 8410L
        let actual2 = _2 puzzleInputt

        if  actual2 = expected2 then
            printf "Test 2 OK %i\n" actual2
            let result = _2 puzzleInput
            printf "11. desember 2 %i\n" result
            // 553224968560 
            // trekker fra 82... 553224968478
            // Minste var for høyt...
        else
            printf "11. desember 2 - Test did not match. Expected %i but was %i\n" expected2 actual2

// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Text.RegularExpressions

module Program =

    let processInput lp (puzzle_input:string seq) = puzzle_input |> Seq.map lp

    let solutions_1 filename =

        let solution_1_1 (inputs: (int*int) seq) =

            let a2 = inputs |> Seq.map fst |> Seq.sort
            let b2 = inputs |> Seq.map snd |> Seq.sort

            (Seq.zip a2 b2)
            |> Seq.map ( fun (a,b) -> Math.Abs (a - b))
            |> Seq.sum

        let solution_1_2 inputs =

            let a = inputs |> Seq.map fst |> Seq.sort
            let b = inputs |> Seq.map snd |> Seq.sort

            a |> Seq.fold (fun score itm ->
                let c = b |> Seq.filter (fun i -> i = itm) |> Seq.length
                score + (itm * c)
                )
                0

        let lineParser (line:string) =
            let elts =
                line.Split "   "
                |> Array.map (fun si ->
                    (si.Trim()) |> Int32.Parse
                    )
            (elts[0] , elts[1])

        filename
        |> System.IO.File.ReadAllLines
        |> (processInput lineParser)
        |> solution_1_1
        |> printf "Solution 1 1 : %i\n"

        filename
        |> System.IO.File.ReadAllLines
        |> (processInput lineParser)
        |> solution_1_2
        |> printf "Solution 1 2 : %i\n"


    let solutions_2 filename =

        let lineParser (line:string) =
            let elts =
                line.Split " "
                |> Array.map (fun si ->
                    (si.Trim()) |> Int32.Parse
                    )
            elts

        let rec checkdiff direction last items =
            if (Seq.length items) = 0 then
                true
            else
                let itm = Seq.head items
                let diff = itm - last
                let direction =
                    if direction = 0 then
                        if diff > 0 then 1 else -1
                    else
                        direction
                if not (diff * direction >= 1 && diff * direction <= 3) then
                    false
                else
                    checkdiff direction (Seq.head items) (Seq.tail items)

        let solution_2_1 inputs =
            inputs
            |> Seq.map (fun itms -> checkdiff 0 (Seq.head itms) (Seq.tail itms) )
            |> Seq.filter (fun valid -> valid)
            |> Seq.length

        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map lineParser
        |> solution_2_1
        |> printf "Solution 2 1 : %i\n"

        let rec checkdiffWithSkip skipIdx itms =
            let length = itms |> Seq.length
            if skipIdx >= length then
                false
            else
                let s =
                    if skipIdx = -1 then
                        itms
                    else
                        // printfn "skipping index %i" skipIdx
                        let first = itms |> Seq.take (skipIdx)
                        let rest = itms |> Seq.skip (skipIdx + 1)
                        // printfn "First: %A Rest %A" first rest
                        Seq.append first rest
                //printfn "Elements: %A" s
                if (checkdiff 0 (Seq.head s) (Seq.tail s)) then
                    true
                else
                    checkdiffWithSkip (skipIdx+1) itms

        let solution_2_2 inputs =
            inputs
            |> Seq.map (fun itms ->
                let r = checkdiffWithSkip -1 itms
                printfn "inputs -> %b" r
                r
                )
            |> Seq.filter (fun valid -> valid)
            |> Seq.length

        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map lineParser
        |> solution_2_2
        |> printf "Solution 2 2 : %i\n"

    let solutions_3 filename =

        let matches input pattern =
        // mul\([0-9]{1,3},[0-9]{1,3}\)
            Regex.Matches(input, pattern)
            |> Seq.cast<Match>
            //|> Seq.groupBy (fun m -> m.Value)
            |> Seq.map (fun m -> m.Value)

        let parseOperation (instr:string) =
            instr.Substring 4
            |> (fun instr -> instr.Substring(0, (instr.Length - 1) ))
            |> (fun nums ->
                let ns =
                    nums.Split(",")
                    |> Array.map (fun n -> Int64.Parse n)
//                    printfn "Input: %A" ns
                ns[0] * ns[1]
                )

        let solution_3_1 line =
            //printfn "Input: %s" line
            (matches line "mul\([0-9]{1,3},[0-9]{1,3}\)")
            |> Seq.map parseOperation
            |> Seq.sum

        let rec parseOperationsWithStop enabled ops =
            if (ops |> Seq.length = 0) then
                0L
            else
                let op = Seq.head ops
                match op with
                | "don't()" ->
                    printfn "don't"
                    parseOperationsWithStop false (ops |> Seq.tail)
                | "do()" ->
                    printfn "do"
                    parseOperationsWithStop true (ops |> Seq.tail)
                | _ ->
                    let s = if enabled then (parseOperation op) else 0
                    s + parseOperationsWithStop enabled (ops |> Seq.tail)

        let solution_3_2 line =
            let ops = (matches line "(mul\([0-9]{1,3},[0-9]{1,3}\))|(don't\(\))|(do\(\))")
            //ops |> Seq.length
            parseOperationsWithStop true ops

        // filename
        // |> System.IO.File.ReadAllText
        // |> solution_3_1
        // |> printf "Solution 3 1 : %i\n"

        filename
        |> System.IO.File.ReadAllText
        |> solution_3_2
        |> printf "Solution 3 2 : %i\n"

    let solutions_4 filename =

        let lettermatch pos =
            match pos with
            | 0 -> 'X'
            | 1 -> 'M'
            | 2 -> 'A'
            | 3 -> 'S'

        let checkbounds (x,y) matrix =
            if x < 0 || y < 0 then
                false
            elif y >= (matrix |> Array.length) then
                false
            elif x >= (matrix |> Array.head |> Array.length) then
                false
            else
                true

        let rec findword (x,y) (dx,dy) pos matrix =
            if pos = 4 then
                true
            elif not (checkbounds (x,y) matrix) then
                false
            else
                //let lm = lettermatch pos
                if matrix[y][x] = (lettermatch pos) then
                    findword (x+dx,y+dy) (dx,dy) (pos+1) matrix
                else
                    false

        let solution_4_1 matrix =
            let dirs = [(1,0); (-1,0); (0,1); (0,-1); (1,1); (1,-1); (-1,1);(-1,-1)]
            let rangex = seq {for i in 0..(matrix |> Array.head |> Array.length) - 1 do i}
            let rangey = seq {for i in 0..(matrix |> Array.length) - 1 do i}

            rangex |> Seq.map (fun x ->
                rangey |> Seq.map (fun y ->
                    dirs |> Seq.map (fun d ->
                        if findword (x,y) d 0 matrix then
                            1
                        else
                            0
                        )
                    |> Seq.sum)
                |> Seq.sum
                )
            |> Seq.sum

        let lettermatchMAS pos =
            match pos with
            | 0 -> 'M'
            | 1 -> 'A'
            | 2 -> 'S'

        let rec findMAS (x,y) (dx,dy) pos matrix =
            if pos = 3 then
                true
            elif not (checkbounds (x,y) matrix) then
                false
            else
                //let lm = lettermatch pos
                if matrix[y][x] = (lettermatchMAS pos) then
                    findMAS (x+dx,y+dy) (dx,dy) (pos+1) matrix
                else
                    false

        let solution_4_2 matrix =
            let rangex = seq {for i in 0..(matrix |> Array.head |> Array.length) - 1 do i}
            let rangey = seq {for i in 0..(matrix |> Array.length) - 1 do i}

            // Finn alle A-er
            let potentialPositions =
                rangex |> Seq.map (fun x ->
                    rangey |> Seq.map (fun y ->
                        if matrix[y][x] = 'A' then
                            Some (x,y)
                        else
                            None
                        )
                    )
                |> Seq.collect id
                |> Seq.filter (fun itm -> itm.IsSome)
                |> Seq.map (fun i ->
                    //let (x,y) = i.Value
                    // printfn "found A-spot at (%i,%i)" x y
                    i.Value)
                |> List.ofSeq
                // |> List.skip 3
                // |> List.take 1

            printfn "Found %i A-spots" (potentialPositions |> List.length)

            // M S
            //  A
            // M S

            // directions to search (firstdir, offset), (seconddir, offset)
            let pattern_offsets =
                [
                    // down-right, down-left
                    [(1,1),(-1,-1);(-1,1),(1,-1)];
                    // down-right, up_right
                    [(1,1),(-1,-1);(1,-1),(-1,1)];
                    // up-left, down-left
                    [(-1,-1),(1,1);(-1,1),(1,-1)];
                    // up-left, up-right
                    [(-1,-1),(1,1);(1,-1),(-1,1)];
                ]

            potentialPositions
            |> Seq.map (fun (x,y) ->
                    pattern_offsets
                    |> Seq.map (fun patterns ->
                        let allMatches=
                             patterns
                            |> List.forall (fun pattern ->
                                let (direction, (dx, dy)) = pattern
                                let (dirx, diry) = direction
                                // printfn "A-pos = (%i,%i) Search from (%i,%i), dir (%i,%i)" x y (x+dx) (y+dy) dirx diry
                                let found= findMAS (x+dx,y+dy) direction 0 matrix
                                // if found then
                                //     printfn "found at A-spot (%i,%i)" (x) (y)
                                found
                            )
                        if allMatches then 1 else 0
                    )
                    |> Seq.sum)
            |> Seq.sum

        filename
        |> System.IO.File.ReadAllLines
        |> Array.map (fun line -> line |> Array.ofSeq)
        |> solution_4_1
        |> printf "Solution 4 1 : %i\n"

        filename
        |> System.IO.File.ReadAllLines
        |> Array.map (fun line -> line |> Array.ofSeq)
        |> solution_4_2
        |> printf "Solution 4 2 : %i\n"

    let solutions_5 filename =

        let rec readPagerule lines rules = 
            let l = List.head lines 
            let rest = List.tail lines
            if String.length l = 0 then
                (rest, rules)
            else
                let rulep = 
                    l.Split '|'  
                    |> Array.map (fun page -> Int32.Parse page)
                let rule = (rulep |> Array.head , rulep |> Array.tail |> Array.head)

                let rules = List.append rules [rule]
                readPagerule rest rules 

        let rec readPageSequence (lines: string list) = 
            if lines.IsEmpty then
                []
            else
                let l = List.head lines
                let pages = 
                    l.Split ',' 
                    |> Array.map (fun p -> Int32.Parse p) 
                    |> List.ofArray

                List.append [pages] (readPageSequence (List.tail lines))

        let parseInput (lines : string list) =

            let (rest, rules) = readPagerule lines List.empty
            let pages =readPageSequence rest 
            let rules = rules |> List.sortBy (fun itm -> fst itm)
            (rules, pages)

        let validateRule pages rule  =           
            let (before, after) = rule
            let idxBefore = pages |> List.tryFindIndex (fun elt -> elt = before)
            let idxAfter = pages |> List.tryFindIndex (fun elt -> elt = after)
            match (idxBefore, idxAfter) with
            | (_,None) -> true
            | (Some idx1, Some idx2) -> idx1 < idx2
            | (None,_) -> true

        let solution_5_1 rules pages =
            pages 
            |> List.map (fun ps -> 
                let isValid = rules |> List.forall (validateRule ps)
                if isValid then
                    ps[ps.Length / 2]
                else
                    0
                )
            |> List.sum

        let reorder (pages:int list) rule =
            let (before, after) = rule
            let idxBefore = pages |> List.tryFindIndex (fun elt -> elt = before)
            let idxAfter = pages |> List.tryFindIndex (fun elt -> elt = after)
            match (idxBefore, idxAfter) with
            | (_,None) -> pages
            | (None,_) -> pages
            | (Some b, Some a) -> 
                if b < a then 
                    pages
                else
                    //printfn "Rule [%i(%i) before %i(%i)] on list %A" before b after a pages

                    let pages' = 
                        [
                        if a = 0 then List.empty else pages |> List.take a;
                        [pages[b];pages[a]];

                        pages |> List.skip (a+1) |> (fun rest -> if rest = [] then [] else rest |> List.take (b-a-1));

                        if b+1 > List.length pages then 
                            [] 
                        else 
                            pages |> List.skip (b+1)
                        ]
                    //printfn "%A => Rule [%i(%i) before %i(%i)] =>  %A" pages before b after a pages'
                    List.concat pages'

        let solution_5_2 rules pages=
            pages 
            |> List.filter (fun ps -> 
                let isValid = 
                    rules 
                    |> List.forall (validateRule ps)
                not isValid
                )
            //|> List.skip 2
            |> List.map (fun ps ->
                let li = 
                    rules 
                    |> List.fold (fun ps rule -> 
                        reorder ps rule
                        ) 
                        ps 
                //printfn "Corrected list %A" li
                li[li.Length / 2]
                )
            |> List.sum

        let (rules, pages) = 
            filename 
            |> System.IO.File.ReadAllLines
            |> List.ofArray
            |> parseInput

        (solution_5_1 rules pages)
        |> printf "Solution 5 1 : %i\n"

        (solution_5_2 rules pages)
        |> printf "Solution 5 2: %i\n"

    let direction guard =
        match guard with
        | '<' -> (-1,0)
        | '>' -> (1,0)
        | '^' -> (0,-1)
        | 'v' -> (0,1)

    let turn guard =
        match guard with
        | '<' -> '^'
        | '^' -> '>'
        | '>' -> 'v'
        | 'v' -> '<'
    let rec findSymbol row radar symbol =
        let x = List.tryFindIndex (fun c -> c = symbol) (List.head radar)
        if Option.isSome x then 
            (x.Value, row)
        else
            findSymbol (row+1) (List.tail radar) symbol

    let findGuard row radar guard =
        findSymbol row radar guard

    let replaceAtIdx x sym (row : char list) =
        [
        if x = 0 then [] else row[0..x-1]
        [sym]; 
        row[(x+1)..]
        ]
        |> List.concat 

    let setTrace pos sym (radar : char list list) =

            let (x,y) = pos
            [
            if y = 0 then [] else radar[0..y-1] ;
            [(replaceAtIdx x sym radar[y])];
            radar[y+1..] ;
            ]
            |> List.concat

//        setTraceR 0 pos radar

    let isOutOfBounds (x,y) (radar: char list list) =
        if radar.Length <= y then
            true
        elif radar.Head.Length <= x then
            true
        else
            false

    let getSymbol (x,y) (radar : char list list) =
        if isOutOfBounds (x,y) radar then
            '%' // to allow move and not turn
        else
            radar[y][x] 

    let isObstacle pos radar =
        let (x,y) = pos
        let itIs = (getSymbol pos radar) = '#'
        itIs


    let rec getNewDirection (x,y) guard radar =
        let (dx,dy) = direction guard

        if isObstacle (x+dx,y+dy) radar then
            getNewDirection (x,y) (turn guard) radar
        else
            guard

    let findNextPosition curr guard = 
        let (x,y) = curr
        let (dx,dy) = direction guard
        //printfn "Move from (%i, %i) to (%i, %i) %s" x y nx ny (if guard <> guard' then "turn" else "-")
        (x+dx,y+dy)

    let rec moveGuard curr guard radar =

        // Check radar ahead, turn if obstacle
        let guard' = getNewDirection curr guard radar
        let (nx,ny)= findNextPosition curr guard'

        let radar' = setTrace curr 'X' radar
        if isOutOfBounds (nx,ny) radar then
            printfn "out of bounds (%i, %i)" nx ny
            radar'
        else
            radar' |> moveGuard (nx,ny) guard'

    let solution_6_1 curr guard (radar : char list list) = 
        let (x,y) = curr
        printfn "Guard postion %i,%i, radar map size (%i, %i)" x y radar.Head.Length radar.Length

        radar 
        |> moveGuard curr guard

    let checkBeenBefore (x,y) guard locations = 
        let newguard = turn guard
        let beenBefore=
            locations 
            |> List.tryFind (fun (ix,iy,guard) -> ix = x && iy = y && guard = newguard)
    
        if beenBefore.IsSome then
            // a loop is detected! Obstacle should be set ahead
            let (ox,oy)= findNextPosition (x,y) guard
            Some (ox,oy)
        else 
            None

    let rec findLoopPoints curr guard radar (locations : (int*int*char) list) loopPoints =

        let (x,y) = curr
        let locations' = List.append locations [(x,y,guard)]
        //printfn "Posisjon: (%i, %i, %c), " x y guard

        let guard' = getNewDirection curr guard radar
        let (nx,ny)= findNextPosition curr guard'

        if isOutOfBounds (nx,ny) radar then
            printfn "Guard left the map (%i, %i)" nx ny
            loopPoints
        elif guard <> guard' then // there is an obstacle already, continue
            findLoopPoints (nx,ny) guard' radar locations' loopPoints
        else
            let crossPointObstacle = checkBeenBefore curr guard locations

            // Will reach old path if the guard turns now.
            let tryTurn = turn guard
            let (txd, tyd) = direction tryTurn

            // MANGLER DENNE:
            // Må stoppe ved første #, og så fortsette slik 
            // guard ville ha gjort til enten går out of bounds,
            // eller kommer inn på sporet?

            let findAlong=
                 locations 
                 |> List.tryFind (fun (ix,iy,guard) ->
                    if txd <> 0 then // check horisontal
                        iy = y && (ix-x)*txd > 0 && (guard = tryTurn )
                    else // check vertical
                        ix = x && (iy-y)*tyd > 0 && (guard = tryTurn )
                    )
            let alongNewPath = if findAlong.IsSome then Some (nx,ny) else None

            let loopPoints' =
                if crossPointObstacle.IsSome then
                    let (ox, oy) = crossPointObstacle.Value
                    printfn "Krysser sporet: (%i, %i)  retning %c - loop point (%i, %i)" x y guard ox oy

                    List.append loopPoints [crossPointObstacle.Value]
                elif alongNewPath.IsSome then // Om du snur nå, så finnes det et punkt over med samme x-verdi og retning -> fører til en loop.

                    let (ox, oy) = alongNewPath.Value
                    printfn "På linje med sporet: (%i, %i)  retning %c - loop point (%i, %i)" x y guard ox oy

                    List.append loopPoints [alongNewPath.Value]
                else
                    let beenBefore=
                        locations 
                        |> List.tryFind (fun (ix,iy,_) -> ix = x && iy = y)
                    if beenBefore.IsSome then
                        let (_,_, bguard) = beenBefore.Value
                        printfn "Krysser sporet: (%i, %i) feil retning %c, %c ikke loop point " x y guard bguard

                    loopPoints

            findLoopPoints (nx,ny) guard' radar locations' loopPoints'

    let solution_6_2 curr guard (radar : char list list) = 
        let (x,y) = curr
        let loopPoints= findLoopPoints curr guard radar [(x,y,guard)] []
        printfn "Loop points %A" loopPoints
        loopPoints

    let rec countPositions sym radar = 
        if List.isEmpty radar then 
            0
        else
            let cnt = radar.Head |> List.filter (fun c -> c = sym) |> List.length
            cnt + countPositions sym radar.Tail

    let solutions_6 filename = 

        let radar =
            filename
            |> System.IO.File.ReadAllLines
            |> List.ofArray
            |> List.map (fun line -> line |> Seq.toList )
        let guard = '^'
        let curr = findGuard 0 radar guard
        let (x,y) = curr

        let radar = (solution_6_1 curr guard radar)
        //printfn "%A" radar
        radar
        |> countPositions 'X'
        |> printfn "Solution 6 1 : %i\n"

        let loopPoints = (solution_6_2 curr guard radar)
        //printfn "%A" loopPoints
        let radar' =
            loopPoints  |> List.fold (fun r l -> 
                setTrace l 'O' r
                ) radar

        printfn "Map:"
        radar' |> List.iter (fun row -> 
            row |> List.iter (fun c ->  printf "%c" (if c = '.' then ' ' else c))
            printfn ""
            )

        loopPoints
        |> (fun loops -> loops.Length)

        |> printfn "Solution 6 2 : %i\n"
        printfn "Guard postion %i,%i, radar map size (%i, %i)" x y radar.Head.Length radar.Length
        // Gets 882, too low...

    let rec calculateOps testval (result :int64) op (vals :int64 list) =
        
        if vals.IsEmpty then
            if testval = result then 
                [result] 
            else
                []
        else
            let next = vals.Head
            let result' = 
                if op = 0 then 
                    result + next
                elif op = 1 then
                    result * next
                else
                    Int64.Parse (sprintf "%i%i" result next)
            let res1 = calculateOps testval result' 0 vals.Tail;
            let res2 = calculateOps testval result' 1 vals.Tail;

            List.append res1 res2

    let rec calculateOps2 testval (result :int64) op (vals :int64 list) =
        
        if vals.IsEmpty then
            if testval = result then 
                [result] 
            else
                []
        else
            let next = vals.Head
            let result' = 
                if op = 0 then 
                    result + next
                elif op = 1 then
                    result * next
                else
                    let rs =sprintf "%i%i" result next
                    let r= Int64.Parse (rs)
                    r
            
            let res1 = calculateOps2 testval result' 0 vals.Tail;
            let res2 = calculateOps2 testval result' 1 vals.Tail;
            let res3 = calculateOps2 testval result' 2 vals.Tail;

            List.append (List.append res1 res2) res3

    let solution_7_1 equations =
        equations 
        |> List.map (fun (testval, vals) ->
            let res1 = calculateOps testval 0L 0 vals;
            let res2 = calculateOps testval 1L 1 vals;

            if res1.IsEmpty && res2.IsEmpty then 0L else
                printfn "Result: %i" testval
                testval
            )
        |> List.sum

    let solution_7_2 equations =
        equations 
        |> List.map (fun (testval, vals) ->
            let res1 = calculateOps2 testval 0L 0 vals;
            let res2 = calculateOps2 testval 1L 1 vals;
            let res3 = calculateOps2 testval 0L 2 vals;

            if res1.IsEmpty && res2.IsEmpty && res3.IsEmpty then 0L else
                printfn "Result: %i" testval
                testval
            )
        |> List.sum

    let solutions_7 filename = 
        
        let inputs =
            filename
            |> System.IO.File.ReadAllLines
            |> List.ofArray

        let equations =
            inputs 
            |> List.map (fun s -> 
                let eqs = s.Split ":" 
                let ops = 
                    eqs[1].Trim().Split " " 
                    |> Array.map (fun v -> 
                        Int64.Parse (v.Trim()))
                    |> List.ofArray

                (Int64.Parse eqs[0], ops)
                )


        solution_7_1 equations
        |> printfn "Solution 7 1 : %i\n"

        solution_7_2 equations
        |> printfn "Solution 7 2 : %i\n"

    let rec scanLineForAntennas x (line: char list) =
        if line.IsEmpty then
            []
        else
            let found = if line.Head <> '.' then [(x,line.Head)] else []
            let rest = (scanLineForAntennas (x+1) line.Tail)
            List.append found rest

    let rec collectAntennas y (lines : string list) =
        if lines.IsEmpty then
            []
        else
            // Find antennas on the current line
            let found = 
                lines.Head 
                |>  (fun line -> 
                    line 
                    |> List.ofSeq
                    |> scanLineForAntennas 0 
                    |> List.map (fun (x,freq) -> (x,y,freq))
                    )

            List.append found ( collectAntennas (y+1) lines.Tail)

    let parseAntennaMap lines = 
        let antennas = collectAntennas 0 lines
        antennas

    let calculateAntinodes dim node1 node2 =
        let (x1,y1, _) = node1
        let (x2,y2, _) = node2
        let (dx, dy) = ((x1-x2), (y1-y2))

        let (dimx, dimy) = dim
        [(x2-dx,y2-dy)]
        |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= dimx && y <= dimy)

    let calculateAntinodes2 dim node1 node2 =
        let (x1,y1, _) = node1
        let (x2,y2, _) = node2
        let (dx, dy) = ((x1-x2), (y1-y2))

        let (dimx, dimy) = dim

        // calculate the count within the map-range...
        // 11 / -1
        // 11 / -2
        // 8,1 og 5,2
        // dxy = (3,-1)
        let dxs = abs (dimx / dx)
        let dys = abs (dimy / dy)
        let dmin = min dys dxs // not exactly right, but will limit no of tries.

        let ans = seq {
            for i in 1..dmin ->
                let x = x1+dx*i
                let y = y1+dy*i
                if x >= 0 && y >= 0 && x <= dimx && y <= dimy then  // removes antinodes outside the map.
                    [(x,y)]
                else
                    []
            }
        ans |> List.concat |> List.ofSeq

    let createListOfAntinodes calculateAN dim map =
        let sameFreq = 
            map 
            |> List.groupBy (fun (x,y,freq) -> freq)

        let antinodes =
            sameFreq
            |> List.map (fun (key, value) ->
                let a1 =seq {for a in value -> a }
                let a2 =seq {for a in value -> a }

                a1 
                |> Seq.map (fun a1 ->
                    a2 |> Seq.map (fun a2 ->
                        let (x1,y2,_) = a1
                        let (x2,y2,_) = a2
                        if a1 = a2 then
                            []
                        else
                            let an =calculateAN dim a1 a2
                            //printfn "Antennas %c %A %A => antinodes : %A" key a1 a2 an
                            an
                        )
                        |> Seq.concat
                    )
                |> Seq.concat
                )
            |> Seq.concat
            |> Seq.distinct
            |> List.ofSeq
        antinodes

    let solution_8_1  (dimx, dimy) map =
        createListOfAntinodes calculateAntinodes (dimx, dimy) map
        |> List.length

    let solution_8_2  (dimx, dimy) map =
        let antinodes = 
            createListOfAntinodes calculateAntinodes2 (dimx, dimy) map
        List.append
           antinodes 
           (map |> List.map (fun (x,y,_) -> (x,y))) // antennas are also antinodes
        |> List.distinct
        |> List.length

    let solutions_8 filename =

        let maps =
            filename
            |> System.IO.File.ReadAllLines
            |> List.ofArray

        let map =
            maps 
            |> parseAntennaMap 

        let dimx = maps.Head.Length - 1
        let dimy = maps.Length - 1

        printfn "Dimension (%i, %i)" dimx dimy

        solution_8_1 (dimx, dimy) map
        |> printfn "Solution 8 1 : %i\n"

        solution_8_2 (dimx, dimy) map
        |> printfn "Solution 8 2 : %i\n"


    let rec parseFilestring isFile fileId (diskformat : char seq) = 
        if (Seq.isEmpty diskformat) then
            Seq.empty
        else
            let size = 
                (Seq.head diskformat)
                |> (fun s -> int(s)) 

            let segment =
                seq {
                    for i in 1..size -> 
                        if isFile then 
                            fileId
                        else 
                            -1
                        }

            let result = 
                Seq.append 
                    segment 
                    (parseFilestring (not isFile) (if isFile then (fileId + 1 ) else fileId) (Seq.tail diskformat))

            result

    let solution_9_1 filemap = 
        1

    let solutions_9 filename =

        let diskmap =
            filename
            |> System.IO.File.ReadAllLines
            |> List.ofArray
            |> List.head

        let filemap = 
            parseFilestring true 0 diskmap

        solution_9_1 filemap
        |> printfn "Solution 8 1 : %i\n"
        

    let puzzle n =
        match n with
        | 1 -> solutions_1 "inputs/1.txt"
        | 2 -> solutions_2 "inputs/2.txt"
        | 3 -> solutions_3 "inputs/3.txt"
        | 4 -> solutions_4 "inputs/4.txt"
        | 5 -> solutions_5 "inputs/5.txt"
        | 6 -> solutions_6 "inputs/6.txt"
        | 7 -> solutions_7 "inputs/7.txt"
        | 8 -> solutions_8 "inputs/8.txt"
        | 9 -> solutions_9 "inputs/9_test.txt"
        | _ -> printf "Ingen luke er valgt."

    [<EntryPoint>]
    let main _ =
        puzzle 6
        0

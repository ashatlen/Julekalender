namespace AoC2023.Luke

module Luke10 =
    open System
    open System.IO

    // | is a vertical pipe connecting north and south.
    // - is a horizontal pipe connecting east and west.
    // L is a 90-degree bend connecting north and east.
    // J is a 90-degree bend connecting north and west.
    // 7 is a 90-degree bend connecting south and west.
    // F is a 90-degree bend connecting south and east.
    // . is ground; there is no pipe in this tile.
    // S is the starting position of the animal;

    type Pipes =
        | Start = 1
        | Vertical = 2
        | Horizontal = 3
        | NorthEast = 4
        | NorthWest = 5
        | SouthEast = 6
        | SouthWest = 7
        | NoPipe = 0

    type Direction = 
        | North = 1
        | South = 2
        | East = 3
        | West = 4
        | Stop = 5

    let mapToPipe (s: char) =
        match s with 
        | 'S' -> Pipes.Start
        | '|' -> Pipes.Vertical
        | '-' -> Pipes.Horizontal
        | 'L' -> Pipes.NorthEast
        | 'J' -> Pipes.NorthWest
        | '7' -> Pipes.SouthWest
        | 'F' -> Pipes.SouthEast
        | '.' -> Pipes.NoPipe
        | c -> failwith (sprintf "Ukjent symbol %c!" c)

    let parseMap (lines:string[]) =
        lines 
        |> Array.map (fun l ->
            l 
            |> Array.ofSeq 
            |> Array.map mapToPipe
            |> Array.toList
        )
        |> Array.toList

    let moveNext  (x,y) fromDirection (map:Pipes list list) =
        let nextPos =
            match fromDirection with
            | Direction.North -> (x,y-1)
            | Direction.South -> (x,y+1)
            | Direction.East -> (x+1,y)
            | Direction.West -> (x-1,y)
            | _ -> failwith "Direction is invalid"

        let (nx, ny) = nextPos
        let landingPipe = map[ny][nx]            

        let nextDir =
            if landingPipe = Pipes.Start then
                Direction.Stop
            else
                match fromDirection with
                | Direction.North ->
                    match landingPipe with
                    | Pipes.SouthEast -> Direction.East
                    | Pipes.SouthWest -> Direction.West
                    | Pipes.Vertical -> fromDirection
                    | p -> failwith (sprintf "Not a possible move from %A, pipe is %A" fromDirection p)
                | Direction.South ->
                    match landingPipe with
                    | Pipes.NorthEast -> Direction.East
                    | Pipes.NorthWest -> Direction.West
                    | Pipes.Vertical -> fromDirection
                    | p -> failwith (sprintf "Not a possible move from %A, pipe is %A" fromDirection p)
                | Direction.East -> 
                    match landingPipe with
                    | Pipes.NorthWest -> Direction.North
                    | Pipes.SouthWest -> Direction.South
                    | Pipes.Horizontal -> fromDirection
                    | p -> failwith (sprintf "Not a possible move from %A, pipe is %A" fromDirection p)
                | Direction.West ->
                    match landingPipe with
                    | Pipes.NorthEast -> Direction.North
                    | Pipes.SouthEast -> Direction.South
                    | Pipes.Horizontal -> fromDirection
                    | p -> failwith (sprintf "Not a possible move from %A, pipe is %A" fromDirection p)
                | _ -> failwith "Direction is invalid"
        (nextPos, nextDir)

    let countLength xy direction (map: Pipes list list)  =
        let mutable length = 0
        let mutable nposdir = moveNext xy direction map
        let mutable stop = false
        while not stop do
            let (np, ndir) = nposdir
            let (nx, ny) = np
            Console.WriteLine (sprintf "Moving %A to pipe %A, next position is %A, moving %A\n" direction (map[ny][nx]) np ndir) 
            if ndir = Direction.Stop then
                stop <- true
            else
                length <- length + 1
                nposdir <- (moveNext np ndir map)
        length

    let findStart (map: Pipes list list) =
        let mutable start = (-1,-1)

        Console.WriteLine (sprintf "Map dimension is %i %i" (map[0].Length-1) (map.Length-1))
        for y in 0..(map.Length-1) do
            for x in 0..map[0].Length-1 do
                if map[y][x] = Pipes.Start then 
                    start <- (x,y)

        let (x,y) = start

        Console.WriteLine (sprintf "Starting at position = %A"(x,y))

        // Find the first direction
        let result = 
            if x > 0 && [Pipes.Horizontal; Pipes.NorthEast; Pipes.SouthEast] |> List.contains (map[x-1][y]) then
                Direction.West
            elif x < map[0].Length-1 && [Pipes.Horizontal; Pipes.NorthWest; Pipes.SouthWest] |> List.contains (map[x+1][y]) then
                Direction.East
            elif y > 0 && [Pipes.Vertical; Pipes.NorthEast; Pipes.NorthWest] |> List.contains (map[x][y-1]) then
                Direction.North
            else // y < map.Length-1 && [Pipes.Vertical; Pipes.NorthEast; Pipes.NorthWest] |> List.contains (map[x][y+1]) then
                Direction.South
        ((x,y),result)

    let findLoop (map: Pipes list list) =
        // Find the first step
        // Determine intial direction.
        let sxy, startDirection = findStart map
        let (x,y) = sxy
        if ( x < 0 || y < 0) then
            Console.WriteLine "Did not find a starting position"
            -1
        else
            Console.WriteLine (sprintf "Starting at %A move %A \n" sxy startDirection)
            countLength sxy startDirection map
        
    let _1 inputfile =
        let puzzleInput= File.ReadAllText(inputfile)
        let map = 
            puzzleInput.Split("\r\n") 
            |> parseMap

        let loop = findLoop map
        (loop / 2) + 1

    let _2 inputfile =
        let puzzleInput= File.ReadAllText(inputfile)
        printf "10. desember 2 input file %s" inputfile
        -1

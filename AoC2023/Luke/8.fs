namespace AoC2023

module Luke8 =
    open System
    open System.IO


    // let mutable (nodes:Map<string, (string*string)>) = Map([])
    // let mutable (directions:char array) = [] |> List.toArray

    let parseNode (n:string) =
        let tokes = n.Split(" = ")
        let junction = tokes[1].Split(", ")
        let l = junction[0].Substring(1)
        let r= junction[1].Substring(0,junction[1].Length-1)
        (tokes[0], (l,r))
        

    let parseInput (i: string)=
        let sections= i.Split("\r\n\r\n")

        let nl =
            sections[1].Split("\r\n")
            |> Array.map parseNode

        // let lastNode = nl.[nl.Length-1]
        // printf "Last node = %A" lastNode

        let nodes =
            nl
            |> Map<string,(string*string)>

        let dirs = sections[0] |> List.ofSeq
        (dirs , nodes)

    // let gotoNextNode (fromNode:string) (diridx:int) =
    //     let d = directions[diridx]
    //     let node = nodes.Item(fromNode)
    //     if d = 'L' then fst node else snd node

//     let rec navigateToNode (fromNode:string) (targetNode:string) (diridx:int) cnt =
//          if fromNode = targetNode then
//             0
//         else
//             //let next = gotoNextNode fromNode diridx directions nodes        
//             let d = directions[diridx]
//             let node = nodes.Item(fromNode)
//             let next = if d = 'L' then fst node else snd node
//             let nextdir = if diridx >= directions.Length - 1 then 0 else diridx + 1
//             1 + (navigateToNode next targetNode nextdir (cnt + 1))

    let navigateToNodeLoop (dirs:char list) (ns: Map<string,(string*string)>) startNode (isTargetNode: string -> bool) =

        let mutable fromNode = startNode
        let mutable cnt = 0
        let mutable diridx= 0
        let mutable stop = false

        while not stop do
            cnt <- (cnt + 1)
            let d = dirs[diridx]
//            printf "%i node %s (%c)  dirIdx= %i \n" cnt fromNode d diridx
            let tn = ns.TryFind(fromNode)
            if tn = None then
                printf "Kan ikke finne node %s\n" fromNode
                stop <- true //avslutt, algoritmenfeil?
            else
                let nextNode = if d = 'L' then fst tn.Value else snd tn.Value
                fromNode <- nextNode
                diridx <- if diridx >= dirs.Length - 1 then 0 else diridx + 1
                stop <- (cnt > 40000) || (isTargetNode fromNode)

        let tn = ns.TryFind(fromNode)
//        printf "Neste node fra %s er node %A\n" fromNode tn.Value

        (cnt, diridx)

    let calculateLoopCycles (dirs:char list) (ns: Map<string,(string*string)>) =

        let startingNodes = 
            ns 
            |> Map.filter 
                (fun k n ->  k.EndsWith "A")
            |> Map.map (fun k v -> k)
            |> Map.toList
            |> List.map (fun (k,v) -> k) 

//        printf "startingNodes # %i \n" startingNodes.Length
        let findIndex (arr:(int*int) seq) elem = 
                let f =
                    arr 
                    |> Seq.filter (fun (s,d) -> d = elem)
                if Seq.isEmpty f then
                    None
                else
                    Some (Seq.head f)
        let findLoopCycle startNode = 
            let mutable endIdx = [] |> Seq.ofList
            
            let mutable (dejavu:(int*int) Option) = None
            while dejavu.IsNone do
                let (steps, diridx) = navigateToNodeLoop dirs ns startNode (fun n -> n.EndsWith("Z"))
                dejavu <- findIndex endIdx diridx
                // if dejavu.IsSome then
                //     printf "Found a cycle: %A\n\n" dejavu.Value
                // else
                //     printf "steps, diridx # %i %i \n" steps diridx
                if not dejavu.IsSome then
                    endIdx <- Seq.append endIdx [(steps,diridx)]

            dejavu.Value

        let loops =
            startingNodes |> List.map (fun k -> 
                let (steps, _) = findLoopCycle k
                printf "steps # %i \n" steps
                steps
                )

        loops

    let _1 (input:string) =
        // Prøver å spare minne. Vet ikke hvorfor stacken går full?
        let (dirs, ns) =parseInput input
        let (steps, _) = navigateToNodeLoop dirs ns "AAA" (fun k -> k = "ZZZ")
        printf "8. desember 1 %i\n" steps
    
    let primefactors x = 

        let check = seq { let limit = uint64((ceil(sqrt(float(x)))));
                        for x in Seq.concat [seq { yield 2UL }; { 3UL .. 2UL .. limit }] do
                            yield x }

        let getFirstOrDefault def ns =
            if ns |> Seq.isEmpty then
                def
            else
                ns |> Seq.head

        let nextfactor x = 
            match x with
            |1UL -> x
            |_ -> check
                |> Seq.skipWhile(fun idx -> x % idx <> 0UL)
                |> getFirstOrDefault x

        let rec getfactors x factors =
            match nextfactor x  with
            |1UL -> factors
            |factor -> (factors |> List.append [factor]) |> getfactors (x / factor)

        [] |> getfactors x


    let _2 input =
        let (dirs, ns) =parseInput input

        // let znodes = 
        //     ns 
        //     |> Map.filter 
        //         (fun k n ->  k.EndsWith "Z")
        //     |> Map.map (fun k v -> k)
        //     |> Map.toList
        //     |> List.map (fun (k,v) -> k) 
        // printf "Det er %i sluttnoder (som ender på Z)\n" znodes.Length

        let steps = calculateLoopCycles dirs ns

        let primes = 
            steps 
            |> List.map (fun s -> 
                let r= primefactors (uint64 s)
//                printf "Factors %A\n" r
                r
                )
            // |> List.map (fun l -> 
            //     l 
            //    |> List.tail  // Første faktor er felles i datasettet. fjerner det...
            //    |> List.head)
            |> List.concat
            |> Set.ofList

        printf "All different factors: %A" primes
        // 14631604759649UL

        let leastCF = primes |> Set.fold (fun p n -> p * uint64 n) 1UL
        printf "Least CF %A\n" leastCF

        // too low
        // 51701783603UL
        let result = leastCF

        // let largestCF = steps |> List.fold (fun p n -> p * uint64 n) 1UL
        // printf "Larges CF %A\n" largestCF 
        //too high 8291628133348365195UL

        // Felles faktorer?
        //let primes = primefactors result
        //too low 791014139

        printf "8. desember 2 %A\n" result // 14631604759649


    let puzzle =
        let puzzleInput= File.ReadAllText(@"dec8.txt")
        let puzzleInputt= File.ReadAllText(@"dec8t.txt")

        //_1 puzzleInput
        _2 puzzleInput
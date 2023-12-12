namespace AoC2023

module Luke7 =
    open System
    open System.IO



    let (sortedHand:char list) = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']

    let (sortedHandJoker:char list) = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']

    let buildFrequencyMap  hand =
        let fm = 
            sortedHandJoker 
            |> List.fold 
                (fun (freqList: (char*int) list) c -> (c,0) :: freqList)
                []
        let frequencyMap =
             hand 
             |> Array.ofSeq
             |> Array.fold (fun (hm:(char*int) list) x ->
                 let idx = 
                    List.findIndex 
                        (fun elt -> (fst elt) = x)
                        hm
                 let (c,f) = hm[idx]
                 let newItem = (c,f+1)
                 if idx = 0 then
                     newItem :: hm.Tail
                 else
                    newItem :: hm[idx+1..] |> List.append hm[0..(idx-1)] 
                ) 
                fm
        frequencyMap

    let findHandRank (hand: string) =

        let frequencyMap = buildFrequencyMap hand

        let hv = 
            frequencyMap 
            |> List.map snd
            |> List.sortDescending

        let rank =
            match hv[0] with
            | v when v >= 4 -> v + 2 // 4 eller 5 like => rank == 6 or 7
            | v when v = 3  -> hv[1] + 3 // full house =5, tree of a kind =4 
            | v when v = 2  -> hv[1] + 1 // two pairs = 3, one pair = 2
            | _ -> 1

        rank

    let analyseHandRankJoker (hand: string) =

        let frequencyMap = buildFrequencyMap hand

        let jokerCnt = 
            frequencyMap 
            |> List.find (fun (c,f) -> 'J' = c) 
            |> snd

        let hv = 
            frequencyMap 
            |> List.map snd
            |> List.sortDescending

        let rank =
            match hv[0] with
            | v when v = 4 -> 
                if jokerCnt = 4 then 7
                else (v+jokerCnt) + 2 // 4 eller 5 like => rank == 6 or 7
            | v when v = 3  -> 
                    // Spesial, dersom flest jokere...
                    if jokerCnt = 3 then
                        5 + hv[1]   // 1 -> fire like, 2 -> fem like)
                    elif jokerCnt > 0 then
                         2 + v + jokerCnt   // blir 4 eller 5 like
                    else
                        hv[1] + 3 // full house =5, tree of a kind =4 
            | v when v = 2  ->
                    match jokerCnt with
                    | 0 -> hv[1] + 1    // two pairs = 3, one pair = 2
                    | 1 -> hv[1] + 3    // 2 par -> full house = 5 eller 1 par -> 3 like = 4
                    | 2 -> if hv[1] = 2 then 6
                           else 4 // 3 like
                    | _ -> 4 + jokerCnt // 1 par + 2 -> 4, +3 -> 5 like. ( 6 eller 7)
            | v ->
                 match jokerCnt with
                 | 0 -> v + jokerCnt    // 1
                 | 1 -> v + jokerCnt    // et par
                 | 2 -> 4               // tre like
                 | 3 -> 6               // fire like
                 | _ -> 7               // fem like
        (hv,rank, findHandRank hand)
    
    let findHandRankJoker (hand: string) =
        let (hv, rankjoker, rank) = analyseHandRankJoker hand
//        printf "Hand %s - frq %A - rank %i\n" hand hv rank
        (rankjoker, rankjoker - rank)

    let cardRankCompare hand chal =
        let (hl, cl) =
            [for i in 0..5 -> i]
            |> List.map (fun i ->
                let h = sortedHand |> List.findIndex hand[i]
                let c = sortedHand |> List.findIndex chal[i]
                (h,c)
                )
            |> List.unzip
        let hn = hl[1..] |> List.fold (fun s h -> s*10 + h ) hl[0] 
        let cn = cl[1..] |> List.fold (fun s h -> s*10 + h ) cl[0] 
        if hn > cn then 1 else -1

    let cardRankImpl (hand:string) sortOrder =
        hand 
        |> Array.ofSeq 
        |> Array.fold (fun s i -> 
            let idx = 
                sortOrder 
                |> List.findIndex (fun elt -> elt = i)
            s + (sprintf "%02i" (13-idx))    // første er størst..
            )
            ""

    let cardRank (hand:string) =
        sortedHand |> cardRankImpl hand

    let cardRankJoker (hand:string) =
        sortedHandJoker |> cardRankImpl hand

    let analyseHandScore cardRank (hr1:(string*int)) =
        let (h1,r1) = hr1
        (sprintf "%i" r1), (cardRank h1)

    let handScore (hr1:(string*int)) =
        let (r,s) = analyseHandScore cardRank hr1
        r + s

    let handScoreJoker (hr1:(string*int)) =
        let (r,s) = analyseHandScore cardRankJoker hr1
        r + s

    let _1 (input:string list) =
        let hands = 
            input 
            |> List.map (fun h -> 
                let hb = h.Split " "
                let (hand, bid) = (hb[0], Int32.Parse hb[1])
                let hr = findHandRank hand
                let hs = handScore (hand, hr)
                (bid, hs) //(sh,bid)
                )
            |> List.sortBy (fun itm -> snd itm)

        let score =
            hands
            |> List.zip [for i in 1..input.Length -> i]
            |> List.map (fun (i, (bid, hs)) -> bid * i)
            |> List.sum

//            |> List.map fst
        printf "7. desember 1 %i\n" score
        //250453939

    let _2 (input:string list) =
        let hands = 
            input 
            |> List.map (fun h -> 
                let hb = h.Split " "
                let (hand, bid) = (hb[0], Int32.Parse hb[1])
                let (hr, elevation) = findHandRankJoker hand
                let rc = analyseHandScore cardRankJoker (hand, hr)
                (hand, bid, rc, elevation) //(sh,bid)
                )
            |> List.sortBy (fun itm -> 
                let (_,_,(r, c),_) = itm
                r + c
            )

        hands 
        |> List.iter (fun itm ->
            let (hand,bid,(r,c),elevation) = itm
            printf "Hand %s score %s %s elevation %i\n" hand r c elevation
        )

        let result =
            hands
            |> List.zip [for i in 1..input.Length -> i]
            |> List.map (fun (i, (hand, bid, hs, _)) -> bid * i)

        let score =
            result
            |> List.sum

//            |> List.map fst
//  249689094 - feil, for høyt
//  248652697 - riktig!
        printf "7. desember 2 %i\n" score

    let puzzle =
        let puzzleInput= File.ReadAllLines(@"dec7.txt") |> List.ofArray
        let puzzleInputt= File.ReadAllLines(@"dec7t.txt") |> List.ofArray

        // ["JA8J2";"AJAJJ";"AJAAJ";"J2JJJ"]
        // |> List.iter (fun hand ->
        //     let (rank, elevation) =findHandRankJoker hand
        //     printf "Hand %s rank %i joker elevation %i\n" hand rank elevation
        // )
        // _1 puzzleInput
        _2 puzzleInput

namespace AoC2023

module Luke6 =
    open System
    open System.IO

    let _1 (input : (int*int) list) =
        let result = 
            input
            |> List.map (fun (tot,record)->
                let c = 
                    [for t in 1..tot -> t]
                    |>  List.map(fun t ->
                        let speed = t
                        let movingTime = tot - t
                        let distance = t * movingTime
                        distance
                        )
                    |> List.map (fun d -> d > record)
                    |> List.filter (fun f -> f) 
                c.Length
                )
            |> List.fold (fun state c -> state *c) 1
        printf "6. desember 1 %i" result

    let _2 (input:(int64*int64) list) =
        let result = 
            input
            |> List.map (fun (tot,record)->
                let c = 
                    [for t in 1L..tot -> t]
                    |>  List.map(fun t ->
                        let speed = t
                        let movingTime = tot - t
                        let distance = t * movingTime
                        distance
                        )
                    |> List.map (fun d -> d > record)
                    |> List.filter (fun f -> f) 
                c.Length
                )
            |> List.fold (fun state c -> state *c) 1
        printf "6. desember 2 %i" result

    let puzzle =
        // let puzzleInput= [(7,9);(15,40);(30,200)]
        // let puzzleInput= [(49,356);(87,1378);(78,1502);(95,1882)]
        // _1 puzzleInput

        // let puzzleInput= [(71530,940200)]
        let puzzleInput = [(49877895L,356137815021882L)]
        _2 puzzleInput

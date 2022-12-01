
module Tredje

    //type HateCount = Map<string, int>
    //let addHateCount = fun fromF toF (hates: HateCount) ->
    //    hates 
    //    |> Map.add fromF (if hates.ContainsKey(fromF) 
    //        then hates.Item(fromF) |> (fun rs -> rs + 1) 
    //        else 1
    //        )

    //let parseHateCount = fun hates (token:string[]) ->
    //    if (token.[1]) = "hates" then 
    //        hates |> addHateCount (token.[0]) (token.[2])
    //    else
    //        hates
    //let parseHatesOnly = fun (lines) ->
    //    lines |> List.fold parseHateCount Map.empty


    open System
    open System.Numerics
    open System.IO

    type FriendRelations = Map<string, bool>
    type PersonEntryList = Map<string, FriendRelations>

    let addFriend = fun fromF toF (friends: PersonEntryList) ->
         friends 
         |> Map.add fromF
                (if (friends.ContainsKey fromF) then
                    friends.Item(fromF) |> Map.add toF false
                else
                    Map.empty |> Map.add toF false
                )

    let addHate = fun fromF toF (friends: PersonEntryList) ->
        friends |> Map.add fromF (friends.Item(fromF) |> (fun rs -> rs |> Map.add toF true))


    let parseFriendItem = fun friends (token:string[]) ->
        if token.[0] = "friends" then
            friends
            |> addFriend (token.[1]) (token.[2])  
            |> addFriend (token.[2]) (token.[1])  
        else
            friends

    let parseHateItem = fun friends (token:string[]) ->
        if (token.[1]) = "hates" then 
            friends |> addHate (token.[0]) (token.[2])
        else
            friends

    let tokenizeLine = fun (line:string) ->
        let tokens= line.Split ' '
        tokens |> Array.filter (fun i -> i.Length > 0)

    let tokeniseFile = fun filename ->
        let streamReader= File.OpenText(filename)

        let mutable lines:list<string[]> = list.Empty
        while not streamReader.EndOfStream do
            let l= streamReader.ReadLine() |> tokenizeLine 
            lines <- List.append lines [l]
        lines

    let parseAllRelationships= fun (lines) ->

        let f= lines |> List.fold parseFriendItem Map.empty
        lines |> List.fold parseHateItem f

    
    let TredjeDesember = fun () ->
        let lines= tokeniseFile "FriendsHates.txt"
        let data= parseAllRelationships lines
   
        let orderedData=
            data
            |> Map.map (fun person friends -> 
                    friends
                    |> Map.toList 
                    |> (fun x -> (x.Length, x |> List.filter (fun (f, h) -> h) |> (fun hates -> hates.Length)))
                    )
            |> Map.toList
            |> List.sortByDescending (fun (person, (friends, hates)) -> hates)

        orderedData |> List.iter (fun (person, (friends, hates)) -> printfn "%s has %d relations, but hates %d" person friends hates)

        let friendship= (orderedData |> List.sumBy ( fun (p, (f, h)) -> f)) / 2
        let hates = (orderedData |> List.sumBy ( fun (p, (f, h)) -> h))

        printfn "Sum of friendships %d, hates %d, in total %d" friendship hates (friendship + hates)

        printfn "Got %d persons, the worst cameleon is %s" orderedData.Length (orderedData.[0] |> fun (p, c) -> p)

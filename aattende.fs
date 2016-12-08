namespace Julekalender

module Åttende= 

    open System
    open System.IO

    let ladderList = 
        ([(3,17); (8,10); (15,44); (22,5); (39,56); (49,75); (62,45); (64,19); (65,73); (80,12); (87,79)]:(int*int) list) 
        |> Map.ofList

    let movePlayer ladders currentLoc steps=
        let nextMove= if (currentLoc + steps) <= 90 then currentLoc + steps else currentLoc
            
        if (ladderList.ContainsKey(nextMove) ) then
            (ladderList.Item(nextMove), 1)
        else
            (nextMove, 0)

    let rec calculateMoves (playerLocations:Map<int,int>) ladderCount (dieThrowRepo:StreamReader) currentPlayerNo =
        let nextDieThrow=  System.Int32.Parse(dieThrowRepo.ReadLine())
        let (nextLocation,usedLadder) = movePlayer ladderList (playerLocations.Item currentPlayerNo) nextDieThrow
        if (nextLocation = 90 ) then
            (ladderCount, currentPlayerNo)
        else
            let nextPlayerNo = if currentPlayerNo = 1337 then 1 else currentPlayerNo + 1
            let playerLocations' = playerLocations |> Map.add currentPlayerNo nextLocation
            calculateMoves playerLocations' (ladderCount + usedLadder) dieThrowRepo nextPlayerNo

    let snakeAndLadder dieThrowsFile =
        let streamReader= File.OpenText(dieThrowsFile)
        let playerLocations= Map.ofArray [|for (i:int) in 1..1337 -> (i, 1)|]
        let (ladderCount, currentPlayerNo) = (calculateMoves playerLocations 0 streamReader 1)
        printfn "Player %d won, %d ladders was used, result is %d" ladderCount currentPlayerNo (ladderCount * currentPlayerNo)



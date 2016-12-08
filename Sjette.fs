namespace Julekalender
module Sjette=

    open System
    open System.Numerics
    open System.IO

    type Position= {
        x: int64
        y: int64
        verdi: int64
        }
    type PositionMap= Map<int64*int64, Position>

    let lagPosition x y= {x= x; y=y; verdi= x+y}
    let finnAvstand p1 p2 = Math.Abs(p2.x - p1.x) + Math.Abs(p2.y - p1.y)

    let muligeFlytt (from:Position)= 
        [
        lagPosition (from.x-2L) (from.y-1L);
        lagPosition (from.x-2L) (from.y+1L);
        lagPosition (from.x-1L) (from.y-2L);
        lagPosition (from.x-1L) (from.y+2L);
        lagPosition (from.x+1L) (from.y-2L);
        lagPosition (from.x+1L) (from.y+2L);
        lagPosition (from.x+2L) (from.y-1L);
        lagPosition (from.x+2L) (from.y+1L);
        ]

    let rec flyttSpringer (from:Position) (visited:PositionMap) (numLeaps:int64) (maxLeap:int64)=
        if (numLeaps < 1000L) then
            let nesteFlytt= 
                    muligeFlytt from
                    |> Seq.map (fun p -> match visited.TryFind (p.x, p.y) with
                                            | Some v -> v
                                            | None -> p
                                            )
            let minsteHopp= nesteFlytt |> Seq.sortByDescending (fun p -> from.verdi - p.verdi) |> Seq.head

            printfn "Flytter springer til (%d, %d) ganger minste verdi er %d" minsteHopp.x minsteHopp.y maxLeap

            let visited'= visited |> Map.add (from.x, from.y) {from with verdi= if from.verdi = 1000L then 0L else 1000L}
            let maxLeap'= Math.Max(maxLeap, (finnAvstand minsteHopp from))
            flyttSpringer minsteHopp visited' (numLeaps+1L) maxLeap'
        else
            maxLeap

    let springerFlytting= fun() ->
        let moves = 1L
        let maxLeap = flyttSpringer (lagPosition 0L 0L) Map.empty 0L 0L
        printfn "6. Flyttet springer %d ganger største avstand i hoppene var %d..." 100 maxLeap

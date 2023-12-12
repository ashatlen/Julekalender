namespace AoC2023

module Luke10 =
    open System
    open System.IO

    let _1 input =
        printf "10. desember 1 %s" "TBS"

    let _2 input =
        printf "10. desember 2 %s" "TBS"

    let puzzle =
        let puzzleInput= File.ReadAllText(@"dec10.txt")
        let puzzleInputt= File.ReadAllText(@"dec10t.txt")
        _1 puzzleInputt
        //_2 puzzleInputt

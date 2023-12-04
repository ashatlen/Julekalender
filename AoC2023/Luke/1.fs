namespace AoC2023

module Luke1 =
    open System
    open System.IO

    let rec last = function
        | hd :: [] -> hd
        | hd :: tl -> last tl
        | _ -> failwith "Empty list %s."
    let findNumbers = fun ( line : string ) ->
        line 
        |> Seq.toList
        |> List.map (fun c ->
            if Char.IsDigit(c) then
                Some c
            else
                None
            )
        |> List.choose id

    let _1_1 lines = 
        let numbers = 
            lines
            |>  Seq.map (fun l ->
                let ns = findNumbers l
                match ns with
                | f::[]-> Int32.Parse (Array.ofList [f ; f])
                | f::r -> Int32.Parse (Array.ofList [f; last r])
                | [] -> failwith "The array should always have content."
                )
        
        printf "1.desember 1 %i\n" (Seq.sum numbers)

    let _1_2 lines =

        let rec transformMnems i (line: string) =
            let digits = [
                ("o1e","one"); 
                ("t2o","two"); 
                ("t3e","three");
                ("4","four"); 
                ("5e","five"); 
                ("6","six"); 
                ("7n","seven"); 
                ("e8t","eight"); 
                ("n9e","nine")]

            if i >= digits.Length then 
                line
            else
                let (digit, mnem) = digits[i]
                let (line', i') = 
                    let idx= line.IndexOf(mnem)
                    if idx = 0 then
                        (digit + line[idx+mnem.Length..line.Length-1], i)
                    elif idx > 0 then
                        (line[0..idx-1] + digit + line[idx+mnem.Length..line.Length-1], i)
                    else
                        (line, i+1)
                transformMnems i' line'

        let numbers = 
            lines
            |>  Seq.map (fun l ->
                let xlatl = transformMnems 0 l 
                let ns = findNumbers xlatl
                let fila =
                    match ns with
                    | f::[]-> Int32.Parse (Array.ofList [f ; f])
                    | f::r -> Int32.Parse (Array.ofList [f; last r])
                    | [] -> failwith "The array should always have content."

                printf "%s\n" xlatl // + " " + (sprintf "%i" fila))
                fila
                )
        
        printf "1.desember 1_2 %i\n" (Seq.sum numbers)

    let _1 =
        let lines= File.ReadAllLines(@"dec1.txt") |> Seq.ofArray 

//         let testInput = @"1abc2
// pqr3stu8vwx
// a1b2c3d4e5f
// treb7uchet"
//         let linestest = testInput.Split "\n" |> Seq.ofArray
//        _1_1 lines

        let testlines= @"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineightseven2
zoneight234
7pqrstsixteen"
//        let lines = testlines.Split "\n" |> Seq.toArray

        _1_2 lines

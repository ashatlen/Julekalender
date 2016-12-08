namespace Julekalender

module Sjuende=

    open System.IO

    let parseInstructions (line:string)= 
        let tokens= line.Split(' ') |> Array.toList
        match tokens with
        | ["walk"; l; "meters"; direction] -> 
                let length= System.Int32.Parse l
                match direction with
                | "north" -> (length, 0)
                | "south" -> (-length, 0)
                | "west" -> (0, length)
                | "east" -> (0, -length)
                | _ -> failwith "Wrong direction"
        | _ -> failwith (sprintf "Could not parse line! %s" line)

    let rec readInstructions (streamReader:StreamReader)=
        if (streamReader.EndOfStream) then
            []
        else
            let line= streamReader.ReadLine()
            let instruction= (parseInstructions line)
            List.append [instruction] (readInstructions streamReader)

    let summereAlvesteg filename =
        let streamReader= File.OpenText(filename)
        let instructions = readInstructions(streamReader)
        let (dx, dy) = instructions |> List.fold (fun (sx, sy) (px, py) -> (sx+px, sy+py)) (0,0)
        printfn "Leste %d instruksjoner (%d %d)." instructions.Length dx dy



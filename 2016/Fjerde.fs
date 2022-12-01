namespace Julekalender

module Fjerde=

    open System
    open System.Numerics
    open System.IO

    let isClapNumber num = 
        let sv= sprintf "%d" num
        num > 0 && num % 7 = 0 || (sv.IndexOf('7') >= 0)

    let append arr elt = List.append arr [elt]

    let rec checkClapNumbers idx (arr:int list) =
        if arr.Length = 1338 then
            (idx, arr)
        else 
            let (newidx, value) = 
                    if isClapNumber (arr.Length+1) 
                        then 
                            (idx+1, arr.[idx])                        
                        else 
                            (idx, arr.Length+1)
            checkClapNumbers newidx (append arr value)
        

    let FjerdeDesember = fun () ->
        let (idx,xs)= checkClapNumbers 0 [1]
        printfn "Element nummer %d har verdien %d (%d verdier er byttet ut)" 1337 xs.[1336] idx

    let rec getClapNumberUsedCount number =
        if number < 7 then
            0
        else 
            (if isClapNumber number then 1 else 0) + (getClapNumberUsedCount (number - 1))

    let rec calculateClapNumber number = 
        if isClapNumber number then
            let used= getClapNumberUsedCount number
            calculateClapNumber used
        else
            number

    let FjerdeDesemberStateless = fun () ->
            printfn "Element nummer %d har verdien %d" 1337 (calculateClapNumber 1337)

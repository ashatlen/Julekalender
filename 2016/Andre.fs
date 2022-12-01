
module Andre

open System
open System.Numerics
open System.IO


let fib = fun t1 t2 -> t1 + t2

let rec sumOfFib = fun (sum:int64) (firstFib:int64) (secondFib:int64) ->
//        printfn "Fibonaccitest %d %d == %d" 1 2 (fib 1L 2L)
    let nextFib= fib firstFib secondFib
    if nextFib > 4000000000L then 
        printfn "stopper på %d" nextFib
        sum
    else    
        let sum= if nextFib % 2L = 0L 
                    then 
                        if nextFib < 300L then 
                            printfn "Tar med %d" nextFib
                        sum + nextFib 
                    else 
                        sum
        sumOfFib sum secondFib nextFib

let andreDesember = fun () ->
    let result= sumOfFib 0L 1L 1L
    printfn "Fibonaccitallene som er partall og under 4 mrd, er summert %d" result


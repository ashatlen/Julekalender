namespace Julekalender

module Niende=

    open System
    open System.IO

    let rec registerTranxations (accounts:Map<Guid, int>) (transactions:StreamReader)=
        let transaction= transactions.ReadLine().Split(' ') |> Array.toList
        match transaction with
        | ["None";accName;amount] -> 
            let toAccount= Guid.Parse accName
            let currBalance= 
                match (Map.tryFind toAccount accounts ) with 
                | Some x -> x
                | _ -> 0

            transactions |> Map.add 
        | ["toAccount";toAccount;amount] ->
            


        accounts

    let transaksjonshistorie = 
        let transaksjons= File.OpenText("Transaksjoner.txt")
        registerTranxations Map.empty transaksjons
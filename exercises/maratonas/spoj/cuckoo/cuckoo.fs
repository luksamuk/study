// http://www.spoj.com/problems/CUCKOO/
// Resultado: wrong answer

module cuckoo
open System

let ReadIntLine() =
    Console.ReadLine().Split [|' '|]
    |> Array.map (fun x -> (Int32.Parse x))

//let ReadFromConsole() =
//    Console.ReadLine().Split [|' '|]

[<EntryPoint>]
let main argv =
    let t = ReadIntLine().[0]

    for i = 0 to t - 1 do
        let out = ReadIntLine()
        let m   = out.[0]
        let n   = out.[1]

        let hashes = [|
            for j = 0 to m - 1 do
                yield ReadIntLine()
        |]

        let table = [|
            for j = 0 to n - 1 do
                yield None
        |]

        let rehash = ref false

        let rec putOnHash idx hash =
            match table.[ (hashes.[idx].[hash]) ] with
            | Some val1 ->
                match snd val1 with
                | 0 ->
                    table.[ (hashes.[idx].[hash]) ] <- Some (idx, hash)
                    putOnHash (fst val1) 1
                | _ -> rehash := true
            | None -> table.[ (hashes.[idx].[hash]) ] <- Some (idx, hash)

        for j = 0 to m - 1 do
            putOnHash j 0

        // Debug
        //printfn "DEBUG\n\
        //         m = %A\n\
        //         n = %A\n\
        //         hashes =\n    %A\n\
        //         table = \n    %A\n"
        //         m n hashes table

        match !rehash with
        | false -> printfn "successful hashing"
        | true  -> printfn "rehash necessary"
    0
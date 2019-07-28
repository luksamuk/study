// http://www.spoj.com/problems/HASH/
// Resultado: inconclusivo

module Hashing
open System

let ReadIntLine() =
    Console.ReadLine().Split [|' '|]
    |> Array.map (fun x -> (Int32.Parse x))

//let ReadFromConsole() =
//    Console.ReadLine().Split [|' '|]

let hash a b x n m = (a * (x + n)) + (b % m)

[<EntryPoint>]
let main argv =
    // Leia o primeiro número
    let input = ReadIntLine()

    // Pegue o número t
    let t = input.[0]

    // Construa arrays a partir do input
    let nums = [|
        for i = 0 to t - 1 do
            let input = ReadIntLine()
            yield [|
                for j = 0 to 6 do
                    yield input.[j]
            |]
        |]

    // Debug: Imprima as listas de números coletadas
    //printfn "Input: %A\n" nums

    // [ a; b; x; n; c; d; m ]
    // i é impresso quando c <= hash <= d
    for a in nums do
        // Debug: Mostre o caso de testes atual
        //printfn "Verificando para %A" a
        // Percorrendo para 0 <= i <= n
        for i = 0 to a.[3] do
            // hash (a, b, x, n, m)
            let result = hash a.[0] a.[1] a.[2] i a.[6]
            // Debug: Resultado
            //printfn " Parcial: %A <= %A <= %A, \
            //          onde i = %A"
            //          a.[4] result a.[5] i
            // Pattern matching para saber se
            // deve ser impresso
            match result with
            | result when
                // c <= result <= d
                   (result >= a.[4])
                && (result <= a.[5]) ->
                    // Então imprima
                    printfn "%A" i
            // Do contrário, não
            | _ -> ()

    // Return
    0

// http://www.spoj.com/problems/TEST/
// Resultado: ok

module LifeUniverseEverything
open System


[<EntryPoint>]
let main argv =
    let mutable inp = 0
    while inp <> 42 do
        inp <- Int32.Parse <| Console.ReadLine()
        match inp with
        | inp when inp = 42 -> ()
        | _ -> printfn "%d" inp
    0

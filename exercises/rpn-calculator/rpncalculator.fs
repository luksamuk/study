module rpncalculator

open System
open Stack

let Operators =
    [| "+"; "-"; "*"; "/" |]

let ReadFromConsole() =
    Console.ReadLine().Split [|' '|]

let getNext (mystack : byref<_>) =
    let value = Stack.top mystack
    mystack <- Stack.pop mystack
    value

// Main function
[<EntryPoint>]
let main argv =
    printfn "RPN Calculator v0.1"
    printfn "Parses and calculates your expressions in reverse polish notation."

    let mutable running = true
    // Parse lines
    while running do
        printf "> "
        let input = ReadFromConsole()
        let mutable operationStack = EmptyStack
        for a in input do
            match a with
            | a when Operators |> Array.contains a ->
                // Pop operands
                let operand2 = getNext &operationStack
                let operand1 = getNext &operationStack
                match a with
                | "+" ->
                    let result = operand1 + operand2
                    operationStack <- operationStack |> Stack.push result
                | "-" ->
                    let result = operand1 - operand2
                    operationStack <- operationStack |> Stack.push result
                | "*" ->
                    let result = operand1 * operand2
                    operationStack <- operationStack |> Stack.push result
                | "/" ->
                    let result = operand1 / operand2
                    operationStack <- operationStack |> Stack.push result
                | _ -> failwith "Unknown operator."
            | "quit" ->
                running <- false
            | _ ->
                operationStack <- operationStack |> Stack.push (float a)
        match running with
        | true -> printfn "Result: %f" (operationStack |> Stack.top)
        | false -> ()

    0

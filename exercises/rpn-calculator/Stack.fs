module Stack
    type 'a stack =
    | EmptyStack
    | StackNode of 'a * 'a stack
    
    let empty stack =
        match stack with
        | EmptyStack -> true
        | _          -> false
    
    let push x stack =
        StackNode(x, stack)
    
    let pop stack =
        match stack with
        | EmptyStack ->
            failwith "Empty stack; cannot pop"
        | StackNode(head, tail) -> tail
    
    let top stack =
        match stack with
        | EmptyStack ->
            failwith "Empty stack; cannot check top"
        | StackNode(head, tail) -> head

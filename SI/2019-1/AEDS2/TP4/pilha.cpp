#include <cstdio>
#include <cstring>
#include <cstdlib>



/* ============ Stack ============ */
struct stack_node_t
{
    long int      info;
    stack_node_t* prev;
};

struct stack_t
{
    stack_node_t* top;
};

stack_t
make_stack(void)
{
    // Yup, using initializer lists.
    stack_t st = { NULL };
    return st;
}

inline bool
stack_is_empty(stack_t& st)
{
    return !st.top;
}

void
stack_push(stack_t& st, long int info)
{
    stack_node_t* node = new stack_node_t;
    node->info = info;
    if(stack_is_empty(st)) {
        node->prev = NULL;
        st.top = node;
        return;
    }
    node->prev = st.top;
    st.top     = node;
}

void
stack_pop(stack_t& st)
{
    if(!stack_is_empty(st)) {
        stack_node_t* aux = st.top->prev;
        delete st.top;
        st.top = aux;
    }
}

void
stack_print(stack_t& st)
{
    const stack_node_t* itr;
    for(itr = st.top; itr != NULL; itr = itr->prev) {
        printf("%ld", itr->info);
        if(itr->prev) putchar(32);
    }
    putchar(10);
}

void
stack_clear(stack_t& st)
{
    while(!stack_is_empty(st))
        stack_pop(st);
}



/* ============ REPL ============ */
static const char* operations[] =
    { "Empilhar",
      "Desempilhar",
      "Imprimir",
      "Finalizar" };

enum operation_t
{
     PUSH_OP,
     POP_OP,
     PRINT_OP,
     FINISH_OP,
     UNKNOWN_OP
};

operation_t
parse_operation(const char* text)
{
    for(int i = 0; i < 4; i++) {
        if(!strncmp(operations[i], text, strlen(operations[i])))
            return (operation_t) i;
    }
    return UNKNOWN_OP;
}

long int
parse_operand(const char* text)
{
    return strtol(text, NULL, 10);
}


void
run_repl(stack_t& stack)
{
    bool should_finish = false;
    char buffer[255];
    long int operand_buffer;
    do {
        // Fetch operation
        scanf("%s", buffer);
        operation_t op = parse_operation(buffer);

        // Dispatch operation
        switch(op) {
        case PUSH_OP:
            scanf("%s", buffer);
            operand_buffer = parse_operand(buffer);
            stack_push(stack, operand_buffer);
            break;
        case POP_OP:
            stack_pop(stack);
            break;
        case PRINT_OP:
            stack_print(stack);
            break;
        case FINISH_OP:
            should_finish = true;
        default: break;
        };
    } while(!should_finish);
}



/* =========== Entry point =========== */
int
main(void)
{
    stack_t stack = make_stack();
    run_repl(stack);
    stack_clear(stack);
    return 0;
}

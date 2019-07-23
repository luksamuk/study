#include <cstdio>
#include <cstring>
#include <cstdlib>

// This exercise is very similar to the other one.
// The only changes are the operations and the
// data structure.

/* =========== Queue =========== */
struct queue_node_t
{
    long int      info;
    queue_node_t* next;
};

struct queue_t
{
    queue_node_t* first;
    queue_node_t* last;
};

queue_t
make_queue(void)
{
    queue_t qu = { NULL, NULL };
    return qu;
}

inline bool
queue_is_empty(queue_t& qu)
{
    return !qu.first;
}

void
queue_enqueue(queue_t& qu, long int info)
{
    queue_node_t* node = new queue_node_t;
    node->info = info;
    if(queue_is_empty(qu)) {
        node->next = NULL;
        qu.first = node;
        qu.last  = node;
        return;
    }
    qu.last->next = node;
    qu.last = node;
}

void
queue_dequeue(queue_t& qu)
{
    if(!queue_is_empty(qu)) {
        queue_node_t* aux = qu.first->next;
        if(!aux) qu.last = NULL;
        delete qu.first;
        qu.first = aux;
    }
}

void
queue_print(queue_t& qu)
{
    const queue_node_t* itr;
    for(itr = qu.first; itr != NULL; itr = itr->next) {
        printf("%ld", itr->info);
        if(itr->next) putchar(32);
    }
    putchar(10);
}

void
queue_clear(queue_t& qu)
{
    while(!queue_is_empty(qu))
        queue_dequeue(qu);
}



/* ============ REPL ============ */
static const char* operations[] =
    { "Enfileirar",
      "Desenfileirar",
      "Imprimir",
      "Finalizar" };

enum operation_t
{
     ENQUEUE_OP,
     DEQUEUE_OP,
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
run_repl(queue_t& queue)
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
        case ENQUEUE_OP:
            scanf("%s", buffer);
            operand_buffer = parse_operand(buffer);
            queue_enqueue(queue, operand_buffer);
            break;
        case DEQUEUE_OP:
            queue_dequeue(queue);
            break;
        case PRINT_OP:
            queue_print(queue);
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
    queue_t queue = make_queue();
    run_repl(queue);
    queue_clear(queue);
    return 0;
}

#ifndef LIST_H
#define LIST_H

typedef struct LIST_NODE_T {
    char *string;
    struct LIST_NODE_T *prev;
    struct LIST_NODE_T *next;
} list_node_t;

void         list_insert(list_node_t **list, char *string);
list_node_t *list_find(list_node_t *list, const char *string);
void         list_delete(list_node_t **node);

#endif

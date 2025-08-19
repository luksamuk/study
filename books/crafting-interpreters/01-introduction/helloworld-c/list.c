#include "list.h"
#include <stdlib.h>
#include <string.h>

void
list_insert(list_node_t **list, char *string)
{
    list_node_t *end = *list;
    if(end == NULL) {
        *list = end = malloc(sizeof(list_node_t));
        end->prev = end->next = NULL;
    } else {
        while(end->next != NULL)
            end = end->next;
        end->next = malloc(sizeof(list_node_t));
        end->next->prev = end;
        end->next->next = NULL;
        end = end->next;
    }

    end->string = malloc(strlen(string) * sizeof(char));
    strcpy(end->string, string);
}

list_node_t *
list_find(list_node_t *list, const char *string)
{
    if(list == NULL) return NULL;
    list_node_t *itr = list;
    do {
        if(strcmp(string, itr->string) == 0) break;
        itr = itr->next;
    } while(itr != NULL);
    return itr;
}

void
list_delete(list_node_t **node)
{
    if(*node == NULL) return;

    list_node_t *aux = *node;
    if(aux->prev != NULL) aux->prev->next = aux->next;
    if(aux->next != NULL) aux->next->prev = aux->prev;
    free(aux->string);
    *node = aux->next;
    free(aux);
}

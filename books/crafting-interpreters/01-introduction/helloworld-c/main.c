#include <stdio.h>
#include "list.h"

int
main(void)
{
    printf("Hello, world!\n");

    list_node_t *list = NULL;
    list_insert(&list, "Hello");
    list_insert(&list, "Cruel");
    list_insert(&list, "World");

    printf("List contents:\n");
    for(const list_node_t *itr = list; itr != NULL; itr = itr->next) {
        printf("  - %s\n", itr->string);
    }
    putchar('\n');

    printf("Find word 'Cruel': ");
    list_node_t *match = list_find(list, "Cruel");
    printf("%s\n", (match != NULL) ? "Found" : "Not found");

    printf("Remove word 'Cruel'... ");
    list_delete(&match);
    printf("Done\n");

    printf("Find word 'World'... ");
    match = list_find(list, "World");
    printf("%s\n", (match != NULL) ? "Found" : "Not found");

    printf("List contents backwards:\n");
    for(const list_node_t *itr = match; itr != NULL; itr = itr->prev) {
        printf("  - %s\n", itr->string);
    }
    putchar('\n');

    printf("Clear list... ");
    int i = 0;
    while(list != NULL) {
        list_delete(&list);
        printf("%d... ", ++i);
    }
    printf("Done\n");

    printf("Check if list was cleared... %s\n",
           (list == NULL) ? "Yes" : "No");
    
    return 0;
}

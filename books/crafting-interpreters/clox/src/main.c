#include <clox/foo.h>
#include <clox/linked_list.h>
#include <stdio.h>

int
main(void)
{
    int result = sum(2, 3);
    printf("Result = %d\n", result);

    linkedlist list = make_linkedlist();
    linkedlist_insert(&list, "foo");
    linkedlist_insert(&list, "bar");
    linkedlist_insert(&list, "test");

    struct LINKEDLIST_NODE *aux;

    aux = list.root;
    while(aux) {
        printf("%s\n", aux->info);
        aux = aux->next;
    }

    linkedlist_delete(&list, "foo");

    printf("Number of elements: %u\n", linkedlist_count(&list));
    
    aux = list.root;
    while(aux) {
        printf("%s\n", aux->info);
        aux = aux->next;
    }
    
    printf("Number of elements: %u\n", linkedlist_count(&list));

    destroy_linkedlist(&list);

    return 0;
}


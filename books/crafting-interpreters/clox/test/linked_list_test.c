#include <criterion/criterion.h>
#include <clox/linked_list.h>

Test(linked_list, create_and_delete) {
    linkedlist list = make_linkedlist();
    cr_assert_eq(NULL, list.root);

    linkedlist_insert(&list, "foo");
    cr_assert_neq(NULL, list.root);
    
    destroy_linkedlist(&list);
    cr_assert_eq(NULL, list.root);
}

Test(linked_list, insert) {
    linkedlist list = make_linkedlist();
    
    linkedlist_insert(&list, "foo");
    linkedlist_insert(&list, "bar");
    linkedlist_insert(&list, "baz");
    
    cr_assert_eq(3, linkedlist_count(&list));

    linkedlist_insert(&list, NULL);
    cr_assert_eq(3, linkedlist_count(&list));
    
    destroy_linkedlist(&list);
}

Test(linked_list, find) {
    linkedlist list = make_linkedlist();
    
    linkedlist_insert(&list, "foo");
    linkedlist_insert(&list, "bar");
    linkedlist_insert(&list, "baz");
    
    cr_assert(linkedlist_find(&list, "baz"));
    cr_assert(linkedlist_find(&list, "bar"));
    cr_assert(linkedlist_find(&list, "foo"));
    cr_assert(!linkedlist_find(&list, "quux"));
    cr_assert(!linkedlist_find(&list, "barrr"));
    cr_assert(!linkedlist_find(&list, NULL));
    
    destroy_linkedlist(&list);
}

Test(linked_list, delete) {
    linkedlist list = make_linkedlist();
    
    linkedlist_insert(&list, "foo");
    linkedlist_insert(&list, "bar");
    linkedlist_insert(&list, "baz");
    
    cr_assert_eq(3, linkedlist_count(&list));

    linkedlist_delete(&list, "baz");
    cr_assert(!linkedlist_find(&list, "baz"));
    cr_assert_eq(2, linkedlist_count(&list));

    linkedlist_delete(&list, "foo");
    cr_assert(!linkedlist_find(&list, "foo"));
    cr_assert_eq(1, linkedlist_count(&list));

    linkedlist_delete(&list, "bar");
    cr_assert(!linkedlist_find(&list, "bar"));
    cr_assert_eq(0, linkedlist_count(&list));
    
    cr_assert_eq(NULL, list.root);
    
    destroy_linkedlist(&list);
}

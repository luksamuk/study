#ifndef LINKED_LIST_H_INCLUDED
#define LINKED_LIST_H_INCLUDED

struct LINKEDLIST_NODE {
    char *info;
    struct LINKEDLIST_NODE *prev;
    struct LINKEDLIST_NODE *next;
};

typedef struct {
    struct LINKEDLIST_NODE *root;
} linkedlist;

linkedlist make_linkedlist();
void       linkedlist_insert(linkedlist*, const char*);
int        linkedlist_find(linkedlist*, const char*);
void       linkedlist_delete(linkedlist*, const char*);
void       linkedlist_clear(linkedlist*);
unsigned   linkedlist_count(linkedlist*);
void       destroy_linkedlist(linkedlist*);

#endif

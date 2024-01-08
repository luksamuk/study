#include <clox/linked_list.h>

#include <stdlib.h>
#include <string.h>

typedef struct LINKEDLIST_NODE node;

node *get_end(node *root);
node *make_node(const char *txt);
node *find_info(node *root, const char *text);
void destroy_node(node *n);
void clear_nodes_from(node *n);
unsigned node_count_from(node *n);

linkedlist
make_linkedlist()
{
    linkedlist l;
    l.root = NULL;
    return l;
}

void
destroy_linkedlist(linkedlist *l)
{
    linkedlist_clear(l);
}

void
linkedlist_insert(linkedlist *l, const char *txt)
{
    if(!txt) return;
    
    node *n = make_node(txt);
    
    if(!l->root) {
        l->root = n;
        return;
    }
    
    node *end = get_end(l->root);
    end->next = n;
    n->prev = end;
}

int
linkedlist_find(linkedlist *l, const char *txt)
{
    if(!txt) return 0;
    
    node *n = find_info(l->root, txt);
    return (n != NULL);
}

void
linkedlist_delete(linkedlist *l, const char *txt)
{
    if(!txt) return;

    node *n = find_info(l->root, txt);
    if(!n) return;

    if(!n->prev) l->root = n->next;

    destroy_node(n);
}

void
linkedlist_clear(linkedlist *l)
{
    clear_nodes_from(l->root);
    l->root = NULL;
}

unsigned
linkedlist_count(linkedlist *l)
{
    return node_count_from(l->root);
}


// ==============================

node *
get_end(node *root)
{
    while(root->next) {
        root = root->next;
    }
    return root;
}

node *
make_node(const char *txt)
{
    unsigned len = strlen(txt);
    char *txtcpy = (char *)malloc(sizeof(char) * (len + 1));
    strncpy(txtcpy, txt, len + 1);
    node *n = malloc(sizeof(node));
    n->info = txtcpy;
    n->prev = n->next = NULL;
    return n;
}

node *
find_info(node *root, const char *text)
{
    if(!root) return NULL;
    
    if(strcmp(root->info, text) == 0)
        return root;

    return find_info(root->next, text);
}

void
destroy_node(node *n)
{
    node *prev = n->prev;
    node *next = n->next;

    if(prev) prev->next = next;
    if(next) next->prev = prev;

    free(n->info);
    n->info = NULL;
    n->next = n->prev = NULL;
    free(n);
}

void
clear_nodes_from(node *n)
{
    if(!n) return;

    node *next = n->next;
    destroy_node(n);
    clear_nodes_from(next);
}

unsigned
node_count_from(node *n)
{
    unsigned len = 0u;
    while(n) {
        len++;
        n = n->next;
    }
    return len;
}

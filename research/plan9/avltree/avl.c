#include <u.h>
#include <libc.h>

int
max(int a, int b)
{
	return (a > b) ? a : b;
}

struct AVL_NODE_T
{
	void *info;
	struct AVL_NODE_T *left;
	struct AVL_NODE_T *right;
};

typedef struct AVL_NODE_T avl_node_t;

struct AVL_TREE_T
{
	avl_node_t *root;
};

typedef struct AVL_TREE_T avl_tree_t;

struct QUEUE_T
{
	avl_node_t **nodes;
	ulong size;
	ulong elems;
};

typedef struct QUEUE_T queue_t;

void
initqueue(queue_t *q)
{
	q->nodes = malloc(2 * sizeof (avl_node_t*));
	q->size  = 2;
	q->elems = 0;
}

void
enqueue(queue_t *q, avl_node_t *n)
{
	ulong newsize, newelems;
	newelems = q->elems + 1;
	if(q->size < newelems) {
		newsize  = q->size * 2;
		q->nodes =
			realloc(q->nodes,
					newsize * sizeof (avl_node_t*));
		q->size  = newsize;
	}
	q->nodes[q->elems] = n;
	q->elems = newelems;
}

avl_node_t*
dequeue(queue_t *q)
{
	int i;
	avl_node_t *n;
	n = q->nodes[0];
	for(i = 0; i < q->elems; i++) {
		q->nodes[i] = q->nodes[i + 1];
	}
	q->elems--;
	return n;
}

void
destroyqueue(queue_t *q)
{
	free(q->nodes);
	q->size  = 0;
	q->elems = 0;
}

typedef int(*compare_fn_t)(void*, void*);

typedef void(*print_fn_t)(void*);

int
avl_node_height(avl_node_t *node)
{
	if(node == nil) {
		return -1;
	} else if((node->left == nil)
			  && (node->right == nil)) {
		return 0;
	}
	return 1 + max(avl_node_height(node->left),
				   avl_node_height(node->right));
}

int
avl_tree_height(avl_tree_t tree)
{
	return avl_node_height(tree.root);
}

int
avl_balance_index(avl_node_t *node)
{
	int leftidx, rightidx;
	if(node == nil) {
		return 0;
	}
	leftidx = !(node->left == nil)
		? (1 + avl_node_height(node->left))
		: 0;
	rightidx = !(node->right == nil)
		? (1 + avl_node_height(node->right))
		: 0;
	return leftidx - rightidx;
}

avl_node_t*
avl_leftrot(avl_node_t *node)
{
	avl_node_t *aux;
	aux = node->right->left;
	node->right->left = node;
	node = node->right;
	node->left->right = aux;
	return node;
}

avl_node_t*
avl_rightrot(avl_node_t *node)
{
	avl_node_t *aux;
	aux = node->left->right;
	node->left->right = node;
	node = node->left;
	node->right->left = aux;
	return node;
}

avl_node_t*
avl_dblrightrot(avl_node_t *node)
{
	node->left = avl_leftrot(node->left);
	return avl_rightrot(node);
}

avl_node_t*
avl_dblleftrot(avl_node_t *node)
{
	node->right = avl_rightrot(node->right);
	return avl_leftrot(node);
}

avl_node_t*
avl_node_balance(avl_node_t *node)
{
	int coef;
	coef = avl_balance_index(node);
	if(abs(coef) == 2) {
		if(coef > 0) {
			if(avl_balance_index(node->left) == -1) {
				node = avl_dblrightrot(node);
			} else {
				node = avl_rightrot(node);
			}
		} else if(coef < 0) {
			if(avl_balance_index(node->right) == 1) {
				node = avl_dblleftrot(node);
			} else {
				node = avl_leftrot(node);
			}
		}
	}
	return node;
}

avl_node_t*
avl_node_insert(avl_node_t *node, void *info, compare_fn_t cmp)
{
	int compare;
	if(node == nil) {
		node = malloc(sizeof (avl_node_t));
		node->info  = info;
		node->left  = nil;
		node->right = nil;
		return node;
	}

	compare = cmp(info, node->info);

	if(compare == 0) {
		node->left = avl_node_insert(node->left, info, cmp);
	} else if(compare == 1) {
		node->right = avl_node_insert(node->right, info, cmp);
	}

	return avl_node_balance(node);
}

avl_tree_t
avl_tree_insert(avl_tree_t tree, void *info, compare_fn_t cmp)
{
	tree.root = avl_node_insert(tree.root, info, cmp);
	return tree;
}

void
avl_print_node_inorder(avl_node_t *node, print_fn_t p)
{
	if(node == nil)
		return;
	avl_print_node_inorder(node->left, p);
	p(node->info);
	avl_print_node_inorder(node->right, p);
}

void
avl_print_inorder(avl_tree_t tree, print_fn_t p)
{
	avl_print_node_inorder(tree.root, p);
	print("\n");
}

void
avl_print_node_preorder(avl_node_t *node, print_fn_t p)
{
	if(node == nil)
		return;
	p(node->info);
	avl_print_node_preorder(node->left, p);
	avl_print_node_preorder(node->right, p);
}

void
avl_print_preorder(avl_tree_t tree, print_fn_t p)
{
	avl_print_node_preorder(tree.root, p);
	print("\n");
}

void
avl_print_node_postorder(avl_node_t *node, print_fn_t p)
{
	if(node == nil)
		return;
	avl_print_node_postorder(node->left, p);
	avl_print_node_postorder(node->right, p);
	p(node->info);
}

void
avl_print_postorder(avl_tree_t tree, print_fn_t p)
{
	avl_print_node_postorder(tree.root, p);
	print("\n");
}

void
avl_print_level(avl_tree_t tree, print_fn_t p)
{
	queue_t q;
	initqueue(&q);
	enqueue(&q, tree.root);
	while(q.elems != 0) {
		avl_node_t *n = dequeue(&q);
		if(n != nil) {
			p(n->info);
			if(n->left != nil)
				enqueue(&q, n->left);
			if(n->right != nil)
				enqueue(&q, n->right);
		}
	}
	destroyqueue(&q);
	print("\n");
}

void
avl_print_triangle(avl_tree_t tree, print_fn_t p)
{
	queue_t *curr, *other, *aux;
	queue_t q1;
	queue_t q2;

	initqueue(&q1);
	initqueue(&q2);

	curr  = &q1;
	other = &q2;

	enqueue(curr, tree.root);

	while(curr->elems != 0) {
		avl_node_t *n = dequeue(curr);
		if(n == nil) {
			print("* ");
		} else {
			p(n->info);
			enqueue(other, n->left);
			enqueue(other, n->right);
		}
		if((curr->elems == 0) && (other->elems != 0)) {
			aux  = curr;
			curr = other;
			other = aux;
			print("\n");
		}
	}
	destroyqueue(&q1);
	destroyqueue(&q2);
	print("\n");
}

void
avl_node_clear(avl_node_t *node)
{
	if(node == nil)
		return;
	avl_node_clear(node->left);
	avl_node_clear(node->right);
	free(node->info);
	free(node);
}

avl_tree_t
avl_tree_clear(avl_tree_t tree)
{
	avl_node_clear(tree.root);
	tree.root = nil;
	return tree;
}

int
compare_nums(void *a, void *b)
{
	int *an, *bn;
	an = (int*)a;
	bn = (int*)b;
	return (*an) > (*bn);
}

void
print_num(void* n)
{
	int *num;
	num = (int*)n;
	print("%d ", *num);
}

void
main()
{
	srand(time(0));

	avl_tree_t tree;
	tree.root = nil;

	int *buffer;
	int i;

	for(i = 0; i < 20; i++) {
		buffer = malloc(sizeof (int));
		*buffer = rand() % 999;
		tree = avl_tree_insert(tree, buffer, compare_nums);
	}

	print("Em ordem: ");
	avl_print_inorder(tree, print_num);

	print("Pre ordem: ");
	avl_print_preorder(tree, print_num);

	print("Pos ordem: ");
	avl_print_postorder(tree, print_num);

	print("Em nivel: ");
	avl_print_level(tree, print_num);

	print("Em triangulo:\n");
	avl_print_triangle(tree, print_num);

	tree = avl_tree_clear(tree);

	exits(nil);
}

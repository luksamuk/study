#include <iostream>
#include <cmath>
#include <queue>
#include <vector>

enum TreePrintStyle
{
    TREEPRINT_INORDER,
    TREEPRINT_PREORDER,
    TREEPRINT_POSTORDER,
    TREEPRINT_LEVEL,
    TREEPRINT_TRIANGLE
};

template<typename T>
class AVLTree
{
public:
    AVLTree();
    AVLTree(std::vector<T>);
    ~AVLTree();

    bool insert(T);
    void print(TreePrintStyle s = TREEPRINT_INORDER) const;
    void clear(void);
    bool search(const T) const;
    bool remove(const T);
private:
    struct node_t
    {
        T info;
        node_t *left;
        node_t *right;
    };

    node_t *_root;

    int _height(const node_t*) const;
    int _balance_index(const node_t*) const;
    void _lrot(node_t*&);
    void _rrot(node_t*&);
    void _rrot_dbl(node_t*&);
    void _lrot_dbl(node_t*&);
    void _balance(node_t*&);

    bool _insert(T, node_t*&);

    void _print_inorder(const node_t*) const;
    void _print_preorder(const node_t*) const;
    void _print_postorder(const node_t*) const;
    void _print_bylevel(const node_t*) const;
    void _print_triangle(const node_t*) const;

    void _clear(node_t*);

    bool _search(const node_t*, const T&) const;

    node_t *_detach_rightmost(node_t*&);
    bool _remove(node_t*&, const T&);
};

template<typename T>
int AVLTree<T>::_height(const AVLTree::node_t *node) const
{
    if(!node) return -1;
        
    if(!node->left && !node->right)
        return 0;

    return 1 + std::max(_height(node->left),
                        _height(node->right));
}

template<typename T>
int AVLTree<T>::_balance_index(const AVLTree::node_t *node) const
{
    if(!node) return 0;
        
    int left_idx =
        (!node->left) ? 0 : (1 + _height(node->left));
    int right_idx =
        (!node->right) ? 0 : (1 + _height(node->right));

    return left_idx - right_idx;
}

template<typename T>
void AVLTree<T>::_lrot(AVLTree::node_t*& root)
{
    AVLTree::node_t *b = root->right->left;
    root->right->left = root;
    root = root->right;
    root->left->right = b;
}

template<typename T>
void AVLTree<T>::_rrot(AVLTree::node_t*& root)
{
    AVLTree::node_t *b = root->left->right;
    root->left->right = root;
    root = root->left;
    root->right->left = b;
}

template<typename T>
void AVLTree<T>::_rrot_dbl(AVLTree::node_t*& root)
{
    _lrot(root->left);
    _rrot(root);
}

template<typename T>
void AVLTree<T>::_lrot_dbl(AVLTree::node_t*& root)
{
    _rrot(root->right);
    _lrot(root);
}

template<typename T>
void AVLTree<T>::_balance(AVLTree::node_t*& node)
{
    if(!node) return;
    int coef = _balance_index(node);
    if(std::abs(coef) == 2) {
        if(coef == 2) {
            if(_balance_index(node->left) == -1)
                _rrot_dbl(node);
            else _rrot(node);
        } else if(coef == -2) {
            if(_balance_index(node->right) == 1)
                _lrot_dbl(node);
            else _lrot(node);
        }
    }
}

template<typename T>
bool AVLTree<T>::_insert(T info, AVLTree::node_t*& node)
{
    if(!node) {
        node = new AVLTree::node_t;
        node->info = info;
        node->left = nullptr;
        node->right = nullptr;
        return true;
    }

    bool ret_value;
    
    if(info < node->info)
        ret_value = _insert(info, node->left);
    else if(info > node->info)
        ret_value = _insert(info, node->right);
    else ret_value = false; // info == node->info

    if(ret_value)
        _balance(node);
    
    return ret_value;
}

template<typename T>
void AVLTree<T>::_print_inorder(const AVLTree::node_t *node) const
{
    if(!node) return;
    _print_inorder(node->left);
    std::cout << node->info << ' ';
    _print_inorder(node->right);
}

template<typename T>
void AVLTree<T>::_print_preorder(const AVLTree::node_t *node) const
{
    if(!node) return;
    std::cout << node->info << ' ';
    _print_inorder(node->left);
    _print_inorder(node->right);
}

template<typename T>
void AVLTree<T>::_print_postorder(const AVLTree::node_t *node) const
{
    if(!node) return;
    _print_inorder(node->left);
    _print_inorder(node->right);
    std::cout << node->info << ' ';
}

template<typename T>
void AVLTree<T>::_print_bylevel(const AVLTree::node_t *node) const
{
    if(!node) return;
    std::queue<const AVLTree::node_t*> nodes;
    nodes.push(node);

    while(!nodes.empty()) {
        const AVLTree::node_t *front = nodes.front();
        nodes.pop();
        if(front) {
            nodes.push(front->left);
            nodes.push(front->right);
            std::cout << front->info << ' ';
        }
    }
}

template<typename T>
void AVLTree<T>::_print_triangle(const AVLTree::node_t *node) const
{
    if(!node) return;
    std::queue<const AVLTree::node_t*> curr;
    std::queue<const AVLTree::node_t*> next;

    curr.push(node);

    while(!curr.empty()) {
        const AVLTree::node_t *front = curr.front();
        curr.pop();
        if(!front)
            std::cout << '*';
        else {
            next.push(front->left);
            next.push(front->right);
            std::cout << front->info;
        }

        std::cout << ' ';
        if(curr.empty() && !next.empty()) {
            std::swap(curr, next);
            std::cout << std::endl;
        }
    }
}

template<typename T>
void AVLTree<T>::_clear(AVLTree::node_t* node)
{
    if(!node) return;
    _clear(node->left);
    _clear(node->right);
    delete node;
}

template<typename T>
bool AVLTree<T>::_search(const AVLTree::node_t* node, const T& info) const
{
    if(!node) return false;

    if(node->info == info) return true;

    if(info < node->info) {
        return _search(node->left, info);
    }

    return _search(node->right, info);
}

template<typename T>
bool AVLTree<T>::_remove(AVLTree::node_t*& node, const T& info)
{
    // Not found
    if(!node) return false;

    if(node->info == info) {
        bool no_left  = !node->left;
        bool no_right = !node->right;
        if(no_left && no_right) {
            // No children; delete
            delete node;
            node = nullptr;
        } else if(!no_left && !no_right) {
            // Both children
            AVLTree::node_t *rightmost_left;
            rightmost_left = _detach_rightmost(node->left);
            rightmost_left->left  = node->left;
            rightmost_left->right = node->right;
            delete node;
            node = rightmost_left;
        } else {
            // Raise single child
            AVLTree::node_t *tmp;
            tmp = no_left ? node->right : node->left;
            delete node;
            node = tmp;
        }
        // Balance new node
        _balance(node);
        return true;
    }

    // Recursively remove
    return _remove((info < node->info)
                   ? node->left
                   : node->right,
                   info);
}

template<typename T>
typename AVLTree<T>::node_t*
AVLTree<T>::_detach_rightmost(AVLTree::node_t*& node)
{
    if(!node->right) {
        AVLTree::node_t *tmp = node;
        node = node->left;
        return tmp;
    }

    AVLTree::node_t *rightmost = _detach_rightmost(node->right);
    _balance(node);
    return rightmost;
}

template<typename T>
AVLTree<T>::AVLTree() : _root(nullptr) {}

template<typename T>
AVLTree<T>::AVLTree(std::vector<T> vals) : _root(nullptr)
{
    for(T val : vals)
        _insert(val, _root);
}

template<typename T>
AVLTree<T>::~AVLTree()
{
    _clear(_root);
}

template<typename T>
bool AVLTree<T>::insert(T info)
{
    return _insert(info, _root);
}

template<typename T>
void AVLTree<T>::print(TreePrintStyle style) const
{
    switch(style) {
    case TREEPRINT_INORDER:
        _print_inorder(_root);
        break;
    case TREEPRINT_PREORDER:
        _print_preorder(_root);
        break;
    case TREEPRINT_POSTORDER:
        _print_postorder(_root);
        break;
    case TREEPRINT_LEVEL:
        _print_bylevel(_root);
        break;
    case TREEPRINT_TRIANGLE:
        _print_triangle(_root);
        break;
    default: std::cout << "Unimplemented"; break;
    }
    std::cout << std::endl;
}

template<typename T>
void AVLTree<T>::clear(void)
{
    _clear(_root);
    _root = nullptr;
}

template<typename T>
bool AVLTree<T>::search(const T info) const
{
    return _search(_root, info);
}

template<typename T>
bool AVLTree<T>::remove(const T info)
{
    return _remove(_root, info);
}

template<typename T>
inline void
test_debrief(AVLTree<T>& tree)
{
    std::cout << "Final tree:" << std::endl;
    tree.print(TREEPRINT_TRIANGLE);
    std::cout << std::endl;
    std::cout << "In order:   ";
    tree.print(TREEPRINT_INORDER);
    std::cout << "Preorder:   ";
    tree.print(TREEPRINT_PREORDER);
    std::cout << "Post-order: ";
    tree.print(TREEPRINT_POSTORDER);
    std::cout << std::endl;
}

void
test_raw(void)
{
    std::cout << "## Insercao de elementos, um a um"
              << std::endl;
    AVLTree<int> tree;
    for(const int num : {0, 3, 6, 2, 1, 4, 90, 36, 49}) {
        tree.insert(num);
        std::cout << "In order: ";
        tree.print();
        std::cout << "By level: ";
        tree.print(TREEPRINT_LEVEL);
        std::cout << std::endl;
    }

    test_debrief<int>(tree);

    std::cout << "Clearing tree" << std::endl;
    tree.clear();
    std::cout << std::endl;
}

void
test_ctor(void)
{
    std::cout << "## Insercao de elementos via ctor"
              << std::endl;
    AVLTree<int> tree({35, 39, 51, 20, 13, 28, 22, 32, 25, 33});

    test_debrief<int>(tree);

    std::cout << "Limpando arvore" << std::endl;
    tree.clear();
    std::cout << std::endl;
}

void
test_char(void)
{
    std::cout << "## Arvore de caracteres" << std::endl;
    AVLTree<char> tree({'M', 'G', 'B', 'H', 'S', 'P', 'F', 'C'});

    test_debrief<char>(tree);
}

void
test_search(void)
{
    std::cout << "## Teste de pesquisa" << std::endl;
    AVLTree<int> tree({5, 9, 30, 2, 20, 32});

    test_debrief<int>(tree);

    for(int num : {2, 5, 31, 44}) {
        std::cout << num << " esta na arvore? "
                  << (tree.search(num) ? 'T' : 'F')
                  << std::endl << std::endl;
    }
}

void
test_removal(void)
{
    std::cout << "## Remocao de elementos"
              << std::endl;
    AVLTree<int> tree({35, 39, 51, 20, 13, 28, 22, 32, 25, 33});

    std::cout << "# Inicial:\n";
    tree.print(TREEPRINT_TRIANGLE);
    std::cout << std::endl;

    for(int num : {13, 39, 42, 25, 59, 28}) {
        std::cout << "# Removendo " << num << "...\n";
        bool ret = tree.remove(num);
        std::cout << "Removido? " << (ret ? 'Y' : 'N')
                  << std::endl;
        test_debrief<int>(tree);
    }
}

int
main(void)
{
    test_raw();
    test_ctor();
    test_char();
    test_search();
    test_removal();

    return 0;
}

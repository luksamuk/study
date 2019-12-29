#include <iostream>
#include <cmath>
#include <queue>
#include <vector>
#include <fstream>
#include <string>
#include <cstdlib>
#include <ctime>

enum TreePrintStyle
{
    TREEPRINT_INORDER,
    TREEPRINT_PREORDER,
    TREEPRINT_POSTORDER,
    TREEPRINT_LEVEL,
    TREEPRINT_TRIANGLE,
    TREEPRINT_GRAPHVIZ,
    TREESHOW_GRAPHVIZ,
    TREESHOW_XDOT
};

template<typename T>
class RBTree
{

private:

enum COLOR
{
    RED,
    BLACK
};

struct node_t
{
    T      info;
    COLOR  color;
    node_t *left;
    node_t *right;
    node_t *parent;
};

node_t *_root;

int _height(const node_t *node) const
{
    if(!node) return -1;
        
    if(!node->left && !node->right)
        return 0;

    return 1 + std::max(_height(node->left),
                        _height(node->right));
}

void _lrot(node_t*& root, node_t*& ptr)
{
    node_t *right_ptr = ptr->right;
    ptr->right = right_ptr->left;

    if(ptr->right) {
        ptr->right->parent = ptr;
    }

    right_ptr->parent = ptr->parent;

    if(!ptr->parent) {
        root = right_ptr;
    } else if(ptr == ptr->parent->left) {
        ptr->parent->left = right_ptr;
    } else {
        ptr->parent->right = right_ptr;
    }

    right_ptr->left = ptr;
    ptr->parent = right_ptr;
}

void _rrot(node_t*& root, node_t*& ptr)
{
    node_t *left_ptr = ptr->left;
    ptr->left = left_ptr->right;

    if(ptr->left) {
        ptr->left->parent = ptr;
    }

    left_ptr->parent = ptr->parent;

    if(!ptr->parent) {
        root = left_ptr;
    } else if(ptr == ptr->parent->left) {
        ptr->parent->left = left_ptr;
    } else {
        ptr->parent->right = left_ptr;
    }

    left_ptr->right = ptr;
    ptr->parent = left_ptr;
}

void _fixup(node_t*& root, node_t*& ptr)
{
    node_t* parent_ptr      = nullptr;
    node_t* grandparent_ptr = nullptr;

    while((ptr != _root) &&
          (ptr->color == RED) &&
          (ptr->parent->color == RED))
    {
        parent_ptr      = ptr->parent;
        grandparent_ptr = ptr->parent->parent;

        // Caso A:
        // Pai é o filho esquerdo do avô
        if(parent_ptr == grandparent_ptr->left) {
            node_t* uncle_ptr = grandparent_ptr->right;

            // Caso 1:
            // O tio de ptr existe e também é vermelho:
            // Basta recolorir.
            if(uncle_ptr && uncle_ptr->color == RED) {
                grandparent_ptr->color = RED;
                parent_ptr->color      = BLACK;
                uncle_ptr->color       = BLACK;
                // Vá para o avô e refaça.
                ptr = grandparent_ptr;
            }
            // Mas se o tio é preto (ou é nulo)...
            else {
                
                // Caso 3:
                // ptr é o filho direito de seu pai:
                // requer uma rotação à esquerda.
                if(ptr == parent_ptr->right) {
                    _lrot(root, parent_ptr);
                    // Vá para o pai e continue.
                    ptr        = parent_ptr;
                    parent_ptr = ptr->parent;
                }

                // Caso 2:
                // ptr é o filho esquerdo de seu pai:
                // requer uma rotação à direita.
                _rrot(root, grandparent_ptr);

                // Troque as cores do avô e do pai
                {
                    COLOR c = parent_ptr->color;
                    parent_ptr->color = grandparent_ptr->color;
                    grandparent_ptr->color = c;
                }

                // Vá para o pai e continue.
                ptr = parent_ptr;
            }   
        }
        // Caso B:
        // Pai de ptr é o filho direito do avô de ptr
        else {
            node_t* uncle_ptr = grandparent_ptr->left;

            // Os casos abaixo são uma repetição do que ocorre
            // em A, porém com a direção das rotações e
            // ponteiros invertida.
            if(uncle_ptr && uncle_ptr->color == RED) {
                grandparent_ptr->color = RED;
                parent_ptr->color      = BLACK;
                uncle_ptr->color       = BLACK;
                ptr = grandparent_ptr;
            } else {
                if(ptr == parent_ptr->left) {
                    _rrot(root, parent_ptr);
                    ptr = parent_ptr;
                    parent_ptr = ptr->parent;
                }

                _lrot(root, grandparent_ptr);
                {
                    COLOR c = parent_ptr->color;
                    parent_ptr->color = grandparent_ptr->color;
                    grandparent_ptr->color = c;
                }
                ptr = parent_ptr;
            }
        }
    } // while...

    // Independente de tudo, colora a raiz de preto
    _root->color = BLACK;
}

node_t *_bst_insert(node_t *root, node_t *ptr)
{
    // Se raiz está vazia, retorne um novo nó
    if(!root) return ptr;

    // Se não, recorra árvore abaixo
    if(ptr->info < root->info) {
        root->left         = _bst_insert(root->left, ptr);
        root->left->parent = root;
    } else if(ptr->info > root->info) {
        root->right         = _bst_insert(root->right, ptr);
        root->right->parent = root;
    }

    // Se o nó é igual a algum outro, retorne a
    // raiz sem mudanças
    return root;
}

bool _insert(T info)
{
    node_t* node = new node_t;
    node->info   = info;
    node->color  = RED;
    node->left   = nullptr;
    node->right  = nullptr;
    node->parent = nullptr;

    _root = _bst_insert(_root, node);
    if((!node->parent) && (node != _root)) {
        // Nó não inserido
        delete node;
        return false;
    }
    _fixup(_root, node);
    return true;
}

void _print_inorder(const node_t *node) const
{
    if(!node) return;
    _print_inorder(node->left);
    std::cout << node->info << ' ';
    _print_inorder(node->right);
}

void _print_preorder(const node_t *node) const
{
    if(!node) return;
    std::cout << node->info << ' ';
    _print_inorder(node->left);
    _print_inorder(node->right);
}

void _print_postorder(const node_t *node) const
{
    if(!node) return;
    _print_inorder(node->left);
    _print_inorder(node->right);
    std::cout << node->info << ' ';
}

void _print_bylevel(const node_t *node) const
{
    if(!node) return;
    std::queue<const node_t*> nodes;
    nodes.push(node);

    while(!nodes.empty()) {
        const node_t *front = nodes.front();
        nodes.pop();
        if(front) {
            nodes.push(front->left);
            nodes.push(front->right);
            std::cout << front->info << ' ';
        }
    }
}

void _print_triangle(const node_t *node) const
{
    if(!node) return;
    std::queue<const node_t*> curr;
    std::queue<const node_t*> next;

    curr.push(node);

    while(!curr.empty()) {
        const node_t *front = curr.front();
        curr.pop();
        if(!front)
            std::cout << "*:B";
        else {
            next.push(front->left);
            next.push(front->right);
            std::cout << front->info << ':'
                      << (front->color == RED
                          ? 'R'
                          : 'B');
        }

        std::cout << ' ';
        if(curr.empty() && !next.empty()) {
            std::swap(curr, next);
            std::cout << std::endl;
        }
    }
}

void _gen_graphviz(std::ostream& oss, const node_t *node) const
{
    // Print graphviz header
    oss << "graph G {" << std::endl
        << "bgcolor=\"#00000000\";" << std::endl
        << "graph["
        // << "nodesep=\"0.5\", "
        << "ranksep = \"0.2\", "
        << "dpi = 150, "
        // << "splines = \"curved\", "
        << "fixedsize = true];" << std::endl
        << "node[shape=circle, "
        << "fontcolor=white, "
        << "fillcolor=white, "
        << "style=filled];" << std::endl;

    std::queue<const node_t*> nodes;
    if(node) nodes.push(node);

    int nullidx = 0;

    while(!nodes.empty()) {
        const node_t *front = nodes.front();
        nodes.pop();

        // Print node properties
        oss << front->info
            << "[fillcolor="
            << (front->color == RED
                ? "red"
                : "black")
            << "];" << std::endl;
        // Print node children
        if(front->left) {
            oss << front->info
                << ":sw -- "
                << front->left->info
                << ":n;"
                << std::endl;
            // Enqueue existing child
            nodes.push(front->left);
        } else {
            oss << "nil" << nullidx
                << "[label=\"nil\", "
                << "shape=plain, "
                << "fontsize=9, "
                << "style=\"\", "
                << "fontcolor=black]"
                << std::endl;
            oss << front->info
                << ":sw -- "
                << "nil" << nullidx
                << ';'
                << std::endl;
            nullidx++;
        }
        
        if(front->right) {
            oss << front->info
                << ":se -- "
                << front->right->info
                << ":n;"
                << std::endl;
            nodes.push(front->right);
        } else {
            oss << "nil" << nullidx
                << "[label=\"nil\", "
                << "shape=plain, "
                << "fontsize=9, "
                << "style=\"\", "
                << "fontcolor=black]"
                << std::endl;
            oss << front->info
                << ":se -- "
                << "nil" << nullidx
                << ';'
                << std::endl;
            nullidx++;
        }
    }
    
    oss << '}' << std::endl;
}

void _print_graphviz(const node_t *node) const
{
    _gen_graphviz(std::cout, node);
}

void _save_graphviz(const node_t *node) const
{
    std::ofstream out;
    out.open("/tmp/rbtree.dot");

    if(!out.is_open()) {
        std::cerr << "Erro ao gerar o arquivo" << std::endl;
        return;
    }

    _gen_graphviz(out, node);
    out.close();
}

void _show_graphviz(const node_t *node, bool interact = false) const
{
    _save_graphviz(node);

    if(!interact) {
        if(system("/usr/bin/dot /tmp/rbtree.dot -Kdot -Tpng -o /tmp/rbtree.png")) {
            std::cerr << "Erro ao gerar a imagem" << std::endl;
            return;
        }
        
        if(system("/usr/bin/feh /tmp/rbtree.png")) {
            std::cerr << "Erro ao mostrar a imagem" << std::endl;
            return;
        }
    } else {
        if(system("/usr/bin/xdot /tmp/rbtree.dot &")) {
            std::cerr << "Erro ao mostrar a arvore" << std::endl;
            return;
        }
    }
}

void _clear(node_t* node)
{
    if(!node) return;
    _clear(node->left);
    _clear(node->right);
    delete node;
}

bool _search(const node_t* node, const T& info) const
{
    if(!node) return false;

    if(node->info == info) return true;

    if(info < node->info) {
        return _search(node->left, info);
    }

    return _search(node->right, info);
}

public:

RBTree() : _root(nullptr) {}

RBTree(std::vector<T> vals) : _root(nullptr)
{
    for(T val : vals)
        _insert(val);
}

~RBTree()
{
    _clear(_root);
}

bool insert(T info)
{
    return _insert(info);
}

void print(TreePrintStyle style = TREEPRINT_INORDER) const
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
    case TREEPRINT_GRAPHVIZ:
        _print_graphviz(_root);
        break;
    case TREESHOW_GRAPHVIZ:
        _show_graphviz(_root);
        break;
    case TREESHOW_XDOT:
        _show_graphviz(_root, true);
        break;
    default: std::cout << "Unimplemented"; break;
    }
    std::cout << std::endl;
}

void clear(void)
{
    _clear(_root);
    _root = nullptr;
}

bool search(const T info) const
{
    return _search(_root, info);
}

void save_graphviz(void) const
{
    _save_graphviz(_root);
}

};

template<typename T>
inline void
test_debrief(RBTree<T>& tree)
{
    std::cout << "Arvore:" << std::endl;
    tree.print(TREEPRINT_TRIANGLE);
    std::cout << std::endl;
    std::cout << "Em ordem:  ";
    tree.print(TREEPRINT_INORDER);
    std::cout << "Preordem:  ";
    tree.print(TREEPRINT_PREORDER);
    std::cout << "Pos-ordem: ";
    tree.print(TREEPRINT_POSTORDER);
    std::cout << std::endl;
    tree.save_graphviz();
}

void
test_raw(void)
{
    std::cin.sync();
    std::cin.get();
    std::cout << "## Insercao de elementos, um a um"
              << std::endl;
    RBTree<int> tree;
    for(const int num : {0, 3, 6, 2, 1, 4, 90, 36, 49}) {
        std::cout << "-- Inserindo: " << num << std::endl;
        tree.insert(num);
        test_debrief<int>(tree);
        std::cout << "Pressione uma tecla para continuar." << std::endl;
        std::cin.sync();
        std::cin.get();
    }
    std::cout << std::endl;
}

void
test_ctor(void)
{
    std::cin.sync();
    std::cin.get();
    std::cout << "## Insercao de elementos via construtor"
              << std::endl;
    RBTree<int> tree({35, 39, 51, 20, 13, 28, 22, 32, 25, 33});

    test_debrief<int>(tree);

    std::cout << "Pressione uma tecla para continuar." << std::endl;
    std::cin.sync();
    std::cin.get();
    std::cout << std::endl;
}

void
test_char(void)
{
    std::cin.sync();
    std::cin.get();
    std::cout << "## Arvore de caracteres" << std::endl;
    RBTree<char> tree({'M', 'G', 'B', 'H', 'S', 'P', 'F', 'C'});

    test_debrief<char>(tree);

    std::cout << "Pressione uma tecla para continuar." << std::endl;
    std::cin.sync();
    std::cin.get();
    std::cout << std::endl;
}

void
test_search(void)
{
    std::cin.sync();
    std::cin.get();
    std::cout << "## Teste de pesquisa" << std::endl;
    RBTree<int> tree({5, 9, 30, 2, 20, 32});

    test_debrief<int>(tree);

    for(int num : {2, 5, 31, 44}) {
        std::cout << num << " esta na arvore? "
                  << (tree.search(num) ? 'T' : 'F')
                  << std::endl;
    }
    std::cout << std::endl;

    std::cout << "Pressione uma tecla para continuar." << std::endl;
    std::cin.sync();
    std::cin.get();
    std::cout << std::endl;
}

void
test_menu(void)
{
    char c = 0;
    do {
        std::cout << "1. Teste de insercao\n"
                  << "2. Teste de construcao\n"
                  << "3. Teste de uso com caracteres\n"
                  << "4. Teste de pesquisa\n"
                  << "0. Voltar\n"
                  << "Opcao: ";
        std::cin.sync();
        c = std::cin.get();
        std::cin.sync();

        switch(c) {
        case '1':
            test_raw();
            break;
        case '2':
            test_ctor();
            break;
        case '3':
            test_char();
            break;
        case '4':
            test_search();
            break;
        default: break;
        };
    } while ('0' != c);
}

void
repl(void)
{
    srand(time(NULL));
    RBTree<int> tree;
    tree.save_graphviz();

    std::cout << "Para ajuda, digite \'help\'."
              << std::endl;
        
    std::string input;
    do {
        std::cout << "> ";
        std::cin >> std::ws;
        std::getline(std::cin, input, '\n');
        if(input == "clear") {
            std::cout << "Limpando a arvore..."
                      << std::endl;
            tree.clear();
            tree.save_graphviz();
        } else if(input == "show") {
            tree.print(TREESHOW_GRAPHVIZ);
        } else if(input == "inter") {
            tree.print(TREESHOW_XDOT);
        } else if(input == "inorder") {
            tree.print(TREEPRINT_INORDER);
        } else if(input == "preorder") {
            tree.print(TREEPRINT_PREORDER);
        } else if(input == "postorder") {
            tree.print(TREEPRINT_POSTORDER);
        } else if(input == "levels") {
            tree.print(TREEPRINT_LEVEL);
        } else if(input == "triangle") {
            tree.print(TREEPRINT_TRIANGLE);
        } else if(input == "graphviz") {
            tree.print(TREEPRINT_GRAPHVIZ);
        } else if(input == "refresh") {
            tree.save_graphviz();
        } else if(input == "rand") {
            int num = rand() % 100;
            bool result = tree.insert(num);
            tree.save_graphviz();
            if(result) {
                std::cout << num << std::endl;
            } else {
                std::cout << num
                          << " ja esta na arvore!"
                          << std::endl;
            }
        } else if(input == "test") {
            test_menu();
        } else if(input == "help") {
            std::cout << "Comandos disponiveis:\n"
                      << "refresh   Atualiza a arvore em disco.\n"
                      << "clear     Limpa a arvore.\n"
                      << "show      Mostra a arvore numa imagem.\n"
                      << "inter     Mostra a arvore na ferramenta xdot.\n"
                      << "inorder   Impressao em-ordem.\n"
                      << "preorder  Impressao pre-ordem.\n"
                      << "postorder Impressao pos-ordem.\n"
                      << "levels    Impressao em niveis.\n"
                      << "triangle  Impressao triangular.\n"
                      << "graphviz  Imprime codigo GraphViz.\n"
                      << "rand      Adiciona numero aleatorio.\n"
                      << "help      Mostra este texto.\n"
                      << "test      Menu de testes pre-prontos.\n"
                      << "<numero>  Adiciona o numero a arvore.\n"
                      << "quit      Sai deste console.\n"
                      << std::endl;
        } else if(input != "quit") {
            try {
                int num = std::stoi(input);
                bool result = tree.insert(num);
                tree.save_graphviz();
                if(!result) {
                    std::cout << num
                              << " ja esta na arvore!"
                              << std::endl;
                }
            } catch(const std::invalid_argument& e) {
                std::cout << "Comando desconhecido"
                          << std::endl;
            } catch(const std::out_of_range& e) {
                std::cout << "Numero informado e grande demais"
                          << std::endl;
            }
        }
    } while(input != "quit");
}

int
main(void)
{
    std::cout << "Teste de Arvore Red-Black"
              << std::endl;
    repl();
    return 0;
}

#include <iostream>

/* ESTRUTURAS DA ÁRVORE

  Os elementos a seguir compreendem a modelagem dos dados
  da estrutura de uma Árvore Rubro-Negra.
**/

// ENUMERAÇÃO: Cor de um nó
// Enumeração responsável por representar a coloração.
// Possui semântica de número inteiro em runtime.
enum COR
{
    VERMELHO,
    PRETO
};

// ESTRUTURA: Nós da árvore
// Estrutura dos nós da árvore. Comparada à AVL,
// possui informações extras de cor do nó e um ponteiro
// ao nó pai do mesmo.
struct node_t
{
    int    info;
    COR    cor;
    node_t *esq;
    node_t *dir;
    node_t *pai;
};


/* PARTE INTERNA

  Os elementos a seguir compreendem informações internas
  para o funcionamento da Árvore Rubro-Negra, não sendo
  projetadas para uso direto do programador.
**/


// GLOBAL: Ponteiro para a raiz da árvore.
node_t *_raiz;


// FUNÇÃO: Rotação à esquerda
// Realiza uma rotação à esquerda na árvore, de forma
// similar à AVL, porém tomando cuidados extras para com o
// ponteiro para o pai do nó atual.
// Nota: "raiz" é raiz da sub-árvore, não da árvore inteira.
void
_rot_esq(node_t*& raiz, node_t*& ptr)
{
    // Guarde uma referência ao ponteiro à direita
    node_t *dir_ptr = ptr->dir;
    // Redefina o ponteiro à direita como sendo o
    // elemento à esquerda do mesmo ponteiro. Isto
    // torna "dir_ptr" desconexo por enquanto.
    ptr->dir = dir_ptr->esq;

    // Se o novo elemento à direita não for nulo,
    // defina seu pai como sendo o elemento atual
    if(ptr->dir) {
        ptr->dir->pai = ptr;
    }

    // Defina o pai do nó (ainda) desconexo como
    // sendo o pai do nó atual
    dir_ptr->pai = ptr->pai;

    // As comparações a seguir envolvem substituir
    // "ptr" por "dir_ptr" na árvore, tornando "ptr"
    // temporariamente desconexo.
    if(!ptr->pai) {
        // Se o pai desta sub-árvore é nulo, estamos
        // tratando da raiz da árvore; então,
        // redefina a raiz como sendo "dir_ptr".
        raiz = dir_ptr;
    } else if(ptr == ptr->pai->esq) {
        // Se "ptr" é filho esquerdo do pai, então
        // defina "dir_ptr" como o novo filho esquerdo
        // deste pai.
        ptr->pai->esq = dir_ptr;
    } else {
        // Se "ptr" é filho direito do pai, então
        // defina "dir_ptr" como o novo filho direito
        // deste pai.
        ptr->pai->dir = dir_ptr;
    }

    // Defina o filho esquerdo da nova raiz da sub-árvore
    // ("dir_ptr") como sendo "ptr".
    dir_ptr->esq = ptr;
    // Defina o pai de "ptr" como sendo "dir_ptr", a nova
    // raiz da sub-árvore.
    ptr->pai = dir_ptr;
}

// FUNÇÃO: Rotação à direita
// Realiza uma rotação à esquerda na árvore, de forma
// similar à AVL, porém tomando cuidados extras para com o
// ponteiro para o pai do nó atual.
// Nota: "raiz" é raiz da sub-árvore, não da árvore inteira.
void
_rot_dir(node_t*& raiz, node_t*& ptr)
{
    // Guarde uma referência ao ponteiro à esquerda 
    node_t *esq_ptr = ptr->esq;
    // Redefina o ponteiro à esquerda como sendo o
    // elemento à direita do mesmo ponteiro. Isto
    // torna "esq_ptr" desconexo por enquanto.
    ptr->esq = esq_ptr->dir;

    // Se o novo elemento à esquerda não for nulo,
    // defina seu pai como sendo o elemento atual
    if(ptr->esq) {
        ptr->esq->pai = ptr;
    }

    // Defina o pai do nó (ainda) desconexo como
    // sendo o pai do nó atual
    esq_ptr->pai = ptr->pai;

    // As comparações a seguir envolvem substituir
    // "ptr" por "esq_ptr" na árvore, tornando "ptr"
    // temporariamente desconexo.
    if(!ptr->pai) {
        // Se o pai desta sub-árvore é nulo, estamos
        // tratando da raiz da árvore; então,
        // redefina a raiz como sendo "esq_ptr".
        raiz = esq_ptr;
    } else if(ptr == ptr->pai->esq) {
        // Se "ptr" é filho esquerdo do pai, então
        // defina "esq_ptr" como o novo filho esquerdo
        // deste pai.
        ptr->pai->esq = esq_ptr;
    } else {
        // Se "ptr" é filho direito do pai, então
        // defina "esq_ptr" como o novo filho direito
        // deste pai.
        ptr->pai->dir = esq_ptr;
    }

    // Defina o filho direito da nova raiz da sub-árvore
    // ("esq_ptr") como sendo "ptr".
    esq_ptr->dir = ptr;
    // Defina o pai de "ptr" como sendo "esq_ptr", a nova
    // raiz da sub-árvore.
    ptr->pai = esq_ptr;
}


// FUNÇÃO: Correção de cores.
// Realiza a análise de um nó chamado "ptr", de acordo com
// os casos de análise de cor durante a inserção na Árvore
// Rubro-Negra.
// Os ponteiros para nós "raiz" e "ptr" são passados por
// referência, uma vez que devem tolerar modificação direta
// onde existirem como variáveis, na memória.
// Nota: "raiz" é raiz da sub-árvore, não da árvore inteira.
void
_fixup(node_t*& raiz, node_t*& ptr)
{
    node_t* pai_ptr = NULL;
    node_t* avo_ptr = NULL;

    while((ptr != _raiz) &&
          (ptr->cor != PRETO) &&
          (ptr->pai->cor == VERMELHO))
    {
        pai_ptr = ptr->pai;
        avo_ptr = ptr->pai->pai;

        // Caso A:
        // Pai é o filho esquerdo do avô
        if(pai_ptr == avo_ptr->esq) {
            node_t* tio_ptr = avo_ptr->dir;

            // Caso 1:
            // O tio de ptr existe e também é vermelho:
            // Basta recolorir. O pai e o tio tornam-se
            // pretos, e o avô torna-se vermelho.
            if(tio_ptr && tio_ptr->cor == VERMELHO) {
                avo_ptr->cor = VERMELHO;
                pai_ptr->cor = PRETO;
                tio_ptr->cor = PRETO;
                // Mude o foco da correção para o avô.
                ptr = avo_ptr;
            }
            // Mas se o tio é preto (ou é nulo)...
            else {

                // Caso 3:
                // ptr é o filho direito de seu pai:
                // requer uma rotação à esquerda.
                if(ptr == pai_ptr->dir) {
                    _rot_esq(raiz, pai_ptr);
                    // Mude o foco da correção para o pai.
                    ptr     = pai_ptr;
                    pai_ptr = ptr->pai;
                }

                // Caso 2:
                // ptr é o filho esquerdo de seu pai:
                // requer uma rotação à direita.
                _rot_dir(raiz, avo_ptr);

                // Troque as cores do avô e do pai
                // NOTA: Este é um escopo avulso, apenas
                // para conter a existência da variável
                // "c".
                {
                    COR c = pai_ptr->cor;
                    pai_ptr->cor = avo_ptr->cor;
                    avo_ptr->cor = c;
                }
                
                // Mude o foco da correção para o pai.
                ptr = pai_ptr;
            }
        } // Fim do Caso A (pai é filho esquerdo)
        // Caso B:
        // Pai é o filho direito do avô.
        else {
            node_t* tio_ptr = avo_ptr->esq;

            // Os casos abaixo são uma repetição do que ocorre
            // em A, porém com a direção das rotações e
            // ponteiros invertida.
            if(tio_ptr && tio_ptr->cor == VERMELHO) {
                avo_ptr->cor = VERMELHO;
                pai_ptr->cor = PRETO;
                tio_ptr->cor = PRETO;
                ptr = avo_ptr;
            } else {
                if(ptr == pai_ptr->esq) {
                    _rot_dir(raiz, pai_ptr);
                    ptr     = pai_ptr;
                    pai_ptr = ptr->pai;
                }

                _rot_esq(raiz, avo_ptr);
                {
                    COR c = pai_ptr->cor;
                    pai_ptr->cor = avo_ptr->cor;
                    avo_ptr->cor = c;
                }
                ptr = pai_ptr;
            }
        } // Fim do Caso B (pai é filho direito)
    } // Fim do while

    // Por definição, SEMPRE colora a raiz usando a cor
    // PRETA.
    _raiz->cor = PRETO;
}


// FUNÇÃO: Inserção recursiva
// Esta função insere, recursivamente um nó "ptr" na árvore
// binária informada, independente de sua natureza. A
// inserção ocorre com base na reescrita recursiva da
// árvore, sendo assim, a primeira chamada a esta função
// retorna a nova raiz da árvore, por exemplo. Caso a
// informação do nó já esteja na árvore, a função será
// encerrada e o nó "ptr" não será alterado.
node_t*
_inserir_rec(node_t *raiz, node_t *ptr)
{
    // Se raiz está vazia, retorne um novo nó
    if(!raiz) return ptr;

    // Se não, recorra árvore abaixo
    if(ptr->info < raiz->info) {
        raiz->esq         = _inserir_rec(raiz->esq, ptr);
        raiz->esq->pai = raiz;
    } else if(ptr->info > raiz->info) {
        raiz->dir         = _inserir_rec(raiz->dir, ptr);
        raiz->dir->pai = raiz;
    }

    // Se o nó é igual a algum outro, retorne a
    // raiz sem mudanças
    return raiz;
}

// FUNÇÃO: Inserção (interna)
// Esta função é responsável por instanciar um novo nó para
// a árvore, e então tenta inseri-lo recursivamente na mesma,
// através da função anterior. Caso a inserção seja feita
// com sucesso, também será realizado um conserto retroativo
// das cores na árvore, iniciando pelo nó que foi criado.
bool
_inserir(int info)
{
    // Crie o nó, defina-o com cor vermelha
    node_t* node = new node_t;
    node->info   = info;
    node->cor    = VERMELHO;
    node->esq    = NULL;
    node->dir    = NULL;
    node->pai    = NULL;

    // Insira o nó
    _raiz = _inserir_rec(_raiz, node);

    // Se não possui pai e não é a raiz,
    // então não foi inserido; destrua-o
    // e retorne falso
    if((!node->pai) && (node != _raiz)) {
        delete node;
        return false;
    }

    // Conserte as cores, partindo do nó
    // inserido e subindo
    _fixup(_raiz, node);
    return true;
}

// FUNÇÃO: Impressão em ordem (interna)
// Realiza uma impressão em ordem de uma sub-árvore,
// se possível.
void
_imp_ordem(node_t *node)
{
    if(!node) return;
    _imp_ordem(node->esq);
    std::cout << node->info << ' ';
    _imp_ordem(node->dir);
}

// FUNÇÃO: Impressão pré-ordem (interna)
// Realiza uma impressão pré-ordem de uma sub-árvore,
// se possível.
void
_imp_pre(node_t *node)
{
    if(!node) return;
    std::cout << node->info << ' ';
    _imp_ordem(node->esq);
    _imp_ordem(node->dir);
}

// FUNÇÃO: Impressão pós-ordem
// Realiza uma impressão pósordem de uma sub-árvore,
// se possível.
void
_imp_pos(node_t *node)
{
    if(!node) return;
    _imp_ordem(node->esq);
    _imp_ordem(node->dir);
    std::cout << node->info << ' ';
}

// FUNÇÃO: Limpeza (interna)
// Limpa todos os nós da sub-árvore, se possível, incluindo
// a raiz da mesma. Esta não é uma função de remoção
// propriamente dita, uma vez que só serve para destruir
// e desalocar uma sub-árvore.
void
_clear(node_t* node)
{
    if(!node) return;
    _clear(node->esq);
    _clear(node->dir);
    delete node;
}

// FUNÇÃO: Pesquisa
// Pesquisa por um elemento na sub-árvore informada. O
// elemento pesquisado deve ser passado por referência.
bool
_search(node_t* node, int& info)
{
    if(!node) return false;

    if(node->info == info) return true;

    if(info < node->info) {
        return _search(node->esq, info);
    }

    return _search(node->dir, info);
}


/* INTERFACE DO PROGRAMADOR

   Os elementos a seguir constituem elementos que podem ser
   diretamente utilizados pelo programador que venha a fazer
   uso deste código.
**/

// FUNÇÃO: Construção da árvore
// Prepara a árvore para utilização, antes de qualquer
// inserção na mesma.
void
constroi_arvore()
{
    _raiz = NULL;
}

// FUNÇÃO: Destruição da árvore
// Limpa toda a árvore, a partir da raiz e desalocando toda
// a memória utilizada.
void
destroi_arvore()
{
    _clear(_raiz);
}

// FUNÇÃO: Inserção
// Insere uma informação na árvore, informando se a inserção
// foi realizada.
bool
inserir(int info)
{
    return _inserir(info);
}

// FUNÇÃO: Impressão pré-ordem
// Imprime os elementos da árvore em pré-ordem.
void
imp_pre()
{
    _imp_pre(_raiz);
    std::cout << std::endl;
}

// FUNÇÃO: Impressão pós-ordem
// Imprime os elementos da árvore em pós-ordem.
void
imp_pos()
{
    _imp_pos(_raiz);
    std::cout << std::endl;
}

// FUNÇÃO: Impressão em ordem
// Imprime os elementos da árvore em ordem.
void
imp_ordem()
{
    _imp_ordem(_raiz);
    std::cout << std::endl;
}

// FUNÇÃO: Pesquisa binária
// Procura uma certa informação na árvore, dizendo se o
// elemento existe.
bool
pesquisa(int info)
{
    return _search(_raiz, info);
}

/* PONTO DE ENTRADA

   Constitui o ponto onde a aplicação é iniciada.
**/

int
main(void)
{
    // Construção da árvore
    constroi_arvore();

    // Inserção de alguns números arbitrários
    int numeros[] = {0, 3, 5, 11, 6, 7, -3, -5};
    for(int i = 0; i < 8; i++) {
        inserir(numeros[i]);
    }

    // Impressão da árvore
    std::cout << "Pre-ordem: ";
    imp_pre();
    std::cout << "Pos-ordem: ";
    imp_pos();
    std::cout << "Em ordem:  ";
    imp_ordem();

    // Pesquisa de alguns elementos na árvore
    std::cout << "2 esta na arvore?  "
              << (pesquisa(2) ? 'S' : 'N')
              << std::endl;

    std::cout << "11 esta na arvore? "
              << (pesquisa(11) ? 'S' : 'N')
              << std::endl;

    // Liberação da memória
    destroi_arvore();
    
    return 0;
}

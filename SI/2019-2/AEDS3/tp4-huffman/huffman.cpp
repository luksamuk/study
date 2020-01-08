/*
 ************************************************
 * Huffman Compress/Decompress v1.0             *
 * Copyright (c) 2020 Lucas S. Vieira           *
 *                                              *
 * Este codigo e distribuido sob a licenca MIT. *
 * Veja o arquivo LICENSE pra mais detalhes.    *
 ************************************************
 */

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cstring>
#include <cctype>
#include <vector>
#include <algorithm>
#include <queue>
#include <map>
#include <set>
#include <cstdint>
#include <bitset>

using std::cout;
using std::cerr;
using std::endl;

typedef std::map<char, uint64_t> freq_map_t;

void
show_freq_map(const freq_map_t freq_map)
{
    cout << " Char        |   Freq " << endl
         << "-------------+--------" << endl;
    for(auto pair : freq_map) {
        cout << ' ';
        if(isspace(pair.first) || !isprint(pair.first)) {
            if(pair.first == EOF) {
                cout << std::setfill(' ')
                     << std::setw(10)
                     << "EOF";
            } else {
                cout << "0x"
                     << std::setfill('0')
                     << std::setw(8)
                     << std::hex << ((int)pair.first)
                     << std::dec;
            }
        } else {
            cout << std::setfill(' ')
                 << std::setw(10)
                 << pair.first;
        }
        
        cout << "  |  "
             << std::setfill(' ')
             << std::setw(5)
             << std::dec
             << pair.second << endl;
    }
}

struct huffman_node_t
{
    char           c;
    uint64_t       freq;
    huffman_node_t *left;
    huffman_node_t *right;
    huffman_node_t *parent;
};

auto huff_cmp =
    [](huffman_node_t *a, huffman_node_t *b) -> bool {
        return (a->freq < b->freq);
    };

typedef
    std::multiset<huffman_node_t*, decltype(huff_cmp)>
    node_set_t;

typedef std::map<char, std::vector<bool>> bit_map_t;

void
print_bitmap(bit_map_t &bitmap)
{
    for(auto pair : bitmap) {
        if(isspace(pair.first) || !isprint(pair.first)) {
            if(pair.first == EOF) {
                cout << std::setfill(' ')
                     << std::setw(10) << "EOF";
            } else {
                cout << "0x"
                     << std::setfill('0')
                     << std::setw(8)
                     << std::hex << ((int)pair.first)
                     << std::dec;
            }
        } else {
            cout << std::setfill(' ')
                 << std::setw(10)
                 << pair.first;
        }
        cout << " => ";
        for(auto bit : pair.second) {
            cout << (bit ? '1' : '0');
        }
        cout << endl;
    }
}

typedef std::map<std::vector<bool>, char> char_map_t;

void
print_charmap(char_map_t &charmap)
{
    for(auto pair : charmap) {
        if(isspace(pair.second) || !isprint(pair.second)) {
            if(pair.second == EOF) {
                cout << std::setfill(' ')
                     << std::setw(10) << "EOF";
            } else {
                cout << "0x"
                     << std::setfill('0')
                     << std::setw(8)
                     << std::hex << ((int)pair.second)
                     << std::dec;
            }
        } else {
            cout << std::setfill(' ')
                 << std::setw(10)
                 << pair.second;
        }
        cout << " <= ";
        for(auto bit : pair.first) {
            cout << (bit ? '1' : '0');
        }
        cout << endl;
    }
}

void
count_characters(std::istream &stream, freq_map_t &freq)
{
    while(stream.good()) {
        char c = stream.get();
        freq[c]++;
    }
}

node_set_t
build_freq_nodeset(const freq_map_t &freqs)
{
    node_set_t freq_nodes(huff_cmp);

    // Percorra os pares (caractere, frequencia),
    // criando nós-folha para cada um e adicionando-os
    // ao conjunto de nós
    for(auto pair : freqs) {
        huffman_node_t *node = new huffman_node_t;
        node->c    = pair.first;
        node->freq = pair.second;
        node->left = node->right = node->parent = nullptr;
        freq_nodes.insert(node);
    }
    
    return freq_nodes;
}

huffman_node_t*
extract_minimum(node_set_t &freq_nodes)
{
    if(freq_nodes.empty())
        return nullptr;

    huffman_node_t *ret = *freq_nodes.begin();
    freq_nodes.erase(freq_nodes.begin());
    return ret;
}

huffman_node_t*
huffman(const node_set_t &C)
{
    size_t     n = C.size();
    node_set_t Q(C);
    
    for(auto i = 1u; i <= n - 1u; i++) {
        huffman_node_t *x, *y;
        x = extract_minimum(Q);
        y = extract_minimum(Q);

        // Crie um novo nó 'z'
        huffman_node_t *z = new huffman_node_t;
        z->c = '\0'; // Sem caractere associado
        z->parent = nullptr;

        z->left  = x;
        z->right = y;
        x->parent = y->parent = z;
        z->freq = x->freq + y->freq;
        
        Q.insert(z);
    }
    return extract_minimum(Q);
}

freq_map_t
build_frequency_map(std::istream &is)
{
    freq_map_t freq;
    count_characters(is, freq);
    return freq;
}

huffman_node_t*
build_huffman_tree(freq_map_t &freq)
{
    // Cria um conjunto de nós-folha
    node_set_t leaf_set = build_freq_nodeset(freq);
    
    // Execute o algoritmo de Huffman, criando
    // a árvore em si
    auto tree_root = huffman(leaf_set);

    return tree_root;
}

void
destroy_tree(huffman_node_t *root)
{
    if(!root) return;
    destroy_tree(root->left);
    destroy_tree(root->right);
    delete root;
}

node_set_t
find_leaves(huffman_node_t *root)
{
    node_set_t leaves(huff_cmp);
    
    std::queue<huffman_node_t*> node_queue;
    node_queue.push(root);
    
    while(!node_queue.empty()) {
        huffman_node_t *node = node_queue.front();
        node_queue.pop();

        if(node->c == '\0') {
            node_queue.push(node->left);
            node_queue.push(node->right);
        } else {
            leaves.insert(node);
        }
    }
    return leaves;
}

void
gen_graphviz(std::ostream& oss, const huffman_node_t *node)
{
    // Print graphviz header
    oss << "graph G {" << std::endl
        << "\tbgcolor=\"#00000000\";" << std::endl
        << "\tgraph["
        << "ranksep = \"0.2\", "
        << "fixedsize = true];" << std::endl
        << "\tnode[shape=circle, "
        << "fontcolor=black, "
        << "fillcolor=white, "
        << "style=filled];" << std::endl;

    std::queue<const huffman_node_t*> nodes;
    if(node) nodes.push(node);

    while(!nodes.empty()) {
        const huffman_node_t *front = nodes.front();
        nodes.pop();

        // Print node properties
        oss << "\tp" << std::hex
            << ((long long int)front)
            << std::dec;
        if(front->c == '\0') {
            oss << "[label=\""
                << front->freq
                << "\"];" << std::endl;
        } else {
            oss << "[shape=record, "
                << "label=\"";
            if(isspace(front->c) || !isprint(front->c)) {
                if(front->c == EOF) {
                    oss << "EOF";
                } else {
                    oss << "0x" << std::hex
                        << std::setfill('0')
                        << std::setw(2)
                        << ((int)front->c)
                        << std::dec;
                }
            } else {
                oss << '\'';
                if(front->c == '\'') {
                    oss << "\\\'";
                } else if(front->c == '\"') {
                    oss << "\\\"";
                } else if(front->c == '<') {
                    oss << "\\<";
                } else if(front->c == '>') {
                    oss << "\\>";
                } else if(front->c == '{') {
                    oss << "\\<";
                } else if(front->c == '}') {
                    oss << "\\>";
                } else {
                    oss << front->c;
                }
                   oss << '\'';
            }
            oss << " | " << front->freq
                << "\"];" << endl;
        }
        
        // Print node children
        if(front->left) {
            oss << "\tp" << std::hex
                << ((long long int)front)
                << std::dec
                << ":sw -- "
                << "p" << std::hex
                << ((long long int)front->left)
                << std::dec << ":n"
                << "[label=\"0\"];"
                << endl;
            // Enqueue existing child
            nodes.push(front->left);
        }
        
        if(front->right) {
            oss << "\tp" << std::hex
                << ((long long int)front)
                << std::dec
                << ":se -- "
                << "p" << std::hex
                << ((long long int)front->right)
                << std::dec << ":n"
                << "[label=\"1\"];"
                << endl;
            nodes.push(front->right);
        }
    }
    
    oss << '}' << std::endl;
}

void
print_graphviz(const huffman_node_t *node)
{
    gen_graphviz(cout, node);
}

#ifdef USE_GRAPHVIZ
void
save_graphviz(const huffman_node_t *node)
{
    std::ofstream out;
    out.open("/tmp/huffmantree.dot");

    if(!out.is_open()) {
        cerr << "Erro ao gerar o arquivo temporario GraphViz"
             << endl;
        return;
    }

    gen_graphviz(out, node);
    out.close();
}
#endif

void
show_graphviz(const huffman_node_t *node, bool interact = false)
{
#ifdef USE_GRAPHVIZ
    save_graphviz(node);

    if(!interact) {
        if(system("/usr/bin/dot "
                  "/tmp/huffmantree.dot "
                  "-Kdot "
                  "-Tpng "
                  "-o /tmp/huffmantree.png")) {
            cerr << "Erro ao gerar a imagem temporaria" << endl;
            return;
        }

#ifdef USE_FEH        
        if(system("/usr/bin/feh "
                  "/tmp/huffmantree.png")) {
            cerr << "Erro ao mostrar a imagem temporaria" << endl;
            return;
        }
#endif // USE_FEH
    } else {
#ifdef USE_XDOT
        if(system("/usr/bin/xdot "
                  "/tmp/huffmantree.dot &")) {
            cerr << "Erro ao mostrar a arvore" << endl;
            return;
        }
#endif // USE_XDOT
    }
#endif // USE_GRAPHVIZ
}

bit_map_t
make_bit_map(huffman_node_t *root)
{
    bit_map_t bitmap;

    if(!root) return bitmap;

    node_set_t leaves = find_leaves(root);

    // Percorra todas as folhas, criando uma entrada no
    // charmap para cada uma delas
    for(auto leaf : leaves) {
        auto ptr = leaf;
        auto c   = leaf->c;
        while(ptr->parent) {
            // Insira o bit apropriado no final do vetor
            if(ptr == ptr->parent->left) {
                bitmap[c].push_back(false);
            } else {
                bitmap[c].push_back(true);
            }
            ptr = ptr->parent;
        }
        // Finalmente, inverta-o
        std::reverse(bitmap[c].begin(), bitmap[c].end());
    }

    return bitmap;
}

struct huffman_pair
{
    char     letter;
    uint32_t num_bits;
    uint8_t  dangling_bits;
    uint32_t num_bytes;
    uint8_t  *bits;
};

struct huffman_header
{
    char         flag[8] = "HUFFMAN";
    uint64_t     alphabet_size;
    uint8_t      dangling_space;
    huffman_pair *letters;
};

huffman_header
make_huffman_header(bit_map_t &bitmap)
{
    huffman_header h;
    h.alphabet_size = bitmap.size();
    h.letters = new huffman_pair[h.alphabet_size];

    auto itr = bitmap.begin();
    for(auto i = 0u; i < h.alphabet_size; i++) {
        h.letters[i].letter   = itr->first;
        h.letters[i].num_bits = itr->second.size();
        
        size_t arr_size = itr->second.size() / 8;
        if(itr->second.size() % 8) {
            arr_size++;
        }

        h.letters[i].bits      = new uint8_t[arr_size];
        h.letters[i].num_bytes = arr_size;

        { // Dump bits
            size_t current_byte = 0;
            uint8_t bit_buffer  = 0u;
            size_t current_bit  = 0;
            for(auto bit : itr->second) {
                if(current_bit == 8) {
                    h.letters[i].bits[current_byte] = bit_buffer;
                    bit_buffer  = 0u;
                    current_bit = 0;
                    current_byte++;
                }
            
                if(bit) {
                    bit_buffer |= ((uint8_t)(1u << (7u - current_bit)));
                }
                current_bit++;
            }
            // Dump last byte
            if(current_byte != arr_size)
                h.letters[i].bits[current_byte] = bit_buffer;
            // Store number of dangling bits
            h.letters[i].dangling_bits = 8u - current_bit;
        } // End of bit dump

        itr++;
    }
    return h;
}

void
destroy_huffman_header(huffman_header &h)
{
    for(auto i = 0u; i < h.alphabet_size; i++) {
        delete [] h.letters[i].bits;
    }
    delete [] h.letters;
}

void
print_header(const huffman_header &h)
{
    cout << "flag:           " << h.flag           << endl
         << "alphabet_size:  " << h.alphabet_size  << endl
         << "dangling_space: "
         << (int)h.dangling_space << endl
         << "letters:" << endl;
    for(auto i = 0u; i < h.alphabet_size; i++) {
        auto letter = h.letters + i;
        cout << "\tletter:        ";
        if(isspace(letter->letter) || !isprint(letter->letter)) {
             cout << "0x" << std::hex
                  << std::setfill('0')
                  << std::setw(8)
                  << ((int)letter->letter)
                  << std::dec;
        } else {
            cout << letter->letter;
        }
        cout << endl;

        cout << "\tnum_bits:      " << letter->num_bits      << endl
             << "\tdangling_bits: "
             << (int)letter->dangling_bits << endl
             << "\tnum_bytes:     " << letter->num_bytes     << endl
             << "\tbits:          ";

        for(auto j = 0u; j < letter->num_bytes; j++) {
            std::bitset<8> x(letter->bits[j]);
            cout << x << ' ';
        }
        cout << std::dec << endl << endl;
    }
}

class FileWriter
{
private:
    uint8_t              bit_buffer;
    uint8_t              buffered_bits;
    std::ofstream        stream;
    std::vector<uint8_t> written_bytes;
    
    void dump_bits();

public:
    FileWriter();
    ~FileWriter();

    void open(const std::string filename);
    bool is_open() const;
    void close();
    void write_bits(std::vector<bool> &bits);
    void write_header(const huffman_header &head);
    int  get_dangling_space() const;
};

FileWriter::FileWriter()
{
    bit_buffer    = 0u;
    buffered_bits = 0u;
}

FileWriter::~FileWriter()
{
    this->close();
}

#define bin_write(s, x)                         \
    s.write((const char*)&x, sizeof x)

void
FileWriter::dump_bits()
{
    if(buffered_bits > 0u) {
        written_bytes.push_back(bit_buffer);
        buffered_bits = 0u;
        bit_buffer    = 0u;
    }
}

void
FileWriter::open(const std::string filename) {
    stream.open(filename.c_str(), std::ios::binary);
}

bool
FileWriter::is_open() const {
    return stream.is_open();
}

void
FileWriter::close() {
    if(!stream.is_open())
        return;
    
    dump_bits();

    for(auto byte : written_bytes) {
        bin_write(stream, byte);
    }
    
    stream.close();
}

void
FileWriter::write_bits(std::vector<bool> &bits) {
    for(auto bit : bits) {
        if(buffered_bits == 8u) {
            dump_bits();
        }
        uint8_t proper_bit = bit ? 1u : 0u;
        proper_bit <<= (7u - buffered_bits);
        bit_buffer |= proper_bit;
        buffered_bits++;
    }
}

void
FileWriter::write_header(const huffman_header &head) {
    if(!stream.is_open()) {
        return;
    }

    stream.write(head.flag, 8 * sizeof(char));
    bin_write(stream, head.alphabet_size);
    bin_write(stream, head.dangling_space);
    for(auto i = 0u; i < head.alphabet_size; i++) {
        auto pair = head.letters + i;
        bin_write(stream, pair->letter);
        bin_write(stream, pair->num_bits);
        bin_write(stream, pair->dangling_bits);
        bin_write(stream, pair->num_bytes);
        for(auto j = 0u; j < pair->num_bytes; j++) {
            bin_write(stream, pair->bits[j]);
        }
    }
}

int
FileWriter::get_dangling_space() const
{
    return (8u - buffered_bits);
}

class FileReader
{
private:
    uint8_t        bit_buffer;
    uint8_t        buffered_bits;
    uint32_t       dangling_bits;
    std::ifstream  stream;

    void fetch_bits();

public:
    FileReader();
    ~FileReader();

    void           open(const std::string filename);
    bool           is_open() const;
    huffman_header read_header();
    char_map_t     make_charmap(huffman_header &h);
    bool           translate_bits(const std::string outfile_name,
                                  const char_map_t &charmap);
    void           close();
};

FileReader::FileReader()
{
    bit_buffer    = 0u;
    buffered_bits = 0u;
    dangling_bits = 0u;
}

FileReader::~FileReader()
{
    this->close();
}

#define bin_read(s, x)                          \
    s.read((char*)&x, sizeof x)

void
FileReader::fetch_bits()
{
    if(buffered_bits == 0u) {
        bin_read(stream, bit_buffer);
        buffered_bits = 8u;
        if(stream.eof()) {
            buffered_bits = 0u;
            bit_buffer    = 0u;
        }
    }
}

void
FileReader::open(const std::string filename)
{
    stream.open(filename.c_str(), std::ios::binary);
}

bool
FileReader::is_open() const
{
    return stream.is_open();
}

huffman_header
FileReader::read_header()
{
    huffman_header h;

    if(!stream.is_open())
        return h;

    stream.read(h.flag, 8 * sizeof(char));
    h.flag[7] = '\0';
    if(strcmp(h.flag, "HUFFMAN")) {
        throw "Arquivo invalido";
    }

    bin_read(stream, h.alphabet_size);
    bin_read(stream, h.dangling_space);

    h.letters = new huffman_pair[h.alphabet_size];
    for(auto i = 0u; i < h.alphabet_size; i++) {
        auto pair = h.letters + i;
        bin_read(stream, pair->letter);
        bin_read(stream, pair->num_bits);
        bin_read(stream, pair->dangling_bits);
        this->dangling_bits = pair->dangling_bits;
        bin_read(stream, pair->num_bytes);

        pair->bits = new uint8_t[pair->num_bytes];
        for(auto j = 0u; j < pair->num_bytes; j++) {
            bin_read(stream, pair->bits[j]);
        }
    }
    
    return h;
}

char_map_t
FileReader::make_charmap(huffman_header &h)
{
    char_map_t charmap;
    
    for(auto i = 0u; i < h.alphabet_size; i++) {
        auto pair = h.letters + i;
        std::vector<bool> bitvec;
        for(auto j = 0u; j < pair->num_bytes; j++) {
            for(uint8_t offset = 8; offset > 0; offset--) {
                if((j == pair->num_bytes - 1) &&
                   (offset <= pair->dangling_bits)) {
                    break;
                }
                uint8_t bit =
                    pair->bits[j] & ((uint8_t)(1u << (offset - 1)));
                bitvec.push_back(bit ? true : false);
            }
        }
        charmap[bitvec] = pair->letter;
    }
    
    return charmap;
}

bool
FileReader::translate_bits(const std::string outfile_name,
                           const char_map_t &charmap)
{
    std::ofstream ofs(outfile_name.c_str());
    if(!ofs.is_open()) {
        return false;
    }

    std::vector<bool> read_bits;
    while(stream.good()) {
        fetch_bits();
        if(buffered_bits == 0u) {
            break;
        }

        if(stream.peek() == EOF &&
           buffered_bits == dangling_bits) {
            break;
        }
        
        uint8_t current =
            bit_buffer & ((uint8_t)(1u << (buffered_bits - 1u)));
        read_bits.push_back(current ? true : false);
        buffered_bits--;

        auto itr = charmap.find(read_bits);
        if(itr != charmap.end()) {
            ofs << itr->second;
            read_bits.clear();
        }
    }
    ofs.close();
    return true;
}

void
FileReader::close()
{
    if(stream.is_open()) {
        stream.close();
    }
}

// Nomes de entrada e saída padrão para
// compressão.
static std::string output_filename = "a.out";
static std::string input_filename;

// Opções de argumentos
static bool extract    = false;
static bool dbg_output = false;
static int  dbg_type   = 0;
static bool dbg_bits   = false;
static bool dbg_header = false;

void
show_info()
{
    cout << "Huffman Compress/Decompress v1.0"             << endl
         << "Copyright (c) 2020 Lucas S. Vieira"           << endl
         << "Este codigo e distribuido sob a licenca MIT." << endl
         << "Veja o arquivo LICENSE para mais detalhes."   << endl
         << endl;
}

void
show_help()
{
    cout << "Uso:" << endl << endl
         << "\thuffman [opcoes...] <entrada> [-o <saida>]"       << endl
         << endl
         << "Argumentos da linha de comando:"                    << endl
         << endl
         << "  <entrada>  \tArquivo a ser operado."              << endl
         << endl
         << "  -o <saida> \tArquivo a ser escrito apos a"        << endl
         << "             \toperacao (opcional)."                << endl
         << endl
         << "  -x         \tExtrai o arquivo de entrada para a"  << endl
         << "             \tsaida informada."                    << endl
         << endl
         << "  --dot      \tMostra codigo GraphViz da arvore"    << endl
         << "             \tde Huffman."                         << endl
         << endl

#ifdef USE_FEH
         << "  --image    \tMostra a arvore de Huffman no feh."  << endl
         << endl
#endif // USE_FEH

#ifdef USE_XDOT
         << "  --xdot     \tMostra arvore de Huffman no xdot."   << endl
         << endl
#endif // USE_XDOT

         << "  --freq     \tMostra a tabela de frequencias."     << endl
         << endl
         << "  --bits     \tMostra o mapa de bits para o "       << endl
         << "             \talfabeto."                           << endl
         << endl
         << "  --head     \tMostra o cabecalho do arquivo"       << endl
         << "             \tapos comprimido."                    << endl
         << endl
         << "  --help     \tMostra este texto de ajuda."         << endl
         << endl
         << "  --info     \tMostra informacoes do programa e "   << endl
         << "             \tencerra."                            << endl
         << endl;
}

int
huffman_compress(void)
{
    // Abra o arquivo
    std::ifstream ifs(input_filename.c_str());

    if(!ifs.is_open()) {
        cerr << "Impossivel abrir o arquivo de entrada "
             << input_filename
             << endl;
        return 1;
    }
    
    // Gere mapa de frequências
    freq_map_t freq_map = build_frequency_map(ifs);
    ifs.clear();
    ifs.seekg(0);

    // Imprime mapa de frequências, se necessário
    if(dbg_output && dbg_type == 3) {
        show_freq_map(freq_map);
        ifs.close();
        return 0;
    }

    // Construa a árvore de Huffman
    auto root = build_huffman_tree(freq_map);

    // Mais ferramentas de debug, se utilizadas
    if(dbg_output) {
        int ret = 0;
        switch(dbg_type) {
        case 0:
            print_graphviz(root);
            break;
#ifdef USE_GRAPHVIZ
#ifdef USE_FEH
        case 1:
            show_graphviz(root);
            break;
#endif // USE_FEH

#ifdef USE_XDOT
        case 2:
            show_graphviz(root, true);
            break;
#endif // USE_XDOT
#endif // USE_GRAPHVIZ
        default:
            cerr << "Opcao de visualizacao nao-suportada"
                 << endl;
            ret = 1;
            break;
        }
        
        ifs.close();
        destroy_tree(root);
        return ret;
    }

    // Construa o mapa de bits para os caracteres.
    // Este mapa de bits deverá ser salvo
    // para descompressão
    bit_map_t bitmap = make_bit_map(root);

    // Destrua a árvore
    destroy_tree(root);
    
    if(dbg_bits) {
        print_bitmap(bitmap);
        ifs.close();
        return 0;
    }

    // Crie o cabeçalho do mapa de bits
    huffman_header h =
        make_huffman_header(bitmap);

    if(dbg_header) {
        print_header(h);
        ifs.close();
        return 0;
    }

    // Abra o arquivo de saída
    FileWriter fw;
    fw.open(output_filename);
    if(!fw.is_open()) {
        cerr << "Erro ao abrir arquivo de saida "
             << output_filename << endl;
        destroy_huffman_header(h);
        ifs.close();
        return 1;
    }

    
    // Reescreva o arquivo passo-a-passo
    while(ifs.good()) {
        char c = ifs.get();
        fw.write_bits(bitmap[c]);
    }
    // Feche, finalmente, o arquivo de entrada
    ifs.close();
    
    // Recupere o espaço extra ao final, armazene no cabeçalho
    h.dangling_space = fw.get_dangling_space();

    // Escreva o cabeçalho
    fw.write_header(h);

    // Feche o arquivo, escrevendo as informações
    fw.close();

    // Destrua o cabeçalho
    destroy_huffman_header(h);

    return 0;
}

int
huffman_extract(void)
{
    if(dbg_output) {
        cerr << "O processo de extracao nao remonta uma "
             << "arvore ou mapa de frequencias que possam "
             << endl
             << "ser visualizados"
             << endl;
        return 1;
    }

    // Abra o arquivo de entrada
    FileReader fr;
    fr.open(input_filename);
    if(!fr.is_open()) {
        cerr << "Impossivel abrir o arquivo de entrada "
             << input_filename << endl;
        return 1;
    }

    // Crie e popule o cabeçalho
    huffman_header h;

    try {
        h = fr.read_header();
    } catch(const char* &e) {
        cerr << "Erro ao ler o arquivo "
             << input_filename << ": "
             << e << endl;
        fr.close();
        return 1;
    }

    if(dbg_header) {
        print_header(h);
        destroy_huffman_header(h);
        fr.close();
        return 0;
    }

    // Cria mapa de caracteres
    // (Mapa reverso de bits)
    char_map_t charmap = fr.make_charmap(h);
    destroy_huffman_header(h);

    if(dbg_bits) {
        print_charmap(charmap);
        fr.close();
        return 0;
    }

    // Reescreva o arquivo na saida
    if(!fr.translate_bits(output_filename, charmap)) {
        cerr << "Erro ao traduzir o arquivo "
             << input_filename << endl;
        fr.close();
        return 1;
    }

    fr.close();
    return 0;
}

int
resolve_args(int argc, char **argv)
{
    for(int i = 1; i < argc; i++) {
        if(!strcmp(argv[i], "-o")) {
            i++;
            if(i >= argc) {
                cerr << "Nome do arquivo nao informado"
                     << endl;
                return 1;
            }
            output_filename = argv[i];
        } else if(!strcmp(argv[i], "-x")) {
            extract    = true;
        } else if(!strcmp(argv[i], "--dot")) {
            dbg_output = true;
            dbg_type   = 0;
        } else if(!strcmp(argv[i], "--image")) {
            dbg_output = true;
            dbg_type   = 1;
        } else if(!strcmp(argv[i], "--xdot")) {
            dbg_output = true;
            dbg_type   = 2;
        } else if(!strcmp(argv[i], "--freq")) {
            dbg_output = true;
            dbg_type   = 3;
        } else if(!strcmp(argv[i], "--bits")) {
            dbg_bits   = true;
        } else if(!strcmp(argv[i], "--head")) {
            dbg_header = true;
        } else if(!strcmp(argv[i], "--help")) {
            show_help();
            return 0;
        } else if(!strcmp(argv[i], "--info")) {
            show_info();
            return 0;
        } else {
            // Caso haja um nome de saída, colete-o
            input_filename = argv[i];
        }
    }

    if(input_filename == "") {
        cerr << "Informe um arquivo a ser operado."
             << endl;
        return 1;
    } else if(input_filename == output_filename) {
        cerr << "Usar o mesmo arquivo para entrada e "
             << "saida nao e recomendado" << endl;
        return 1;
    }

    return -1;
}

int
main(int argc, char **argv)
{
    // Parsing de argumentos do console
    // Devemos pelo menos um argumento. Lembrando
    // que argv[0] é a linha de execução do aplicativo.
    if(argc <= 1) {
        show_info();
        show_help();
        return 1;
    }

    int ret;
    if((ret = resolve_args(argc, argv)) >= 0) {
        return ret;
    }

    if(!extract) {
        return huffman_compress();
    } else {
        return huffman_extract();
    }
}

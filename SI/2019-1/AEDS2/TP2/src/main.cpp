#include <cstdio>
#include <cstring>
#include <chrono>

#include "files.hpp"
#include "sort.hpp"

using namespace std::chrono;

enum ord_method
{
    NONE_M,
    BUBBLE_M,
    SELECTION_M,
    INSERTION_M,
    MERGE_M,
    QUICK_M
};

#define STRING_EQ(t1, t2) (!strcmp(t1, t2))

inline void
print_numbers(const int* numbers, const size_t size)
{
    for(size_t i = 0; i < size; i++) {
        printf("%d ", numbers[i]);
    }
    putchar(10);
}

inline const char*
method_name(ord_method m) {
    switch(m) {
    case NONE_M:      return "Wut?"; // Wut?
    case BUBBLE_M:    return "Bubble";
    case SELECTION_M: return "Selection";
    case INSERTION_M: return "Insertion";
    case MERGE_M:     return "Merge";
    case QUICK_M:     return "Quick";
    }
    // isso aqui nunca deveria acontecer, mas...
    return "<unknown>";
}

long long int
dispatch_method(int* numbers, size_t size, ord_method m)
{
    auto t1 = high_resolution_clock::now();
    switch(m) {
    case NONE_M:      puts("Putz!"); break; // Putz!
    case BUBBLE_M:    bubblesort(numbers, size);    break;
    case SELECTION_M: selectionsort(numbers, size); break;
    case INSERTION_M: insertionsort(numbers, size); break;
    case MERGE_M:     mergesort(numbers, size);     break;
    case QUICK_M:     quicksort(numbers, size);     break;
    }
    auto t2 = high_resolution_clock::now();

    auto duration = duration_cast<microseconds>(t2 - t1).count();
    return duration;
}

void
show_help(const char* program_path)
{
    puts("Trabalho Pratico 2 de AEDS II -- UFVJM");
    puts("Tema: Metodos de ordenacao");
    puts("Periodo: 2019/1");
    puts("Copyright (c) Lucas Vieira");
    putchar(10);
    puts("Este projeto e distribuido sob a licença MIT.");
    puts("Veja o arquivo \"LICENSE\" para detalhes.");
    putchar(10);
    putchar(10);
    puts("Uso:");
    printf("\t%s metodo arquivo [args...]\n", program_path);
    putchar(10);
    puts("Argumentos possiveis:");
    puts("\tarquivo       Qualquer arquivo composto");
    puts("\t              apenas de dados numericos");
    putchar(10);
    puts("\t--bubble      Usa BubbleSort.");
    putchar(10);
    puts("\t--selection   Usa SelectionSort.");
    putchar(10);
    puts("\t--insertion   Usa InsertionSort.");
    putchar(10);
    puts("\t--merge       Use MergeSort.");
    putchar(10);
    puts("\t--quick       Usa QuickSort.");
    putchar(10);
    puts("\t--verbose     Mostra detalhes e o");
    puts("\t -v           conjunto de numeros, antes");
    puts("\t              depois da ordenacao.");
    putchar(10);
    puts("\t--help        Mostra este texto de ajuda");
    puts("\t -h           e encerra a execucao.");
    putchar(10);
    putchar(10);
    puts("Os argumentos de entrada do programa sao intercambiaveis.");
    puts("Arquivos e metodos informados por ultimo tem prioridade.");
}

int
main(int argc, char** argv)
{
    /* Parsing de argumentos */
    ord_method  method   = NONE_M;
    const char* filename = nullptr;
    bool        verbose  = false;

    for(int i = 1; i < argc; i++) {
        if(STRING_EQ(argv[i],      "--bubble"))
            method = BUBBLE_M;
        else if(STRING_EQ(argv[i], "--selection"))
            method = SELECTION_M;
        else if(STRING_EQ(argv[i], "--insertion"))
            method = INSERTION_M;
        else if(STRING_EQ(argv[i], "--merge"))
            method = MERGE_M;
        else if(STRING_EQ(argv[i], "--quick"))
            method = QUICK_M;
        else if(STRING_EQ(argv[i], "--verbose")
                || STRING_EQ(argv[i], "-v"))
            verbose = true;
        else if(STRING_EQ(argv[i], "--help")
                || STRING_EQ(argv[i], "-h")) {
            show_help(argv[0]);
            return 0;
        }
        else filename = argv[i];
    }

    if(!filename) {
        puts("Erro: nenhum arquivo informado.");
        puts("Para ajuda, use o argumento --help.");
        return 1;
    }

    if(method == NONE_M) {
        puts("Erro: Informe um metodo de ordenacao.");
        puts("Para ajuda, use o argumento --help.");
        return 1;
    }


    /* Entrada */
    size_t n_numbers;
    int* numbers = read_numbers(filename, n_numbers);

    if(!numbers) {
        puts("Erro: o arquivo nao existe ou esta vazio.");
        puts("Para ajuda, use o argumento --help.");
        return 1;
    }


    /* Informações iniciais (modo verbose apenas) */
    if(verbose) {
        printf("Ordenando numeros atraves de %s...\n",
               method_name(method));
        puts("Numeros:");
        print_numbers(numbers, n_numbers);
    }


    /* Ordenação */
    long long int time_taken_microseconds =
        dispatch_method(numbers, n_numbers, method);


    /* Debriefing */
    if(verbose) {
        printf("%9.9s: %10.1lld microssegundos (%s)\n",
               method_name(method),
               time_taken_microseconds,
               filename);
        puts("Ordenados:");
        print_numbers(numbers, n_numbers);
    } else {
        printf("%9.9s: %10.1lld microssegundos\n",
               method_name(method),
               time_taken_microseconds);
    }


    /* Limpeza */
    delete [] numbers;

    return 0;
}

#include <cstring>
#include <cstdio>

#include "files.hpp"

int*
read_numbers(const char* filename, size_t& out_size)
{
    FILE* fp;
    fp = fopen(filename, "r");
    if(!fp) {
        out_size = 0;
        return nullptr;
    }

    int v[100000];
    int buffer;
    out_size = 0;

    /* Nao teremos mais que cem mil numeros, entao nao
     * precisamos receber mais que isso.
     * Alguns programadores sabios uma vez me disseram:
     * "Um programa sem limites esta fadado ao fracasso".
     * Poetico, nao? Mas faz sentido. */
    while((out_size < 100000)
          && (fscanf(fp, "%d", &buffer) != EOF)) {
        v[out_size] = buffer;
        out_size++;
    }
    
    if(out_size == 0) {
        return nullptr;
    }

    int* vec = new int[out_size];
    memcpy(vec, v, out_size * sizeof(int));
    return vec;
}

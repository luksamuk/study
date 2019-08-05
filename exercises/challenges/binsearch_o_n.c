#include <stdio.h>

int
_bin_search_rec(int vec[], int p, int q, int key)
{
    int pivot = (p + q) / 2;
    if(vec[pivot] == key)
        return pivot;

    if(q <= p) return -1;

    int lside = _bin_search_rec(vec, p, pivot, key);
    int rside = _bin_search_rec(vec, pivot + 1, q, key);

    if(lside != -1) return lside;
    if(rside != -1) return rside;

    return -1;
}

int
PesquisaBinaria(int Vetor[], int tam, int chave)
{
    return _bin_search_rec(Vetor, 0, tam - 1, chave);
}

int
main(void)
{
    int vec[] = { 1, 2, 4, 5, 7, 9 };
    printf("%d\n", PesquisaBinaria(vec, 6, 4));
    return 0;
}

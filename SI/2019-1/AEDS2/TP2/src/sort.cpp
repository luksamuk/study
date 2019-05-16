#include <cstdlib>
#include <cstring>
#include <cstdio>

#include "sort.hpp"

inline void
swap(int& a, int& b)
{
    int temp = a;
    a = b;
    b = temp;
}

/* ================== BubbleSort ==================== */

void
bubblesort(int* numbers, size_t size)
{
    // Para cada elemento no vetor
    for(size_t i = 0; i < size; i++) {
        // Faça com que o elemento "mais pesado" à frente
        // (começando do final) "desça" para o início
        for(size_t j = size - 1; j > i + 1; j--) {
            if(numbers[j] < numbers[j - 1])
                swap(numbers[j], numbers[j - 1]);
        }
    }
}

/* ================= SelectionSort ================== */

void
selectionsort(int* numbers, size_t size)
{
    // Para cada elemento do vetor
    for(size_t i = 0; i < size - 1; i++) {
        // Selecione o menor valor entre
        // o atual e todos à frente
        size_t min_idx = i;
        for(size_t j = i + 1; j < size; j++) {
            if(numbers[j] < numbers[min_idx])
                min_idx = j;
        }
        // Mude de lugar com o valor atual
        if(min_idx != i)
            swap(numbers[i], numbers[min_idx]);
    }
}

/* ================= InsertionSort ================== */

void
insertionsort(int* numbers, size_t size)
{
    // Para cada elemento, começando do segundo
    for(size_t i = 1; i < size; i++) {
        // Tome o valor do elemento
        int key  = numbers[i];
        // Começando a partir do elemento anterior
        size_t j = i - 1;
        // Se não estamos no primeiro elemento,
        // e este elemento é maior que o valor tomado,
        // faça-o subir
        while((j > 0) && (numbers[j] > key)) {
            numbers[j + 1] = numbers[j];
            j--;
        }
        // O valor tomado é colocado no fim
        numbers[j + 1] = key;
    }
}

/* =================== MergeSort ==================== */

void
merge(int* numbers, int lbound, int pivot, int rbound)
{
    int size_lnums = (pivot - lbound) + 1,
        size_rnums = rbound - pivot;

    // Buffers temporários para partições. Copia cada
    // metade para um dos buffers
    int *lnums = new int[size_lnums],
        *rnums = new int[size_rnums];
    memcpy(lnums, numbers + lbound, size_lnums * sizeof(int));
    memcpy(rnums, numbers + pivot + 1, size_rnums * sizeof(int));

    // Mescla buffers temporários dentro de numbers
    int i = 0, j = 0, k = lbound;
    while(i < size_lnums && j < size_rnums) {
        if(lnums[i] <= rnums[j]) {
            numbers[k] = lnums[i];
            i++;
        } else {
            numbers[k] = rnums[j];
            j++;
        }
        k++;
    }

    // Copia números remanescentes de cada vetor
    while(i < size_lnums) {
        numbers[k] = lnums[i];
        i++;
        k++;
    }
    while(j < size_rnums) {
        numbers[k] = rnums[j];
        j++;
        k++;
    }

    // Dispõe dos vetores temporários
    delete [] lnums;
    delete [] rnums;
}

void
mergesort_rec(int* numbers, int lbound, int rbound)
{
    if(lbound < rbound) {
        //int pivot = (lbound + rbound) / 2;
        // Cálculo evitando overflow de variáveis
        int pivot = lbound + (rbound - lbound) / 2;

        // Mergesort recursivo nas duas metades
        mergesort_rec(numbers, lbound, pivot);
        mergesort_rec(numbers, pivot + 1, rbound);

        // Mescla as metades no vetor em si
        merge(numbers, lbound, pivot, rbound);
    }
}

void
mergesort(int* numbers, size_t size)
{
    mergesort_rec(numbers, 0, (int) (size - 1));
}

/* =================== QuickSort ==================== */

size_t
qs_part(int* numbers, int lbound, int rbound)
{
    int pivot_val = numbers[rbound];
    int min_idx = lbound - 1;

    // Para cada elemento, em cada partição
    for(int j = lbound; j <= rbound - 1; j++) {
        // Compare o número e o valor no pivô
        if(numbers[j] <= pivot_val) {
            min_idx++; // menor elemento agora é o próximo
            swap(numbers[min_idx], numbers[j]);
        }
    }
    swap(numbers[min_idx + 1], numbers[rbound]);
    return min_idx + 1;
}


void
qs_rec(int* numbers, int lbound, int rbound)
{
    if(lbound < rbound) {
        // qs_part coloca o pivô onde ele deve estar.
        // Tomamos seu índice
        size_t pivot = qs_part(numbers, lbound, rbound);

        // Ordena recursivamente quem está antes
        // e depois do pivô
        qs_rec(numbers, lbound, pivot - 1);
        qs_rec(numbers, pivot + 1, rbound);
    }
}

void
quicksort(int* numbers, size_t size)
{
    qs_rec(numbers, 0, (int) (size - 1));
}

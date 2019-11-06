#include <cstdio>
#include <cstdlib>

int
greatest(int *vec, size_t n)
{
    if(!vec) return 0;
    int res = vec[0];
    for(size_t i = 1; i < n; i++)
        if(vec[i] > res)
            res = vec[i];
    return res;
}

int
smallest(int *vec, size_t n)
{
    if(!vec) return 0;
    int res = vec[0];
    for(size_t i = 1; i < n; i++)
        if(vec[i] < res)
            res = vec[i];
    return res;
}

float
average(int *vec, size_t n)
{
    if(!vec) return 0;
    float sum = 0.0f;
    for(size_t i = 0; i < n; i++)
        sum += vec[i];
    return sum / ((float)n);
}

int
main(void)
{
    int buffer;
    
    printf("Insira a quantidade de numeros: ");
    scanf("%d", &buffer);

    if(buffer <= 0) {
        puts("Quantidade invalida");
        return 1;
    }

    size_t vec_size = (size_t)buffer;
    int *vec        = (int*)malloc(vec_size * sizeof *vec);
    
    printf("Insira %lu numeros:\n", vec_size);
    for(size_t i = 0; i < vec_size; i++)
        scanf("%d", vec + i);

    printf("Maior valor: %d\n"
           "Menor valor: %d\n"
           "Media:       %g\n",
           greatest(vec, vec_size),
           smallest(vec, vec_size),
           average(vec, vec_size));

    free(vec);
    
    return 0;
}

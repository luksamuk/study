#include <cstdio>

long int
potencia(long int x, long int y)
{
    if(y == 0) return 1;
    if(y == 1) return x;
    return x * potencia(x, y - 1);
}

int
main(void)
{
    // Testes
    long int a, b;
    scanf("%ld %ld", &a, &b);
    
    if(b < 0) return 1;
    
    printf("%ld\n", potencia(a, b));
    return 0;
}

#include <cstdio>

#define MIL_PER_INCH 25.4

int
main(void)
{
    int a, b;
    printf("Insira o numerador e o denominador da medida: ");
    scanf("%d %d", &a, &b);
    printf("%lf mm\n", ((double)a / (double)b) * MIL_PER_INCH);
    return 0;
}

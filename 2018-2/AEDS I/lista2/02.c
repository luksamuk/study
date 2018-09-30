#include <stdio.h>

int
main(void)
{
    int c, d, u;
    printf("Insira centenas, dezenas e unidades: ");
    scanf("%d %d %d", &c, &d, &u);
    printf("Dobro do numero: %d\n",
	   2 * ((c * 100) + (d * 10) + u));
    return 0;
}

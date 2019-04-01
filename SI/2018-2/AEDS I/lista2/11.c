#include <stdio.h>

int
main(void)
{
    int a, b;
    printf("Insira dois valores A e B: ");
    scanf("%d %d", &a, &b);

    {
	int tmp = a;
	a = b;
	b = tmp;
    }

    printf("Valores pos-troca:\n"
	   "\tA = %d\n"
	   "\tB = %d\n",
	   a, b);
    
    return 0;
}

#include <stdio.h>

int
main(void)
{
    int number;
    printf("Insira um numero: ");
    scanf("%d", &number);
    printf("Sucessor:   %d\n"
	   "Antecessor: %d\n",
	   number + 1,
	   number - 1);
    return 0;
}

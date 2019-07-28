#include <stdio.h>

int
main(void)
{
    double sum = 0, buffer;
    int i;
    for(i = 0; i < 3; i++) {
	scanf("%lf", &buffer);
	sum += buffer;
    }
    sum /= 3;
    printf("%s\n", (sum >= 6.0) ? "Aprovado" : "Reprovado");
    return 0;
}

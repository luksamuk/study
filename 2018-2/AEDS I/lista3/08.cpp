#include <cstdio>

int
main(void)
{
    float a, b;
    printf("Insira os coeficientes `a` e `b` para a equacao `ax + b = 0`: ");
    scanf("%f %f", &a, &b);

    // a == 0 => Não é equação
    // Não existem indeterminações em equações lineares.
    // `ax + b = 0` => `x = b/a`
    if(a == 0) {
	printf("Nao eh uma equacao\n");
	return 1;
    }
    
    printf("Solucao: %f\n", b / a);
    return 0;
}

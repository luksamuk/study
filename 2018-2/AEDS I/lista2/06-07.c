#include <stdio.h>
#include <math.h> // compile with -lm

// Este arquivo resolve os exercícios 06 e 07.

#define degtorad(d) (M_PI * d) / 180.0
#define radtodeg(r) (180.0 * r) / M_PI

int
main(void)
{
    float angle;

    // 06
    printf("Insira um angulo em graus: ");
    scanf("%f", &angle);
    printf("Arco em radianos: %0.2f\n", degtorad(angle));

    // 07
    printf("Insira um arco em radianos: ");
    scanf("%f", &angle);
    printf("Angulo em graus: %0.2f\n", radtodeg(angle));

    return 0;
}

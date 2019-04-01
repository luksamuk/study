#include <stdio.h>
#include <math.h> // Compile with -lm

float
circle_area(float radius)
{
    return 2.0f * M_PI * radius;
}

int
main(void)
{
    float side;
    printf("Insira o lado do quadrado (m): ");
    scanf("%f", &side);

    // Calculate circle area, then remove it from square's
    // total area so we get the value we want
    printf("Area do quadrado externa ao circulo inscrito: %0.2fm\n",
	   (side * side) - circle_area(side / 2.0f));
    return 0;
}

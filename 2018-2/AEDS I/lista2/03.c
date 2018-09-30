#include <stdio.h>
#include <math.h> // Compile with -lm

float
hypotenuse(float c1, float c2)
{
    return sqrt((c1 * c1) + (c2 * c2));
}

int
main(void)
{
    float width, height;
    printf("Insira base e altura do retangulo (cm): ");
    scanf("%f %f", &width, &height);
    printf("Perimetro: %0.2fcm\n"
	   "Diagonal:  %0.2fcm\n"
	   "Area:      %0.2fcm\n",
	   (width + height) * 2.0f,
	   hypotenuse(width, height),
	   width * height);
    return 0;
}

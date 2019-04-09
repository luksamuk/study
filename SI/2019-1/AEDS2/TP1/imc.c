#include <stdio.h>

int
main(void)
{
    float weight, height;
    scanf("%f %f", &weight, &height);
    printf("%0.2f\n", weight / (height * height));
    return 0;
}

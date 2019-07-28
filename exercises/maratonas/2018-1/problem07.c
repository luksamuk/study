#include <stdio.h>

int
main(void)
{
    float sum = 0.0f, buffer;
    int i, j;
    for(i = 0; i < 4; i++)
	for(j = 0; j < 4; j++) {
	    scanf("%f", &buffer);
	    if(i >= 3 - j) sum += buffer;
	}
    printf("%f\n", sum);
    return 0;
}

#include <stdio.h>

int
main(void)
{
    unsigned int a, b, c;
    scanf("%u %u %u" &a, &b, &c);
    printf("D = %u\n",
	   (((a + b) * (a + b)) + ((b + c) * (b + c))) / 2);
    return 0;
}

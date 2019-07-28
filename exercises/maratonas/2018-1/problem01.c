#include <stdio.h>

int
main(void)
{
    int n;
    scanf("%d", &n);
    printf("The number is %s and %s.\n",
	   (s % 2 == 0) ? "even" : "odd",
	   (s < 0) ? "negative" : "positive");
    return 0;
}

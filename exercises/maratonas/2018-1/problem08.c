#include <stdio.h>
#include <math.h> // compile with flag -lm

int
main(void)
{
    double num;
    printf("Input number > ");
    scanf("%lf", &num);

    int option = 0;
    do {
	printf("Choose an operation.\n\n"
	       "101 - square root\n"
	       "102 - half\n"
	       "103 - 10 perc\n"
	       "104 - double\n"
	       "105 - quit\n\n"
	       "input > ");
	scanf("%d", &option);
	
	option -= 101;
	switch(option) {
	case 0: printf("%lf\n", sqrt(num)); break;
	case 1: printf("%lf\n", num / 2.0); break;
	case 2: printf("%lf\n", num * 0.1); break;
	case 3: printf("%lf\n", num * 2.0); break;
	default: break;
	}
    } while(option != 5);
}

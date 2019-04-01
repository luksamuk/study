#include <cstdio>

int
main(void)
{
    double num;
    scanf("%lf", &num);
    printf("%s\n",
	   [&num]() {
	       return (num < 0.0) ? "negativo"
		   : ((num == 0.0) ? "nulo"
		      : "positivo");
	   }());
    return 0;
}

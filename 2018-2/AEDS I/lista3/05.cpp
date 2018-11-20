#include <cstdio>

#define divisible(x, y) (!(x % y))

int
main(void)
{
    int num;
    scanf("%d", &num);
    printf("%s\n",
	   [&num]() {
	       bool result = divisible(num, 5) && divisible(num, 7);
	       return (result ? "true" : "false");
	   }());
    return 0;
}

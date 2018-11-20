#include <cstdio>

#define absolute(x) ((x < 0) ? -x : x)

int
main(void)
{
    int num;
    scanf("%d", &num);
    printf("%d\n", absolute(num));
    return 0;
}

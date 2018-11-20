#include <cstdio>
#include <algorithm>

int
main(void)
{
    int a, b;
    scanf("%d %d", &a, &b);
    printf("%d\n", std::max(a, b));
    return 0;
}

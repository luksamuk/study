#include <cstdio>
#include <algorithm>

int
main(void)
{
    int a, b, c;
    scanf("%d %d %d", &a, &b, &c);
    printf("%d\n", std::max(a, std::max(b, c)));
    return 0;
}

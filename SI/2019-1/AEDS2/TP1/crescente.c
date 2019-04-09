#include <stdio.h>

int main(void)
{
    int a, b;
    scanf("%d %d", &a, &b);
    if(a < b)
        printf("%d %d\n", a, b);
    else printf("%d %d\n", b, a);
    return 0;
}

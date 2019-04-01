#include <cstdio>

int
main(void)
{
    int num;
    scanf("%d", &num);
    printf("%s\n", (!(num % 2)) ? "par" : "impar");
    return 0;
}

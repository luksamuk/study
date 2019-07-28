#include <stdio.h>

int
main(void)
{
    double salary;
    scanf("%lf", &salary);
    salary *= 0.9;
    salary *= 0.95;
    printf("%lf\n", salary);
    return 0;
}

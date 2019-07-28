#include <stdio.h>

int
main(void)
{
    int cdu;
    scanf("%d", &cdu);
    int c = cdu / 100;
    cdu -= c * 100;
    int d = cdu / 10;
    cdu -= d * 10;

    cdu *= 100;
    cdu += (d * 10) + c;
    printf("%d\n", cdu);
    return 0;
}

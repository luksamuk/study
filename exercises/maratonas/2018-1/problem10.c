#include <stdio.h>

int
main(void)
{
    int ddmmaa;
    scanf("%d", &ddmmaa);
    int dd = ddmmaa / 100000;
    ddmmaa -= (dd * 100000);
    int mm = ddmmaa / 100;
    ddmmaa -= (mm * 100);
    
    ddmmaa *= 10000;
    ddmmaa += (mm * 100) + dd;
    printf("%d\n", ddmmaa);
    return 0;
}

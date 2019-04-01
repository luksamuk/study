#include <stdio.h>

int
main(void)
{
    int seconds;
    printf("Insira o tempo em segundos: ");
    scanf("%d", &seconds);

    int hours = seconds / 3600;
    seconds -= hours * 3600;
    int minutes = seconds / 60;
    seconds -= minutes * 60;

    printf("Tempo formatado (HH:MM:SS): %02d:%02d:%02d\n",
	   hours, minutes, seconds);
    return 0;
}

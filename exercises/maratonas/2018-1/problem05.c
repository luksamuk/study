#include <stdio.h>

int
main(void)
{
    double time, avg_spd;
    scanf("%lf %lf", &time, &avg_spd);

    double distance = time * avg_spd;
    printf("Avg Speed:  %0.2lf\n"
	   "Time taken: %0.2lf\n"
	   "Distance:   %0.2lf\n"
	   "Gas used:   %0.2lf\n",
	   avg_spd, time, distance, (distance / 12.0));
    return 0;
}

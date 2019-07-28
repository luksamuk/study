#include <stdio.h>
#include <math.h> // compile with -lm

int
main(void)
{
    double x1, y1, x2, y2;
    scanf("%lf %lf %lf %lf", &x1, &y1, &x2, &y2);
    double dx = x2 - x1, dy = y2 - y1;
    printf("%lf\n", sqrt((dx * dx) + (dy * dy)));
    return 0;
}

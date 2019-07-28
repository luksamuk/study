#include <stdio.h>

int
is_triangle_possible(double x, double y, double z)
{
    return (x < y + z)
	&& (y < x + z)
	&& (z < x + y);
}

int
main(void)
{
    double x, y, z;
    scanf("%lf %lf %lf", &x, &y, &z);

    if(!is_triangle_possible(x, y, z)) {
	printf("This triangle cannot exist.\n");
    } else {
	printf("This triangle is %s.\n",
	       ((x == y) && (y == z))
	       ? "equilateral"
	       : (((x != y) && (y != z) && (z != x))
		  ? "scalene"
		  // Isosceles test is boring to write,
		  // so we just leave it for last, when
		  // the other ones fail.
		  : "isosceles"));
    }
    return 0;
}

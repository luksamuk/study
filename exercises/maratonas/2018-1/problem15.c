#include <stdio.h>

// retrieves the number of days for a specific month.
// Maybe having an entire function for this is a little
// too redundant.
int
number_of_days(int month)
{
    switch(month) {
    case 1: // fallthrough
    case 3:
    case 5:
    case 7:
    case 8:
    case 10:
    case 12:
	return 31;
    case 2:
	return 28; // ignore day 29
    case 0:
	return 0; // ignore month 0.
    default:
	return 30;
    }
}

// Calculates total number of days until current
// day of year, basically converting a pair (day, month)
// into a range [0..365].
int
total_days(int day, int month)
{
    int total = day;
    int i;
    for(i = 0; i < month; i++)
	total += number_of_days(i);
    return total;
}

// Using macros because I'm just too lazy to write this shit
// for every test. Also, macros in C are horrible.
#define on_range(n, day1, month1, day2, month2)	\
    ((n >= total_days(day1, month1)) && (n <= total_days(day2, month2)))

int
main(void)
{
    // zodiac
    int day, month;
    printf("Insert day and month of birthday > ");
    scanf("%d %d", &day, &month);

    int year_day = total_days(day, month);
    
    // Excuse my bad solution.
    // This results in redundant calculation, I suppose.
    // I still don't want to flood the code with if-and-else-ifs.
    printf("Your sign: %s\n",
	   (on_range(year_day,  22, 12,  31, 12)
	    || on_range(year_day,  1, 1,  20, 1))
	   ? "capricorn"
	   : (on_range(year_day,  21, 1,  19, 2)
	      ? "aquarius"
	      : (on_range(year_day,  20, 2,  20, 3)
		 ? "pisces"
		 : (on_range(year_day,  21, 3,  20, 4)
		    ? "aries"
		    : (on_range(year_day,  21, 4,  20, 5)
		       ? "taurus"
		       : (on_range(year_day,  21, 5,  20, 6)
			  ? "gemini"
			  : (on_range(year_day,  21, 6,  21, 7)
			     ? "cancer"
			     : (on_range(year_day,  22, 7,  22, 8)
				? "leo"
				: (on_range(year_day,  23, 8,  22, 9)
				   ? "virgo"
				   : (on_range(year_day,  23, 9,  22, 10)
				      ? "libra"
				      : (on_range(year_day,  23, 10,  21, 11)
					 ? "scorpio"
					 : "sagitarius")))))))))));
    return 0;
}

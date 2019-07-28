#include <stdio.h>

// Kind of borrowed the days-passed thing from problem 15,
// with tweaks for leap years

int
is_leap_year(int year)
{
    if(year % 4 == 0)
	return (year % 100 != 0);
    return (year % 400 == 0);
}

int
number_of_days(int month, int year)
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
	return is_leap_year(year) ? 29 : 28;
    case 0:
	return 0; // ignore month 0.
    default:
	return 30;
    }
}

int
total_days(int day, int month, int year)
{
    int total = day;
    int i;
    for(i = 0; i < month; i++)
	total += number_of_days(i, year);
    return total;
}


int
main(void)
{
    int birth_day, birth_month, birth_year,
	curr_day, curr_month, curr_year;
    printf("Input birthday (dd mm yyyy) > ");
    scanf("%d %d %d", &birth_day, &birth_month, &birth_year);
    printf("Input current date (dd mm yyyy) > ");
    scanf("%d %d %d", &curr_day, &curr_month, &curr_year);

    int age = curr_year - birth_year;
    // Check if your birthday has not passed
    if(total_days(birth_day, birth_month, birth_year)
       > total_days(curr_day, curr_month, curr_year))
	age--; // if it hasn't, then remove one year from calculus
    
    printf("You are %d years old.\n", age);

    return 0;
}

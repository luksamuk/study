#include <iostream>

int
main(void)
{
    std::cout << "Insira um ano: ";
    int year;
    std::cin >> year;

    std::cout << (!(year % 400) || (!(year % 4) && (year % 100))
		  ? 1
		  : 0;
    return 0;
}

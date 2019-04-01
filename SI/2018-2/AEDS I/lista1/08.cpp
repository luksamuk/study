#include <iostream>

int
main(void)
{
    std::cout << "Insira um ano: ";
    int year;
    std::cin >> year;

    if(!(year % 400) || (!(year % 4) && (year % 100)))
	std::cout << "Ano Bissexto" << std::endl;
    else std::cout << "Ano Nao Bissexto" << std::endl;
    
    return 0;
}

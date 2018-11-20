#include <iostream>

int
main(void)
{
    std::cout << "Insira um numero inteiro: ";
    int a;
    std::cin >> a;
    std::cout << "Sucessor: " << (a + 1)
	      << "Antecessor: " << (a - 1)
	      << std::endl;
    return 0;
}

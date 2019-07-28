#include <cstdio>
#include <set>

int
main(void)
{
    std::multiset<int> numbers;
    for(int i = 0; i < 10; i++) {
	int number;
	scanf("%d", &number);
	numbers.insert(number);
    }

    for(std::multiset<int>::iterator it = numbers.begin();
	it != numbers.end();
	it++) {
	printf("%d ", *it);
    }
    printf("\n");

    for(std::reverse_iterator<std::multiset<int>::iterator> it = numbers.rbegin();
	it != numbers.rend();
	it++) {
	printf("%d ", *it);
    }
    printf("\n");
    return 0;
}

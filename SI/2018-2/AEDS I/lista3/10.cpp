#include <cstdio>
#include <vector>
#include <algorithm>

// Compilar com --std=c++14, ou o compilador sinalizará erros devido
// ao uso de `auto` em lambdas.

int
main(void)
{
    // Magia negra com STL. Nem ferrando que vou ordenar isto na mão.
    // Mas até que seria fácil; o problema é fazer isso para mais valores.
    std::vector<float> v;
    for(int i = 0; i < 3; i++) {
	float buffer;
	scanf("%f", &buffer);
	v.push_back(buffer);
    }

    std::sort(v.begin(), v.end(),
	      [](auto x, auto y) { return x > y; });

    printf("%f\n", v[0] + v[1]);
    
    return 0;
}

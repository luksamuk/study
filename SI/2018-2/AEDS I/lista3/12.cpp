// NOTA: ISTO É PRATICAMENTE COPICOLA DO 11!
// Novamente, compile com --std=c++14.
#include <cstdio>
#include <vector>
#include <algorithm>

int
main(void)
{
    const size_t n_values = 4;
    
    std::vector<float> values;
    for(size_t i = 0; i < n_values; i++) {
	float buffer;
	scanf("%f", &buffer);
	values.push_back(buffer);
    }

    // Mudança: um lambda para ordenar na forma decrescente
    std::sort(values.begin(), values.end(),
	      [](auto x, auto y) { return x > y; });

    for(auto value : values)
	printf("%f ", value);
    puts("");

    return 0;
}

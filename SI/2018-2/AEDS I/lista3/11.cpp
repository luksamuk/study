// Compile com --std=c++11, ou com --std=c++14 mesmo.
#include <cstdio>
#include <vector>
#include <algorithm>

int
main(void)
{
    // Mude este valor se quiser brincar.
    const size_t n_values = 3;
    
    std::vector<float> values;
    for(size_t i = 0; i < n_values; i++) {
	float buffer;
	scanf("%f", &buffer);
	values.push_back(buffer);
    }

    // std::sort trabalha com ordem crescente, por padrão
    std::sort(values.begin(), values.end());

    for(auto value : values)
	printf("%f ", value);
    puts("");

    return 0;
}

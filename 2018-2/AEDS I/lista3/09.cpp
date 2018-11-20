

#include <cstdio>

int
main(void)
{

union linear_system_data {
    struct {
	float a, b, c, d, e, f;
    };
    float values[6];
};

linear_system_data data;

for(size_t i = 0; i < 6; i++)
    scanf("%f", data.values + i);

{
    bool null_value = false;
    for(size_t i = 0; i < 6; i++) {
	if(((i + 1) % 3)
	   && data.values[i] == 0.0f) {
	    null_value = true;
	    break;
	}
    }
	
    if(null_value) {
	printf("Impossivel resolver o sistema\n");
	return 1;
    }
}

float factor = data.a / data.d;

for(size_t i = 3; i < 6; i++)
    data.values[i] *= -1.0 * factor;

if(data.e + data.b == 0.0) {
    printf("Impossivel resolver o sistema\n");
    return 1;
}
float y = (data.c + data.f) / (data.e + data.b);

float x = (data.c - (data.b * y)) / data.a;

printf("Solução:\n"
       "x = %f\n"
       "y = %f\n",
       x, y);
    
return 0;
}

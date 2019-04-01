#include <stdio.h>

#define factor (5.0f / 9.0f)
#define to_fahrenheit(temp_celsius) (temp_celsius / factor) + 32.0f

int
main(void)
{
    float temperature;
    printf("Insira a temperatura (°C): ");
    scanf("%f", &temperature);
    printf("Temperatura em Fahrenheit: %0.2f°F\n",
	   to_fahrenheit(temperature));
    return 0;
}

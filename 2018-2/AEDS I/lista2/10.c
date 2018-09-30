#include <stdio.h>

// Macro reutilizado do exercício anterior
#define input(prompt, var) \
    printf(prompt); \
    scanf("%f", &var);

int
main(void)
{
    float trip_time, avg_spd;
    input("Insira o tempo de viagem (horas):           ", trip_time);
    input("Insira a velocidade media do carro: (Km/H): ", avg_spd);

    float distance = trip_time * avg_spd;
    printf("Gasto de combustivel em litros: %0.2f\n", distance / 12.0f);

    return 0;
}

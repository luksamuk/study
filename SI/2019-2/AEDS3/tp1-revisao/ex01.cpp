#include <cstdio>
#include <cmath>

int
calc_decay_time(float& mass)
{
    int time = 0;
    while(mass >= 0.5f) {
        mass /= 2.0f;
        time += 50;
    }
    return time;
}

int
main(void)
{
    float init_mass;
    printf("Insira a massa inicial: ");
    scanf("%f", &init_mass);

    // Copiamos a massa inicial para uma variável
    // que será passada por referência
    float final_mass = init_mass;

    int decay_time       = calc_decay_time(final_mass);

    int decay_time_hours = (int)truncf(decay_time / 3600);
    decay_time -= decay_time_hours * 3600;
    int decay_time_mins  = (int)truncf(decay_time / 60);
    decay_time -= decay_time_mins * 60;

    // Strings multilinha são concatenadas
    // automaticamente na compilação
    printf("Massa inicial:       %gg\n"
           "Massa final:         %gg\n"
           "Tempo de decaimento: %dh%dm%ds\n",
           init_mass, final_mass,
           decay_time_hours,
           decay_time_mins,
           decay_time);
    return 0;
}

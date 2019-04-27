#include <stdio.h>


int
main(void)
{
    // Este problema tem uma solução elegante, que lembra
    // bastante um heap sort.
    int i, n_teams = 16;
    char teams[16];

    // Populando o array
    for(i = 0; i < n_teams; i++) {
        teams[i] = 'A' + i;
    }

    while(n_teams > 1) {
        // Cada rodada tem uma quantidade de jogos igual à metade
        // do número de times
        n_teams /= 2;

        // Para cada placar recebido, reescreva, em sequência, os
        // ganhadores no vetor.
        for(i = 0; i < n_teams; i++) {
            int a, b;
            scanf("%d %d", &a, &b);
            // Os oponentes para cada jogo estão em 2i e 2i+1,
            //de forma que o vetor funciona como uma árvore heap
            int game_index = 2 * i;
            char winner_index = game_index + ((b > a) ? 1 : 0);
            teams[i] = teams[winner_index];
        }
    }

    // O time restante é o ganhador.
    printf("%c\n", teams[0]);
    
    return 0;
}

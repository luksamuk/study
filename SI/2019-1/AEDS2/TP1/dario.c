#include <stdio.h>

int
winner_of(int a, int b)
{
    if(a < 2) a += 5;
    if(b < 2) b += 5;
    
    if(b <= (a + 2))
        return 1;
    return 0;
}

int
main(void)
{
    int n_rodadas;
    scanf("%d", &n_rodadas);
    int n_dario = 0, n_xerxes = 0;
    while(n_rodadas > 0) {
        int a, b;
        scanf("%d %d", &a, &b);
        n_dario  += winner_of(a, b);
        n_xerxes += winner_of(b, a);
        n_rodadas--;
    }

    printf("%s\n", (n_dario > n_xerxes) ? "dario" : "xerxes");
    
    return 0;
}

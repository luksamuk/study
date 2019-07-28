#include <cstdio>

int main(void)
{
    while(true)
    {
        // n = quantidade de armários do pedido
        // l = armários livres
        int n, l;
        scanf("%d %d", &n, &l);
        if(!n && !l) break;

        // Lista de armários disponíveis
        int* disp = new int[l];
        for(int i = 0; i < l; i++) {
            scanf("%d", &disp[i]);
        }

        int min_swaps = n;

        // Verifique de n até 0 a quantidade
        // de números máximos consecutivos.
        // Por exemplo, se há n consecutivos,
        // então não preciso de trocas! Mas, se não,
        // então passo para n - 1 e assim por diante.
        for(int req = n; req > 0; req--)
        {
            // Dada uma sequência [i, (i + req - 1)]
            // de armários que podem ser consecutivos...
            for(int i = 0; (i + req - 1) < l; i++)
            {
                bool invalid = false;
                int end = (i + req - 1);
                // Para cada par, verifique a diferença.
                for(int j = i; (j + 1) <= end; j++)
                {
                    // Se for maior que 1, então não há consecutivos.
                    if(disp[j + 1] - disp[j] > 1)
                    {
                        invalid = true;
                        // Não faz sentido percorrer o que já sabemos
                        // ser não-consecutivo
                        i = j;
                        break;
                    }
                }
                if(invalid) continue;

                // Todos os slots são válidos.
                // Se req == n aqui, então não precisamos de swaps;
                // a resposta é zero.
                if(req == n) {
                    min_swaps = 0;
                    break;
                }
                else
                {
                    // Mas se req < n, então precisamos
                    // verificar quantos swaps precisam ser feitos.
                    // Para isso, olhamos para a esquerda e para a direita.
                    int diff = n - req;
                    int left, right;
                    left = right = 0;
                    // Para a esquerda
                    // Para a direita

                }
            }

            // Se já temos o número mínimo de swaps (0),
            // não faz sentido continuar.
            if(!min_swaps) break;
        }

        // Mostre o número mínimo de swaps
        printf("%d\n", min_swaps);

        delete [] disp;
    }
    return 0;
}

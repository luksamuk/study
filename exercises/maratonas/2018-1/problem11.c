#include <stdio.h>

#define value_of(n) ((n == 0) ? 1 : ((n == 1) ? 10 : 100))

int
main(void)
{
    int compra;
    printf("Valor da compra (int) > ");
    scanf("%d", &compra);
    
    int pagamento;
    printf("Pagamento recebido (int) > ");
    scanf("%d", &pagamento);

    if(pagamento < compra) return 1; // Não vendo nada fiado.

    int troco = pagamento - compra;
    int troco_holder = troco;

    // NOTA: Dá pra melhorar isso aqui.
    // No fim das contas, a lógica a ser aplicada aqui poderia
    // ser similar aos problemas 9 e 10, e inclusive seria mais
    // fácil se aplicada dessa forma. Bastaria quebrar o número
    // em centenas, dezenas e unidades.
    int notas[3] = {0};
    for(int i = 2, value; i >= 0; i--) {
        value = value_of(i);
	notas[i] = troco / value;
	troco = troco % value;
    }
    
    printf("Valor da compra:                     %d\n"
	   "Valor pago:                          %d\n"
	   "Troco:                               %d\n"
	   "Quantidade minima de notas de troco: %d\n"
	   "Dentre as quais, temos:\n"
	   "\t%d notas de 1\n"
	   "\t%d notas de 10\n"
	   "\t%d notas de 100.\n",
	   compra, pagamento, troco_holder,
	   notas[0] + notas[1] + notas[2],
	   notas[0], notas[1], notas[2]);
    return 0;
}

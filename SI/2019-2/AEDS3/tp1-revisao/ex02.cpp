#include <cstdio>

const char *months[] = {
    "janeiro", "fevereiro", "marco",
    "abril",   "maio",      "junho",
    "julho",   "agosto",    "setembro",
    "outubro", "novembro",  "dezembro"
};

void
print_month(int n)
{
    puts((n < 1 || n > 12)
         ? "Erro: o mes eh invalido"
         : months[n - 1]);
}


int
main(void)
{
    int the_month;
    printf("Informe o numero do mes: ");
    scanf("%d", &the_month);
    print_month(the_month);
    return 0;
}

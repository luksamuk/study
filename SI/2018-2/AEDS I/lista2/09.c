#include <stdio.h>

// Macro bobo, só para evitar ficar repetindo isso toda hora
#define input(prompt, var) \
    printf(prompt); \
    scanf("%f", &var);

int
main(void)
{
    float working_hours, hourly_salary, taxes_perc;
    input("Insira o numero de horas trabalhadas:    ", working_hours);
    input("Insira o valor do salario-hora:          ", hourly_salary);
    input("Insira o percentual de desconto do INSS: ", taxes_perc);

    // Taxas são convertidas do range [0..100] para o range [0..1]
    taxes_perc /= 100.0f;

    float raw_salary    = working_hours * hourly_salary;
    float liquid_salary = raw_salary * (1.0f - taxes_perc);

    printf("Salario bruto:   R$ %0.2f\n"
	   "Salario liquido: R$ %0.2f\n",
	   raw_salary, liquid_salary);
    return 0;
}

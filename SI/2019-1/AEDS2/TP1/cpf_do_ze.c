#include <stdio.h>

#define fill_array(array, n)                            \
    {                                                   \
        int _arr_n;                                     \
        for(_arr_n = 0; _arr_n < n; _arr_n++) {         \
            scanf("%d", (int*)(array + _arr_n));        \
        }                                               \
    }

#define deduce_digit(x)   ((x % 11) < 2) ? 0 : (11 - (x % 11))

int
calc_nj_sum(const int* array, int n_digits)
{
    int iter, result = 0;
    for(iter = 0; iter < n_digits; iter++) {
        int i = (n_digits + 2) - (iter + 1);
        result += (array[iter] * i);
    }
    return result;
}

void
print_cpf(const int* array)
{
    int i;
    for(i = 0; i < 9; i++) {
        putchar('0' + array[i]);
    }
    putchar('-');
    for(; i < 11; i++) {
        putchar('0' + array[i]);
    }
    putchar(10);
}

int
main(void)
{
    int n_cases;
    scanf("%d", &n_cases);

    int curr_case = 0;
    while(curr_case < n_cases) {
        int cpf[11];
        fill_array(cpf, 9);

        int fst_digit = calc_nj_sum(cpf, 9);
        cpf[9]  = deduce_digit(fst_digit);
        int snd_digit = calc_nj_sum(cpf, 10);
        cpf[10] = deduce_digit(snd_digit);

        printf("Caso %d: ", curr_case + 1);
        print_cpf(cpf);
        
        curr_case++;
    }
    return 0;
}

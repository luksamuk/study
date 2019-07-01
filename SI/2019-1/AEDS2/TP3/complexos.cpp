#include <iostream>

/* TAD para números complexos */
struct complex_t {
    int real;
    int imag;
};

void
print_complex(std::ostream& os, const complex_t& complex)
{
    // A impressão do TAD adequa-se às regras do std::cout.
    os << complex.real;
    if(complex.imag >= 0)
        os << '+' << complex.imag;
    else os << complex.imag;
    os << 'i';
}

complex_t
sum_complex(complex_t a, complex_t b)
{
    complex_t result;
    result.real = a.real + b.real;
    result.imag = a.imag + b.imag;
    return result;
}

complex_t
mult_complex(complex_t a, complex_t b)
{
    complex_t result;
    result.real = (a.real * b.real) - (a.imag * b.imag);
    result.imag = (a.real * b.imag) + (b.real * a.imag);
    return result;
}



/* Overload de operadores */
complex_t
operator+(const complex_t& lhs, const complex_t& rhs)
{
    return sum_complex(lhs, rhs);
}

complex_t
operator*(const complex_t& lhs, const complex_t& rhs)
{
    return mult_complex(lhs, rhs);
}

std::ostream&
operator<<(std::ostream& os, const complex_t& obj)
{
    // Apenas invoque a função de impressão.
    print_complex(os, obj);
    return os;
}

std::istream&
operator>>(std::istream& is, complex_t& obj)
{
    // A leitura do TAD adequa-se às regras do std::cin.
    is >> obj.real;
    is >> obj.imag;
    return is;
}



/* Entry point */
int
main(void)
{
    complex_t a, b;

    std::cin >> a >> b;
    
    std::cout << "Soma: " << (a + b) << std::endl
              << "Multi: " << (a * b) << std::endl;
    
    return 0;
}

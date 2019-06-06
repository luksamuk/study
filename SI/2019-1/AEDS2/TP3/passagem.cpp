#include <iostream>
#include <string>

typedef unsigned int code_t;

struct pass_t
{
    code_t      pass_number;
    std::string name;
    std::string date;
    std::string hour;
    std::string origin;
    std::string dest;
    code_t      seat;
};

std::ostream&
operator<<(std::ostream& os, const pass_t& pass)
{
    os << pass.name        << std::endl
       << pass.pass_number << std::endl
       << pass.date        << std::endl
       << pass.hour        << std::endl
       << pass.origin      << std::endl
       << pass.dest        << std::endl
       << pass.seat        << std::endl;
    return os;
}

inline void
eat_newline_chars(std::istream& is)
{
    while(is.peek() == '\n')
        is >> std::ws;
}

std::istream&
operator>>(std::istream& is, pass_t& pass)
{
    eat_newline_chars(is);
    std::getline(is, pass.name);
    is >> pass.pass_number;
    eat_newline_chars(is);
    std::getline(is, pass.date);
    std::getline(is, pass.hour);
    std::getline(is, pass.origin);
    std::getline(is, pass.dest);
    is >> pass.seat;
    eat_newline_chars(is);
    return is;
}



void
print_pass_data(pass_t pass)
{
    std::cout << pass;
}

pass_t
read_pass_data(void)
{
    pass_t pass;
    std::cin >> pass;
    return pass;
}

int
main(void)
{
    pass_t *passes;
    int num_passes;
    std::cin >> num_passes;

    if(num_passes <= 0) return 0;

    passes = new pass_t[num_passes];
    if(!passes) return 1;

    for(int i = 0; i < num_passes; i++) {
        passes[i] = read_pass_data();
    }

    for(int i = 0; i < num_passes; i++) {
        std::cout << "Passageiro " << (i + 1) << std::endl
                  << passes[i]     << std::endl;
    }

    delete [] passes;

    return 0;
}

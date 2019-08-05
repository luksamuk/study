#include <iostream>
#include <optional>
#include <ranges>

using namespace std;

void
print_ten_primes()
{
    using view::iota;
    using view::filter;
    using view::take;
    
    auto is_prime =
        [](int n) {
            if(n % 2 == 0) return false;
            for(int i = 3; i <= n / 2; i += 2)
                if(n % i == 0) return false;
            return true;
        };
    
    for (auto i : iota(1) | filter(is_prime) | take(10)) {
        std::cout << i << ',' ;
    }
}

void
print_ten_pythagorean_triples()
{
    using view::iota;
    using std::ranges::for_each;
    using std::ranges::yield_if;
    using view::take;
    
    auto triples =
        for_each(iota(1), [] (int z) {
            return for_each(iota(1, z+1), [=](int x) {
                return for_each(iota(x, z+1), [=](int y) {
                    return yield_if(x*x + y*y == z*z,
                                    make_tuple(x, y, z));
                });
            });
        });

    for(auto triple : triples | take(10)) {
        std::cout << '('
                  << get<0>(triple) << ','
                  << get<1>(triple) << ','
                  << get<2>(triple) << ')'
                  << std::endl;
    }
}

int
main(void)
{
    print_ten_primes();
    print_ten_pythagorean_triples();
    return 0;
}


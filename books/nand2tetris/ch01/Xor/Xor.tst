load        Xor.hdl,
output-file Xor.out,
compare-to  Xor.cmp,
output-list a%B1.1.1 b%B1.1.1 out%B2.1.2;

set a %B0, set b %B0, eval, output;
set a %B1, set b %B0, eval, output;
set a %B0, set b %B1, eval, output;
set a %B1, set b %B1, eval, output;

load        Mux.hdl,
output-file Mux.out,
compare-to  Mux.cmp,
output-list a%B1.1.1 b%B1.1.1 sel%B2.1.2 out%B2.1.2;

set a %B0, set b %B0, set sel %B0, eval, output;
set a %B0, set b %B0, set sel %B1, eval, output;

set a %B1, set b %B0, set sel %B0, eval, output;
set a %B1, set b %B0, set sel %B1, eval, output;

set a %B0, set b %B1, set sel %B0, eval, output;
set a %B0, set b %B1, set sel %B1, eval, output;

set a %B1, set b %B1, set sel %B0, eval, output;
set a %B1, set b %B1, set sel %B1, eval, output;

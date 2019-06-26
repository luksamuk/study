load        DMux.hdl,
output-file DMux.out,
compare-to  DMux.cmp,
output-list in%B2.1.1 sel%B2.1.2 a%B1.1.1 b%B1.1.1;

set in %B0, set sel %B0, eval, output;
set in %B0, set sel %B1, eval, output;
set in %B1, set sel %B0, eval, output;
set in %B1, set sel %B1, eval, output;

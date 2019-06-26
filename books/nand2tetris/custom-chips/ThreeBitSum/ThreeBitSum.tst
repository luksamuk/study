load HalfSum.hdl,
load Sum.hdl,
load ThreeBitSum.hdl,
output-file ThreeBitSum.out,
output-list a%B3.3.3 b%B3.3.3 out%B4.4.4;

set a %B000, set b %B001, eval, output;
set a %B001, set b %B001, eval, output;
set a %B010, set b %B001, eval, output;
set a %B101, set b %B100, eval, output;
set a %B101, set b %B101, eval, output;
set a %B111, set b %B111, eval, output;


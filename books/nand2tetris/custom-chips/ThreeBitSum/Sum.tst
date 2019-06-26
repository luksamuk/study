load HalfSum.hdl,
load Sum.hdl,
output-file Sum.out,
output-list a%B3.1.3 b%B3.1.3 carryin%B3.1.3 out%B3.1.3 carryout%B3.1.3;

set a 1, set b 0, set carryin 0, eval, output;
set a 0, set b 1, set carryin 0, eval, output;
set a 1, set b 1, set carryin 0, eval, output;
set a 1, set b 1, set carryin 1, eval, output;

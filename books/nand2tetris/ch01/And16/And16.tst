load        And16.hdl,
output-file And16.out,
compare-to  And16.cmp,
output-list a%B1.16.1 b%B1.16.1 out%B1.16.1;

set a 0, set b 0, eval, output;
set a 1, set b 1, eval, output;
set a 2, set b 2, eval, output;
set a 4, set b 4, eval, output;
set a 8, set b 8, eval, output;
set a 16, set b 16, eval, output;
set a 32, set b 32, eval, output;
set a 64, set b 64, eval, output;
set a 128, set b 128, eval, output;
set a 256, set b 256, eval, output;
set a 512, set b 512, eval, output;
set a 1024, set b 1024, eval, output;
set a 2048, set b 2048, eval, output;
set a 4096, set b 4096, eval, output;
set a 8192, set b 8192, eval, output;
set a 16384, set b 16384, eval, output;
set a %B1000000000000000, set b %B1000000000000000, eval, output;

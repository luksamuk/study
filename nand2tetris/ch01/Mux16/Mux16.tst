load        Mux16.hdl,
output-file Mux16.out,
compare-to  Mux16.cmp,
output-list a%B1.16.1 b%B1.16.1 sel%B2.1.2 out%B1.16.1;

set a 0,     set b %B1000000000000000, set sel 0, eval, output;
set a 1,     set b 16384, set sel 1, eval, output;
set a 2,     set b 8192,  set sel 0, eval, output;
set a 4,     set b 4096,  set sel 1, eval, output;
set a 8,     set b 2048,  set sel 0, eval, output;
set a 16,    set b 1024,  set sel 1, eval, output;
set a 32,    set b 512,   set sel 0, eval, output;
set a 64,    set b 256,   set sel 1, eval, output;
set a 128,   set b 0,     set sel 0, eval, output;
set a 256,   set b 256,   set sel 1, eval, output;
set a 512,   set b 512,   set sel 0, eval, output;
set a 1024,  set b 1024,  set sel 1, eval, output;
set a 2048,  set b 2048,  set sel 0, eval, output;
set a 4096,  set b 4096,  set sel 1, eval, output;
set a 8192,  set b 8192,  set sel 0, eval, output;
set a 16384, set b 16384, set sel 1, eval, output;
set a %B1000000000000000, set b 0, set sel 1, eval, output;

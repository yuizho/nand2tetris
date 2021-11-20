load RAM64.hdl,
output-file RAM64.out,
compare-to RAM64.cmp,
output-list in%B1.16.1 load%B3.1.3 address%B1.6.1 out%B1.16.1;

set in %B1111111111111111,
set load 0,
set address 000000,
tick,
tock,
output;

set in %B1111111111111111,
set load 1,
set address 000000,
tick,
tock,
output;

set in %B0000000000000000,
set load 0,
set address 000000,
tick,
tock,
output;

set in %B1000000000000000,
set load 0,
set address %B111111,
tick,
tock,
output;

set in %B1000000000000000,
set load 1,
set address %B111111,
tick,
tock,
output;

set in %B0000000000000000,
set load 0,
set address %B111111,
tick,
tock,
output;
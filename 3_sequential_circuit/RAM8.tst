load RAM8.hdl,
output-file RAM8.out,
compare-to RAM8.cmp,
output-list in%B1.16.1 load%B3.1.3 address%B3.3.3 out%B1.16.1;

set in %B1111111111111111,
set load 0,
set address 000,
tick,
tock,
output;

set in %B1111111111111111,
set load 1,
set address 000,
tick,
tock,
output;

set in %B0000000000000000,
set load 0,
set address 000,
tick,
tock,
output;

set in %B1000000000000000,
set load 0,
set address %B111,
tick,
tock,
output;

set in %B1000000000000000,
set load 1,
set address %B111,
tick,
tock,
output;

set in %B0000000000000000,
set load 0,
set address %B111,
tick,
tock,
output;
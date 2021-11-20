load RAM16K.hdl,
output-file RAM16K.out,
compare-to RAM16K.cmp,
output-list in%B1.16.1 load%B3.1.3 address%B1.14.1 out%B1.16.1;

set in %B1111111111111111,
set load 0,
set address 00000000000000,
tick,
tock,
output;

set in %B1111111111111111,
set load 1,
set address 00000000000000,
tick,
tock,
output;

set in %B0000000000000000,
set load 0,
set address 00000000000000,
tick,
tock,
output;

set in %B1000000000000000,
set load 0,
set address %B11111111111111,
tick,
tock,
output;

set in %B1000000000000000,
set load 1,
set address %B11111111111111,
tick,
tock,
output;

set in %B0000000000000000,
set load 0,
set address %B11111111111111,
tick,
tock,
output;
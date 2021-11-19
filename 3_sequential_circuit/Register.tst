load Register.hdl,
output-file Register.out,
compare-to Register.cmp,
output-list in%B1.16.1 load%B3.1.3 out%B1.16.1;

set in %B1111111111111111,
set load 0,
tick,
tock,
output;

set in %B1111111111111111,
set load 1,
tick,
tock,
output;

set in %B0000000000000000,
set load 0,
tick,
tock,
output;
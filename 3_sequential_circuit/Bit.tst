load Bit.hdl,
output-file Bit.out,
compare-to Bit.cmp,
output-list in%B3.1.3 load%B3.1.3 out%B3.1.3;

set in 1,
set load 0,
tick,
tock,
output;

set in 1,
set load 1,
tick,
tock,
output;

set in 0,
set load 0,
tick,
tock,
output;
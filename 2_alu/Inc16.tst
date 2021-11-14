load Inc16.hdl,
output-file Inc16.out,
compare-to Inc16.cmp,
output-list in%B1.16.1 out%B1.16.1;

set in %B0000000000000000,
eval,
output;

set in %B1111111111111111,
eval,
output;

set in %B0100000000000101,
eval,
output;

set in %B0000000000000100,
eval,
output;
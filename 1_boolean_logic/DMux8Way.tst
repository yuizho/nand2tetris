load DMux8Way.hdl,
output-file DMux8Way.out,
compare-to DMux8Way.cmp,
output-list in%B3.1.3 sel%B1.3.1 a%B3.1.3 b%B3.1.3 c%B3.1.3 d%B3.1.3 e%B3.1.3 f%B3.1.3 g%B3.1.3 h%B3.1.3;

set in 1,
set sel %B000,
eval,
output;

set in 1,
set sel %B001,
eval,
output;

set in 1,
set sel %B010,
eval,
output;

set in 1,
set sel %B011,
eval,
output;

set in 1,
set sel %B100,
eval,
output;

set in 1,
set sel %B101,
eval,
output;

set in 1,
set sel %B110,
eval,
output;

set in 1,
set sel %B111,
eval,
output;
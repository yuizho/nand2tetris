load Max.asm,
output-file Max.out,
compare-to Max.cmp,
output-list RAM[0]%D2.6.2 RAM[1]%D2.6.2 RAM[2]%D2.6.2;

set RAM[0] 15,
set RAM[1] 32;
repeat 14 {
    ticktock;
}
output;

set PC 0, // reset PC (it's important!!!!!)
set RAM[0] 47,
set RAM[1] 22;
repeat 14 {
    ticktock;
}
output;
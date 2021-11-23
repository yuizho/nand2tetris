// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
    @0 // flag typed or untyped
    M=0
    @1 // screen index
    M=-1
(LOOP)
    // reset screen when screen is fille
    @1
    D=M
    @KBD // last index of screen
    D=D-A
    @RESET_SECREEN_INDEX
    D;JEQ
    // check if keyboead is typed or not
    @KBD
    D=M
    @TYPED
    D;JNE
    @UNTYPED
    D;JEQ
(TYPED)
    // show black to screen
    @1
    D=M+1
    M=D
    @SCREEN
    A=D+A
    M=-1
    // reset screen index if needed
    @0
    D=M
    M=-1
    @RESET_SECREEN_INDEX
    D;JEQ
    @LOOP
    0;JMP
(UNTYPED)
    // show black to screen
    @1
    D=M+1
    M=D
    @SCREEN
    A=D+A
    M=0
    // reset sreen index if needed
    @0
    D=M
    M=0
    @RESET_SECREEN_INDEX
    D;JNE
    @LOOP
    0;JMP
(RESET_SECREEN_INDEX)
    @1
    M=-1
    @LOOP
    0;JMP
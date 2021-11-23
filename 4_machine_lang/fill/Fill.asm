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
(INIT)
    @typed // flag typed or untyped
    M=0
    @index // screen index
    M=-1

(LOOP)
    // reset screen when screen is fille
    @index
    D=M
    @KBD // next index of last screen index
    D=D-A
    @RESET_SECREEN_INDEX
    D;JEQ
    // check if keyboead is typed or not
    @KBD
    D=M
    @BLACK
    D;JNE
    @WHITE
    D;JEQ

(BLACK)
    // reset screen index if needed
    @typed
    D=M
    M=1
    @RESET_SECREEN_INDEX
    D;JEQ
    // show black to screen
    @index
    D=M+1
    M=D
    @SCREEN
    A=D+A
    M=-1
    // jump to main loop
    @LOOP
    0;JMP

(WHITE)
    // reset sreen index if needed
    @typed
    D=M
    M=0
    @RESET_SECREEN_INDEX
    D;JNE
    // show black to screen
    @index
    D=M+1
    M=D
    @SCREEN
    A=D+A
    M=0
    // jump to main loop
    @LOOP
    0;JMP

(RESET_SECREEN_INDEX)
    @index
    M=-1
    @LOOP
    0;JMP
// Computes M[2]=max(M[0],M[1]) where M stands for RAM
    @0
    D=M
    @1
    D=D-M
    @FIRST_IS_GREATER
    D;JGT  // if D>0 goto FIRST_IS_GREATER
    @1
    D=M
    @SECOND_IS_GREATER
    0;JMP
(FIRST_IS_GREATER)
    @0
    D=M
(SECOND_IS_GREATER)
    @2
    M=D
(INFINITE_LOOP)  // infinite loop is standard way to terminate programs.
    @INFINITE_LOOP
    0;JMP
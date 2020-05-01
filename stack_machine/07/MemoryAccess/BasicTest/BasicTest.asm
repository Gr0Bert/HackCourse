// Push(Constant,10)
@10
D=A
@SP
A=M
M=D
@SP
M=M+1

// Pop(Local,0)
@LCL
D=M
@0
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D

// Push(Constant,21)
@21
D=A
@SP
A=M
M=D
@SP
M=M+1

// Push(Constant,22)
@22
D=A
@SP
A=M
M=D
@SP
M=M+1

// Pop(Argument,2)
@ARG
D=M
@2
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D

// Pop(Argument,1)
@ARG
D=M
@1
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D

// Push(Constant,36)
@36
D=A
@SP
A=M
M=D
@SP
M=M+1

// Pop(This,6)
@THIS
D=M
@6
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D

// Push(Constant,42)
@42
D=A
@SP
A=M
M=D
@SP
M=M+1

// Push(Constant,45)
@45
D=A
@SP
A=M
M=D
@SP
M=M+1

// Pop(That,5)
@THAT
D=M
@5
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D

// Pop(That,2)
@THAT
D=M
@2
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D

// Push(Constant,510)
@510
D=A
@SP
A=M
M=D
@SP
M=M+1

// Pop(Temp,6)
@SP
M=M-1
A=M
D=M
@11
M=D
@SP

// Push(Local,0)
@LCL
D=M
@0
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1

// Push(That,5)
@THAT
D=M
@5
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1

// Add
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1

// Push(Argument,1)
@ARG
D=M
@1
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1

// Sub
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1

// Push(This,6)
@THIS
D=M
@6
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1

// Push(This,6)
@THIS
D=M
@6
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1

// Add
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1

// Sub
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1

// Push(Temp,6)
@11
D=M
@SP
A=M
M=D
@SP
M=M+1

// Add
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1


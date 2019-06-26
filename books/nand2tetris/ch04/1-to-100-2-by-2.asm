	@i
	M=0
	@sum
	M=0
(LOOP)
	@i
	D=M
	@100
	D=D-A
	@END                    // refers to exact line number
	D;JGT
	@i
	D=M
	@sum
	M=D+M
	@2
	D=A
	@i
	M=D+M
	@LOOP
	0;JMP
(END)
	@END
	0;JMP                   // halt

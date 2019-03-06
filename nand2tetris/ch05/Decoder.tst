load        Decoder.hdl,
output-file Decoder.out,
compare-to  Decoder.cmp,
output-list instrType%B5.1.5 aVal%B1.15.1 mAop%B2.1.3
	    mZx%B2.1.2 mNx%B2.1.2 mZy%B2.1.2 mNy%B2.1.2 mF%B1.1.2 mNo%B2.1.2
	    mCwriteA%B4.1.5 mCwriteD%B4.1.5 mCwriteM%B4.1.5
	    mCjLT%B3.1.3 mCjEQ%B3.1.3 mCjGT%B3.1.3;

// @10
set instruction %B0000000000010000, eval, output;

// M=0
set instruction %B1110101010001000, eval, output;

// @20
set instruction %B0000000000100000, eval, output;

// D=M
set instruction %B1111110000010000, eval, output;

// D=D-A
set instruction %B1110010011010000, eval, output;

// D;JGT
set instruction %B1110001100000001, eval, output;

// M=D+M
set instruction %B1111000010001000, eval, output;

// @2
set instruction %B0000000000000010, eval, output;

// 0;JMP
set instruction %B1110101010000111, eval, output;

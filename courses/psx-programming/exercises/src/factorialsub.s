	.psx
	.create "factorialsub.bin", 0x80010000
	.org 0x80010000
Main:
	li	$a0, 0x6
	jal	Factorial
	nop

Halt:	j Halt
	nop

;;; Subroutine to compute the factorial of a number.
;;; Argument: num ($a0)
Factorial:
	li	$t3, 0x1	; temp
	li	$t4, 0x1	; sum
	li	$t1, 0x1	; i
L1:	bgt	$t1, $a0, EndL1
	nop
	move	$t4, $zero
	move	$t2, $zero	; j
	nop
L2:	bge	$t2, $t1, EndL2
	nop
	addu	$t4, $t3
	addiu	$t2, 0x1
	j	L2
	nop
EndL2:
	move	$t3, $t4
	addiu	$t1, 0x1
	j	L1
	nop
EndL1:
	move	$v0, $t4	; $v0 receives the final result (sum)
	jr	$ra		; return from the subroutine
	nop

End:
	.close

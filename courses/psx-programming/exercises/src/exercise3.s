	.psx
	.create "exercise3.bin", 0x80010000
	.org 0x80010000
Main:
	move	$t2, $zero	; res
	li	$t0, 0x1b	; num
	li	$t1, 0x3	; den
Loop:
	subu	$t0, $t1
	addiu	$t2, 0x1
	bge	$t0, $t1, Loop
	nop
End:
	.close

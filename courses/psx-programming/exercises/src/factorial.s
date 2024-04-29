	.psx
	.create "factorial.bin", 0x80010000
	.org 0x80010000
Main:
	li	$t0, 0x5	; num
	li	$t3, 0x1	; temp
	li	$t4, 0x1	; sum
	li	$t1, 0x1	; i
L1:	bgt	$t1, $t0, End
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
End:
	move	$v0, $t4
	.close

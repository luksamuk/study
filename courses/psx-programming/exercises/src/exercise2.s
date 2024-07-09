	.psx
	.create "exercise2.bin", 0x80010000
	.org 0x80010000
Main:
	li	$t0, 0x1
	move	$t1, $zero
Loop:
	add	$t1, $t0
	addi	$t0, 0x1
	ble	$t0, 0xa, Loop
	nop
End:
	.close

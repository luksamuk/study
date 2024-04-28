	.psx
	.create "fillmemw.bin", 0x80010000
	.org 0x80010000

	BASE_ADDR equ 0x0000

Main:
	li	$t0, 0xa000
	li	$t1, 0xa0ff
	li	$t2, 0x12345678

Loop:
	sw	$t2, BASE_ADDR($t0)
	addi	$t0, $t0, 4
	ble	$t0, $t1, Loop
End:
	.close

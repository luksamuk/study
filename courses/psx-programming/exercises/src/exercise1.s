	.psx
	.create "exercise1.bin", 0x80010000
	.org 0x80010000
Main:
	li	$t0, 0x1
	li	$t1, 0x100
	li	$t2, 0x11
End:
	.close

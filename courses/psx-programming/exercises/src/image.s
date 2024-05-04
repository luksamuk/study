	.psx
	.create "image.bin", 0x80010000
	.org 0x80010000

	IO_BASE_ADDR equ 0x1f80
	STACK_BOTTOM_ADDR equ 0x80103cf0

	GP0 equ 0x1810
	GP1 equ 0x1814

Main:
	lui	$a0, IO_BASE_ADDR
	la	$sp, STACK_BOTTOM_ADDR

	jal	InitGPU
	nop
	jal	ClearScreen
	nop

	;; code goes here

Halt:
	j Halt
	nop

;;; Initializes the GPU.
;;; Args:
;;; $a0: Base address of the I/O ports.
InitGPU:
	li	$t1, 0x00000000	; 00 = reset gpu
	sw	$t1, GP1($a0)	; write packet to GP1
	li	$t1, 0x03000000	; 03 = Display enable
	sw	$t1, GP1($a0)
	li	$t1, 0x08000001 ; 08 = display mode (320x240, 15-bit, NTSC)
	sw	$t1, GP1($a0)
	li	$t1, 0x06c60260	; 06 = H display range
	sw	$t1, GP1($a0)
	li	$t1, 0x07042018	; 07 = V display range
	sw	$t1, GP1($a0)
	li	$t1, 0xe1000400	; E1 = Draw Mode Settings
	sw	$t1, GP0($a0)	; write packet to GP0
	li	$t1, 0xe3000000	; E3 = drawing area topleft
	sw	$t1, GP0($a0)
	li	$t1, 0xe403bd3f	; E4 = drawing area bottomright
	sw	$t1, GP0($a0)
	li	$t1, 0xe5000000	; E5 = drawing offset
	sw	$t1, GP0($a0)
	jr	$ra
	nop
	
;;; Clears the screen.
;;; Args:
;;; $a0: Base address of the I/O ports.
ClearScreen:
	li	$t1, 0x02023000	; 02 = Fill rectangle in VRAM (random dark green color)
	sw	$t1, GP0($a0)
	li	$t1, 0x00000000	; top-left corner {0, 0}
	sw	$t1, GP0($a0)
	li	$t1, 0x00ef013f	; width and height {239, 319} 0xHHHHWWWW
	sw	$t1, GP0($a0)
	jr	$ra
	nop
	
	.close

	.psx
	.create "image24bpp.bin", 0x80010000
	.org 0x80010000

	IO_BASE_ADDR equ 0x1f80
	STACK_BOTTOM_ADDR equ 0x80103cf0

	GP0 equ 0x1810
	GP1 equ 0x1814

	IMG_WIDTH      equ 640
	IMG_HEIGHT     equ 480
	IMG_SIZE_BYTES equ 921600	; 640 * 480 * 3 bytes per pixel
	
Main:
	lui	$a0, IO_BASE_ADDR
	la	$sp, STACK_BOTTOM_ADDR

	jal	InitGPU
	nop

	li	$t1, 0xa0000000	; a0 = Copy rect from CPU to VRAM
	sw	$t1, GP0($a0)
	li	$t1, 0x00000000	; copy area -- topleft (x = 0, y = 0)
	sw	$t1, GP0($a0)
	li	$t1, 0x01e003c0	; copy area -- 0xHHHHWWWW (H = 480, W = 640)
	sw	$t1, GP0($a0)
	li	$t0, IMG_SIZE_BYTES ; load image size in bytes
	li	$t4, 4		    ; load 4 into $t4 (division is not immediate)
	div	$t0, $t4	    ; divide $t0 by 4
	mflo	$t0		; $t0 <- quotient
	la	$t2, Image	; $t2 <- image base address
LoopWords:
	lw	$t1, 0($t2)	; $t1 <- word from array (offset by 0 since t2 changes)
	nop
	sw	$t1, GP0($a0)	; write to GP0
	addiu	$t2, 4		; increment $t2 by a word
	addiu	$t0, $t0, -1	; $t0--
	bnez	$t0, LoopWords	; branch if not equals zero (if $t0 != 0)
	nop
	
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
	li	$t1, 0x08000037	; 08 = Display mode (640x480, 24-bpp, NTSC, interlaced)
	sw	$t1, GP1($a0)
	li	$t1, 0x06c60260	; 06 = H display range (3168..608, not changed)
	sw	$t1, GP1($a0)
	li	$t1, 0x0707e018	; 07 = V display range (504..24)
	sw	$t1, GP1($a0)
	li	$t1, 0xe1000400	; E1 = Draw Mode Settings (not changed)
	sw	$t1, GP0($a0)
	li	$t1, 0xe3000000	; E3 = drawing area topleft (0x0, not changed)
	sw	$t1, GP0($a0)
	li	$t1, 0xe403bd3f	; E4 = drawing area bottomright (not changed)
	sw	$t1, GP0($a0)
	li	$t1, 0xe5000000	; E5 = drawing offset (no offset, not changed)
	sw	$t1, GP0($a0)
	jr	$ra
	nop

Image:
	.incbin "assets/logo.bin"	; includes a 640x480 24bpp image (921,600 bytes)
 	
	.close

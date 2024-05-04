	.psx
	.create "trianglesub.bin", 0x80010000
	.org 0x80010000

	IO_BASE_ADDR equ 0x1f80
	GP0 equ 0x1810
	GP1 equ 0x1814

Main:
	; display setup
	lui	$t0, IO_BASE_ADDR
	li	$t1, 0x00000000	; 00 = reset gpu
	sw	$t1, GP1($t0)	; write packet to GP1
	li	$t1, 0x03000000	; 03 = Display enable
	sw	$t1, GP1($t0)
	li	$t1, 0x08000001 ; 08 = display mode (320x240, 15-bit, NTSC)
	sw	$t1, GP1($t0)
	li	$t1, 0x06c60260	; 06 = H display range
	sw	$t1, GP1($t0)
	li	$t1, 0x07042018	; 07 = V display range
	sw	$t1, GP1($t0)
	li	$t1, 0xe1000400	; E1 = Draw Mode Settings
	sw	$t1, GP0($t0)	; write packet to GP0
	li	$t1, 0xe3000000	; E3 = drawing area topleft
	sw	$t1, GP0($t0)
	li	$t1, 0xe403bd3f	; E4 = drawing area bottomright
	sw	$t1, GP0($t0)
	li	$t1, 0xe5000000	; E5 = drawing offset
	sw	$t1, GP0($t0)

	lui	$a0, IO_BASE_ADDR
	la	$sp, 0x00103cf0		; Initialize stack pointer

	addiu	$sp, -(4 * 7)	; subtract 7 words from $sp that are gonna be pushed

	lui	$a0, IO_BASE_ADDR

	li	$t0, 0xff4472	; Param: Color (0xBBGGRR)
	sw	$t0, 0($sp)	; Push argument onto stack

	li	$t0, 200	; Param: x1
	sw	$t0, 4($sp)

	li	$t0, 40		; Param: y1
	sw	$t0, 8($sp)

	li	$t0, 288	; Param: x2
	sw	$t0, 12($sp)

	li	$t0, 56		; Param: y2
	sw	$t0, 16($sp)

	li	$t0, 224	; Param: x3
	sw	$t0, 20($sp)

	li	$t0, 200	; Param: y3
	sw	$t0, 24($sp)

	jal	DrawFlatTriangle
	nop

Halt:	j Halt
	nop

	;; Subroutine to draw a flat-shaded triangle.
	;; Args:
	;; $a0 = IO_BASE_ADDR (IO ports at 0x1f80****)
	;; 0($sp)  = Color (0xBBGGRR)
	;; 4($sp)  = x1
	;; 8($sp)  = y1
	;; 12($sp) = x2
	;; 16($sp) = y2
	;; 20($sp) = x3
	;; 24($sp) = x3
DrawFlatTriangle:
	lw	$t0, 0($sp)		; $t0 <- color
	lui	$t1, 0x2000		; $t1(MSB) <- command (draw flat triangle)
	or	$t0, $t1		; setup command (0x20) + color
	sw	$t0, GP0($a0)		; write to GP0

	lw	$t0, 8($sp)		; $t0 <- y1
	lw	$t1, 4($sp)		; $t1 <- x1
	sll	$t0, 16			; y1 = y1 << 16
	andi	$t1, 0xffff		; discard anything in x1 after two LSB
	or	$t0, $t1		; $t0 <- $t0 | $t1
	sw	$t0, GP0($a0)		; write vertex 1 to GP0

	lw	$t0, 16($sp)		; $t0 <- y2
	lw	$t1, 12($sp)		; $t1 <- x2
	sll	$t0, 16			; y2 = y2 << 16
	andi	$t1, 0xffff		; discard anything in x2 after two LSB
	or	$t0, $t1		; $t0 <- $t0 | $t1
	sw	$t0, GP0($a0)		; write vertex 2 to GP0

	lw	$t0, 24($sp)		; $t0 <- y3
	lw	$t1, 20($sp)		; $t1 <- x3
	sll	$t0, 16			; y3 = y3 << 16
	andi	$t1, 0xffff		; discard anything in x3 after two LSB
	or	$t0, $t1		; $t0 <- $t0 | $t1
	sw	$t0, GP0($a0)		; write vertex 3 to GP0

	addiu	$sp, (4 * 7)		; reset stack pointer out of convenience
	jr	$ra
	nop

	.close

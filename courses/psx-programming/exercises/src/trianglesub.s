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

	;;  Invoke subroutine to draw flat triangle
	li	$s0, 0xffff00	; Param: Color (0xBBGGRR)
	li	$s1, 200	; Param: x1
	li	$s2, 40		; Param: y1
	li	$s3, 288	; Param: x2
	li	$s4, 56		; Param: y2
	li	$s5, 224	; Param: x3
	li	$s6, 200	; Param: y3
	jal	DrawFlatTriangle
	nop

Halt:
	j Halt
	nop

	;; Subroutine to draw a flat-shaded triangle.
	;; Args:
	;; $a0 = IO_BASE_ADDR (IO ports at 0x1f80****)
	;; $s1 = x1
	;; $s2 = y1
	;; $s3 = x2
	;; $s4 = y2
	;; $s5 = x3
	;; $s6 = x3
DrawFlatTriangle:

	lui	$t0, 0x2000	; 0x20 = flat triangle
	or	$t1, $t0, $s0	; setup command+color on $t1
	sw	$t1, GP0($a0)	; write to GP0

	sll	$s2, $s2, 16		; y1 = y1 << 16
	andi	$s1, $s1, 0xffff	; discard anything in x1 after two LSB
	or	$t1, $s1, $s2		; $t1 = x1 | y1 (at respective offsets)
	sw	$t1, GP0($a0)		; write vertex 1 to GP0

	sll	$s4, $s4, 16
	andi	$s3, $s3, 0xffff
	or	$t1, $s3, $s4		; $t1 = x2 | y2 (at respective offsets)
	sw	$t1, GP0($a0)		; write vertex 2 to GP0

	sll	$s6, $s6, 16
	andi	$s5, $s5, 0xffff
	or	$t1, $s5, $s6		; $t1 = x3 | y3 (at respective offsets)
	sw	$t1, GP0($a0)		; write vertex 3 to GP0

	jr	$ra
	nop

	.close

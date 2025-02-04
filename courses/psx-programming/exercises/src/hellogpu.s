	.psx
	.create "hellogpu.bin", 0x80010000
	.org 0x80010000

	IO_BASE_ADDR equ 0x1f80

	GP0 equ 0x1810
	GP1 equ 0x1814

Main:
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

	li	$t1, 0x02023000	; 02 = Fill rectangle in VRAM (random dark green color)
	sw	$t1, GP0($t0)
	li	$t1, 0x00000000	; top-left corner {0, 0}
	sw	$t1, GP0($t0)
	li	$t1, 0x00ef013f	; width and height {239, 319} 0xHHHHWWWW
	sw	$t1, GP0($t0)

	li	$t1, 0x2099ffff	; draw flat colored triangle, yellow color
	sw	$t1, GP0($t0)
	li	$t1, 0x003c0050	; V1: 80x60
	sw	$t1, GP0($t0)
	li	$t1, 0x00b4005a	; V2: 90x180
	sw	$t1, GP0($t0)
	li	$t1, 0x009600f0	; V3: 240x150
	sw	$t1, GP0($t0)

	li	$t1, 0x28ff8000	; draw flat colored quad, blue color
	sw	$t1, GP0($t0)
	li	$t1, 0x00b400c8	; vertex 4
	sw	$t1, GP0($t0)
	li	$t1, 0x001e00b4	; vertex 2
	sw	$t1, GP0($t0)
	li	$t1, 0x008c0046	; vertex 3
	sw	$t1, GP0($t0)
	li	$t1, 0x005a0028	; vertex 1
	sw	$t1, GP0($t0)

	li	$t1, 0x300000ff	; draw gourand colored triangle, start color: red
	sw	$t1, GP0($t0)
	li	$t1, 0x002d00a0	; vertex 1
	sw	$t1, GP0($t0)
	li	$t1, 0x0000ff00	; color 2: green
	sw	$t1, GP0($t0)
	li	$t1, 0x007d0064	; vertex 2
	sw	$t1, GP0($t0)
	li	$t1, 0x00ff0000	; color 3: blue
	sw	$t1, GP0($t0)
	li	$t1, 0x007d00dc	; vertex 3
	sw	$t1, GP0($t0)

Halt:
	j Halt
	nop
	
	.close

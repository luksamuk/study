	.psx
	.create "joypad.bin", 0x80010000
	.org 0x80010000

	IO_BASE_ADDR	equ 0x1f80
	GP0		equ 0x1810
	GP1		equ 0x1814

	PAD_L2		equ 0x0001 	; L2 (bit 0)
	PAD_R2		equ 0x0002	; R2 (bit 1)
	PAD_L1		equ 0x0004	; L1 (bit 2)
	PAD_R1		equ 0x0008	; R1 (bit 3)
	PAD_T		equ 0x0010 	; triangle (bit 4)
	PAD_C		equ 0x0020	; circle (bit 5)
	PAD_X		equ 0x0040	; X (bit 6)
	PAD_S		equ 0x0080	; square (bit 7)
	PAD_SELECT	equ 0x0100	; select (bit 8)
	PAD_L3		equ 0x0200 	; L3 (bit 9, analog mode only)
	PAD_R3		equ 0x0400	; R3 (bit 10, analog mode only)
	PAD_START	equ 0x0800	; Start (bit 11)
	PAD_UP		equ 0x1000	; Up (bit 12)
	PAD_RIGHT	equ 0x2000	; Right (bit 13)
	PAD_DOWN	equ 0x4000	; Down (bit 14)
	PAD_LEFT	equ 0x8000	; Left (bit 15)

PadBuffer:	.word 0		; Pad buffer (automatically stored every frame)
PadData:	.word 0		; Pad data (read from vsync routine)

XPos:	.word 0
YPos:	.word 0

Main:
	li	$t1, 0x15
	li	$a0, 0x20000001	; Argument 1
	li	$t2, 0xB0	; Address of BIOS routine we're trying to invoke
	la	$a1, PadBuffer	; Argument 2: Pad buffer addr to autoupdate each frame
	jalr	$t2		; Jump to BIOS routine
	nop

	lui	$a0, IO_BASE_ADDR
	li	$t1, 0x00000000	; 00 = reset gpu
	sw	$t1, GP1($a0)	; write packet to GP1
	li	$t1, 0x03000000	; 03 = Display enable
	sw	$t1, GP1($a0)
	li	$t1, 0x08000001 ; 08 = display mode (320x240, 15-bit, NTSC)
	sw	$t1, GP1($a0)
	li	$t1, 0x06c60260	; 06 = H display range (3168..608)
	sw	$t1, GP1($a0)
	li	$t1, 0x07042018	; 07 = V display range (264..24)
	sw	$t1, GP1($a0)

	li	$t1, 0xe1000400	; E1 = Draw Mode Settings
	sw	$t1, GP0($a0)	; write packet to GP0
	li	$t1, 0xe3000000	; E3 = drawing area topleft
	sw	$t1, GP0($a0)
	li	$t1, 0xe403bd3f	; E4 = drawing area bottomright
	sw	$t1, GP0($a0)
	li	$t1, 0xe5000000	; E5 = drawing offset
	sw	$t1, GP0($a0)

Refresh:

WaitVSync:
        la      $a1, PadBuffer  ; Load Pad Buffer Address
Wait:                           ; Wait for vsync and store XOR pad data
        lw      $t0, 0($a1)     ; Load pad buffer
        nop                     ; Delay slot
        beqz    $t0, Wait       ; if (PadBuffer == 0), we wait
        nor     $t0, $r0        ; NOR compliment pad data bits (delay slot)
        sw      $r0, 0($a1)     ; Store zero to pad buffer
        la      $a1, PadData    ; Load pad data address
        sw      $t0, 0($a1)     ; Store pad data

PressUp:
	la	$a1, PadData	; Load input data address
	lw	$t0, 0($a1)	; Load input data word
	nop
	andi	$t0, PAD_UP	; $t0 = input status
	beqz	$t0, PressDown	; If not pressed, bypass and test next button
	nop

	la	$t2, YPos
	lw	$t3, 0($t2)
	nop
	addiu	$t3, $t3, -1	; YPos--
	sw	$t3, 0($t2)

PressDown:
	la	$a1, PadData
	lw	$t0, 0($a1)
	nop
	andi	$t0, PAD_DOWN
	beqz	$t0, PressRight
	nop

	la	$t2, YPos
	lw	$t3, 0($t2)
	nop
	addiu	$t3, $t3, 1	; YPos++
	sw	$t3, 0($t2)

PressRight:
	la	$a1, PadData
	lw	$t0, 0($a1)
	nop
	andi	$t0, PAD_RIGHT
	beqz	$t0, PressLeft
	nop

	la	$t2, XPos
	lw	$t3, 0($t2)
	nop
	addiu	$t3, $t3, 1	; XPos++
	sw	$t3, 0($t2)

PressLeft:
	la	$a1, PadData
	lw	$t0, 0($a1)
	nop
	andi	$t0, PAD_LEFT
	beqz	$t0, EndInputCheck
	nop

	la	$t2, XPos
	lw	$t3, 0($t2)
	nop
	addiu	$t3, $t3, -1	; XPos--
	sw	$t3, 0($t2)

EndInputCheck:

ClearScreen:
	li	$t1, 0x02023000	; 02 = Fill rectangle in VRAM (random dark green color)
	sw	$t1, GP0($a0)
	li	$t1, 0x00000000	; top-left corner {0, 0}
	sw	$t1, GP0($a0)
	li	$t1, 0x00ef013f	; width and height {239, 319} 0xHHHHWWWW
	sw	$t1, GP0($a0)

DrawRect:
        li      $t1, 0x0200ff00 ; flat rectangle with color
        sw      $t1, GP0($a0)

        la      $t2, YPos
        lw      $t3, 0($t2)
        nop
        sll     $t3, $t3, 16    ; YPos << 16
        la      $t2, XPos
        lw      $t4, 0($t2)
        nop
        andi    $t4, $t4, 0xffff ; clear MSB in XPos
        or      $t5, $t3, $t4
        sw      $t5, GP0($a0)    ; Write coordinates to GP0 (YYYYXXXX)

        li      $t1, 0x00200020 ; Fill area 0xHHHHWWWW (32x32)
        sw      $t1, GP0($a0)

	j Refresh
	nop

	.close

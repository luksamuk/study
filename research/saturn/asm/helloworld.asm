	org	$6004000 		; Base address for BIN file on Saturn

	;; ==============================
	;; SCREEN SETUP
	;; ==============================

	;; First set up r11 with the base address for VDP 2 registers.
	;; Notice that it is the same as the addresses starting with 0
	;; (refer to https://www.chibialiens.com/sh2/saturn.php), but in this
	;; case we're doing cache-through access
	mov.l	#$25f80000, r11

	add	#$e, r11	; Point to VDP2 RAMCTL ($25f8000e)
	;; cc = 0 -> 5-bit RGB
	;;        C-cc--VVRRRRRRRR
	mov.w	#%1000001111111111, r6
	mov.w	r6, @r11

	add	#$2e, r11		; Point to VDP2 MPOFN ($25f8003c)
	;;        -NNN-nnn-NNN-nnn
	mov	#%0000000000000000, r2
	mov.w	r2, @r11

	;; The following instructions start r11 at $25f80078, and then
	;; pre-decrement this register (@-reg) by the size referenced by
	;; the instruction (e.g. mov.w will result in a 2-byte decrement).
	;; All these registers are being initialized with the zeroes contained
	;; in the r2 register.
	add	#$3c, r11
	mov.w	r2, @-r11		; VDP2 SCYDN0
	mov.w	r2, @-r11		; VDP2 SCYIN0
	mov.w	r2, @-r11		; VDP2 SCXDN0
	mov.w	r2, @-r11		; VDP2 SCXIN0

	add	#-72, r11		; Point to VDP2 CHCTLA (NBG0, NBG1)
	;; c = colors (16/256/2K/32K/16M).
	;; e = 1 -> bitmap format
	;; nn = Bitmap size (512x256, 512x512, 1024x256, 1024x512)
	;;        --CCNNES-cccnnes
	mov.w	#%0000000000010010, r2	; nn=bitmap size
	mov.w	r2, @r11

	add	#-8, r11		; Point to VDP2 BGON
	;; RN = Transparency
	;; rn = enabled
	;;        ---RNNNN--rrnnnn
	mov.w	#%0000000100000001, r8
	mov.w	r8, @r11

	add	#-32, r11		; Point to VDP2 TVMD
	;; D = 1 -> Screen on
	;; B = 0 -> No blackout
	;; L = interlace
	;; H = Hres
	;; V = Vres
	;;        D------BLLVV-HHH
	mov.l	#%1000000000000000, r10 ; using mov.l to avoid sign confusion?
	mov.w	r10, @r11

	;; ==============================
	;; PALETTE SETUP
	;; ==============================

	;; Transfer four colors to VDP2 CRAM so we can work with them.
	;; We have 4KB of CRAM mapped at $25f00000 (cache-through).
	mov.l	#$25f00000, r6		; Point to VDP2 CRAM
	mov.l	#Palette, r8
	mov	#4, r0			; register r0 used as loop index
PaletteLoop:
	mov.w	@r8+, r1
	mov.w	r1, @r6
	add	#2, r6

	dt	r0			; Decrement and test
	bf	PaletteLoop

	;; ==============================
	;; SCREEN CURSOR
	;; ==============================

	mov.l	#UserRam, r14		; Point to work RAM
	mov	#0, r0			; Set cursor position to 0, 0
	mov.l	r0, @(CursorX,r14)	; Store X and Y at cursor offset
	mov.l	r0, @(CursorY,r14)

	mov.l	#TxtHelloWorld, r13 	; r13 is an argument to subroutine
	bsr	PrintString
	nop

InfiniteLoop:
	bra InfiniteLoop
	nop

	;; ==============================
	;; SUBROUTINES
	;; ==============================

PrintString:
	sts.l	pr, @-sp		; Save return address on stack
PrintStringAgain:
	mov.b	@r13+, r0		; Store character in r0; increment ptr
	mov	#-1, r1			; Test if char is 255
	cmp/eq	r0, r1
	bt	PrintStringDone
	nop
	bsr	PrintChar		; Print char at r0
	nop
	bra	PrintStringAgain
	nop
PrintStringDone:
	lds.l	@sp+, pr		; Restore return address
	rts
	nop


PrintChar:
	;; Backup all used registers onto the stack
	sts.l	mach, @-sp
	sts.l	macl, @-sp
	mov.l	r0, @-sp
	mov.l	r1, @-sp
	mov.l	r2, @-sp
	mov.l	r3, @-sp
	mov.l	r4, @-sp
	mov.l	r5, @-sp
	mov.l	r6, @-sp

	;; Calculate offset on bitmap font. Forn has no character below 32,
	;; and each character uses 8 bytes (one byte per 8-pixel line)
	mov.l	#Font, r5	; font data pointer on r5
	add	#-32, r0	; retrieve character index
	shll	r0
	shll	r0
	shll	r0		; r0 <- r0 * 8 (8 bytes per character)
	add	r0, r5		; final character image start offset

	mov.l	#$25e00000, r6	; Point to VDP2 screen VRAM
	mov.l	#UserRAM, r4	; Point to work ram

	;; Calculate VRAM destination for character data.
	;; Screen is 512 bytes wide, one byte per pixel.
	;; Screen base address is $25e00000
	mov.l	@(CursorX,r4), r0
	shll	r0
	shll	r0
	shll	r0		; convert CursorX from char idx to byte
	add	r0, r6		; store in r6

	mov.l	@(CursorY,r4), r0
	shll	r0
	shll	r0
	shll	r0		; convert CursorY from char idx to byte
	mov.l	#512, r1
	mulu	r0, r1		; byte-indexed CursorY * 512
	sts	macl, r0	; store low result in r0
	add	r0, r6		; r6 <- r6 + r0

	;; Since the font stores one bit per pixel, we need to convert it to
	;; one byte per pixel. So we load byte per byte of our font and perform
	;; bit shifting magic from right to left on the pixel.
	mov	#7, r1		; go to rightmost pixel of character line by+
	add	r1, r6		; +incrementing vram destination by 7
	mov	#8, r1		; r1 <- number of Y lines on character
NextYLine:
	mov	#8, r0		; bits per line
	mov.b	@r5+, r3	; get one font texture byte
NextXPixel:
	mov	#1, r2		; Fill pixel completely
	rotcr	r3		; Get single bit, store it on carry bit in SR
	bt	CharPixelSet	; If bit is full, go to pixel drawing
	mov	#0, r2		; If not, just clear the pixel
CharPixelSet:
	mov.b	r2, @r6		; Write pixel
	add	#-1, r6		; Go to the left
	dt	r0		; Decrease X
	bf	NextXPixel	; If at end, go to next pixel

	mov.l	#512+8, r0	; A single line is 512 bytes + 8 bytes (pixels)
	add	r0, r6		; Go to next line
	dt	r1		; decrease Y and test if not 0
	bf	NextYLine

	mov.l	@(CursorX,r4), r0
	add	#1, r0
	mov.l	r0, @(CursorX,r4)

	;; Restore registers
	mov.l	@sp+, r6
	mov.l	@sp+, r5
	mov.l	@sp+, r4
	mov.l	@sp+, r3
	mov.l	@sp+, r2
	mov.l	@sp+, r1
	mov.l	@sp+, r0
	lds.l	@sp+, macl
	lds.l	@sp+, mach
	rts
	nop
	
	
	
	;; ==============================
	;; DATA
	;; ==============================
	ltorg

Palette:
	;;       -BBBBBGGGGGRRRRR
	dc.w	%0011100000000000
	dc.w	%0000001111111111
	dc.w	%0111111111100000
	dc.w	%0000000000011111

TxtHelloWorld:	dc.b "Hello World 12345 ?!", 255
Font:		binclude "./res/Font96.FNT"

	;; ==============================
	;; VARIABLES AND CONSTANTS
	;; ==============================

CursorX	equ 0
CursorY	equ 4
	align 4
UserRam: ds.l 8

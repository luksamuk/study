.include "../headers/Header.inc"
.include "../headers/Snes_Init.asm"
VBlank: 			; Needed for header.inc
	RTI

Start:
	; Initialize the SNES
	Snes_Init
	; set accumulator to 8-bits so we modify single bytes
	sep	#$20

	; Force vblank
	lda	#%10000000	; turn off screen
	sta	$2100
	lda	#%11100000 	; load low byte of green color
	sta	$2122
	lda	#%00000000	; load high byte of green color
	sta	$2122
	lda	#%00001111	; turn screen on with brightness 15 (100%)
	sta	$2100

Halt:
	jmp Halt
	

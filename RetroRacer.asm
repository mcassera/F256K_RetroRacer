;Retro Racer Concept
;Michael Cassera 2024
; 


; Master Memory Unit
MMU_IO_CTRL 	= $01					;MMU I/O Control

; Vicky control
VKY_MSTR_CTRL_0 = $d000					; Vicky Master Control Register 0
VKY_MSTR_CTRL_1 = $d001					; Vicky Master Control Register 1
VKY_BRDR_CTRL   = $d004					; Vicky Border Control Register

; Vicky Background Color
VKY_BKG_COL_B   = $d00d					; Vicky Graphics Background Color Blue
VKY_BKG_COL_G   = $d00e					; Vicky Graphics Background Color Green
VKY_BKG_COL_R   = $d00f					; Vicky Graphics Background Color Red

; Vicky layer control
VKY_LAYER_CTRL_0= $d002
VKY_LAYER_CTRL_1= $d003

; Tile Set Registers
VKY_TS0_AD_L	= $d280					; Vicky Tile 0 Image Start Address LOW BYTE
VKY_TS0_AD_M	= $d281					; Vicky Tile 0 Image Start Address MEDIUM BYTE
VKY_TS0_AD_H	= $d282					; Vicky Tile 0 Image Start Address HIGH BYTE

; Tile Map Registers
VKY_TM0_CTRL	= $d20c					; Tile Map 0 Control
VKY_TM0_AD_L	= $d20d					; Tile Map 0 Start Address LOW BYTE
VKY_TM0_AD_M	= $d20e					; Tile Map 0 Start Address MEDIUM BYTE
VKY_TM0_AD_H	= $d20f					; Tile Map 0 Start Address HIGH BYTE
VKY_TM0_SZ_X	= $d210					; Tile Map 0 Size X
VKY_TM0_SZ_Y	= $d212					; Tile Map 0 Size Y
VKY_TM0_POS_X_L = $d214					; Tile Map 0 X Position & Scroll LOW BYTE
VKY_TM0_POS_X_H = $d215					; Tile Map 0 X Position & Scroll HIGH BYTE
VKY_TM0_POS_Y_L = $d216					; Tile Map 0 Y Position & Scroll LOW BYTE
VKY_TM0_POS_Y_H = $d217					; Tile Map 0 Y Position & Scroll HIGH BYTE

; Sprite registers
VKY_SP0_CTRL	= $D900					; Sprite 0 Control Register
VKY_SP0_AD_L   	= $D901					; Sprite 0 Pixel Data Address Register
VKY_SP0_AD_M   	= $D902
VKY_SP0_AD_H   	= $D903
VKY_SP0_POS_X_L = $D904					; Sprite 0 X Position Register
VKY_SP0_POS_X_H = $D905
VKY_SP0_POS_Y_L = $D906					; Sprite 0 X Position Register
VKY_SP0_POS_Y_H = $D907

; Vicky Color Look Up Table Regsiters
VKY_GR_CLUT_0  	= $d000					; Graphics LUT #0 in I/O page 1
VKY_GR_CLUT_1  	= $d400					; Graphics LUT #1 in I/O page 1

; Interrupt Registers
VIRQ			= $fffe					; Pointer to IRQ routine (LOW Byte)
INT_PEND_0		= $d660					; Pending register for interrupts 0-7
INT_PEND_1		= $d661					; Pending register for interrupts 8-15
INT_MASK_0		= $d66c					; Mask register for interrupts 0-7
INT_MASK_1		= $d66d					; Mask register for interrupts 8-15

; Misc Variables for Indirect Indexing
ptr_src			= $80					; A pointer to read data
ptr_dst			= $82					; A pointer to write data


.cpu "w65c02"

*=$a0									; Set up buffer for Kernel communication
.dsection zp							; Define position for zp (zero page)
.cerror * > $af, "Too many Zero page variables"

*=$2000									; Our regular code goes here
.dsection code							; define that section of code

*=$2000		
start:									; Start of the program
	.include "api.asm"					; This is the Kernel API for 
	

										; for communication


;SetupKernel:							; Set up the API to work with

		.section zp						; Zero page section $20 to $28
event:	.dstruct	kernel.event.event_t
		.send



*=$2000
	stz MMU_IO_CTRL						; should do this on every program

;init_events:
	lda #<event
	sta kernel.args.events
	lda #>event
	sta kernel.args.events+1

;Set up TinyVicky to display sprites
	lda #%00110100						; Graphic, Tiles, & Sprites Engine enabled  |xx|GM|SP|TL|BM|GR|OV|TX|
	sta VKY_MSTR_CTRL_0					;                  		                    | 0| 0| 1| 1| 0| 1| 0| 0|

	stz VKY_MSTR_CTRL_1					; 320 x 240, 60 Hz
	stz VKY_BRDR_CTRL					; No Border
	
	lda #$88							; Set the background color
	sta VKY_BKG_COL_R
	lda #$88
	sta VKY_BKG_COL_G
	lda #$88
	sta VKY_BKG_COL_B
	
;Set up TinyVicky to display tiles
	lda #%01000000						;Layer 0 = bitmap 0, Layer 1 = tilemap 0  |xx|LA YE R1|xx|LA YE R0|
	sta VKY_LAYER_CTRL_0				;										  | 0| 1| 0| 0| 0| 0| 0| 0|
	lda #%00000101						;Layer 2 = TileMap 1					  |xx|xx|xx|xx|xx|LA YE R2|	
	sta VKY_LAYER_CTRL_1				;										  | 0| 0| 0| 0| 0| 1| 0| 1|

;Set TileSet for our Image
	lda #<TileSet
	sta VKY_TS0_AD_L
	lda #>TileSet
	sta VKY_TS0_AD_M
	lda #$00							; This should be equivelent to zero since we are staying in the first 64K of Ram 
	sta VKY_TS0_AD_H
	
;Set Tile Map 0
	lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
	sta VKY_TM0_CTRL					;										   | 0| 0| 0| 0| 0| 0| 0| 1|
	lda #20								; Tile Map Size 20 X 
	sta VKY_TM0_SZ_X
	lda #200							; Tile Map Size 200 Y
	sta VKY_TM0_SZ_Y

	lda #<TileMapData					; Point to the Tile Map Data, LOW BYTE
	sta VKY_TM0_AD_L
	lda #>TileMapData					; Point to the Tile Map Data, MEDIUM BYTE
	sta VKY_TM0_AD_M
	lda #$00							; Point to the Tile Map Data, HIGH BYTE
	sta VKY_TM0_AD_H

;Load the sprite CLUT into memory
	lda #$01							; Change I/O control to page 1
	sta MMU_IO_CTRL
	lda #<CLUT0							; Set source pointer to CLUT for color information
	sta ptr_src
	lda #>CLUT0
	sta ptr_src+1
	
	lda #<VKY_GR_CLUT_0					; Set destination pointer to Graphics CLUT 0
	sta ptr_dst
	lda #>VKY_GR_CLUT_0
	sta ptr_dst+1
	
	ldx #$00							; X is the number of colors to copy, check for 152
	ldy #$9a
	jsr makeClut
	
	lda #$00							; Set the I/O page back to 0
	sta MMU_IO_CTRL
	
;Set up Sprites
init_sp0:
	lda #<spriteLoadData				;Get the start address for the sprite register data
	sta ptr_src							; Low and high byte for indirect indexting
	lda #>spriteLoadData
	sta ptr_src+1

	lda #<VKY_SP0_CTRL					; Get the start address for the sprite registers
	sta ptr_dst							; both low and high byte
	lda #>VKY_SP0_CTRL
	sta ptr_dst+1
	ldx #$00

spriteLoop:
	ldy #$00							; set the Y register to 0 and count up assign all the sprites

offsetLoop:
	lda (ptr_src),y						; Read byte from our sprite table 
	sta (ptr_dst),y						; write byte to the sprite control registers
	iny
	cpy #$38
	bne offsetLoop			
	jmp GameStart
	
; Color LUT loop. used for multiple LUTS if you have more than 1
makeClut:
	sty totalColors
color_loop:
	ldy #$00							; Y points to the color component (Blue Red Green Alpha)
comp_loop:
	lda (ptr_src),y						; Read byte from our color table 
	sta (ptr_dst),y						; write byte to the Graphic CLUT
	iny
	cpy #$04							; Do 4 bytes for one color + Alpha
	bne comp_loop
	
	inx
	cpx totalColors						; Loop for all colors of the CLUT
	beq done_lut
	
	clc									; Move the source pointer to the next Color
	lda ptr_src
	adc #$04
	sta ptr_src
	lda ptr_src+1
	adc #$00
	sta ptr_src+1
	
	clc									; Move the destination pointer to the next Color
	lda ptr_dst
	adc #$04
	sta ptr_dst
	lda ptr_dst+1
	adc #$00
	sta ptr_dst+1
	
	jmp color_loop						; and start copying the next color

done_lut:
	rts
	
; Game Loop starts here!	
GameStart:
	lda #kernel.args.timer.FRAMES		; set the Timer to Frames
	ora #kernel.args.timer.QUERY		; and query what frame we're on
	sta kernel.args.timer.units			; store in units parameter
	jsr kernel.Clock.SetTimer			; jsr to Kernel Routine
	bcs skipSet							; If Carry set, ignore
	adc #$01							; if not add 1 to Accumulator for next frame
	sta $d0
skipSet:
	jsr SetTimer						; Let's get the kernel set up for the timer

loop:	
	jsr handle_events
	jmp loop

handle_events:
	lda kernel.args.events.pending		; Peek at the queue to see if anything is pending
	bpl done_handle_events				; Nothing to do
	jsr kernel.NextEvent				; Get the next event.
	bcs done_handle_events				; If Carry is set, skip the handler
	jsr dispatch						; Handle the event
	jmp handle_events					; go and check for another event
done_handle_events:
	rts 								

dispatch:
	lda event.type						; get the event type from Kernel
	cmp #kernel.event.timer.EXPIRED		; is the event timer.EXPIRED?
	beq UpdateScreen					; run the screen update
	cmp #kernel.event.JOYSTICK			; is the event a Joystick Change?
	beq setJoyStick
	rts

;Read the joystick on event and set a direction to be used during the SOF cycle
setJoyStick:
	lda event.joystick.joy1
	sta $d8								; Temp save joystick state
	stz joyX							; reset all directions and button to zero
	stz joyY
	stz joyB
jUp:
	lda $d8								; reload joystick state
	and  #$01							; check for up
	beq jDown							; if not up skip to test down
	dec joyY							; if up dec joy Y direction to $ff
	bra jLeft							; skip down test
jDown:
	lda $d8								; reload joystick state
	and #$02							; check for down
	beq jLeft							; if not down skip to test left
	inc joyY							; incrment Joy y direction to $01
jLeft:
	lda $d8								; reload joystick state
	and #$04							; check for left
	beq jRight							; if not left skipt to test right
	dec joyX							; decriment joy X direction to $ff
	bra jButton							; skip to button test
jRight:
	lda $d8								; reload the joystick state
	and #$08							; check for Right
	beq jButton							; if not right, skipt to button test
	inc joyX							; increment joy X to $01
jButton:
	lda $d8								; reload joystick state
	and #$10							; check for button 0
	beq jDone							; no button, we are done
	inc joyB							; yes botton, set joy B to 1
jDone:
	rts										

; These are events that happen at SOF, 60 times per second
UpdateScreen:
	jsr SetTimer						; Reset the next timer for SOF

;update road							; Moving the Road by adjust the Tile Map
	sec									; Set the carry for subtraction
	lda roadMove						; load Road position (fixed decimal byte)
	sbc player+8						; add player + 7 (player speed decimal Byte)
	sta roadMove						; and store
	lda roadMove+1						; load the low byte of road position (Tilemap Low)
	sbc player+9						; add player + 8 (player speed byte)
	sta roadMove+1						; and save
	sta VKY_TM0_POS_Y_L					; Write to Vicky Map Position Y, Low Byte

;update car position
	lda joyX							; Get the X direction of the joystick
	beq	testY							; do nothing if no direction
	bmi moveLeft						; if X direction is negative skip to move left
moveRight:								
	clc									; Clear the carry for addition			
	lda player+1						; load player + 1 (x Position (L))
	adc #$02							; move 2 pixels to the right
	sta player+1						; and store
	sta VKY_SP0_POS_X_L					; Set the Vicky sprite 0 positiong L
	lda player+2						; load player + 2 (x Position (H))
	adc #$00							; Add Carry if needed
	sta player+2						; and store
	sta VKY_SP0_POS_X_H					; set the Vicky Sprite 0 Position H
	bra testY							; skip to test Y
moveLeft:
	sec 								; set the carry for subtraction
	lda player+1						; load player + 1 (x Position (L))
	sbc #$02							; move 2 pixels to the left
	sta player+1						; and store
	sta VKY_SP0_POS_X_L					; set the Vicky sprite 0 position L
	lda player+2						; load player + 2 (x Position (H))
	sbc #$00							; subtract carry if needed
	sta player+2						; and store
	sta VKY_SP0_POS_X_H					; set the Vicky Sprite 0 Position H

;Joystick Y controls speed
testY:
	lda joyY							; Load Joystick Y position
	beq upDateTraffic						; if no position, we are done
	bpl slowDown						; if joystick is down, then slow down
speedUp:
	clc									; clear carry for addition
	lda player+8						; load player + 7 (speed L)
	adc #$08							; Add $08
	sta player+8						; asd store
	lda player+9						; load player + 8 (speed H)
	adc #$00							; Add carry
	cmp #$08							; If we hit 7, we hit top speed
	beq topSpeed						; stop adding
	sta player+9						; store if not over top speed
	bra upDateTraffic						; we are done
topSpeed:
	sec 								; we've hit top speed so we need
	lda player+8						; to subtract that speed low byte
	sbc #$08
	sta player+8
	bra upDateTraffic						; done
slowDown:
	sec									; Set carry for subtraction to slow down
	lda player+8						; load player speed L
	sbc #$08 							; subtract 8
	sta player+8						; and store
	lda player+9						; load player speed H
	sbc #$00							; subtract carry
	bmi noSpeed							; Have we hit a negative number?
	sta player+9						; if not, store the new speed
	bra upDateTraffic						; and done
noSpeed:
	stz player+8						; make sure all speed bytes are at zero since we're stopped
	bra upDateTraffic

upDateTraffic:
	


	rts


; Set the timer to the next SOF for the Game Loop
SetTimer:	
	inc $d0
	lda $d0
	sta kernel.args.timer.absolute		; store in timer.absolute paramter
	sta kernel.args.timer.cookie		; saved as a cookie to the kernel (same as frame number)
	lda #kernel.args.timer.FRAMES		; set the Timer to Frames
	sta kernel.args.timer.units			; store in units parameter
	jsr kernel.Clock.SetTimer			; jsr to Kernel routine to set timer

done:
	rts



; ***Working Memory***

totalColors:	.byte $00				; This is a variable for the y index for the CLUT load routine

joyX:			.byte $00				; signed X direction
joyY:			.byte $00				; signed Y direction
joyB:			.byte $00				; Button 0 Activated?
roadMove:		.byte $00,$00			; Low/High Road Movement Position

; LUT for Traffic tracking. 12 cars at most?
traffic:		; every time a car enters the screen we'll need to determine car type, driving style, speed, lane, etc
;			Sprite,	X_POS_L,	X_POS_H,	Y_POS_FP,	Y_POS_L,	Y_POS_H,	car type,	current lane,	speed L,	speed H,	Driver type 
;																														(1 Defensive, 2 aggressive, 3 racing, 4 police)
	.byte	$01,	86,			$00,		$00,		$00,		$00,		$02,		$01,			$50,		$04,		$01
	.byte	$02,	120,		$00,		$00,		$00,		$00,		$03,		$02,			$50,		$05,		$01	
	.byte	$03,	152,		$00,		$00,		$00,		$00,		$03,		$03,			$70,		$04,		$01	
	.byte	$04,	184,		$00,		$00,		$00,		$00,		$03,		$04,			$90,		$03,		$01	
	.byte	$05,	216,		$00,		$00,		$00,		$00,		$03,		$05,			$60,		$04,		$01	
	.byte	$06,	248,		$00,		$00,		$00,		$00,		$03,		$06,			$80,		$05,		$01	
	.byte	$07,	86,			$00,		$00,		$00,		$00,		$03,		$01,			$00,		$00,		$01	
	.byte	$08,	120,		$00,		$00,		$00,		$00,		$03,		$02,			$00,		$00,		$01	
	.byte	$09,	152,		$00,		$00,		$00,		$00,		$03,		$03,			$00,		$00,		$01	
	.byte	$0a,	184,		$00,		$00,		$00,		$00,		$03,		$04,			$00,		$00,		$01	
	.byte	$0b,	216,		$00,		$00,		$00,		$00,		$03,		$05,			$00,		$00,		$01	
	.byte	$0c,	248,		$00,		$00,		$00,		$00,		$03,		$06,			$00,		$00,		$01		
	
player:
	.byte	$00,	184,		$00,		$00,		220,		$00,		$01,		$07,			$00,		$00,		$00	

;Sprite register info
spriteLoadData:
;			CLUT0/ENABLE, ADD_L   , ADD_M,    ADD_H, X_L,X_H,  Y_L,Y_H,  
	.byte 	$01,          <sprite1, >sprite1, $00,   184,  0,  204,  0
	.byte	$01,          <sprite2, >sprite2, $00,    86,  0,  188,  0
	.byte	$01,          <sprite3, >sprite3, $00,   120,  0,   50,  0
	.byte 	$01,          <sprite4, >sprite4, $00,   152,  0,   75,  0
	.byte	$01,		  <sprite9, >sprite9, $00,   184,  0,  100,  0
	.byte	$01,		  <sprite10,>sprite10,$00,   216,  0,  150,  0
	.byte 	$01,		  <sprite7, >sprite7, $00,   248,  0,   60,  0



;Image Data Starts here
;Palette Indexed Colors
CLUT0:
;begin palette
;154
	.byte	$88,$99,$f6,$ff				; This 1st color is always transparent.
	.byte	$60,$6c,$f3,$ff
	.byte	$40,$4e,$e8,$ff
	.byte	$23,$1c,$e5,$ff
	.byte	$1d,$19,$dd,$ff
	.byte	$16,$17,$d0,$ff
	.byte	$11,$14,$c4,$ff
	.byte	$a,$12,$b0,$ff
	.byte	$b1,$8f,$f4,$ff
	.byte	$92,$62,$f0,$ff
	.byte	$7a,$40,$ec,$ff
	.byte	$63,$1e,$e9,$ff
	.byte	$60,$1b,$d8,$ff
	.byte	$5b,$18,$c2,$ff
	.byte	$57,$14,$ad,$ff
	.byte	$4f,$e,$88,$ff
	.byte	$d8,$93,$ce,$ff
	.byte	$c8,$68,$ba,$ff
	.byte	$bc,$47,$ab,$ff
	.byte	$b0,$27,$9c,$ff
	.byte	$aa,$24,$8e,$ff
	.byte	$a2,$1f,$7b,$ff
	.byte	$9a,$1b,$6a,$ff
	.byte	$8c,$14,$4a,$ff
	.byte	$db,$9d,$b3,$ff
	.byte	$cd,$75,$95,$ff
	.byte	$c2,$57,$7e,$ff
	.byte	$b7,$3a,$67,$ff
	.byte	$b1,$35,$5e,$ff
	.byte	$a8,$2d,$51,$ff
	.byte	$a0,$27,$45,$ff
	.byte	$92,$1b,$31,$ff
	.byte	$da,$a8,$9f,$ff
	.byte	$cb,$86,$79,$ff
	.byte	$c0,$6b,$5c,$ff
	.byte	$b5,$51,$3f,$ff
	.byte	$ab,$49,$39,$ff
	.byte	$9f,$3f,$30,$ff
	.byte	$93,$35,$28,$ff
	.byte	$7e,$23,$1a,$ff
	.byte	$ff,$bf,$af,$ff
	.byte	$ff,$a7,$91,$ff
	.byte	$fe,$8f,$73,$ff
	.byte	$fc,$77,$56,$ff
	.byte	$ef,$6c,$4e,$ff
	.byte	$de,$5e,$45,$ff
	.byte	$ce,$50,$3b,$ff
	.byte	$b1,$36,$2a,$ff
	.byte	$fa,$d4,$81,$ff
	.byte	$f7,$c3,$4f,$ff
	.byte	$f6,$b6,$29,$ff
	.byte	$f4,$a9,$3,$ff
	.byte	$e5,$9b,$3,$ff
	.byte	$d1,$88,$2,$ff
	.byte	$bd,$77,$2,$ff
	.byte	$9b,$57,$1,$ff
	.byte	$ea,$de,$80,$ff
	.byte	$e1,$d0,$4d,$ff
	.byte	$da,$c6,$26,$ff
	.byte	$d4,$bc,$0,$ff
	.byte	$c1,$ac,$0,$ff
	.byte	$a7,$97,$0,$ff
	.byte	$8f,$83,$0,$ff
	.byte	$64,$60,$0,$ff
	.byte	$c4,$cb,$80,$ff
	.byte	$ac,$b6,$4d,$ff
	.byte	$9a,$a6,$26,$ff
	.byte	$88,$96,$0,$ff
	.byte	$7b,$89,$0,$ff
	.byte	$6b,$79,$0,$ff
	.byte	$5c,$69,$0,$ff
	.byte	$40,$4d,$0,$ff
	.byte	$72,$d5,$72,$ff
	.byte	$41,$bd,$42,$ff
	.byte	$2b,$af,$2b,$ff
	.byte	$24,$9b,$25,$ff
	.byte	$8,$8f,$a,$ff
	.byte	$7,$7e,$a,$ff
	.byte	$0,$6f,$5,$ff
	.byte	$2,$53,$d,$ff
	.byte	$a5,$e1,$c5,$ff
	.byte	$81,$d5,$ae,$ff
	.byte	$65,$cc,$9c,$ff
	.byte	$4a,$c3,$8b,$ff
	.byte	$42,$b3,$7c,$ff
	.byte	$38,$9f,$68,$ff
	.byte	$2f,$8b,$55,$ff
	.byte	$1e,$69,$33,$ff
	.byte	$9c,$ee,$e6,$ff
	.byte	$75,$e7,$dc,$ff
	.byte	$57,$e1,$d4,$ff
	.byte	$39,$dc,$cd,$ff
	.byte	$33,$ca,$c0,$ff
	.byte	$2b,$b4,$af,$ff
	.byte	$24,$9d,$9e,$ff
	.byte	$17,$77,$82,$ff
	.byte	$9d,$f5,$ff,$ff
	.byte	$76,$f1,$ff,$ff
	.byte	$58,$ee,$ff,$ff
	.byte	$3b,$eb,$ff,$ff
	.byte	$35,$d8,$fd,$ff
	.byte	$2d,$c0,$fb,$ff
	.byte	$25,$a8,$f9,$ff
	.byte	$17,$7f,$f5,$ff
	.byte	$82,$e0,$ff,$ff
	.byte	$4f,$d5,$ff,$ff
	.byte	$28,$ca,$ff,$ff
	.byte	$7,$c1,$ff,$ff
	.byte	$0,$b3,$ff,$ff
	.byte	$0,$a0,$ff,$ff
	.byte	$0,$8f,$ff,$ff
	.byte	$0,$6f,$ff,$ff
	.byte	$80,$cc,$ff,$ff
	.byte	$4d,$b7,$ff,$ff
	.byte	$26,$a7,$ff,$ff
	.byte	$0,$00,$ff,$ff
	.byte	$0,$8c,$fb,$ff
	.byte	$0,$7c,$f5,$ff
	.byte	$0,$6c,$ef,$ff
	.byte	$0,$51,$e6,$ff
	.byte	$91,$ab,$ff,$ff
	.byte	$65,$8a,$ff,$ff
	.byte	$43,$70,$ff,$ff
	.byte	$22,$57,$ff,$ff
	.byte	$1e,$51,$f4,$ff
	.byte	$19,$4a,$e6,$ff
	.byte	$15,$43,$d8,$ff
	.byte	$c,$36,$bf,$ff
	.byte	$a4,$aa,$bc,$ff
	.byte	$7f,$88,$a1,$ff
	.byte	$63,$6e,$8d,$ff
	.byte	$48,$55,$79,$ff
	.byte	$41,$4c,$6d,$ff
	.byte	$37,$40,$5d,$ff
	.byte	$2e,$34,$4e,$ff
	.byte	$23,$27,$3e,$ff
	.byte	$ee,$ee,$ee,$ff
	.byte	$e0,$e0,$e0,$ff
	.byte	$bd,$bd,$bd,$ff
	.byte	$9e,$9e,$9e,$ff
	.byte	$75,$75,$75,$ff
	.byte	$61,$61,$61,$ff
	.byte	$42,$42,$42,$ff
	.byte	$21,$21,$21,$ff
	.byte	$c5,$be,$b0,$ff
	.byte	$ae,$a4,$90,$ff
	.byte	$9c,$90,$78,$ff
	.byte	$8b,$7d,$60,$ff
	.byte	$7a,$6e,$54,$ff
	.byte	$64,$5a,$45,$ff
	.byte	$4f,$47,$37,$ff
	.byte	$38,$32,$26,$ff
	.byte	$0,$0,$0,$00
	.byte	$ff,$ff,$ff,$ff
;end palette

sprite1:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$8c,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$63,$63,$8c,$63,$63,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$8c,$63,$63,$63,$8c,$63,$63,$63,$8c,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$8c,$63,$63,$63,$8c,$63,$63,$63,$8c,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$8c,$63,$63,$63,$8c,$63,$63,$63,$8c,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$8c,$63,$63,$63,$8c,$63,$63,$63,$8c,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$8c,$63,$63,$63,$63,$63,$63,$63,$8c,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$8c,$8c,$63,$63,$25,$25,$25,$25,$25,$63,$63,$8c,$8c,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$63,$25,$25,$25,$25,$25,$25,$25,$25,$25,$63,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$63,$25,$25,$25,$25,$25,$25,$25,$63,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$25,$63,$25,$25,$25,$25,$25,$63,$25,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$25,$63,$63,$63,$63,$63,$63,$63,$25,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$25,$63,$63,$63,$63,$63,$63,$63,$25,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$25,$63,$63,$63,$63,$63,$63,$63,$25,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$25,$63,$63,$63,$63,$63,$63,$63,$25,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$8c,$25,$25,$63,$63,$63,$63,$63,$63,$63,$25,$25,$8c,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$25,$63,$63,$63,$63,$63,$63,$63,$25,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$63,$25,$25,$25,$25,$25,$25,$25,$63,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$25,$63,$25,$25,$25,$25,$25,$25,$25,$63,$25,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$63,$63,$25,$25,$25,$25,$25,$25,$25,$63,$63,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$63,$25,$25,$25,$25,$25,$25,$25,$63,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$63,$63,$25,$25,$25,$25,$25,$63,$63,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$63,$63,$63,$63,$8c,$63,$63,$63,$63,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$63,$63,$63,$63,$8c,$63,$63,$63,$63,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$63,$63,$63,$63,$63,$8c,$63,$63,$63,$63,$63,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$63,$63,$63,$63,$8c,$63,$63,$63,$63,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$63,$97,$97,$97,$97,$97,$97,$97,$97,$97,$63,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$7f,$97,$97,$97,$97,$97,$7f,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite2:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$57,$4b,$4b,$4b,$57,$4b,$4b,$4b,$57,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$57,$4b,$4b,$4b,$57,$4b,$4b,$4b,$57,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$57,$4b,$4b,$4b,$57,$4b,$4b,$4b,$57,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$57,$4b,$4b,$4b,$57,$4b,$4b,$4b,$57,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$4b,$4b,$4b,$4b,$57,$4b,$4b,$4b,$4b,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$4b,$96,$96,$96,$96,$96,$96,$96,$4b,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$96,$96,$96,$96,$96,$96,$96,$96,$96,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$96,$96,$96,$96,$96,$96,$96,$96,$96,$96,$96,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$96,$96,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$96,$96,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$96,$96,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$96,$96,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$96,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$96,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$97,$97,$97,$97,$97,$97,$97,$97,$97,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$4b,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$4b,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$4b,$85,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$4b,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite3:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$00,$00,$97,$97,$97,$97,$97,$97,$97,$00,$00,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$97,$7f,$7f,$7f,$1b,$7f,$7f,$7f,$97,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$7f,$1b,$7f,$7f,$7f,$1b,$7f,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$7f,$97,$97,$97,$97,$97,$7f,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$7f,$97,$2a,$2a,$2a,$2a,$2a,$97,$7f,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$97,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$97,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$2a,$2a,$96,$96,$96,$96,$96,$2a,$2a,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$96,$96,$96,$96,$96,$96,$96,$96,$96,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$2a,$96,$96,$96,$96,$96,$96,$96,$2a,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$2a,$96,$96,$96,$96,$96,$96,$96,$2a,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$2a,$96,$96,$96,$96,$96,$96,$96,$2a,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$2a,$96,$96,$96,$96,$96,$96,$96,$2a,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$96,$96,$96,$96,$96,$96,$96,$96,$96,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$2a,$96,$96,$96,$96,$96,$96,$96,$2a,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$2a,$96,$2a,$2a,$2a,$2a,$2a,$96,$2a,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$96,$96,$2a,$2a,$2a,$2a,$2a,$96,$96,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$96,$97,$97,$97,$97,$97,$97,$97,$96,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$97,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$97,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$7f,$1b,$1b,$1b,$1b,$1b,$1b,$1b,$7f,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$7f,$1b,$7f,$7f,$7f,$7f,$7f,$1b,$7f,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$97,$1b,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$1b,$97,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$7f,$1b,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$1b,$7f,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$7f,$1b,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$1b,$7f,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$7f,$97,$97,$97,$97,$97,$97,$97,$7f,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite4:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$2a,$2a,$2a,$2a,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$2a,$2a,$97,$2a,$2a,$97,$2a,$2a,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$2a,$2a,$97,$2a,$2a,$97,$2a,$2a,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$2a,$2a,$97,$2a,$2a,$97,$2a,$2a,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$97,$97,$97,$97,$97,$97,$97,$97,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$25,$25,$25,$25,$25,$25,$25,$25,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$25,$25,$25,$25,$25,$25,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$2a,$2a,$97,$25,$25,$97,$2a,$2a,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$97,$2a,$2a,$2a,$2a,$97,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$25,$25,$2a,$2a,$25,$25,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$25,$25,$2a,$2a,$25,$25,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$25,$25,$2a,$2a,$25,$25,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$25,$25,$2a,$2a,$25,$25,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$25,$25,$2a,$2a,$25,$25,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$97,$25,$25,$2a,$2a,$25,$25,$97,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$97,$97,$2a,$2a,$2a,$2a,$97,$97,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$2a,$2a,$25,$25,$25,$25,$2a,$2a,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$2a,$97,$97,$97,$97,$97,$97,$2a,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$2a,$25,$25,$25,$25,$25,$25,$2a,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$2a,$97,$97,$97,$97,$97,$97,$2a,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$2a,$2a,$25,$25,$25,$25,$2a,$2a,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$2a,$7f,$7f,$2a,$2a,$2a,$2a,$7f,$7f,$2a,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$00,$00,$00,$00,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite5:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$80,$80,$80,$80,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$80,$80,$97,$80,$80,$97,$80,$80,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$80,$80,$80,$97,$80,$80,$97,$80,$80,$80,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$80,$80,$80,$97,$80,$80,$97,$80,$80,$80,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$80,$97,$97,$97,$97,$97,$97,$97,$97,$80,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$25,$25,$25,$25,$25,$25,$25,$25,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$25,$25,$25,$25,$25,$25,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$80,$80,$97,$25,$25,$97,$80,$80,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$97,$80,$80,$80,$80,$97,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$25,$25,$80,$80,$25,$25,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$25,$25,$80,$80,$25,$25,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$25,$25,$80,$80,$25,$25,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$25,$25,$80,$80,$25,$25,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$25,$25,$80,$80,$25,$25,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$97,$25,$25,$80,$80,$25,$25,$97,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$97,$97,$80,$80,$80,$80,$97,$97,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$80,$80,$25,$25,$25,$25,$80,$80,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$80,$80,$97,$97,$97,$97,$97,$97,$80,$80,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$80,$80,$25,$25,$25,$25,$25,$25,$80,$80,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$80,$80,$97,$97,$97,$97,$97,$97,$80,$80,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$80,$80,$80,$25,$25,$25,$25,$80,$80,$80,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$80,$7f,$7f,$80,$80,$80,$80,$7f,$7f,$80,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$00,$00,$00,$00,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite6:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$89,$89,$89,$89,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$89,$97,$89,$89,$97,$89,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$89,$89,$89,$97,$89,$89,$97,$89,$89,$89,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$89,$89,$89,$97,$89,$89,$97,$89,$89,$89,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$89,$97,$97,$97,$97,$97,$97,$97,$97,$89,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$25,$25,$25,$25,$25,$25,$25,$25,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$25,$25,$25,$25,$25,$25,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$89,$89,$97,$25,$25,$97,$89,$89,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$97,$89,$89,$89,$89,$97,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$25,$25,$89,$89,$25,$25,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$25,$25,$89,$89,$25,$25,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$25,$25,$89,$89,$25,$25,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$25,$25,$89,$89,$25,$25,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$25,$25,$89,$89,$25,$25,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$25,$25,$89,$89,$25,$25,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$97,$97,$89,$89,$89,$89,$97,$97,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$89,$25,$25,$25,$25,$89,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$89,$89,$97,$97,$97,$97,$97,$97,$89,$89,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$89,$89,$25,$25,$25,$25,$25,$25,$89,$89,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$89,$89,$97,$97,$97,$97,$97,$97,$89,$89,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$89,$89,$89,$25,$25,$25,$25,$89,$89,$89,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$7f,$7f,$89,$89,$89,$89,$7f,$7f,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$00,$00,$00,$00,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite7:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$29,$29,$29,$29,$29,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$8c,$29,$29,$29,$8c,$29,$29,$29,$8c,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$8c,$29,$29,$8c,$29,$8c,$29,$29,$8c,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$8c,$29,$29,$8c,$29,$8c,$29,$29,$8c,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$8c,$29,$29,$8c,$29,$8c,$29,$29,$8c,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$8c,$29,$8c,$29,$29,$29,$8c,$29,$8c,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$8c,$29,$8c,$29,$29,$29,$8c,$29,$8c,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$8c,$29,$8c,$8c,$8c,$8c,$8c,$29,$8c,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$29,$8c,$8c,$29,$29,$29,$29,$29,$8c,$8c,$29,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$29,$8c,$29,$25,$25,$25,$25,$25,$29,$8c,$29,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$29,$25,$25,$25,$25,$25,$25,$25,$25,$25,$29,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$29,$25,$25,$25,$25,$25,$25,$25,$25,$25,$29,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$29,$25,$25,$25,$25,$25,$25,$25,$29,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$25,$29,$25,$29,$29,$29,$25,$29,$25,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$25,$29,$29,$29,$29,$29,$29,$29,$25,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$25,$29,$29,$29,$29,$29,$29,$29,$25,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$25,$29,$29,$29,$29,$29,$29,$29,$25,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$25,$29,$29,$29,$29,$29,$29,$29,$25,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$8c,$25,$25,$29,$29,$29,$29,$29,$29,$29,$25,$25,$8c,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$29,$29,$29,$29,$29,$29,$29,$29,$29,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$29,$25,$25,$25,$25,$25,$25,$25,$29,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$25,$29,$25,$25,$25,$25,$25,$25,$25,$29,$25,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$29,$29,$25,$25,$25,$25,$25,$25,$25,$29,$29,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$29,$29,$25,$25,$25,$25,$25,$25,$25,$29,$29,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$29,$8c,$29,$25,$25,$25,$25,$25,$29,$8c,$29,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$8c,$29,$29,$29,$29,$29,$29,$29,$8c,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$29,$8c,$29,$29,$29,$29,$29,$29,$29,$8c,$29,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite8:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$76,$83,$76,$83,$76,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$76,$83,$76,$83,$76,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$76,$83,$76,$83,$76,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$76,$83,$76,$83,$76,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$83,$76,$76,$83,$76,$83,$76,$76,$83,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$83,$76,$76,$83,$76,$83,$76,$76,$83,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$83,$76,$76,$83,$76,$83,$76,$76,$83,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$83,$76,$76,$83,$76,$83,$76,$76,$83,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$83,$76,$83,$83,$83,$83,$83,$76,$83,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$83,$83,$25,$25,$25,$25,$25,$83,$83,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$25,$25,$25,$25,$25,$25,$25,$25,$25,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$25,$25,$25,$25,$25,$25,$25,$25,$25,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$76,$25,$25,$25,$25,$25,$25,$25,$76,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$25,$76,$25,$83,$76,$83,$25,$76,$25,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$25,$76,$76,$83,$76,$83,$76,$76,$25,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$25,$76,$76,$83,$76,$83,$76,$76,$25,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$25,$76,$76,$83,$76,$83,$76,$76,$25,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$25,$76,$76,$83,$76,$83,$76,$76,$25,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$25,$76,$83,$83,$83,$76,$83,$83,$83,$76,$25,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$83,$76,$76,$76,$76,$76,$76,$76,$83,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$76,$25,$25,$25,$25,$25,$76,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$25,$25,$25,$25,$25,$25,$25,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$25,$25,$25,$25,$25,$25,$25,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$76,$25,$25,$25,$25,$25,$25,$25,$76,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$76,$83,$25,$25,$25,$25,$25,$25,$25,$83,$76,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$76,$76,$76,$76,$76,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$76,$83,$76,$76,$76,$76,$76,$76,$76,$83,$76,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite9:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$22,$22,$22,$96,$22,$22,$22,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$96,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$96,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$96,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$96,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$96,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$96,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$97,$97,$97,$97,$97,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$97,$25,$25,$25,$25,$25,$97,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$96,$97,$25,$25,$25,$25,$25,$25,$25,$97,$96,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$25,$25,$25,$25,$25,$25,$25,$25,$25,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$25,$99,$25,$25,$25,$25,$25,$25,$25,$99,$25,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$25,$25,$99,$99,$99,$99,$99,$99,$99,$25,$25,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$25,$25,$99,$99,$7f,$7f,$7f,$99,$99,$25,$25,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$25,$25,$99,$99,$7f,$7f,$7f,$99,$99,$25,$25,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$25,$25,$99,$99,$7f,$7f,$7f,$99,$99,$25,$25,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$22,$22,$99,$99,$99,$99,$99,$99,$99,$22,$22,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$25,$25,$99,$99,$99,$99,$99,$99,$99,$25,$25,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$25,$25,$99,$25,$25,$25,$25,$25,$99,$25,$25,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$25,$99,$99,$25,$25,$25,$25,$25,$99,$99,$25,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$99,$99,$25,$25,$25,$25,$25,$25,$25,$99,$99,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$99,$22,$96,$25,$25,$25,$25,$25,$96,$22,$99,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$96,$96,$96,$96,$96,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$22,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$22,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$22,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$96,$22,$22,$22,$22,$22,$22,$22,$22,$22,$96,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$22,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$22,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite10:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$00,$00,$00,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$00,$97,$00,$00,$00,$97,$00,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$57,$57,$97,$97,$97,$97,$97,$97,$97,$57,$57,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$57,$57,$96,$57,$57,$57,$57,$57,$96,$57,$57,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$57,$96,$57,$57,$57,$57,$57,$57,$57,$96,$57,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$57,$96,$57,$57,$57,$57,$57,$57,$57,$96,$57,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$96,$57,$57,$57,$57,$57,$57,$57,$57,$57,$96,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$96,$57,$57,$57,$57,$57,$57,$57,$57,$57,$96,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$96,$96,$57,$96,$96,$57,$96,$57,$96,$96,$57,$96,$96,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$57,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$57,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$84,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$84,$97,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$97,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$84,$84,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$84,$84,$97,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$84,$84,$84,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite11:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$20,$89,$20,$89,$20,$89,$89,$20,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$20,$89,$20,$20,$20,$89,$89,$20,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$20,$89,$20,$89,$20,$89,$89,$20,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$20,$89,$89,$20,$89,$20,$89,$89,$89,$20,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$20,$89,$89,$20,$89,$20,$89,$89,$89,$20,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$20,$89,$89,$20,$20,$20,$89,$89,$89,$20,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$97,$97,$97,$97,$97,$97,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$20,$25,$25,$25,$25,$25,$25,$25,$25,$20,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$97,$25,$25,$25,$25,$97,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$20,$25,$89,$97,$25,$25,$97,$89,$25,$20,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$97,$97,$89,$89,$89,$89,$97,$97,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$25,$97,$89,$89,$89,$89,$97,$25,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$25,$97,$89,$89,$89,$89,$97,$25,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$25,$97,$89,$89,$89,$89,$97,$25,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$25,$97,$89,$89,$89,$89,$97,$25,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$20,$25,$97,$89,$89,$89,$89,$97,$25,$20,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$97,$89,$89,$89,$89,$89,$89,$97,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$89,$97,$97,$97,$97,$97,$97,$89,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$97,$25,$25,$25,$25,$25,$25,$97,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$97,$97,$97,$97,$97,$97,$97,$97,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$20,$89,$89,$89,$89,$89,$89,$89,$89,$20,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$20,$20,$89,$89,$89,$89,$89,$89,$20,$20,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$20,$20,$89,$20,$20,$20,$20,$89,$20,$20,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$97,$97,$97,$97,$97,$97,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite

sprite12:
;begin Sprite
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$00,$00,$00,$97,$99,$97,$00,$00,$00,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$00,$00,$97,$99,$97,$00,$00,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$97,$00,$97,$7f,$99,$7f,$97,$00,$97,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$97,$97,$99,$7f,$99,$7f,$99,$97,$97,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$99,$7f,$99,$99,$99,$7f,$99,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$99,$7f,$99,$99,$99,$7f,$99,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$7f,$99,$99,$7f,$99,$99,$7f,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$7f,$99,$7f,$99,$7f,$99,$7f,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$7f,$99,$7f,$99,$7f,$99,$7f,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$7f,$7f,$99,$99,$99,$7f,$7f,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$7f,$99,$99,$99,$99,$99,$7f,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$99,$99,$97,$97,$97,$97,$97,$99,$99,$20,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$97,$97,$89,$89,$89,$89,$89,$97,$97,$20,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$89,$89,$89,$89,$89,$89,$89,$89,$89,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$89,$89,$97,$97,$97,$97,$97,$89,$89,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$7f,$1b,$1b,$1b,$97,$1b,$1b,$1b,$7f,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$1b,$1b,$1b,$97,$1b,$1b,$1b,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$97,$1b,$1b,$1b,$97,$1b,$1b,$1b,$97,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$20,$97,$97,$97,$20,$97,$97,$97,$8c,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$99,$99,$99,$20,$99,$99,$99,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$20,$99,$99,$99,$20,$99,$99,$99,$99,$20,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$99,$20,$99,$20,$99,$99,$99,$99,$99,$20,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$99,$20,$99,$20,$99,$99,$99,$99,$99,$20,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$99,$99,$20,$99,$99,$99,$99,$99,$99,$20,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$99,$99,$99,$99,$99,$99,$99,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$99,$20,$99,$99,$99,$99,$99,$99,$99,$20,$99,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$20,$99,$99,$99,$99,$99,$99,$99,$20,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$99,$97,$97,$97,$97,$97,$97,$97,$97,$97,$99,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$97,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;end Sprite



TileSet:
;begin Sprite
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$89,$89,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$95,$95,$95,$95,$95,$95,$95,$63,$63,$95,$95,$95,$95,$95,$95,$95
	.byte	$4a,$4a,$48,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$95
	.byte	$4a,$4a,$4e,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$95,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4e,$4a,$83,$83,$83,$95,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4e,$4a,$4a,$83,$83,$83,$95
	.byte	$4a,$4a,$48,$48,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$83
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$83,$83,$83
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$83,$83,$83
	.byte	$4a,$4a,$4a,$4a,$48,$4a,$4e,$4e,$4a,$4a,$4a,$4a,$83,$83,$83,$83
	.byte	$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$83,$95
	.byte	$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$83,$95
	.byte	$4a,$4a,$4e,$4a,$4a,$4a,$4a,$48,$48,$4a,$4a,$83,$83,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83
	.byte	$4a,$48,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83
	.byte	$4a,$48,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83
	.byte	$4a,$4a,$4a,$4a,$48,$4e,$4e,$4e,$4a,$4a,$48,$48,$4a,$4a,$83,$83
	.byte	$4e,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$83,$83,$83
	.byte	$4a,$48,$48,$4e,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$83,$83,$95
	.byte	$4a,$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$95
	.byte	$4a,$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4e,$4a,$4a,$4a,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4e,$4a,$4a,$4a,$83,$83,$95
	.byte	$48,$4a,$4a,$4a,$4a,$4a,$48,$48,$4a,$4e,$4a,$4a,$4a,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$83,$83,$95,$95
	.byte	$4a,$4a,$4a,$4e,$4e,$4a,$48,$4a,$4a,$4a,$4a,$83,$83,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4e,$4a,$4a,$4a,$83,$83,$83,$95
	.byte	$4a,$4a,$48,$48,$4a,$4a,$4a,$4a,$4e,$4a,$4a,$4a,$83,$83,$83,$95
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4a,$4a,$48,$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4e
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4e,$4a,$4a,$4a,$48,$48,$4a,$4a,$4a
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$48,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$48,$48,$4a,$4a,$4a,$4a,$4e,$4e,$4a,$4a
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4e,$4e,$4a,$4a,$48,$4a,$4a,$4a,$48
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$4a,$4a,$4a,$4e,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4e,$4e,$4a,$48,$4a
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$48,$4a,$4a,$4a,$4a,$4a,$48,$4a
	.byte	$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$48,$4a,$4a
	.byte	$95,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$95,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4e,$4e,$4a,$4a
	.byte	$95,$95,$83,$83,$83,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$83,$4a,$4a,$4e,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$48,$48,$4a,$4a
	.byte	$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$83,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$83,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$83,$83,$4a,$4a,$4a,$4a,$4e,$4e,$4a,$48,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4e,$4a,$4a
	.byte	$95,$83,$83,$83,$83,$4a,$4a,$48,$48,$4a,$4a,$4a,$4a,$4e,$4a,$4a
	.byte	$95,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$48,$4a
	.byte	$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$48,$4a
	.byte	$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$4a,$4a,$48,$48,$4a,$4a,$4e,$4e,$4e,$48,$4a,$4a,$4a,$4a
	.byte	$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4e
	.byte	$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$83,$83,$83,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$4a,$4a,$4a,$4a,$48,$4a,$4a,$4a,$4a,$4e,$48,$48,$4a
	.byte	$95,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4e,$4a,$4a,$4a
	.byte	$95,$83,$83,$4a,$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4e,$4a,$4a,$4a
	.byte	$95,$83,$83,$4a,$4a,$4a,$4e,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$4a,$4a,$4a,$4e,$4a,$48,$48,$4a,$4a,$4a,$4a,$4a,$48
	.byte	$95,$83,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$95,$83,$83,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$83,$83,$4a,$4a,$4a,$4a,$48,$4a,$4e,$4e,$4a,$4a,$4a
	.byte	$95,$83,$83,$83,$4a,$4a,$4a,$4e,$4a,$48,$4a,$4a,$4a,$4a,$4a,$4a
	.byte	$95,$83,$83,$83,$4a,$4a,$4a,$4e,$4a,$4a,$4a,$4a,$48,$48,$4a,$4a
;end Sprite

; We load the tilemap from a .tlm file made by Aseprite as binary data
TileMapData:													;Because the emulator is a little different from the actual I have 2 tilemaps. Only one should be active.
;	.binary "RetroRacerProject/RetroRacerTilemapA.tlm"			;For the emulator version
	.binary "RetroRacerProject/RetroRacerTilemapB.tlm"			;For the actual computer




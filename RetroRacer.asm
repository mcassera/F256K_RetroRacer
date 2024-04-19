;Retro Racer Concept
;Michael Cassera 2024
; 


; Master Memory Unit
MMU_IO_CTRL 	= $01						;MMU I/O Control

; Vicky control
VKY_MSTR_CTRL_0 = $d000						; Vicky Master Control Register 0
VKY_MSTR_CTRL_1 = $d001						; Vicky Master Control Register 1
VKY_BRDR_CTRL   = $d004						; Vicky Border Control Register

; Vicky Background Color
VKY_BKG_COL_B   = $d00d						; Vicky Graphics Background Color Blue
VKY_BKG_COL_G   = $d00e						; Vicky Graphics Background Color Green
VKY_BKG_COL_R   = $d00f						; Vicky Graphics Background Color Red

; Vicky layer control
VKY_LAYER_CTRL_0= $d002
VKY_LAYER_CTRL_1= $d003

; Tile Set Registers
VKY_TS0_AD_L	= $d280						; Vicky Tile 0 Image Start Address LOW BYTE
VKY_TS0_AD_M	= $d281						; Vicky Tile 0 Image Start Address MEDIUM BYTE
VKY_TS0_AD_H	= $d282						; Vicky Tile 0 Image Start Address HIGH BYTE

; Tile Map Registers
VKY_TM0_CTRL	= $d20c						; Tile Map 0 Control
VKY_TM0_AD_L	= $d20d						; Tile Map 0 Start Address LOW BYTE
VKY_TM0_AD_M	= $d20e						; Tile Map 0 Start Address MEDIUM BYTE
VKY_TM0_AD_H	= $d20f						; Tile Map 0 Start Address HIGH BYTE
VKY_TM0_SZ_X	= $d210						; Tile Map 0 Size X
VKY_TM0_SZ_Y	= $d212						; Tile Map 0 Size Y
VKY_TM0_POS_X_L = $d214						; Tile Map 0 X Position & Scroll LOW BYTE
VKY_TM0_POS_X_H = $d215						; Tile Map 0 X Position & Scroll HIGH BYTE
VKY_TM0_POS_Y_L = $d216						; Tile Map 0 Y Position & Scroll LOW BYTE
VKY_TM0_POS_Y_H = $d217						; Tile Map 0 Y Position & Scroll HIGH BYTE

; Sprite registers							; we're starting a $0a for cars in case we want something in front of them (explosions or ???)
VKY_SPa_CTRL	= $D950						; Sprite 0a Control Register
VKY_SPa_AD_L   	= $D951						; Sprite 0a Pixel Data Address Register
VKY_SPa_AD_M   	= $D952
VKY_SPa_AD_H   	= $D953
VKY_SPa_POS_X_L = $D954						; Sprite 0a X Position Register
VKY_SPa_POS_X_H = $D955
VKY_SPa_POS_Y_L = $D956						; Sprite 0a X Position Register
VKY_SPa_POS_Y_H = $D957

; Vicky Color Look Up Table Regsiters
VKY_GR_CLUT_0  	= $d000						; Graphics LUT #0 in I/O page 1
VKY_GR_CLUT_1  	= $d400						; Graphics LUT #1 in I/O page 1

; Interrupt Registers
VIRQ			= $fffe						; Pointer to IRQ routine (LOW Byte)
INT_PEND_0		= $d660						; Pending register for interrupts 0-7
INT_PEND_1		= $d661						; Pending register for interrupts 8-15
INT_MASK_0		= $d66c						; Mask register for interrupts 0-7
INT_MASK_1		= $d66d						; Mask register for interrupts 8-15

; Math Coprocessor
MULU_A_L		= $de00						; unsigned A LOW byte
MULU_A_H		= $de01						; unsigned A HIGH Byte
MULU_B_L		= $de02						; unsigned B LOW byte
MULU_B_H		= $de03						; unsigned B HIGH byte
MULU_LL			= $de10						; A x B byte 0
MULU_LH			= $de11						; A x B byte 1
MULU_HL			= $de12						; A x B byte 3
MULU_HH			= $de13						; A x B byte 4

; Misc Variables for Indirect Indexing
ptr_src			= $80						; A pointer to read data
ptr_dst			= $82						; A pointer to write data

player.sprite	= player					; The sprite number 
player.xLOW		= player+1					; sprite x position low
player.xHI		= player+2					; sprite x position High
player.yFRAC	= player+3					; sprite y position fixed point decimal
player.yLOW		= player+4					; sprite y position low
player.yHI		= player+5					; sprite y position High
player.car		= player+6					; car image data (which art to use)
player.lane		= player+7					; current lane car occupies
player.speedLOW	= player+8					; car speed Low byte
player.speedHI	= player+9					; car speed High byte
player.speedTOP	= player+10					; car top speed (based on car type)
player.xTL		= player+11					; top left boundry box for collision detection
player.yTL		= player+12					; top left boundry box for collision detection
player.xBR		= player+13					; bottom right boundry box for collision detection
player.yBR		= player+14					; bottom right boundry box for collision detection
player.AI 		= player+15					; Car AI 1, defensive, 2 aggressive, 3 racing, 4 police

traffic.sprite	= traffic					; The sprite number 
traffic.xLOW	= traffic+1					; sprite x position low
traffic.xHI		= traffic+2					; sprite x position High
traffic.yFRAC	= traffic+3					; sprite y position fixed point decimal
traffic.yLOW	= traffic+4					; sprite y position low
traffic.yHI		= traffic+5					; sprite y position High
traffic.car		= traffic+6					; car image data (which art to use)
traffic.lane	= traffic+7					; current lane car occupies
traffic.speedLOW= traffic+8					; car speed Low byte
traffic.speedHI	= traffic+9					; car speed High byte
traffic.speedTOP= traffic+10				; car top speed (based on car type)
traffic.xTL		= traffic+11				; top left boundry box for collision detection
traffic.yTL		= traffic+12				; top left boundry box for collision detection
traffic.xBR		= traffic+13				; bottom right boundry box for collision detection
traffic.yBR		= traffic+14				; bottom right boundry box for collision detection
traffic.AI 		= traffic+15				; Car AI 1, defensive, 2 aggressive, 3 racing, 4 police

;.enc 										; set text encoding to screen graphics (for the C64 - F256k similar with letters/numbers)

.cpu "w65c02"								; set the cpu to Western Digital 65C02

*=$a0										; Set up buffer for Kernel communication
.dsection zp								; Define position for zp (zero page)
.cerror * > $af, "Too many Zero page variables"

*=$2000										; Our regular code goes here
.dsection code								; define that section of code

*=$2000		
start:										; Start of the program
	.include "api.asm"						; This is the Kernel API for communication

;SetupKernel:								; Set up the API to work with

		.section zp							; Zero page section $20 to $28
event:	.dstruct	kernel.event.event_t
		.send



*=$2000
; Self running code to send via USB port the F256. Saving wear and tear on the memory card slot.
; dipswitch 1 should be set to on.
		.byte $f2,$56						; Required bytes for the Kernel to identify
		.byte $04,$01						; how big is the program in 8K sections, What slot to map to
		.byte $0b,$20						; the starting address of your program
		.byte $00,$00,$00,$00				; reserved
		.byte $00							; terminating byte

; My program starts here!	
		stz MMU_IO_CTRL						; should do this on every program
		jsr clearTextScreen


;init_events:
		lda #<event
		sta kernel.args.events
		lda #>event
		sta kernel.args.events+1

;Set up TinyVicky to display sprites
		lda #%00110111						; Graphic, Tiles, & Sprites Engine enabled  |xx|GM|SP|TL|BM|GR|OV|TX|
		sta VKY_MSTR_CTRL_0					;                  		                    | 0| 0| 1| 1| 0| 1| 1| 1|

		lda #%00000110						; Text mode options for the overlay 		|xx|xx|FS|FO|MS|2Y|2X|70|
		sta VKY_MSTR_CTRL_1					; 320 x 240, 60 Hz, dbl X & Y				| 0| 0| 0| 0| 0| 1| 1| 0|
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
init_spites:
		
		lda #$01
		sta VKY_SPa_CTRL
		ldx #<Corvair
		ldy #>Corvair
		stx VKY_SPa_CTRL+1
		sty VKY_SPa_CTRL+2
		stz VKY_SPa_CTRL+3
		ldx #22
		ldy #1
		stx VKY_SPa_CTRL+4
		sty VKY_SPa_CTRL+5
		ldx #204
		stx VKY_SPa_CTRL+6
		stz VKY_SPa_CTRL+7

		lda #$01
		sta VKY_SPa_CTRL+8
		sta VKY_SPa_CTRL+16
		sta VKY_SPa_CTRL+24
		sta VKY_SPa_CTRL+32
		sta VKY_SPa_CTRL+40
		sta VKY_SPa_CTRL+48
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
	
; We set up the kernel timer here for the initial SOF
GameStart:
		lda #$01							; Enable the Random Number Generator
		sta $d6a6
		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		ora #kernel.args.timer.QUERY		; and query what frame we're on
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel Routine
		bcs skipSet							; If Carry set, ignore
		adc #$01							; if not add 1 to Accumulator for next frame
		sta $d0
skipSet:
		jsr SetTimer						; Let's get the kernel set up for the timer

; Put the speedometer at the bottom of the screen
		lda #$02
		sta MMU_IO_CTRL
		ldx #$00
textLoop:
		lda speedText,x 
		sta $c49c,x 
		inx
		cpx #$03
		bne textLoop
		stz MMU_IO_CTRL

;Game Loop Starts Here		
GameLoop:									; This is where we sit if not handling events 
		jsr handle_events					; check the events handler
		jsr manage_traffic					; checks on cars location so see if they've left the screen
		jsr handle_events					; we'll check the handler in between other code so it doesn't need to wait if we hit SOF
		jsr Speedometer						; print the speed at the bottom of the screen
		jmp GameLoop

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
		and #$01							; check for up
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

;update road								; Moving the Road by adjust the Tile Map
		sec									; Set the carry for subtraction
		lda roadMove						; load Road position (fixed decimal byte)
		sbc player.speedLOW					; add player + 7 (player speed decimal Byte)
		sta roadMove						; and store
		lda roadMove+1						; load the low byte of road position (Tilemap Low)
		sbc player.speedHI					; add player + 8 (player speed byte)
		sta roadMove+1						; and save
		sta VKY_TM0_POS_Y_L					; Write to Vicky Map Position Y, Low Byte
		stz VKY_TM0_POS_Y_H

;update car position
		lda joyX							; Get the X direction of the joystick
		beq	testY							; do nothing if no direction
		bmi moveLeft						; if X direction is negative skip to move left
moveRight:								
		clc									; Clear the carry for addition			
		lda player.xLOW						; load player + 1 (x Position (L))
		adc #$02							; move 2 pixels to the right
		sta player.xLOW						; and store
		sta VKY_SPa_POS_X_L					; Set the Vicky sprite 0 positiong L
		lda player.xHI						; load player + 2 (x Position (H))
		adc #$00							; Add Carry if needed
		sta player.xHI						; and store
		sta VKY_SPa_POS_X_H					; set the Vicky Sprite 0 Position H
		bra testY							; skip to test Y
moveLeft:
		sec 								; set the carry for subtraction
		lda player.xLOW						; load player + 1 (x Position (L))
		sbc #$02							; move 2 pixels to the left
		sta player.xLOW						; and store
		sta VKY_SPa_POS_X_L					; set the Vicky sprite 0 position L
		lda player.xHI						; load player + 2 (x Position (H))
		sbc #$00							; subtract carry if needed
		sta player.xHI						; and store
		sta VKY_SPa_POS_X_H					; set the Vicky Sprite 0 Position H

;Joystick Y controls speed
testY:
		lda joyY							; Load Joystick Y position
		beq upDateTraffic					; if no position, we are done
		bpl slowDown						; if joystick is down, then slow down
speedUp:
		clc									; clear carry for addition
		lda player.speedLOW					; load player + 7 (speed L)
		adc #$08							; Add $08
		sta player.speedLOW					; asd store
		lda player.speedHI					; load player + 8 (speed H)
		adc #$00							; Add carry
		cmp player.speedTOP					; If we hit the player's top speed
		beq topSpeed						; stop adding
		sta player.speedHI					; store if not over top speed
		bra upDateTraffic					; we are done
topSpeed:
		sec 								; we've hit top speed so we need
		lda player.speedLOW					; to subtract that speed low byte
		sbc #$08
		sta player.speedLOW
		bra upDateTraffic					; done
slowDown:
		sec									; Set carry for subtraction to slow down
		lda player.speedLOW					; load player speed L
		sbc #$10 							; subtract 16
		sta player.speedLOW					; and store
		lda player.speedHI					; load player speed H
		sbc #$00							; subtract carry
		bmi noSpeed							; Have we hit a negative number?
		sta player.speedHI					; if not, store the new speed
		bra upDateTraffic					; and done
noSpeed:
		stz player.speedLOW					; make sure all speed bytes are at zero since we're stopped
		bra upDateTraffic

upDateTraffic:								; We need to set the traffic observable speed relative the the player's speed
		ldx #$00							; X refers to which car are we updating.
carsLoop:
		stz MULU_B_H						; use the coprocessor multiplier to locate the appropriate bytes in the traffic LUT
		stz MULU_A_H
		stx MULU_A_L
		lda #$10							; Each car in the traffic LUT uses 11 bytes
		sta MULU_B_L
		ldy MULU_LL							; Assign that location to Y
		phx									; now push X on to the stack so we can use it for assigning 
		lda #$08							; so we can use the multiplier to locate the sprite table Y pos
		sta MULU_B_L
		ldx MULU_LL							; Assign that location to X

		lda player.speedHI					; determine if a car is going faster than the player
		cmp traffic.speedHI,y				; start with the high byte
		bcc addCarPlacement					; if car is slower, the sprite is traveling down the screen making it an addition problem
		bne subtractCarPlacement			; if car is faster, the sprite is moving up the screen making a subtraction problem
		lda player.speedLOW					; if High byte is equal, test the low byte
		cmp traffic.speedLOW,y		
		beq skipCarPlacement				; if car and player same speed, skip all math
		bcc addCarPlacement					; if car is slower, moving down screen, an addition problem
											; otherwise fall through the player car faster
subtractCarPlacement:
		sec									; Set up for subtraction
		lda player.speedLOW					; load players Speed LOW
		sbc traffic.speedLOW,y				; subtract a car's speed LOW
		sta $80								; store temporarily
		lda player.speedHI					; load player's speed HIGH
		sbc traffic.speedHI,y				; subtract traffic speed HIGH
		sta $81								; store temporarily

		clc									; set up for new subtraction
		lda traffic.yFRAC,y					; load traffic position FRACTION
		adc $80								; subtract relative speed to player LOW
		sta traffic.yFRAC,y					; store new traffic position FRACTION
		lda traffic.yLOW,y					; load traffic Speed LOW
		adc $81								; subtract relative speed to Player HIGH
		sta traffic.yLOW,y					; and store
		sta VKY_SPa_POS_Y_L+8,x				; also set the sprites Y LOW position
		lda traffic.yHI,y					; load the traffic HIGH y Position
		adc #$00							; and subtract the Carry
		sta traffic.yHI,y					; and save
		sta VKY_SPa_POS_Y_H+8,x				; set the sprite Y POS high

		bra skipCarPlacement

addCarPlacement:
		sec									; Set up for subtraction
		lda traffic.speedLOW,y				; load players Speed LOW
		sbc player.speedLOW					; subtract a car's speed LOW
		sta $80								; store temporarily
		lda traffic.speedHI,y 				; load player's speed HIGH
		sbc player.speedHI					; subtract traffic speed HIGH
		sta $81								; store temporarily

		sec									; set up for new subtraction
		lda traffic.yFRAC,y					; load traffic position FRACTION
		sbc $80								; subtract relative speed to player LOW
		sta traffic.yFRAC,y					; store new traffic position FRACTION
		lda traffic.yLOW,y					; load traffic Speed LOW
		sbc $81								; subtract relative speed to Player HIGH
		sta traffic.yLOW,y					; and store
		sta VKY_SPa_POS_Y_L+8,x				; also set the sprites Y LOW position
		lda traffic.yHI,y					; load the traffic HIGH y Position
		sbc #$00							; and subtract the Carry
		sta traffic.yHI,y					; and save
		sta VKY_SPa_POS_Y_H+8,x				; set the sprite Y POS high

skipCarPlacement:							; kind of a stub for horizontal placement of traffic
		clc									; once AI is in place there may be some lane changing
		lda traffic.xLOW,y		
		adc #$00					
		sta traffic.xLOW,y		
		sta VKY_SPa_POS_X_L+8,x	
		lda traffic.xHI,y		
		adc #$00				
		sta traffic.xHI,y		
		sta VKY_SPa_POS_X_H+8,x	


		plx									; we're done with a car's movement so let's pull X back from the stack
		inx									; increment it by 1 to the next car
		cpx #$07							; check to see if we've updated all the cars
		beq doneCarsLoop
		jmp carsLoop						; if not, loop again
	
doneCarsLoop:
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
		rts

manage_traffic:
		ldx #$00
trafficTestLoop:
		stz MULU_B_H						; use the coprocessor multiplier to locate the appropriate bytes in the traffic LUT
		stz MULU_A_H
		stx MULU_A_L
		lda #$10							; Each car in the traffic LUT uses 16 bytes
		sta MULU_B_L
		ldy MULU_LL							; Assign that location to Y

		lda traffic.yHI,y 					; Load the car Y,High value
		bmi aboveScreen						; if negative, we've gone off the top of the screen
		beq doneCar							; if zero, there is no need to change the car
		lda traffic.yLOW,y 					; if other than zero, (presumably 1), check the Low value to see if we've gone off the bottom of the scren
		cmp #$15							; a value of $01 10 takes us completely off the screen so let's check for $01 15 for a little leaway
		bcc doneCar							; if less, than there's nothing to do yet
belowScreen:
		lda #$00							; we've gone off the bottom of the screen so set the Low and High values to Zero to put the car
		sta traffic.yLOW,y					; position on top of the screen. 
		sta traffic.yHI,y			
		bra changeCar						; and jump to change the car type so it seems like we have new traffic ahead of us
aboveScreen:
		lda #$10							; the car has gone off the top of the screen se we need to change the car position	
		sta traffic.yLOW,y					; to the bottom of the screen. $01 10 is just below visable, but not past our check to see
		lda #$01							; if the car has exited off the bottom so there is no conflict
		sta traffic.yHI,y					; we will also just fall through to change the car below.
changeCar:
		phx									; push x onto the stack since we need it for indexing and we're already using it to track which car we're working on
		lda $d6a4							; get a random number from the F256 random number generator
		and #$0f 							; reduce it to 0 through 15
		sta traffic.car,y 					; this is our new car in traffic, store this number
		asl									; do a bit shift left o multiply by two so we can index the sprite art location
		tax									; transer to x for indexing
		lda carsImage,x 					; load the sprite location from the carsImage table Low
		sta $80								; and store in a temp location
		lda carsImage+1,x 					; load the sprite location from the carsIMage table High
		sta $81								; and store in a temp location
		lda traffic.sprite,y 				; Get the sprite hardware number we are using from the traffic table
		sta MULU_A_L						; and multiply it by 8 to get to the right sprite register locations
		lda #$08							; using the coprocessor 
		sta MULU_B_L
		ldx MULU_LL							; and load it into x for indexing
		lda $80								; now grab the sprite artwork address from the temp locations
		sta VKY_SPa_AD_L,x 					; and write to the sprite register locations for LOW
		lda $81
		sta VKY_SPa_AD_M,x 					; and High
		lda traffic.lane,y
		asl 
		tax
		lda laneData,x 
		sta traffic.xLOW,y
		lda laneData+1,x
		sta traffic.xHI,y
		plx									; pull off the stack the value of X for what car in traffic we are dealing with
		
doneCar
		inx									; increment x to get to the next car
		cpx #$07							; did we check all the cars?
		bne trafficTestLoop					; no, loop around to do the next car
		rts									; yes, return to the game loop

clearTextScreen:							; clear the text screen for overlay purposes
		lda #$02							; need to be in the right IO output to write to screen
		sta MMU_IO_CTRL
		ldx #$00							; set x to zero, we'll go once around all 256 bytes
loopClearText:
		lda #$20							; the space character
		sta $c000,x 						; store it in all the screen locations
		sta $c100,x 
		sta $c200,x 
		sta $c300,x 
		sta $c400,x 
		sta $c500,x 
		sta $c600,x 
		sta $c6d1,x 						; this last group is not a full 256 bytes so there's some overlap
		inx									; increment x
		bne loopClearText					; if we haven't looped all the way around to zero, keep looping
		stz MMU_IO_CTRL						; reset io control back to zero
		rts

; Generate the speedometer
Speedometer:
		lda player.speedLOW					; our speed has a fixed point decimal so we need to move that away.  HHHHLLLL LLLL.FFFF
		sta htdIN							; load our low/frac speed into the hex converter variable   LLLL.FFFF
		lda player.speedHI					; load our high/low speed into the hi hex converter variable  HHHHLLLL
		sta htdIN+1

		lsr htdIN+1							; do 4 bit shifts to the right. First the HHHHLLLL byte
		ror htdIN							; then the LLLL.FFFF byte with carry.
		lsr htdIN+1							; after four complete shifts, our bytes will be
		ror htdIN							; 0000HHHH LLLLLLLL, the fraction will be lost
		lsr htdIN+1
		ror htdIN
		lsr htdIN+1
		ror htdIN
		
		jsr htd								; go to the hex to dec converter
		lda #$02
		sta MMU_IO_CTRL						; shift io to the text screen
		ldx htdOUT+1						; get the high byte now in decimal. (should be a 4 bit number so no need to filter)
		lda numberText,x					; load the number from our table
		sta $c498							; and place it on the screen
		lda htdOUT							; load the low byte of our speed
		lsr									; shift over 4 bits to get the middle top 4 bits to the bottom
		lsr
		lsr
		lsr
		tax
		lda numberText,x					; load the number from our table
		sta $c499							; and place on the screen
		lda htdOUT							; reload the low speed byte
		and #$0f							; filter out the top 4 bits
		tax									; transfer to x if indexing
		lda numberText,x					; load the number from the table
		sta $c49a							; and place it on the screen
		stz MMU_IO_CTRL						; reset io to zero
		rts 

; Convert Hex to Decimal for readable Speedometer
htd:    lda htdIN							; this is our input number
		stz htdOUT							; set the output bytes to zero
		stz htdOUT+1
		ldx #$08 							; set a counter
htd1:
		asl htdIN
		rol htdOUT
		rol htdOUT+1

		dex 								; the shifting will happen 7 times. After
		beq htd3							; the last shift, you don't check for digits of
											; 5 or more
		lda htdOUT
		and #$0f
		cmp #$05
		bmi htd2

		clc
		lda htdOUT
		adc #$03
		sta htdOUT
htd2:
		lda htdOUT
		cmp #$50
		bmi htd1

		clc
		adc #$30
		sta htdOUT
		bra htd1
htd3:
		rts



; ***Working Memory***

speedText:
		.text "mph"
numberText:
		.text "0123456789 "

totalColors:	.byte $00					; This is a variable for the y index for the CLUT load routine

htdIN:			.byte $00,$00
htdOUT:			.byte $00,$00

joyX:		.byte $00						; signed X direction
joyY:		.byte $00						; signed Y direction
joyB:		.byte $00						; Button 0 Activated?
roadMove:	.byte $80,$80					; Low/High Road Movement Position

; LUT for Traffic tracking. 12 cars at most?
traffic:		; every time a car enters the screen we'll need to determine car type, driving style, speed, lane, etc
;			Sprite,	X_POS_L, X_POS_H,	Y_POS_FP,	Y_POS_L,	Y_POS_H,	car type,	current lane,	speed L,	speed H,	SpeedTop,	collision boundry,		Driver type 
;																																			(1 Defensive, 2 aggressive, 3 racing, 4 police)
		.byte	$01,	86,		 $00,		$00,		$00,		$00,		$02,		$00,			$50,		$04,		$00,		$00,$00,$00,$00,		$01
		.byte	$02,	120,	 $00,		$00,		$00,		$00,		$03,		$01,			$50,		$05,		$00,		$00,$00,$00,$00,		$01	
		.byte	$03,	152,	 $00,		$00,		$00,		$00,		$03,		$02,			$70,		$04,		$00,		$00,$00,$00,$00,		$01	
		.byte	$04,	184,	 $00,		$00,		$00,		$00,		$03,		$03,			$90,		$03,		$00,		$00,$00,$00,$00,		$01	
		.byte	$05,	216,	 $00,		$00,		$00,		$00,		$03,		$04,			$60,		$04,		$00,		$00,$00,$00,$00,		$01	
		.byte	$06,	248,	 $00,		$00,		$00,		$00,		$03,		$05,			$80,		$05,		$00,		$00,$00,$00,$00,		$01	
		.byte	$07,	86,		 $00,		$00,		$00,		$00,		$03,		$00,			$00,		$00,		$00,		$00,$00,$00,$00,		$01	
		.byte	$08,	120,	 $00,		$00,		$00,		$00,		$03,		$01,			$00,		$00,		$00,		$00,$00,$00,$00,		$01	
		.byte	$09,	152,	 $00,		$00,		$00,		$00,		$03,		$02,			$00,		$00,		$00,		$00,$00,$00,$00,		$01	
		.byte	$0a,	184,	 $00,		$00,		$00,		$00,		$03,		$03,			$00,		$00,		$00,		$00,$00,$00,$00,		$01	
		.byte	$0b,	216,	 $00,		$00,		$00,		$00,		$03,		$04,			$00,		$00,		$00,		$00,$00,$00,$00,		$01	
		.byte	$0c,	248,	 $00,		$00,		$00,		$00,		$03,		$05,			$00,		$00,		$00,		$00,$00,$00,$00,		$01		
	
player:
		.byte	$00,	22,		 $01,		$00,		220,		$00,		$01,		$07,			$00,		$00,		$08,		$00,$00,$00,$00,		$00
player.xFRAC:
		.byte 	$00							; we needed a fixed point decimal for the player car X_POS that the traffic does not need.

carsImage:
		.byte <VWBeetle,>VWBeetle,<VWBeetle,>VWBeetle,<Toyota4Runner,>Toyota4Runner
		.byte <BMW3,>BMW3,<Datsun280Z,>Datsun280Z
		.byte <Corvair,>Corvair,<C10Pickup,>C10Pickup,<Galaxie500,>Galaxie500
		.byte <AMCJavelin,>AMCJavelin,<JeepCJ5,>JeepCJ5,<JeepCJ5,>JeepCJ5,<Fiat131,>Fiat131
		.byte <VWBeetle,>VWBeetle,<Corvair,>Corvair,<C10Pickup,>C10Pickup,<BMW3,>BMW3

laneData:
		;lane       1,      2,      3,      4,      5,      6,shoulder
		.byte $56,$00,$78,$00,$98,$00,$b8,$00,$db,$00,$f8,$00,$16,$01

;Image Data Starts here
.include "RetroRacerCLUT.s"
.include "RetroRacerSprites.s"
.include "RetroRacerTileset.s"
																; We load the tilemap from a .tlm file made by Aseprite as binary data
TileMapData:													;Because the emulator is a little different from the actual I have 2 tilemaps. Only one should be active.
;		.binary "RetroRacerTilemapA.tlm"						;For the emulator version
		.binary "RetroRacerTilemapB.tlm"						;For the actual computer




;Retro Racer Concept
;Michael Cassera 2024
; 


		.include "RetroRacerInit.asm"		; all of our initial settings
		.cpu "w65c02"						; set the cpu to Western Digital 65C02

*=$a0										; Set up buffer for Kernel communication
		.dsection zp						; Define position for zp (zero page)
		.cerror * > $af, "Too many Zero page variables"

*=$2000										; Our regular code goes here
		.dsection code						; define that section of code

*=$1ffd

start:										; ***TEMP CODE FOR PROGRAMMING***
		jmp SC								; Start of the program - We put this jump here so you can load the PGZ into the computer.
											; With the self running code below, you can boot into it in RAM. Without this jump, loading a PGZ will
											; hit the self running code the kernel needs at the start of the slot at $2000 and look like a freeze.
											; hitting the reset on the back of the F256k will start the program.


		.include "api.asm"					; This is the Kernel API for communication

;SetupKernel:								; Set up the API to work with

		.section zp							; Zero page section $20 to $28
event:	.dstruct	kernel.event.event_t
		.send



*=$2000										; ***TEMP CODE FOR PROGRAMMING***
											; Self running code to send via USB port the F256. Allows me to quickly load into emulator
											; dipswitch 1 should be set to on.
		.byte $f2,$56						; Required bytes for the Kernel to identify
		.byte $04,$01						; how big is the program in 8K sections, What slot to map to
		.byte $0b,$20						; the starting address of your program
		.byte $00,$00,$00,$00				; reserved
		.byte $00							; terminating byte

											; *****My program starts here!*****
SC:
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

;Set TileSet 1 for our Game
		lda #<TileSet
		sta VKY_TS1_AD_L
		lda #>TileSet
		sta VKY_TS1_AD_M
		lda #$01							
		sta VKY_TS1_AD_H

;Set TileSet 0 for our Title Image
		lda #<TileSet
		sta VKY_TS0_AD_L
		lda #>TileSet
		sta VKY_TS0_AD_M
		lda #$01							
		sta VKY_TS0_AD_H

		
	
;Set Tile Map 1
		lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
		sta VKY_TM1_CTRL					;										   | 0| 0| 0| 0| 0| 0| 0| 1|
		lda #20								; Tile Map Size 20 X 
		sta VKY_TM1_SZ_X
		lda #200							; Tile Map Size 200 Y	This is really long so we need to add some more scenery.
		sta VKY_TM1_SZ_Y

		lda #<TileMapData					; Point to the Tile Map Data, LOW BYTE
		sta VKY_TM1_AD_L
		lda #>TileMapData					; Point to the Tile Map Data, MEDIUM BYTE
		sta VKY_TM1_AD_M
		lda #$01							; Point to the Tile Map Data, HIGH BYTE
		sta VKY_TM1_AD_H

;Set Tile Map 0
		lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
		sta VKY_TM0_CTRL					;										   | 0| 0| 0| 0| 0| 0| 0| 1|
		lda #20								; Tile Map Size 20 X 
		sta VKY_TM0_SZ_X
		lda #31								; Tile Map Size 200 Y	This is really long so we need to add some more scenery.
		sta VKY_TM0_SZ_Y

		lda #<TitleMapData					; Point to the Tile Map Data, LOW BYTE
		sta VKY_TM0_AD_L
		lda #>TitleMapData					; Point to the Tile Map Data, MEDIUM BYTE
		sta VKY_TM0_AD_M
		lda #$01							; Point to the Tile Map Data, HIGH BYTE
		sta VKY_TM0_AD_H

		lda #$cb
		sta VKY_TM0_POS_Y_L
		stz VKY_TM0_POS_Y_H
		stz VKY_TM0_POS_X_L
		stz VKY_TM0_POS_X_H


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
	
		ldx #$00							; X is the number of colors to copy, check for 154
		ldy #$9a
		jsr makeClut

		lda #<CLUT1							; Set source pointer to CLUT for color information
		sta ptr_src
		lda #>CLUT1
		sta ptr_src+1
	
		lda #<VKY_GR_CLUT_1					; Set destination pointer to Graphics CLUT 0
		sta ptr_dst
		lda #>VKY_GR_CLUT_1
		sta ptr_dst+1
	
		ldx #$00							; X is the number of colors to copy, check for 255
		ldy #$ff
		jsr makeClut
	


		stz MMU_IO_CTRL
setFont:
		lda #<font
		sta $80
		lda #>font
		sta $81
		lda #$c1
		stz $82
		sta $83
		ldy #$00
		ldx #$03
		lda #$01
		sta MMU_IO_CTRL
_sfLoop:
		lda ($80),y
		sta ($82),y 
		iny
		bne _sfLoop
		inc $81
		inc $83
		dex
		bne _sfLoop
		stz MMU_IO_CTRL
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

		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		ora #kernel.args.timer.QUERY		; and query what frame we're on
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel Routine
		bcs skipSet							; If Carry set, ignore
		adc #$01							; if not add 1 to Accumulator for next frame
		sta $d0
skipSet:
		jsr SetTimer						; Let's get the kernel set up for the timer

		jmp endGame
		
;Set up Sprites
init_spites:								; Kind of a stub here (brute force settings)
		
		lda #$01							; Enable the Random Number Generator
		sta Random_Reg
		lda #$01							; Sprite $0a is the players car so we'll
		sta VKY_SPa_CTRL					; initialize and position it here.
		lda Random_L
		and #$03
		clc 
		sta $80
		adc $80
		adc $80
		tax
		lda carsInfo,x					; We're starting with the Corvair
		sta VKY_SPa_AD_L
		lda carsInfo+1,x
		sta VKY_SPa_AD_M
		lda #$01
		sta VKY_SPa_AD_H
;	sta VKY_SPa_CTRL
		lda carsInfo+2,x
		sta player.speedTop

		ldx #22								; This is the right shoulder on the road
		ldy #1
		stx VKY_SPa_POS_X_L
		stx player.xLOW
		sty VKY_SPa_POS_X_H
		sty player.xHI
		ldx #204							; towards the bottom of the screen
		stx VKY_SPa_POS_Y_L
		stx player.yLOW
		lda #$00
		sta VKY_SPa_POS_Y_H
		stz player.yHI
		stz player.yFRAC
		stz player.speedLOW
		stz player.speedHI

		lda #$01							; Traffic starts at sprite $0b, let's just initialize them all.
		sta VKY_SPa_CTRL+8					; $0b
		sta VKY_SPa_CTRL+16					; $0c
		sta VKY_SPa_CTRL+24					; $0d
		sta VKY_SPa_CTRL+32					; $0e
		sta VKY_SPa_CTRL+40					; $0f
		sta VKY_SPa_CTRL+48					; $10

		lda <#cloud1
		sta VKY_SP6_AD_L
		lda >#cloud1
		sta VKY_SP6_AD_M
		lda #$01
		sta VKY_SP6_AD_H

		lda <#cloud2
		sta VKY_SP6_AD_L+8
		lda >#cloud2
		sta VKY_SP6_AD_M+8
		lda #$01
		sta VKY_SP6_AD_H+8

		lda <#cloud3
		sta VKY_SP6_AD_L+16
		lda >#cloud3
		sta VKY_SP6_AD_M+16
		lda #$01
		sta VKY_SP6_AD_H+16

		lda <#cloud4
		sta VKY_SP6_AD_L+24
		lda >#cloud4
		sta VKY_SP6_AD_M+24
		lda #$01
		sta VKY_SP6_AD_H+24

		stz VKY_SP6_CTRL
		stz VKY_SP6_CTRL+8
		stz VKY_SP6_CTRL+16
		stz VKY_SP6_CTRL+24

;		jmp GameStart
	

	


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

; ************************************************************************************************************************************		
;Game Loop Starts Here		
GameLoop:									; This is where we sit if not handling events 
		jsr handle_events					; check the events handler
		jsr manage_traffic					; checks on cars location so see if they've left the screen
		jsr handle_events					; we'll check the handler in between other code so it doesn't need to wait if we hit SOF
		jsr Speedometer						; print the speed at the bottom of the screen
		jsr travelDisplay
		jsr handle_events
		jsr collisionCheck					; Check for any collisions with traffic
		jsr handle_events
		jsr checkHits						; Check to see how many collisions and react 
		jsr handle_events
		lda GO
		bne endGame
		jmp GameLoop

; ************************************************************************************************************************************

endGame:
		stz VKY_SPa_CTRL+8					; $0b
		stz VKY_SPa_CTRL+16					; $0c
		stz VKY_SPa_CTRL+24					; $0d
		stz VKY_SPa_CTRL+32					; $0e
		stz VKY_SPa_CTRL+40					; $0f
		stz VKY_SPa_CTRL+48					; $10


waitLoop:
		jsr Speedometer						; keep display the speed, we should be slowing down
		stz gTimeHI							; make sure the timer is set to zero
		stz gTimeLO
		stz joyX							; make sure we are not gassing or braking
		stz joyY
		lda player.speedHI
		sta SID_L_VOL
		sta SID_R_VOL
		bne _skipSpeed
		lda player.speedLOW
		cmp #$40
		bcs _skipSpeed
		lda #$40
		sta player.speedLOW
		lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
		sta VKY_TM0_CTRL	
		stz smoke
		stz VKY_SP6_CTRL
		stz VKY_SP6_CTRL+8
		stz VKY_SP6_CTRL+16
		stz VKY_SP6_CTRL+24
		stz VKY_SPa_CTRL
		stz mTimer							; stops music play
_skipSpeed:
		jsr handle_events					; check events
		lda joyB							; if the joystick button pressed, start a new race
		beq waitLoop						; if not, keep looping
		stz VKY_TM0_CTRL					; kill title screen
		stz oldHit							; reset hit counter
		stz hit									
		stz smoke							; reset smoke counter
		stz travelLO
		stz travelME
		stz travelHI
		stz GO								; reset Game Over flag
		lda #$3c 							; set the timer to 60 seconds
		sta gTimeHI						
		sta gTimeLO							; and 60 frames since we count at SOF
		stz SID_L1_GATE
		stz SID_L2_GATE
		stz SID_L3_GATE
		stz SID_R1_GATE
		stz SID_R2_GATE
		stz SID_R3_GATE
		
		lda #$0f							; set the SID volume to 15
		sta SID_L_VOL
		sta SID_R_VOL
		lda #$09 
		sta SID_L1_ATDL
		sta SID_L1_STRL
		sta SID_R1_ATDL
		sta SID_R1_STRL
		sta SID_L1_PULS_L
		sta SID_L1_PULS_H
		lda #$42
		sta SID_L2_ATDL
		sta SID_R2_ATDL
		lda #$79
		sta SID_L2_STRL
		sta SID_R2_STRL
		lda #$ff
		sta nCounter
		lda #<notes2
		sta $70
		lda #>notes2
		sta $71
		lda #<notes3
		sta $72
		lda #>notes3
		sta $73
		jmp init_spites						; jump to init sprites to turn the traffice back on and place the player


; ************************************************************************************************************************************

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

; ************************************************************************************************************************************

;Read the joystick on event and set a direction to be used during the SOF cycle
setJoyStick:
		lda event.joystick.joy1
		sta $d8								; Temp save joystick state
		stz joyX							; reset all directions and button to zero
		stz joyY
		stz joyB
		lda GO
		bne jButton
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



; ************************************************************************************************************************************
; These are events that happen at SOF, 60 times per second
UpdateScreen:
		jsr SetTimer						; Reset the next timer for SOF
		jsr updateClock
		stz hitFlag							; clear the collision flag so we can test for collisions again
		lda smoke							; check to see if car is smoking from damage
		beq _skipSmoke						; if not, skip 
		jsr makeSmoke						; if yes, jsr to smoke subroutine
_skipSmoke:
		inc mTimer							; increment the music timer
		lda mTimer							; load and compare with set time
		cmp #$08 							; to determine if we should play the next note
		bne _skipMusic						; if not skip
		jsr playNotes						; if yes, go to music playing routine
_skipMusic:
		clc									;generate a pulse for music
		lda pulse1
		adc #$40
		sta pulse1
		sta SID_L2_PULS_L
		sta SID_R2_PULS_L
		lda pulse1+1
		adc #$00
		sta pulse1+1
		sta SID_L2_PULS_H
		sta SID_R2_PULS_H

updateRoad:									; Moving the Road by adjust the Tile Map
		sec									; Set the carry for subtraction
		lda roadMove						; load Road position (fixed decimal byte)
		sbc player.speedLOW					; add player + 7 (player speed decimal Byte)
		sta roadMove						; and store
		lda roadMove+1						; load the low byte of road position (Tilemap Low)
		sbc player.speedHI					; add player + 8 (player speed byte)
		sta roadMove+1						; and save
		sta VKY_TM1_POS_Y_L					; Write to Vicky Map Position Y, Low Byte
		stz VKY_TM1_POS_Y_H
		lda travelLO
		sta ADD_A_LL
		lda travelME
		sta ADD_A_LH
		lda travelHI
		sta ADD_A_HL
		lda #$01
		sta ADD_A_HH
		lda player.speedHI
		sta ADD_B_LL
		stz ADD_B_LH
		stz ADD_B_HL
		stz ADD_B_HH
		lda ADD_R_LL
		sta travelLO
		lda ADD_R_LH
		sta travelME
		lda ADD_R_HL 
		sta travelHI

;update car position
		clc
		lda player.speedLOW
		adc player.speedHI
		beq placeX
		
		lda joyX							; Get the X direction of the joystick
		beq	placeX							; do nothing if no direction
		bmi moveLeft						; if X direction is negative skip to move left
moveRight:								
		clc									; Clear the carry for addition			
		lda player.xLOW						; load player + 1 (x Position (L))
		adc #$02							; move 2 pixels to the right
		sta player.xLOW						; and store
		lda player.xHI						; load player + 2 (x Position (H))
		adc #$00							; Add Carry if needed
		sta player.xHI						; and store
		bra placeX							; skip to test Y
moveLeft:
		sec 								; set the carry for subtraction
		lda player.xLOW						; load player + 1 (x Position (L))
		sbc #$02							; move 2 pixels to the left
		sta player.xLOW						; and store
		lda player.xHI						; load player + 2 (x Position (H))
		sbc #$00							; subtract carry if needed
		sta player.xHI						; and store

placeX:

		lda player.xHI						; We want to check to see if we're trying to go offroad
		bne _testRight						; if xHI is zero we're checking the left side, != Zero the right
_testLeft:
		lda player.xLOW						; Load xLOW and compare with 3f (left curb)
		cmp #$3f
		bcs _placeSprite					; if we're greater, no problem, place the sprite
		clc
		adc #$03							; if we're into the grass shift sprite right 3 pixels
		sta player.xLOW
		bra _placeSprite					; and place the sprite
_testRight:
		lda player.xLOW						; load xLOW and compare with $26 (right curb)
		cmp #$26
		bcc _placeSprite					; if we're less, no problem, place the sprite
		sec
		sbc #$03							; if we're into the grass, shift sprite 3 pixels to the left
		sta player.xLOW

_placeSprite:
		lda player.xLOW
		sta VKY_SPa_POS_X_L					; set the Vicky sprite 0 position L
		lda player.xHI 
		sta VKY_SPa_POS_X_H					; set the Vicky Sprite 0 Position H


; ************************************************************************************************************************************
;Joystick Y controls speed
testY:

		lda joyY							; Load Joystick Y position
		beq slowDown						; if no position, we are done
		bpl brake							; if joystick is down, then slow down
speedUp:
		clc									; clear carry for addition
		lda player.speedLOW					; load player + 7 (speed L)
		adc #$08							; Add $08
		sta player.speedLOW					; asd store
		lda player.speedHI					; load player + 8 (speed H)
		adc #$00							; Add carry
		cmp player.speedTOP					; If we hit the player's top speed
		bcs topSpeed						; stop adding
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
		sbc #$03							; subtract 03
		sta player.speedLOW					; and store
		lda player.speedHI					; load player speed H
		sbc #$00							; subtract carry
		bmi noSpeed							; Have we hit a negative number?
		sta player.speedHI					; if not, store the new speed
		bra upDateTraffic					; and done
brake:
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
		cpx #$06							; check to see if we've updated all the cars
		beq doneCarsLoop
		jmp carsLoop						; if not, loop again
	
doneCarsLoop:
		rts

; ************************************************************************************************************************************

makeSmoke:									; smoke routine repeated four time for 4 clouds
		lda sm1T							; check the countdown timer for cloud 1
		bne _skipNewSmoke					; if it's not zero, keep displaying it
		lda Random_L						; if not, replace and set new random timer
		and #$3f		
		sta sm1T							; store new timer
		lda player.xLOW						; get the player car position
		sta sm1X							; and set the smoke position
;		sta $82								; and store temp on ZP
		lda player.xHI						; do the same for HI
		sta sm1X+1 	
;		sta $83
		lda #204							; get the Y position - It doesn't change for the player
		sta sm1Y 							; and store
;		lda #$00
		stz sm1Y+1
_skipNewSmoke:
		clc									; now we move the smoke
		lda sm1Y							; load smoke postion Y
		adc #$02							; add 2 to make it trail behind car
		sta sm1Y 							; and store
;		sta $80								; also store temp on ZP
		lda sm1Y+1							; and add the Y HI
		adc #$00
		sta sm1Y+1							; and save
;		sta $81
		lda sm1X							; now load the temp variables
		sta VKY_SP6_POS_X_L					; and write to sprite position registers
		lda sm1X+1							; for X Low, HI and Y Low,HI
		sta VKY_SP6_POS_X_H
		lda sm1Y
		sta VKY_SP6_POS_Y_L
		lda sm1Y+1
		sta VKY_SP6_POS_Y_H
		dec sm1T							; decrease the cloud timer by 1
		lda smoke							; load the smoke counter
		cmp #$01							; do we only have one cloud?
		bne _continueSmoke2					; no, go to smoke 2
		jmp _returnFromSmoke				; yes return from this routine
_continueSmoke2:							
		lda sm2T							; now repeat this routine 3 more times for the
		bne _skipNewSmoke2					; other clouds of smoke.
		lda Random_L
		and #$3f
		sta sm2T
		lda player.xLOW
		sta sm2X
;		sta $82
		lda player.xHI
		sta sm2X+1 
;		sta $83
		lda #204
		sta sm2Y
;		lda #$00
		stz sm2Y+1
_skipNewSmoke2:
		clc
		lda sm2Y
		adc #$02
		sta sm2Y 
;		sta $80
		lda sm2Y+1
		adc #$00
		sta sm2Y+1
;		sta $81
		lda sm2X
		sta VKY_SP6_POS_X_L+8
		lda sm2X+1
		sta VKY_SP6_POS_X_H+8
		lda sm2Y
		sta VKY_SP6_POS_Y_L+8
		lda sm2Y+1
		sta VKY_SP6_POS_Y_H+8
		dec sm2T

		lda smoke
		cmp #$02
		bne _continueSmoke3
		jmp _returnFromSmoke
_continueSmoke3
		lda sm3T
		bne _skipNewSmoke3
		lda Random_L
		and #$3f
		sta sm3T
		lda player.xLOW
		sta sm3X
;		sta $82
		lda player.xHI
		sta sm3X+1 
;		sta $83
		lda #204
		sta sm3Y 
;		lda #$00
		stz sm3Y+1
_skipNewSmoke3:
		clc
		lda sm3Y
		adc #$02
		sta sm3Y 
;		sta $80
		lda sm3Y+1
		adc #$00
		sta sm3Y+1
;		sta $81
		lda sm3X
		sta VKY_SP6_POS_X_L+16
		lda sm3X+1
		sta VKY_SP6_POS_X_H+16
		lda sm3Y
		sta VKY_SP6_POS_Y_L+16
		lda sm3Y+1
		sta VKY_SP6_POS_Y_H+16
		dec sm3T

		lda smoke
		cmp #$03
		bne _continueSmoke4
		jmp _returnFromSmoke

_continueSmoke4
		lda sm4T
		bne _skipNewSmoke4
		lda Random_L
		and #$3f
		sta sm4T
		lda player.xLOW
		sta sm4X
;		sta $82
		lda player.xHI
		sta sm4X+1 
;		sta $83
		lda #204
		sta sm4Y
;		lda #$00
		stz sm4Y+1
_skipNewSmoke4:
		clc
		lda sm4Y
		adc #$02
		sta sm4Y 
;		sta $80
		lda sm4Y+1
		adc #$00
		sta sm4Y+1
;		sta $81
		lda sm4X
		sta VKY_SP6_POS_X_L+24
		lda sm4X+1
		sta VKY_SP6_POS_X_H+24
		lda sm4Y
		sta VKY_SP6_POS_Y_L+24
		lda sm4Y+1
		sta VKY_SP6_POS_Y_H+24
		dec sm4T

_returnFromSmoke:
		rts									; Return to ScreenUpdate


; ************************************************************************************************************************************
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

updateClock:
		sec
		lda gTimeLO 
		sbc #$01
		sta gTimeLO
		lda gTimeHI
		sbc #$00
		sta gTimeHI
		bmi setGameOver
		lda gTimeLO
		bmi mk60
		rts
mk60:
		lda #$3c
		sta gTimeLO
		rts
setGameOver:
		inc GO
		rts

; ************************************************************************************************************************************

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
		beq doneCarJmp						; if zero, there is no need to change the car
		lda traffic.yLOW,y 					; if other than zero, (presumably 1), check the Low value to see if we've gone off the bottom of the scren
		cmp #$15							; a value of $01 10 takes us completely off the screen so let's check for $01 15 for a little leaway
		bcc doneCarJmp						; if less, than there's nothing to do yet
belowScreen:
		lda #$00							; we've gone off the bottom of the screen so set the Low and High values to Zero to put the car
		sta traffic.yLOW,y					; position on top of the screen. 
		sta traffic.yHI,y			
		bra changeCar						; and jump to change the car type so it seems like we have new traffic ahead of us
doneCarJmp:
		jmp doneCar
aboveScreen:
		lda #$10							; the car has gone off the top of the screen se we need to change the car position	
		sta traffic.yLOW,y					; to the bottom of the screen. $01 10 is just below visable, but not past our check to see
		lda #$01							; if the car has exited off the bottom so there is no conflict
		sta traffic.yHI,y					; we will also just fall through to change the car below.
changeCar:
		phx									; push x onto the stack since we need it for indexing and we're already using it to track which car we're working on
		lda Random_L						; get a random number from the F256 random number generator
		and #$0f 							; reduce it to 0 through 15
		sta traffic.car,y 					; this is our new car in traffic, store this number
		asl									; do a bit shift left to multiply by two so we can index the sprite art location
		tax									; transer to x for indexing
		lda carsImage,x 					; load the sprite location from the carsImage table Low
		sta $80								; and store in a temp location
		lda carsImage+1,x 					; load the sprite location from the carsIMage table High
		sta $81								; and store in a temp location
		txa 								; transfer x back to a
		asl									; and double it again
		tax									; and transfer back to x
		lda hitBox,x						; now get the Top Right corner of the car's Collision Bounding Box
		sta traffic.xTL,y					; and store in the traffic look up table
		lda hitBox+1,x 
		sta traffic.yTL,y
		lda hitBox+2,x 						; get the bottem right corner of the car's Collision Bounding Box
		sta traffic.xBR,y 					; and store it in the traffic look up table
		lda hitBox+3,x 
		sta traffic.yBR,y 
		lda traffic.sprite,y 				; Get the sprite hardware number we are using from the traffic table
		sta MULU_A_L						; and multiply it by 8 to get to the right sprite register locations
		lda #$08							; using the coprocessor 
		sta MULU_B_L
		ldx MULU_LL							; and load it into x for indexing
		lda $80								; now grab the sprite artwork address from the temp locations
		sta VKY_SPa_AD_L,x 					; and write to the sprite register locations for LOW
		lda $81
		sta VKY_SPa_AD_M,x 					; and High
		lda #$01
		sta VKY_SPa_AD_H,x
		lda traffic.lane,y
		asl 
		tax

		lda Random_L						; We want variation with cars in their lanes so the player can't split lanes
		sta $80								; so we're going to get a random number and reduce it to 4 bits.	
		and #$04							; bit 3 will determine if we're left or right of center
		beq _less							; The other 2 bits will determine how many pixels off center
		lda $80
		and #$03
		sta $80	
		clc									; this moves the the car 1 to 3 pixels right
		lda laneData,x 
		adc $80
		sta traffic.xLOW,y 
		lda laneData+1,x 
		adc #$00
		sta traffic.xHI,y 
		bra selectSpeed
_less:										; This moves the car 1 to 3 pixel left
		lda $80
		and #$03
		sta $80
		sec
		lda laneData,x 
		sbc $80
		sta traffic.xLOW,y 	
		lda laneData+1,x 
		sbc #$00
		sta traffic.xHI,y 

selectSpeed:
		lda Random_L						; Select a speed between 55mph and 79mph - start with a fractional random number
		sta MULU_A_L
		stz MULU_A_H
		lda #$18							; multiply by $18 - which is a difference of 24mph
		sta MULU_B_L						
		stz MULU_B_H
		clc
		lda MULU_LH							; get the LH byte from the multiplier, but treat it as a LOW byte since we used a fractional multiplier
		adc #$37							; add $37 - which is 55mph
		sta $80								; store temp on zero page
		stz $81								; clear the another zero page
		asl $80								; and do 4 shifts left carrying the hi bit from the LOW to HI byte
		rol $81								; this gets us out speed - since we're using a 4bit fixed point number for LOW
		asl $80
		rol $81 
		asl $80 
		rol $81 
		asl $80
		rol $81 
		lda $80
		sta traffic.speedLOW,y 				; store in the traffic table
		lda $81
		sta traffic.speedHI,y

		plx									; pull off the stack the value of X for what car in traffic we are dealing with
		
doneCar
		inx									; increment x to get to the next car
		cpx #$06							; did we check all the cars?
		beq doneTrafficLoop					; yes, return to the game loop
		jmp trafficTestLoop					; No, loop around to do the next car
doneTrafficLoop:
		rts	

; ************************************************************************************************************************************
; Check for collisions

collisionCheck:
		lda hitFlag							; check to see if we already have a collision. If so we need to wait for screen
		beq _okayToTest						; update to clear the flag since we hit this routine outside the SOF routines.
		rts									; return to game loop if the flag is already set
_okayToTest:
		jsr clearAdder						; Clear the co-processor adder for some math
		ldx player.xTL						; get the car collision boundry
		ldy player.xLOW						; and add it to the actual sprite position
		lda player.xHI						; to get the top left, bottom right coordinates
		jsr collisionAdd
		sta playerLeft_L					; store them in memory to do comparison with all
		stx playerLeft_H 					; the traffic later in the routine
		ldx player.xBR
		lda player.xHI
		jsr collisionAdd
		sta playerRight_L 
		stx playerRight_H
		jsr clearAdder
		ldx player.yTL
		ldy player.yLOW
		lda player.yHI
		jsr collisionAdd
		sta playerTop_L
		stx playerTop_H 	
		ldx player.yBR
		lda player.yHI
		jsr collisionAdd	
		sta playerBot_L
		stx playerBot_H
	
checkTraffic:
		ldx #$00
collisionTestLoop:
		stz MULU_B_H						; use the coprocessor multiplier to locate the appropriate bytes in the traffic LUT
		stz MULU_A_H
		stx MULU_A_L
		lda #$10							; Each car in the traffic LUT uses 16 bytes
		sta MULU_B_L
		ldy MULU_LL							; Assign that location to Y

		jsr clearAdder						; Like the player's car we are calculating the hit box in real space on the
		lda traffic.xTL,y 					; screen for all four corners of the car.
		sta ADD_A_LL						; These will be saved on ZP for fast access.
		lda traffic.xLOW,y 
		sta ADD_B_LL
		lda traffic.xHI,y 
		sta ADD_B_LH
		
		lda ADD_R_LL						; X LEFT
		sta trafficLeft_L
		lda ADD_R_LH
		sta trafficLeft_H

		lda traffic.xBR,y 					; X Right
		sta ADD_A_LL
		lda ADD_R_LL
		sta trafficRight_L
		lda ADD_R_LH
		sta trafficRight_H

		jsr clearAdder
		lda traffic.yTL,y 
		sta ADD_A_LL
		lda traffic.yLOW,y 
		sta ADD_B_LL
		lda traffic.yHI,y 
		sta ADD_B_LH
		
		lda ADD_R_LL						; Y Top
		sta trafficTop_L
		lda ADD_R_LH
		sta trafficTop_H

		lda traffic.yBR,y					; Y Bottom 
		sta ADD_A_LL
		lda ADD_R_LL
		sta trafficBot_L
		lda ADD_R_LH
		sta trafficBot_H

compHitBoxes:								; we calculate a collision between the player and one of the traffic cars
		lda trafficBot_H					; by testing to see if we miss in all four directions.
		cmp playerTop_H						; if we fall all these tests, we end up with a hit.
		bne _skipLO1						; all tests are 16 bit because we use the full screen.
		lda trafficBot_L					; This first test is to see if traffic is above the player. 
		cmp playerTop_L
_skipLO1:
		bcc nextCollisionTest				; traffic is above player, skip the rest of the test and go to the next car.

		lda playerBot_H						; Check if traffic is below the player
		cmp trafficTop_H
		bne _skipLO2
		lda playerBot_L
		cmp trafficTop_L
_skipLO2:
		bcc nextCollisionTest				; skip if below player and go to the next car

		lda playerRight_H 					; See if traffic is to the right of the player
		cmp trafficLeft_H
		bne _skipLO3
		lda playerRight_L 
		cmp trafficLeft_L 
_skipLO3:
		bcc nextCollisionTest				; Skip if traffic is to the right of the player

		lda trafficRight_H 					; check to see if traffic is to the left of the player
		cmp playerLeft_H 
		bne _skipLO4
		lda trafficRight_L 
		cmp playerLeft_L 
_skipLO4:
		bcc nextCollisionTest				; Skip hit, and got to next car.

		inc hitFlag							; set hitFlag to 1 to stop collision testing until after the next screen update
		inc hit								; we failed the tests above se we have hit another car, increment the hit counter

		lda player.xHI						; now we check if player is right or left of traffic car
		cmp traffic.xHI,y
		bne _skipLO5
		lda player.xLOW
		cmp traffic.xLOW,y
_skipLO5:
		bcc bumpRight						; and we adjust traffic accordingly
		bra bumpLeft
bumpReturn:

		lda player.yHI						; now we check if player is in front or behind traffic car
		cmp traffic.yHI,y 
		bne _skipLO6
		lda player.yLOW
		cmp traffic.yLOW,y 
_skipLO6:
		bcc _bumpDown						; and adjust accordingly.
		bra bumpUP
_bumpDown:
		jmp bumpDown

bumpReturn2:

nextCollisionTest:
		inx									; increment x by one to set test for next traffic car.
		cpx #$06							; have we tested all the cars?
		beq doneCollisionCheck				; yes, but jumping up is too far, so jump down
		jmp collisionTestLoop				; no, jump back up to next test
doneCollisionCheck:
		rts									; we are done with collision check. Either we've gone through all the cars, or we hit one.

bumpLeft:									; left of player hits right of traffic
		clc									; player car shifts 6 pixels right
		lda player.xLOW
		adc #$06
		sta player.xLOW
		lda player.xHI
		adc #$00
		sta player.xHI

		sec									; traffic car shifts 1 pixel left
		lda traffic.xLOW,y 
		sbc #$01
		sta traffic.xLOW,y 
		lda traffic.xHI,y 
		sbc #$00
		sta traffic.xHI,y 
		bra bumpReturn 

bumpRight:									; Right of player hits left of traffic
		sec									; player car shifts 6 pixels left
		lda player.xLOW
		sbc #$06
		sta player.xLOW
		lda player.xHI
		sbc #$00
		sta player.xHI

		clc 								; traffic car shifts 1 pixel right
		lda traffic.xLOW,y 
		adc #$01
		sta traffic.xLOW,y 
		lda traffic.xHI,y 
		adc #$00
		sta traffic.xHI,y 
		bra bumpReturn 

bumpUp:										; player car behind traffic car
		sec									; player car slows down
		lda player.speedLOW
		sbc #$50
		sta player.speedLOW
		lda player.speedHI
		sbc #$00
		bmi skipZero
		stz player.speedLOW
		stz player.speedHI

skipZero:		
		sta player.speedHI
		sec 								; traffic car gets bumped a forward 6 pixels
		lda traffic.yLOW,y 
		sbc #$06
		sta traffic.yLOW,y 
		lda traffic.yHI,y 
		sbc #$00
		sta traffic.yHI,y 
		jmp bumpReturn2

bumpDown:
		clc 								; player car hit from behind
		lda traffic.yLOW,y 					; player car maintains speed
		adc #$06							; traffic car gets bumped back 6 pixels
		sta traffic.yLOW,y 
		lda traffic.yHI,y 
		adc #$00
		sta traffic.yHI,y 
		jmp bumpReturn2


collisionAdd:								; Down and dirty add sequence instead of typing it 4 times.
		stx ADD_A_LL						; takes extra time though
		sty ADD_B_LL
		sta ADD_B_LH
		lda ADD_R_LL
		ldx ADD_R_LH
		rts

checkHits:									; Hit Check stub
		lda hit
		cmp oldHit
		beq skipHits
		sta oldHit
		cmp #$05
		beq lowHits
		cmp #$0a
		beq medHits
		cmp #$10
		beq medHits
		lda hit
		cmp #$1d 
		beq gameOVER
		bra skipHits

medHits:
		dec player.speedTop
		lda player.speedTOP
		sta player.speedHI

lowHits:
		inc smoke
		lda smoke
		dec a
		asl
		asl
		asl
		tay
		lda #$01
		sta VKY_SP6_CTRL,y

skipHits:
		rts

gameOVER:
		inc GO 
		bra skipHits

; ************************************************************************************************************************************

clearAdder:									; Clear the coprocessor adder so we don't get any weird reults.
		stz ADD_A_LL
		stz ADD_A_LH
		stz ADD_A_HL
		stz ADD_A_HH
		stz ADD_B_LL
		stz ADD_B_LH
		stz ADD_B_HL
		stz ADD_B_HH
		rts

clearMultiplier:
		stz MULU_A_L
		stz MULU_A_H
		stz MULU_B_L
		stz MULU_B_H
		rts

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

; ************************************************************************************************************************************		
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
		tax									; transfer to x for indexing
		lda numberText,x					; load the number from the table
		sta $c49a							; and place it on the screen
hitOmeter:
		lda hit
		sta htdIN
		stz htdIN+1
		jsr htd
		lda htdOUT
		lsr
		lsr
		lsr
		lsr
		tax
		lda numberText,x 
		sta $c4ac
		lda htdOUT
		and #$0f 
		tax
		lda numberText,x 
		sta $c4ad

gTimer:
		lda gTimeHI
		sta htdIN
		stz htdIN+1
		jsr htd
		lda htdOUT
		lsr
		lsr
		lsr
		lsr
		tax
		lda numberText,x 
		sta $c014
		lda htdOUT
		and #$0f 
		tax
		lda numberText,x 
		sta $c015

		stz MMU_IO_CTRL						; reset MMU back to zero

		rts 

; ************************************************************************************************************************************		
; Distance travelled

travelDisplay:
		lda #$02
		sta MMU_IO_CTRL


		lda travelHI
		lsr
		lsr
		lsr 
		lsr 
		tax
		lda numberText,x 
		sta $c000 
		lda travelHI
		and #$0f
		tax 
		lda numberText,x 
		sta $c001
		lda travelME
		lsr 
		lsr 
		lsr 
		lsr 
		tax
		lda numberText,x 
		sta $c002
		lda travelME
		and #$0f
		tax 
		lda numberText,x 
		sta $c003
		lda travelLO
		lsr 
		lsr 
		lsr 
		lsr 
		tax
		lda numberText,x 
		sta $c004
		lda travelLO
		and #$0f
		tax 
		lda numberText,x 
		sta $c005

		stz MMU_IO_CTRL						; reset MMU back to zero

		rts 


; ************************************************************************************************************************************		
; Music Player

playNotes:
		inc nCounter				  	;increment the note counter
		stz mTimer						; reset the timer	
		ldx nCounter					; load the note from notes1
		lda notes1,x 		
		cmp #$ff						; look for the end flag
		bne nextNote 					; if not $ff play the note
		sta nCounter					; if it is $ff, set the counter and loop
		bra playNotes	
nextNote:
		asl 							; multiply note by two
		tax								; transfer to x for indexing
		stz SID_L1_GATE					; close gate
		stz SID_R1_GATE
		lda freq,x						; load the frequency lo from the table
		sta SID_L1_FREQ_L				; store in register
		sta SID_R1_FREQ_L				; both sid registers
		lda freq+1,x 					; load frequency hi from the table
		sta SID_L1_FREQ_H				; store in the register
		sta SID_R1_FREQ_H				; both register

		lda #$41 						; triangle wave, open gate This is all temp (maybe)
		sta SID_L1_GATE
		lda #$21						; Pulse wave
		sta SID_R1_GATE
;		rts
melody:
		ldy #$00
		lda ($70),y 
		cmp #SK
		beq mdone1
		cmp #RT
		beq rest2
		cmp #$ff
		bne nextMNote
		lda #<notes2
		sta $70
		lda #>notes2
		sta $71
		bra melody
rest2:
		stz SID_L2_GATE
		bra mdone1
nextMNote:
		asl
		tax
		stz SID_L2_GATE
		lda freq,x 
		sta SID_L2_FREQ_L
		lda freq+1,x 
		sta SID_L2_FREQ_H
		lda #$41
		sta SID_L2_GATE
mdone1:
		clc
		lda $70
		adc #$01
		sta $70
		lda $71
		adc #$00
		sta $71
;		rts

harmony:
		ldy #$00
		lda ($72),y 
		cmp #SK
		beq hdone1
		cmp #RT
		beq rest3
		cmp #$ff
		bne nextHNote
		lda #<notes3
		sta $72
		lda #>notes3
		sta $73
		bra harmony
rest3:
		stz SID_R2_GATE
		bra hdone1
nextHNote:
		asl
		tax
		stz SID_R2_GATE
		lda freq,x 
		sta SID_R2_FREQ_L
		lda freq+1,x 
		sta SID_R2_FREQ_H
		lda #$41
		sta SID_R2_GATE
hdone1:
		clc
		lda $72
		adc #$01
		sta $72
		lda $73
		adc #$00
		sta $73
		rts

; ************************************************************************************************************************************	
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
		.text "0123456789abcdef"

totalColors:	.byte $00					; This is a variable for the y index for the CLUT load routine

htdIN:			.byte $00,$00
htdOUT:			.byte $00,$00

joyX:		.byte $00						; signed X direction
joyY:		.byte $00						; signed Y direction
joyB:		.byte $00						; Button 0 Activated?
roadMove:	.byte $80,$80					; Low/High Road Movement Position

oldHit:		.byte $00
hit:		.byte $00
hitFlag:	.byte $00
smoke:		.byte $00
gTimeLO:	.byte $3c
gTimeHI:	.byte $3c

travelLO:	.byte $00
travelME:	.byte $00
travelHI:	.byte $00

GO:			.byte $01

sm1X:		.byte $00,$00					; xLO,xHI
sm1Y:		.byte $00,$00					; yLO,yHI
sm2X:		.byte $00,$00					; xLO,xHI
sm2Y:		.byte $00,$00					; yLO,yHI
sm3X:		.byte $00,$00					; xLO,xHI
sm3Y:		.byte $00,$00					; yLO,yHI
sm4X:		.byte $00,$00					; xLO,xHI
sm4Y:		.byte $00,$00					; yLO,yHI

sm1T:		.byte $00						; smoke timer
sm2T:		.byte $00						; smoke timer
sm3T:		.byte $00						; smoke timer
sm4T:		.byte $00						; smoke timer

pulse1:		.byte $00,$00

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
		.byte	$00,	22,		 $01,		$00,		204,		$00,		$01,		$07,			$00,		$00,		$08,		9,2,23,30,		$00
player.xFRAC:
		.byte 	$00							; we needed a fixed point decimal for the player car X_POS that the traffic does not need.

carsImage:									; locations of the pixel art for the cars
		.word VWBeetle,Camaro,Toyota4Runner,BMW3,Datsun280Z
		.word Corvair,C10Pickup,Galaxie500,AMCJavelin,JeepCJ5
		.word JeepCJ5,Fiat131,Mustang,Challenger,C10Pickup,BMW3

carsInfo:
		.word Corvair
		.byte $08
		.word Mustang
		.byte $08
		.word Fiat131
		.byte $08
		.word BMW3
		.byte $08

laneData:									; x position for each lane
		;lane       1,      2,      3,      4,      5,      6,shoulder
		.byte $58,$00,$78,$00,$98,$00,$b8,$00,$d8,$00,$f8,$00,$18,$01

hitBox:										; Bounding squares for collsion detection, by car From CarsImage above.
		.byte 9,3,23,30						; VWBeetle  Top Left and Bottom Right offset from position pointer
		.byte 9,2,23,30						; Camaro
		.byte 8,0,24,31						; Toyota4Runner
		.byte 9,2,23,30						; BMW3
		.byte 9,2,23,30						; Datsun280Z
		.byte 9,2,23,30						; Corvair
		.byte 9,0,23,31						; C10Pickup
		.byte 9,1,23,30						; Galaxie500
		.byte 9,2,23,30						; AMCJavelin
		.byte 9,3,26,29						; JeepCJ5
		.byte 9,3,26,29						; JeepCJ5
		.byte 9,3,22,27						; Fiat131
		.byte 9,2,23,30						; Mustang
		.byte 9,2,23,30						; Challenger
		.byte 9,0,23,31						; C10Pickup
		.byte 9,2,23,30						; BMW3


* = $0500

font:
;		.binary "atari_letters.bin"			; The Atari letters
;		.binary "Retro.bin"					; experimental Letters
;		.binary "atari_future.bin"			; retro atari letters (letters missing a line of data)
		.binary "fat_letters.bin"			; Fat letters

; MuSIC Data Starts Here
		.include "notes.s"
		.include "SID_Freq.s"				; table of SID frequencies for music


;music notes
mTimer:	.byte $00							; counter against TOF for timing purposes
mFlag:	.byte $00							; when counter hits a certain number we've hit a 16th note
nCounter:
		.byte $00							; what note in the sequence have we hit\
n2Counter:
		.byte $00
notes1:
		.byte G2,E2,E2,G2,E2,E2,G2,E2,E2,G2,E2,E2,G2,E2,G2,E2
		.byte G2,E2,E2,G2,E2,E2,G2,E2,E2,G2,E2,E2,G2,E2,G2,E2
		.byte C3,A2,A2,C3,A2,A2,C3,A2,A2,C3,A2,A2,C3,A2,C3,A2
		.byte C3,A2,A2,C3,A2,A2,C3,A2,A2,C3,A2,A2,C3,A2,C3,A2
		.byte $ff																	; temp repeat flag

notes2:
		.byte RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT 	; MEASURE 1
		.byte RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT
		.byte RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT
		.byte RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT
		.byte G4,SK,SK,A4,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 5
		.BYTE G4,SK,SK,A4,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,C5,SK
		.byte SK,SK,SK,B4,SK,SK,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte C5,SK,SK,B4,SK,SK,A4,SK,SK,SK,SK,SK,SK,SK,D4,SK
		.byte G4,SK,SK,A4,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte G4,SK,SK,A4,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,B4,SK	; MEASURE 10
		.byte C5,SK,B4,SK,SK,SK,A4,SK,SK,SK,E4,SK,SK,SK,SK,SK
		.byte C5,SK,B4,SK,SK,SK,A4,SK,SK,SK,E4,SK,SK,SK,D4,SK
		.byte G4,SK,SK,B4,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,A4,SK
		.byte G4,SK,A4,SK,SK,SK,E4,SK,SK,SK,SK,SK,A4,SK,B4,SK
		.byte C5,SK,SK,B4,SK,SK,A4,SK,SK,SK,E4,SK,SK,SK,SK,SK	; MEASURE 15
		.byte C5,SK,B4,SK,SK,SK,A4,SK,SK,SK,E4,SK,C4,SK,D4,SK
		.byte G4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,A4,SK
		.byte E4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,D4,SK,SK,SK
		.byte C5,SK,SK,SK,SK,SK,E4,SK,SK,SK,SK,SK,A4,SK,C5,SK
		.byte E4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,A4,SK	; MEASURE 20
		.byte B4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK	;FIRST CHORD HERE
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,B4,SK
		.byte C5,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte G4,SK,SK,A4,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,D4,SK	; MEASURE 25
		.byte G4,SK,SK,A4,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte C5,SK,B4,SK,SK,SK,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte C5,SK,B4,SK,SK,SK,A4,SK,SK,SK,SK,SK,SK,SK,E4,SK
		.byte G4,SK,SK,SK,B4,SK,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK   ;MEASURE 29 START
		.byte G4,SK,B4,SK,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 30
		.byte C5,SK,B4,SK,RT,RT,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte C5,SK,B4,SK,RT,RT,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte G4,SK,B4,SK,RT,RT,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte G4,SK,B4,SK,SK,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte C5,SK,B4,SK,RT,SK,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 35
		.byte C5,SK,B4,SK,A4,SK,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte G4,SK,B4,SK,RT,SK,E4,SK,SK,SK,SK,SK,SK,SK,A4,SK
		.byte G4,SK,A4,SK,RT,SK,E4,SK,SK,SK,SK,SK,SK,SK,B4,SK
		.byte C5,SK,B4,SK,SK,SK,A4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte A4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,RT,SK,SK,SK	; MEASURE 40
		.byte $ff

notes3:
		.byte RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT,RT 	; MEASURE 1
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 5
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 10
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 15
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 20
		.byte G4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,D4,SK
		.byte E4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte RT,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 25
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte D4,SK,SK,SK,G4,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte SK,SK,G4,SK,SK,SK,C4,SK,SK,SK,SK,SK,SK,SK,SK,SK	; MEASURE 30
		.byte A4,SK,RT,SK,E4,SK,E4,SK,SK,SK,SK,SK,SK,SK,RT,SK
		.byte A4,SK,RT,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte D4,SK,F4,SK,E4,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte D4,SK,F4,SK,SK,SK,C4,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte A4,SK,E4,SK,RT,SK,E4,SK,SK,SK,SK,SK,SK,SK,RT,SK	; MEASURE 35
		.byte RT,SK,SK,SK,E4,SK,E4,SK,SK,SK,SK,SK,SK,SK,RT,SK
		.byte RT,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte RT,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK 
		.byte RT,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK
		.byte RT,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK,SK	 ; MEASURE 40
		.byte $ff







;CLUT data		
		.include "RetroRacerCLUT.s"
		.include "RetroRacerTitleCLUT.s"



* = $10000														;Putting the image data in High Memory because it's easier for Vicky to use it than me!

;Image Data Starts here

		.include "RetroRacerSprites.s"
		.include "RetroRacerTileset.s"
		.include "RetroRacerTitleSet.s"
																; We load the tilemap from a .tlm file made by Aseprite as binary data
TileMapData:													;Because the emulator is a little different from the actual I have 2 tilemaps. Only one should be active.
;		.binary "RetroRacerTilemapA.tlm"						;For the emulator version
		.binary "RetroRacerTilemapB.tlm"						;For the actual computer

TitleMapData:

		.binary "RetroRacerTitleMap.tlm"




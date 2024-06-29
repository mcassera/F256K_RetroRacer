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

; Tile Set Registers
VKY_TS1_AD_L	= $d284						; Vicky Tile 1 Image Start Address LOW BYTE
VKY_TS1_AD_M	= $d285						; Vicky Tile 1 Image Start Address MEDIUM BYTE
VKY_TS1_AD_H	= $d286						; Vicky Tile 1 Image Start Address HIGH BYTE

; Tile Map 0 Registers
VKY_TM0_CTRL	= $d200						; Tile Map 0 Control
VKY_TM0_AD_L	= $d201						; Tile Map 0 Start Address LOW BYTE
VKY_TM0_AD_M	= $d202						; Tile Map 0 Start Address MEDIUM BYTE
VKY_TM0_AD_H	= $d203						; Tile Map 0 Start Address HIGH BYTE
VKY_TM0_SZ_X	= $d204						; Tile Map 0 Size X
VKY_TM0_SZ_Y	= $d206						; Tile Map 0 Size Y
VKY_TM0_POS_X_L = $d208						; Tile Map 0 X Position & Scroll LOW BYTE
VKY_TM0_POS_X_H = $d209						; Tile Map 0 X Position & Scroll HIGH BYTE
VKY_TM0_POS_Y_L = $d20a						; Tile Map 0 Y Position & Scroll LOW BYTE
VKY_TM0_POS_Y_H = $d20b						; Tile Map 0 Y Position & Scroll HIGH BYTE

; Tile Map 1 Registers
VKY_TM1_CTRL	= $d20c						; Tile Map 1 Control
VKY_TM1_AD_L	= $d20d						; Tile Map 1 Start Address LOW BYTE
VKY_TM1_AD_M	= $d20e						; Tile Map 1 Start Address MEDIUM BYTE
VKY_TM1_AD_H	= $d20f						; Tile Map 1 Start Address HIGH BYTE
VKY_TM1_SZ_X	= $d210						; Tile Map 1 Size X
VKY_TM1_SZ_Y	= $d212						; Tile Map 1 Size Y
VKY_TM1_POS_X_L = $d214						; Tile Map 1 X Position & Scroll LOW BYTE
VKY_TM1_POS_X_H = $d215						; Tile Map 1 X Position & Scroll HIGH BYTE
VKY_TM1_POS_Y_L = $d216						; Tile Map 1 Y Position & Scroll LOW BYTE
VKY_TM1_POS_Y_H = $d217						; Tile Map 1 Y Position & Scroll HIGH BYTE

; Sprite registers		                    ; we're starting a $0a for cars in case we want something in front of them (explosions or ???)

VKY_SP6_CTRL	= $D930						; Sprite 0a Control Register
VKY_SP6_AD_L   	= $D931						; Sprite 0a Pixel Data Address Register
VKY_SP6_AD_M   	= $D932
VKY_SP6_AD_H   	= $D933
VKY_SP6_POS_X_L = $D934						; Sprite 0a X Position Register
VKY_SP6_POS_X_H = $D935
VKY_SP6_POS_Y_L = $D936						; Sprite 0a X Position Register
VKY_SP6_POS_Y_H = $D937

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

; SID Registers
SID_L1_FREQ_L   = $d400                     ; Left Sid Registers
SID_L1_FREQ_H   = $d401
SID_L1_PULS_L   = $d402
SID_L1_PULS_H   = $d403
SID_L1_GATE     = $d404
SID_L1_ATDL     = $d405
SID_L1_STRL     = $d406

SID_L2_FREQ_L   = $d407
SID_L2_FREQ_H   = $d408
SID_L2_PULS_L   = $d409
SID_L2_PULS_H   = $d40a
SID_L2_GATE     = $d40b
SID_L2_ATDL     = $d40c
SID_L2_STRL     = $d40d

SID_L3_FREQ_L   = $d40e
SID_L3_FREQ_H   = $d40f
SID_L3_PULS_L   = $d410
SID_L3_PULS_H   = $d411
SID_L3_GATE     = $d412
SID_L3_ATDL     = $d413
SID_L3_STRL     = $d414

SID_L_FLT_L     = $d415
SID_L_FLT_H     = $d416
SID_L_RES       = $d417
SID_L_VOL       = $d418

SID_R1_FREQ_L   = $d500                     ; Right Sid Registers
SID_R1_FREQ_H   = $d501
SID_R1_PULS_L   = $d502
SID_R1_PULS_H   = $d503
SID_R1_GATE     = $d504
SID_R1_ATDL     = $d505
SID_R1_STRL     = $d506

SID_R2_FREQ_L   = $d507
SID_R2_FREQ_H   = $d508
SID_R2_PULS_L   = $d509
SID_R2_PULS_H   = $d50a
SID_R2_GATE     = $d50b
SID_R2_ATDL     = $d50c
SID_R2_STRL     = $d50d

SID_R3_FREQ_L   = $d50e
SID_R3_FREQ_H   = $d50f
SID_R3_PULS_L   = $d510
SID_R3_PULS_H   = $d511
SID_R3_GATE     = $d512
SID_R3_ATDL     = $d513
SID_R3_STRL     = $d514

SID_R_FLT_L     = $d515
SID_R_FLT_H     = $d516
SID_R_RES       = $d517
SID_R_VOL       = $d518

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

ADD_A_LL		= $de08
ADD_A_LH		= $de09
ADD_A_HL		= $de0a
ADD_A_HH		= $de0b
ADD_B_LL		= $de0c
ADD_B_LH		= $de0d
ADD_B_HL		= $de0e
ADD_B_HH		= $de0f
ADD_R_LL		= $de18
ADD_R_LH		= $de19
ADD_R_HL		= $de1a
ADD_R_HH		= $de1b

;Random NUmber Generator
Random_Reg		= $d6a6
Random_L		= $d6a4

; Misc Variables for Indirect Indexing
ptr_src			= $80						; A pointer to read data
ptr_dst			= $82						; A pointer to write data

playerLeft_L 	= $90
playerLeft_H 	= $91 
playerRight_L 	= $92 
playerRight_H 	= $93 
playerTop_L 	= $94 
playerTop_H 	= $95 
playerBot_L 	= $96 
playerBot_H 	= $97 

trafficLeft_L 	= $98
trafficLeft_H 	= $99 
trafficRight_L 	= $9a 
trafficRight_H 	= $9b 
trafficTop_L 	= $9c 
trafficTop_H 	= $9d 
trafficBot_L 	= $9e 
trafficBot_H 	= $9f 

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


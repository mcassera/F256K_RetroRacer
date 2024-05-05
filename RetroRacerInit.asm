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


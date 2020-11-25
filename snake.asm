.label R6510 = $0001
.label SCREEN_RAM = $0400
.label CHARSET1_ROM = $D000
.label CHARSET2_ROM = $D800
.label COLOR_RAM = $D800
.label CHAR_MEMORY = $2000

.label CIAPRA = $DC00
.label CIAPRB = $DC01
.label CIACRA = $DC0E

// Zero page Kernel variables
.label LSTX = $c5 	// Matrix coordinate of last key pressed (#64=None)

// SID registers
.label SIDSTART = $D400
.label SIGVOL = $D418
.label FRELO1 = $D400
.label FREHI1 = $D401
.label PWLO1 = $D402
.label PWHI1 = $D403
.label VCREG1 = $D404
.label ATDCY1 = $D405
.label SUREL1 = $D406
.label FRELO2 = $D407
.label FREHI2 = $D408
.label VCREG2 = $D40B
.label ATDCY2 = $D40C

.label UP = $00
.label RIGHT = $01
.label DOWN = $02
.label LEFT = $03

*=$61 "Zeropage" virtual
XLOCATION: .byte $00    // $61
YLOCATION: .byte $00    // $62
TEMP1: .byte $00		// $63
VECTOR4: .word $0000    // $64
SNAKE_INDEX: .byte $00		// $66
GAME_DELAY: .byte $00	// $67 // used to perform the delay loop
TEMP5: .byte $00		// $68
TEMP6: .byte $00		// $69
TEMP7: .byte $00		// $6A
SNAKE_TAIL_INDEX: .byte $00		// $6B

*=$6d "Zeropage" virtual
VECTOR3: .word $0000

*=$8b "Zeropage" virtual
VECTOR1: .word $0000
VECTOR2: .word $0000

*=$1f00 "Variables" virtual
SurroundingContent: .fill 4, 0
//UpperLocationContent: .byte $00 // $1f00 store up location content 
//RightLocationContent: .byte $00 // $1f01 store right location content 
//LowerLocationContent: .byte $00 // $1f02 store down location content 
//LeftLocationContent: .byte $00  // $1f03 store left location content
*=$1f10 "Variables" virtual
TAIL_SCREEN_ADDRESS: .word $0000 // $1f10-$1f11
SKIP_DRAW_TAIL: .byte $00 // $1f12 if $00 then the current snake tail will be drawn, else will be skipped since covered by another body part.
SNAKE_HEAD_OFFSET: .byte $00 // $1f13 used to animate snake head (alternate between $0 and $4 for 2 different chars)
SNAKE_ANIMATION_COUNTER: .byte $00 // $1f14 
SNAKE_VOICE_CTRL_INDEX: .byte $00 // $1f15 index used to modify the voice 2 control register used for snake
EGGS_REMAINING: .byte $00 // $1f16 equal to $01 if some eggs are remaining (else 0)
END_OF_GAME: .byte $00 // $1f17 $01 if we have been eaten up
PLAYER_CURRENT_X: .byte $00 // $1f18  rapport avec PlayerX
PLAYER_CURRENT_Y: .byte $00 // $1f19 rapport avec PlayerY
PLAYER_NEXT_X: .byte $00 // $1f1a rapport avec PlayerX
PLAYER_NEXT_Y: .byte $00 // $1f1b rapport avec PlayerY
GAME_PHASE: .byte $00 // $1f1c $00 if eggs remaining, $01 if no more eggs and we can eat snakes
SNAKES_COUNTER: .byte $00 // $1f1d 
PLAYER_CHAR: .byte $00 // $1f1e

*=$3000 "Snakedata" virtual
SNAKE_DATA:
SNAKE_X: .fill 16, 0		// $3000 X locations of all 15 snakes
SNAKE_Y: .fill 16, 0		// $3010 Y locations of all 15 snakes
SNAKE_LENGTH: .fill 16, 0	// $3020
SNAKE_HEADING: .fill 16, 0	// $3030 All 15 snakes headings (UP,RIGHT,DOWN,LEFT)
.fill 64, 0
SNAKES_DATA_BLOCKS:
SNAKE1_DATA: .fill $80, 0	// Starts at $3080. 128 bytes Block of data for snake 1, divided in 4 * 32 bytes sections.
SNAKE2_DATA: .fill $80, 0
SNAKE3_DATA: .fill $80, 0
SNAKE4_DATA: .fill $80, 0
SNAKE5_DATA: .fill $80, 0
SNAKE6_DATA: .fill $80, 0
SNAKE7_DATA: .fill $80, 0
SNAKE8_DATA: .fill $80, 0
SNAKE9_DATA: .fill $80, 0
SNAKE10_DATA: .fill $80, 0
SNAKE11_DATA: .fill $80, 0
SNAKE12_DATA: .fill $80, 0
SNAKE13_DATA: .fill $80, 0
SNAKE14_DATA: .fill $80, 0
SNAKE15_DATA: .fill $80, 0 // Starts at $3780. 128 bytes Block of data for snake 15

*=$3800 "Colorramcopy" virtual
COLORAM_COPY: .fill 1024, $00	

.label VECTOR1L = VECTOR1
.label VECTOR1H = VECTOR1+1
.label VECTOR2L = VECTOR2
.label VECTOR2H = VECTOR2+1
.label VECTOR3L = VECTOR3
.label VECTOR3H = VECTOR3+1
.label VECTOR4L = VECTOR4
.label VECTOR4H = VECTOR4+1

*=$0801 "Program"

// 100 POKE53280,1:POKE53281,0
.byte $17, $08	// pointer to next basio line stored at $0817
.byte $64, $00	// line number : 100
.byte $97		// basic 'POKE' keyword code
.byte $35, $33, $32, $38, $30, $2c, $31	// "53280,1" petscii encode text
.byte $3a 		// ":" petscii encode text
.byte $97		// basic 'POKE' keyword code
.byte $35, $33, $32, $38, $31, $2c, $30	// "53281,0" petscii encode text
.byte $00		// end of line

// *=$0817
// Mask the upper nibble of VICII chip memory control register and set location of character memory to offset $2000 of
// used VICII bank (0, which is default at $0000-$3FFF). Charset will be located at $2000.
// 110 POKE53272,(PEEK(53272)AND240)OR8 
.byte $33, $08	// pointer to next basic line stored at $0833
.byte $6e, $00	// line number : 110
.byte $97		// basic 'POKE' keyword code
.byte $35, $33, $32, $37, $32, $2c, $28	// "53272,(" petscii encode text
.byte $c2		// basic 'PEEK' keyword code
.byte $28, $35, $33, $32, $37, $32, $29	// "(53272)" petscii encode text
.byte $af		// basic 'AND' keyword code
.byte $32, $34, $30, $29  // "240)" petscii encode text
.byte $b0		// basic 'OR' keyword code
.byte $38		// "8" petscii encode text
.byte $00		// end of line

// *=$0833
// 120 SYS2304
.byte $3d, $08	// pointer to next basic line stored at $083d
.byte $78, $00	// line number : 120
.byte $9e		// basic 'SYS' keyword code
.byte $32, $33, $30, $34 // "2304" petscii encode text
.byte $00		// end of line

.byte $00, $00	// end of basic program

.segmentdef Filler
.fill 193, $AA	// insert TAX instructions (unexecuted code) until machine code start address 

*=$0900
	jmp Main 

HeadingDecisionTable: {
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $03,$03,$03,$03,$03,$03,$03,$03
	.byte $02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$03,$02,$03,$02,$03,$02,$03
	.byte $01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$03,$03
	.byte $01,$01,$01,$01,$01,$01,$02,$02
	.byte $01,$01,$01,$01,$01,$01,$02,$03
}

/**
 * Compute the snake datablock start address (ranging $3080-$3780)
 * (SNAKE_DATA ($3000) + $80 * SnakeIndex) 
 * 
 * Input : SNAKE_INDEX contains the snake index (1..15)
 * Output : VECTOR3 contains the corresponding block start address 
 */
//*=$0943
GetDataBlockForSnake: {
	lda #>SNAKE_DATA
	sta VECTOR3H 	
	lda #<SNAKE_DATA
	sta VECTOR3L	
	lda SNAKE_INDEX		
	// Shift bit 0 of snake index into the carry
	lsr
	// Shift the carry into bit 7 of VECTOR3L. This mean that for even index VECTOR3L = $00, and for odd VECTOR3L = $80  
	// (Note : works only if SNAKE_DATA is at the start of a memory page)
	ror VECTOR3L		
	clc 
	// Add SNAKE_INDEX / 2 to the higher byte (e.g for snake 1, VECTOR3H = #>SNAKE_DATA. For snake 2, VECTOR3H = #>SNAKE_DATA + 1)
	adc VECTOR3H		
	sta VECTOR3H
	rts
}

/**
 * An egg has just been eaten. Play a sound effect and increment score on screen.
 */
 EggEaten: {
	lda #$21
	sta $d404    // voice 1: control register
	ldx #$04
!:	
	inc SCREEN_RAM,x
	lda SCREEN_RAM,x
	cmp #$25
	bne !+
	lda #$1b
	sta SCREEN_RAM,x
	dex 
	bpl !-
!:
	rts 
 }

/**
 * Compute the Screen RAM address for current XLOCATION and YLOCATION.
 * Also set another vector to point inside ($3800..$3BFF) for the same screen coordinates.
 * (Usage of this area still to be determined...)
 * 
 * Input :  XLOCATION contains the column index 
 *          YLOCATION contains the row index 
 *
 * Output : VECTOR1 : the Screen RAM location = $0400 + Y*40 + X
 *          VECTOR2 : the ($3800..$3BFF) location = $3800 + Y*40 + X
 */
//*=$0970
ComputePointersForXY:
	lda #$00
	sta VECTOR1H
	// address offset is Y coordinate * 40 (in Screen Ram)
	// 6510 has no multiplication. So shift left 3 times to get Y*8 and left 5 times to get Y*32
	// then offset equals Y*32 + Y*8 = Y*40
	lda YLOCATION	// load Y coordinate
	asl
	asl
	asl
	tax // X stores Y coordinate * 8
	asl
	rol VECTOR1H
	asl
	rol VECTOR1H
	sta VECTOR1L
	// Y * 32 is now stored in VECTOR1L
	txa
	clc
	// Add Y * 8 to the lower byte
	adc VECTOR1L
	sta VECTOR1L
	bcc !+
	inc VECTOR1H
!:
	clc
	// Now time to add the X coordinate to address pointed to by VECTOR1
	lda XLOCATION // load X coordinate
	adc VECTOR1L
	sta VECTOR1L
	bcc !+
	inc VECTOR1H
!:
	// At this point , VECTOR1 = Y * 40 + X.
	// Let VECTOR1 += $0400
	// and VECTOR2 = $3800 + VECTOR1
	lda VECTOR1L
	sta VECTOR2L
	lda VECTOR1H
	sta VECTOR2H
	clc
	lda #$04		// Cause Screen RAM starts $0400
	adc VECTOR1H
	sta VECTOR1H
	clc
	lda #>COLORAM_COPY
	adc VECTOR2H
	sta VECTOR2H
	rts

UnknownRoutine5: {
	ldx #$00
	lda #$00
	sta SCREEN_RAM,x
	sta SCREEN_RAM+$100,x
	sta SCREEN_RAM+$200,x
	sta SCREEN_RAM+$300,x
	sta COLORAM_COPY,x
	sta COLORAM_COPY+$100,x
	sta COLORAM_COPY+$200,x
	sta COLORAM_COPY+$300,x
	inx 
	bne $09b2
	rts 
}

*=$09ce "SnakeRotationTable"
TurnLeftTable:
   .byte LEFT,UP,RIGHT,DOWN
//*=$09d2 
TurnRightTable:
   .byte RIGHT,DOWN,LEFT,UP
//*=$09d6 
GoAheadTable:
   .byte UP,RIGHT,DOWN,LEFT
//*=$09da 
TurnBackTable:
   .byte DOWN,LEFT,UP,RIGHT
*=$09de "unknownData5"  
   .byte $15,$0c,$11,$09,$0a,$16,$09,$12
   .byte $13,$0b,$15,$0a,$0b,$14,$0c,$16

/**
 * Load a random byte in A.
 * It uses a clock byte to increase a pointer into memory.
 *
 * Input : None 
 * Output : a random byte in A
 */
//*=$09ee
GetRandom: {
	// read the third clock byte that is updated by the kernel.
	// It is updated by the IRQ interrupt that scans the keyboard.
	lda $a2
	adc VECTOR4L
	sta VECTOR4L
	bcc !+
	inc VECTOR4H
!:
	lda VECTOR4H
	cmp #$e0
	bcc !+
	lda #$e0	  // for better randomness, it would be better to set VECTOR4H to 0.
	sta VECTOR4H
!:
	ldy #$00
	lda (VECTOR4),y
	rts 
}

/**
 * Compute the heading of snake for the next game tick.
 * It is done by checking the screen char code of the locations surrounding snake head location.
 * If multiple directions can be taken, a decision table and a random value are used. 
 * 
 * Input:   SNAKE_INDEX [$00..$0f]
 *
 * Output : TEMP5 new heading [UP,RIGHT,DOWN,LEFT]
 *          TEMP6 previous heading [UP,RIGHT,DOWN,LEFT]
 *          XLOCATION snake current X
 *          YLOCATION snake current Y
 */
//*=$0a07
ComputeNewSnakeHeading: {
	// Fill 4 surrounding locations content with $01 (0 means empty, anything else mean not reachable)
	ldx #$00
	lda #$01
!:
	sta SurroundingContent,x
	inx
	cpx #$04
	bne !-

	// Store location of current snake in XLOCATION and YLOCATION
	ldx SNAKE_INDEX
	lda SNAKE_X,x
	sta XLOCATION
	lda SNAKE_Y,x
	sta YLOCATION

CheckUpperLocation: 
	// Is snake on the top border ?
	ldy #$00
	cmp #$02
	// If true, no need to check upper location
	beq CheckLowerLocation
	// If not, compute screen address of the location on top of current one
	dec YLOCATION
	jsr ComputePointersForXY
	inc YLOCATION
	// Load char code of the top location and save it in Upper Location Content
	lda (VECTOR1),y
	sta SurroundingContent + UP
	// If location is empty, then go on with bottom location
	beq CheckLowerLocation
	// If location is the player, then assume location is empty
	// and go on with bottom location
	cmp PLAYER_CHAR
	bne !CheckForEgg+
	lda #$0
	sta SurroundingContent + UP
	beq CheckLowerLocation
!CheckForEgg:
	// If location is an egg, then assume location is empty ONLY if we are the red snake
	cmp #$17
	bne !CheckForSnake+
	lda SNAKE_INDEX
	cmp #$02
	bne CheckLowerLocation
	lda #$0
	sta SurroundingContent + UP
	jmp CheckLowerLocation
!CheckForSnake:
	// If square is neither empty, nor an egg or the player, then it must be a snake.
	// Load color of the top location. If is the same as current snake color, assume location is empty.
	lda (VECTOR2),y
	sec
	sbc SNAKE_INDEX
	// If the top location is an other snake or a wall, then Upper Location Content <> $00
	sta SurroundingContent + UP

CheckLowerLocation:
	// Is snake on the bottom border ?
	lda #$18
	cmp YLOCATION
	// If true, no need to check bottom location
	beq CheckLeftLocation
	// If not, compute screen address of the location on bottom of current one
	inc YLOCATION
	jsr ComputePointersForXY
	dec YLOCATION
	// Load char code of the left location and save it in Lower Location Content
	lda (VECTOR1),y
	sta SurroundingContent + DOWN
	// If location is empty, then go on with left location
	beq CheckLeftLocation
	// If location is the player, then assume location is empty
	// and go on with left location
	cmp PLAYER_CHAR
	bne !CheckForEgg+
	lda #$00
	sta SurroundingContent + DOWN
	beq CheckLeftLocation
!CheckForEgg:
	// If location is an egg, then assume location is empty ONLY if we are the red snake
	cmp #$17
	bne !CheckForSnake+
	lda SNAKE_INDEX
	cmp #$02
	bne CheckLeftLocation
	lda #$00
	sta SurroundingContent + DOWN
	jmp CheckLeftLocation
!CheckForSnake:
	// If square is neither empty, nor an egg or the player, then it must be a snake.
	// Load color of the bottom location. If is same as current snake color, assume location is empty
	lda (VECTOR2),y
	sec
	sbc SNAKE_INDEX
	// If the bottom location is an other snake or a wall, then Lower Location Content <> $00
	sta SurroundingContent + DOWN

CheckLeftLocation:
	// Is snake on the left border ?
	lda XLOCATION
	// If true, no need to check left location
	beq CheckRightLocation
	// If not, compute screen address of the location on left of current one
	dec XLOCATION
	jsr ComputePointersForXY
	inc XLOCATION
	// Load char code of the left location and save it in Left Location Content
	lda (VECTOR1),y
	sta SurroundingContent + LEFT
	// If location is empty, then go on with right location
	beq CheckRightLocation
	// If location is the player, then assume location is empty
	// and go on with right location
	cmp PLAYER_CHAR
	bne !CheckForEgg+
	lda #$00
	sta SurroundingContent + LEFT
	beq CheckRightLocation
!CheckForEgg:
	// If location is an egg, then assume location is empty ONLY if we are the red snake
	cmp #$17
	bne !CheckForSnake+
	lda SNAKE_INDEX
	cmp #$02
	bne CheckRightLocation
	lda #$00
	sta SurroundingContent + LEFT
	jmp CheckRightLocation
!CheckForSnake:
	// If square is neither empty, nor an egg or the player, then it must be a snake.
	// Load color of the left location. If is same as current snake color, assume location is empty
	lda (VECTOR2),y
	sec
	sbc SNAKE_INDEX
	// If the left location is an other snake or a wall, then Left Location Content <> $00
	sta SurroundingContent + LEFT

CheckRightLocation:
	// Is snake on the right border ?
	lda #$27
	cmp XLOCATION
	// If true, no need to check right location
	beq SelectRandomValidMove
	// If not, compute screen address of the location on right of current one
	inc XLOCATION
	jsr ComputePointersForXY
	dec XLOCATION
	// Load char code of the right location and save it in Right Location Content
	lda (VECTOR1),y
	sta SurroundingContent + RIGHT
	// If location is empty, then go on with final code
	beq SelectRandomValidMove
	// If location is the player, then assume location is empty
	// and go on with final code
	cmp PLAYER_CHAR
	bne !CheckForEgg+
	lda #$00
	sta SurroundingContent + RIGHT
	beq SelectRandomValidMove
!CheckForEgg:
	// If location is an egg, then assume location is empty ONLY if we are the red snake
	cmp #$17
	bne !CheckForSnake+
	lda SNAKE_INDEX
	cmp #$02
	bne SelectRandomValidMove
	lda #$00
	sta SurroundingContent + RIGHT
	jmp SelectRandomValidMove
!CheckForSnake:
	// If square is neither empty, nor an egg or the player, then it must be a snake.
	// Load color of the right location. If is same as current snake color, assume location is empty
	lda (VECTOR2),y
	sec
	sbc SNAKE_INDEX
	// If the right location is an other snake or a wall, then Right Location Content <> $00
	sta SurroundingContent + RIGHT

SelectRandomValidMove:
	// First, compute valid moves among ahead, left and right.
	// Use the precomputed turn tables to find out which square is in front of snake, 
	// on the left, and on the right. If these squares are empty, then set bits in 
	// the accumulator accordingly.
	// Turn tables are indexed by the current heading value (0=up, 1=right, 2=down, 3=left)
	lda #$00
	// Save current snake heading in TEMP6. (0=up, 1=right, 2=down, 3=left).
	// It is going to be used as index in the turn tables
	ldx SNAKE_INDEX
	ldy SNAKE_HEADING,x
	sty TEMP6
	// Can the snake can keep on in the same heading ?
	// Find out if the square ahead is the upper, right, lower or left square
	ldx GoAheadTable,y 	
	// Is this square reachable ?
	ldy SurroundingContent,x
	bne CheckSquareOnLeft
	// If so, set bit 5 in accumulator
	lda #$20
CheckSquareOnLeft:
	// Can the snake turn left ?
	ldy TEMP6
	// Find out if the square on the left is the upper, right, lower or left square
	ldx TurnLeftTable,y
	// Is this square reachable ?
	ldy SurroundingContent,x
	bne CheckSquareOnRight
	// If so, set bit 4 in accumulator
	ora #$10
CheckSquareOnRight:
	// Can the snake turn right ?
	ldy TEMP6
	// Find out if the square on the right is the upper, right, lower or left square
	ldx TurnRightTable,y
	// Is this square reachable ?
	ldy SurroundingContent,x
	bne !+
	// If so, set bit 3 in accumulator
	ora #$08
!:
	// At this point, A contains 3 bits, one for each possible heading choice
	//
	// %00111000
	//    ^^^
	//    |||
	//    keep heading
	//     ||
	//     turn left
	//      |
	//      turn right

	sta TEMP7
	// Get a random value in [0..7]
	jsr GetRandom
	and #$07
	clc
	adc TEMP7

	ldy TEMP6

	tax
	// Read the decision table to get the new heading.
    // Random bits are used as index (8 values) for any possible heading combination.
    // If snake cannot keep heading, cannot turn left or right, then turn back (table read 0)
    // If snake can only keep heading, then heading does not change (table read 1)
    // If snake can only turn left, then turn left (table read 2)
    // If snake can only turn right, then turn right (table read 3)
	// If snake can only turn right or left, then turn right 50% of time, left 50% of time (table read 2 or 2)
	// If snake can only keep heading or turn left, then turn left 25% of time (table read 1 or 2)
	// If snake can only keep heading or turn right, then turn right 25% of time (table read 1 or 3)
	// If snake can only keep heading or turn left or right, then turn right 12.5% of time, left 12.5% of time right 
	// (table read 0 (turn back), 1 (go ahead), 2 (turn left) or 3 (turn right))
	lda HeadingDecisionTable,x
	// Table return '0=turn back'
	beq TurnBack
	// Table return '1=go ahead'
	cmp #$01
	beq KeepHeading
	// Table return '2=turn left'
	cmp #$02
	beq TurnLeft
	// Table return '3=turn right'
	lda TurnRightTable,y
	jmp end
TurnBack:
	lda TurnBackTable,y
	jmp end
KeepHeading:
	lda GoAheadTable,y
	jmp end
TurnLeft:
	lda TurnLeftTable,y
end:
	sta TEMP5
	rts
}

/**
 * This routine make all alive snake progress on playfield.
 * A new heading is computed at each call and the head progress in this direction.
 * Finally, the tail is updated to reflect the snake progress.
 */
//*=$0b62
MoveSnakes: {
	// iterator over all 15 snakes
	lda #$0f
	sta SNAKE_INDEX		   
Loop:
	// Get current snake length
	ldx SNAKE_INDEX		   
	lda SNAKE_LENGTH,x  
	// If snake length == 0, it has been eaten. Skip it
	bne !+
	jmp NextSnake
!:

	//////////////////////////
	// Update head location //
	//////////////////////////

	// 1) Compute the new heading of this snake, make it progress one square in this direction
	//    and update is head location (SNAKE_X, SNAKE_Y)
	//
	// Compute new heading of snake, returned in TEMP5. 
	// Current snake location is also returned in XLOCATION and YLOCATION
	jsr ComputeNewSnakeHeading	
	// Load snake data block in VECTOR3 (SNAKE_DATA + $80 * SNAKE_INDEX)
	jsr GetDataBlockForSnake  

	// Update XLOCATION, YLOCATION according to the snake heading
	// Load the current heading of snake ? (0=up, 1=right, 2=down, 3=left)
	lda TEMP5
	// Snake goes up ?
	bne CheckGoRight
	dec YLOCATION
	jmp UpdateSnakeLocation
CheckGoRight:
	cmp #RIGHT
	bne CheckGoDown
	inc XLOCATION
	jmp UpdateSnakeLocation
CheckGoDown:
	cmp #DOWN
	bne GoLeft
	inc YLOCATION
	jmp UpdateSnakeLocation
GoLeft:	
	dec XLOCATION

UpdateSnakeLocation:
	// Store snake color (same as index) into TEMP1. It is required to draw snake squares.
	ldx SNAKE_INDEX
	stx TEMP1
	// Update current snake location with XLOCATION, YLOCATION
	lda XLOCATION
	sta SNAKE_X,x
	lda YLOCATION
	sta SNAKE_Y,x

	// 2) Compute the Screen address for the new head position.
	//    Store this address at the first index of the snake data block (offset $00 for LSB, offset $20 for MSB).
	//    Compute the new head character based of current heading.
	//    Store this character in data block (offset $40) ans draw it at the head Screen location	
	//
	// Load Screen address of the new snake position in VECTOR1
	jsr ComputePointersForXY
    lda VECTOR1L
    // VECTOR3 points to the current snake data block.
    // Store lower byte of Screen address of snake in block, offset 0 
    ldy #$00
    sta (VECTOR3),y
    // Store higher byte of Screen address of snake in block, offset $20 
    ldy #$20
    lda VECTOR1H
    sta (VECTOR3),y
    // Store snake head char code (heading + 1) at offset $40 of block.
    // Get heading.
    ldx TEMP5
    inx 
    txa 
    ldy #$40
    sta (VECTOR3),y
    // Push new head char code onto stack
    pha 
    // Animate snake mouth by adding the animation offset (can be either $0 or $04)
    // Note: at very first call, the offset is $03 (?). That results in a bad head char for only first snake drawing
    adc SNAKE_HEAD_OFFSET
    // Draw the snake head on screen
    // TEMP1 contains snake color (=index)
    // VECTOR1 contains the new snake position
    // A contains the head char code
    jsr SetScreenAtLocation

    // 3) Store the tail character with the same heading as current snake heading in data block (offset $60)
    //    Note: the game seems not to use this value from data block (used for debugging ?)
    // Restore head char code from stack
    pla 
    // Store snake tail char code (head code + $0c) at offset $60 of block.
    clc 
    adc #$0c
    ldy #$60
    sta (VECTOR3),y

  	////////////////////////////
	// Update first body part //
	////////////////////////////

    // Compute the code of the char to be drawn at the previous snake head position and update Screen with this char.
    //
    // It is based on the previous and the current heading.
    // All combinations are stored in a precomputed table.
    // 
    // Compute x register value so that bit 0-1 contain previous heading (TEMP6), bit 2-3 contains new heading (TEMP5)
    // e.g if previous was left (3) and new is down (2), the x = %00001011 = $0B
    //
    // Previous | current  |  x  | char     Previous | current  |  x  | char 
    // ---------+----------+-----+------    ---------+----------+-----+------ 
    //    UP    |    UP    | $00 | $15        RIGHT  |    UP    | $01 | $0C
    //    UP    |  RIGHT   | $04 | $0A        RIGHT  |  RIGHT   | $05 | $16
    //    UP    |   DOWN   | $08 | $13        RIGHT  |   DOWN   | $09 | $0B
    //    UP    |   LEFT   | $0C | $0B        RIGHT  |   LEFT   | $0D | $14
    //
    // Previous | current  |  x  | char     Previous | current  |  x  | char 
    // ---------+----------+-----+------    ---------+----------+-----+------ 
    //   DOWN   |    UP    | $02 | $11        LEFT   |    UP    | $03 | $09
    //   DOWN   |  RIGHT   | $06 | $09        LEFT   |  RIGHT   | $07 | $12
    //   DOWN   |   DOWN   | $0A | $15        LEFT   |   DOWN   | $0B | $0A
    //   DOWN   |   LEFT   | $0E | $0C        LEFT   |   LEFT   | $0F | $16
    lda TEMP5
    asl 
    asl 
    clc 
    adc TEMP6
    tax 
    // Load the precomputed char code
    lda $09de,x
	// Store snake second body char code at offset $41 of block.
	ldy #$41
    sta (VECTOR3),y
    // Save the updated body chart onto the stack
    pha 
    // Get the screen address of the previous snake location (it is initialized at start of each game)
    ldy #$01
    lda (VECTOR3),y
    sta VECTOR1L
    ldy #$21
    lda (VECTOR3),y
    sta VECTOR1H
    // Restore the updated body char from the stack
    pla 
    // and draw the snake body char
    jsr SetScreenAtLocation

    ///////////////////////
    // Clear tail square //
    ///////////////////////

    // If the last body part on snake is a tail character, then it's time to clear this square, 
    // as the snake has progressed.
    //
	// Read the char code of the current indexed snake body square :
    // Initialize the tail index with snake length
    ldx SNAKE_INDEX
    lda SNAKE_LENGTH,x
    // Load screen address of the last snake body part into VECTOR1
    sta SNAKE_TAIL_INDEX
    tay 
    lda (VECTOR3),y
    sta VECTOR1L
    lda SNAKE_TAIL_INDEX
    ora #$20
    tay 
    lda (VECTOR3),y
    sta VECTOR1H
    ldy #$00
    // Then load content of Screen Ram
    // Note: beware ! It points to $0000 for unused body part (it happens in the first frames of a game)
    lda (VECTOR1),y
    // A is loaded with the last body char code.
    // If it is the tail (char codes $0d, $0e, $0f or $10), then clean this square.
    cmp #$0d
    beq TailFound
    cmp #$0e
    beq TailFound
    cmp #$0f
    beq TailFound
    cmp #$10
    bne Label1
TailFound:
	lda #$00
    jsr SetScreenAtLocation

 Label1:
 	// In current databloc, shift screen address of a snake body square from position SNAKE_LENGTH-1 to position SNAKE_LENGTH
 	// Also save the screen location of tail, so that we can check it another body part is overriding it
	dec SNAKE_TAIL_INDEX
    ldy SNAKE_TAIL_INDEX
    lda (VECTOR3),y
    iny 
    sta (VECTOR3),y
    sta TAIL_SCREEN_ADDRESS	// save lower byte of tail screen address
    lda SNAKE_TAIL_INDEX
    ora #$20
    tay 
    lda (VECTOR3),y
    iny 
    sta (VECTOR3),y 	 		// save higher byte of tail screen address
    sta TAIL_SCREEN_ADDRESS+1

    dec SNAKE_TAIL_INDEX
    // Initialize SKIP_DRAW_TAIL with $00 (false)
    lda #$00
    sta SKIP_DRAW_TAIL 			
Loop2:
	// In current databloc, shift location of a body square at position N-1 to position N
	ldy SNAKE_TAIL_INDEX
    lda (VECTOR3),y
    iny 
    sta (VECTOR3),y
    sta VECTOR1L
    lda SNAKE_TAIL_INDEX
    ora #$20
    tay 
    lda (VECTOR3),y
    iny 
    sta (VECTOR3),y
    // If location at position N is the same as the queue, then don't draw tail (SKIP_DRAW_TAIL = $01)
    cmp TAIL_SCREEN_ADDRESS+1
    bne !+
    lda VECTOR1L
    cmp TAIL_SCREEN_ADDRESS
    bne !+
    lda #$01
    sta SKIP_DRAW_TAIL
!:
	// Load head char code at position N-1 and copy to position N
	lda SNAKE_TAIL_INDEX
    ora #$40
    tay 
    lda (VECTOR3),y
    iny 
    sta (VECTOR3),y

	// Load tail char code at position N-1 and copy to position N
    lda SNAKE_TAIL_INDEX
    ora #$60
    tay 
    lda (VECTOR3),y
    iny 
    sta (VECTOR3),y

    // Loop until all snake body parts have been shifted in data block
    dec SNAKE_TAIL_INDEX
    bpl Loop2

    // Check if the end of tail must be drawn
    lda SKIP_DRAW_TAIL
    bne UpdateCurrentHeading
    lda TAIL_SCREEN_ADDRESS
    sta VECTOR1L
    lda TAIL_SCREEN_ADDRESS+1
    sta VECTOR1H
    ldx SNAKE_INDEX
    lda SNAKE_LENGTH,x
    ora #$60
    tay 
    dey 		// Beware, tail char to use is the one stored in table at index SNAKE_LENGTH-1
    lda (VECTOR3),y
    jsr SetScreenAtLocation

UpdateCurrentHeading:
	ldx SNAKE_INDEX
    lda TEMP5
    sta SNAKE_HEADING,x

NextSnake:
	dec SNAKE_INDEX
	// If all snakes processed, then return
	beq End
	jmp Loop

End:
	rts
}

/**
 * Write the screen char code stored in A and the color contained in TEMP1.
 * Location is based on the Screen RAM address stored in VECTOR1.
 *
 * Color is written both to the Color RAM and playfield mirror area ($3800..$3CFF), at
 * the same offset as position in Screen RAM.
 *
 * Note : A is modified (should be saved before call if reused)
 *
 * Input : A  is the Screen charcode
 *         TEMP1 contains the color
 *         VECTOR1 is a vector to Screen RAM
 */
*=$0c9d
SetScreenAtLocation: {
	tax
	// VECTOR1 contains the Screen Location address where to put accumulator
	// Set VECTOR2 to point to $3400 + Screen Location (i.e Screen RAM copy at $3800..$3BFF)
	lda VECTOR1L
	sta VECTOR2L
	clc
	lda VECTOR1H
	adc #(>COLORAM_COPY)-(>SCREEN_RAM)
	sta VECTOR2H
	ldy #$00
	txa
	// Write A in the Screen RAM
	sta (VECTOR1),y
	lda TEMP1
	// Write TEMP1 in the matching ColorRAMCopy location ($3800..$3BFF)
	sta (VECTOR2),y
	// Write also TEMP1 in the matching Color RAM location
	lda #$a0
	adc VECTOR2H
	sta VECTOR2H
	lda TEMP1
	sta (VECTOR2),y
	rts
}

/**
 * A delay loop to slow down the game engine. Speed is tuned with the GAME_DELAY variable.
 * On each call, the snake sound effect is played also.
 *
 * Input : GAME_DELAY (the higher, the slower)
 * Output : None
 */
*=$0cbd
Delay: {
	ldx GAME_DELAY
Loop:
	lda #$fe
!:
	nop 
	nop 
	nop 
	nop 
	asl 
	bcs !-
	dex 
	bne Loop
	jsr SoundRoutine
	// select noise waveform and start release
	lda #$80				
	sta VCREG1    // voice 1: control register
	rts 
}	

/**
 * This routine use the kernel to read current pressed. 
 * It also read joystick in both port.
 * The target square is checked against border.
 *
 * Input :  PLAYER_CURRENT_X contains current player X [02-04]
 *          PLAYER_CURRENT_Y contains current player Y [00-39]
 *
 * Output : PLAYER_NEXT_X contains new player X
 *          PLAYER_NEXT_Y contains new player Y
 */
CheckPlayerControl: {
	lda PLAYER_CURRENT_X
	sta PLAYER_NEXT_X
	lda PLAYER_CURRENT_Y
	sta PLAYER_NEXT_Y
	// Get the current pressed keyboard Key
	lda LSTX
	// F5 pressed ?
	cmp #$06
	beq Up
	// F7 pressed ?
	cmp #$03
	beq Down
	// ',' pressed ?
	cmp #$2f
	beq Left
	// '.' pressed ?
	cmp #$2c
	beq Right
	lda CIAPRA    // Read CIA1 data port register a (for JOY2)
	and CIAPRB    // Read CIA1 data port register b (for JOY1)
	tax 
	// Joystick UP ?
	and #$01
	beq Up
	// Joystick DOWN ?
	txa 
	and #$02
	beq Down
	// Joystick LEFT ?
	txa 
	and #$04
	beq Left
	// Joystick RIGHT ?
	txa 
	and #$08
	beq Right
	rts 
Up:
	lda PLAYER_CURRENT_Y
	cmp #$02
	beq End
	dec PLAYER_NEXT_Y
End:
	rts 

Down:
	lda PLAYER_CURRENT_Y
	cmp #$18
	beq End
	inc PLAYER_NEXT_Y
	rts 

Left:
	lda PLAYER_CURRENT_X
	cmp #$00
	beq End
	dec PLAYER_NEXT_X
	rts 

Right:
	lda PLAYER_CURRENT_X
	cmp #$27
	beq End
	inc PLAYER_NEXT_X
	rts 
}

/**
 * This routine first read the keyboard and joysticks the next expected location.
 * Then check if player has been eaten up at his current location.
 * If alive, animate his face. Clear current location.
 * If new location is empty, redraw player at new location.
 * If new location contains an egg, play sound, update score, and redraw player at new location.
 * Else, we are trying to eat a snake :
 *   If the eaten square is the end of tail AND game is in phase 1, then devour the whole snake
 *   Else, stay at the previous location
 *
 * Input: PLAYER_CURRENT_X
 *        PLAYER_CURRENT_Y
 */
*=$0d39
CheckForPlayerMove: {
	lda END_OF_GAME
	beq !+
	rts 
!:
	// Read keyboard and joysticks to check for player move
	jsr CheckPlayerControl
	// Store player coordinates in XLOCATION and YLOCATION
	lda PLAYER_CURRENT_X
	sta XLOCATION
	lda PLAYER_CURRENT_Y
	sta YLOCATION
	// Load VECTOR1 with current screen address of player char
	jsr ComputePointersForXY
	ldy #$00
	// Get the code of the screen char at player location
	lda (VECTOR1),y
	// Compare with expected player char code. If it differs, then we have been eaten up !
	cmp PLAYER_CHAR
	beq !+
	// We have been eaten up
	lda #$01
	sta END_OF_GAME
	jsr PlayEatenSoundEffect
	rts 
!:
	// Animate player face by swapping char code $19 and $20 (xor on first 2 bits)
	lda PLAYER_CHAR
	eor #$03
	sta PLAYER_CHAR
	// Clear screen at current player postion (char code 0). Don't mind the color in TEMP1 as character is empty.
	lda #$00
	jsr SetScreenAtLocation
	// Load light red color in TEMP1 (player color)
	lda #$0a
	sta TEMP1
	// Load VECTOR1 with screen address of the location the player is about to move to
	lda PLAYER_NEXT_X
	sta XLOCATION
	lda PLAYER_NEXT_Y
	sta YLOCATION
	jsr ComputePointersForXY
	// Read the screen code of the new location 
	ldy #$00
	lda (VECTOR1),y
	// Is the next location an empty one ? If so, update current coordinates and draw player char
	beq UpdatePlayerLocationAndDraw
	// Is the next location an egg ? 
	cmp #$17
	// If so, play sound effect and update score
	beq EatEgg
	// If not, we are trying to eat a snake.
	// Look for current game phase.
	ldx GAME_PHASE
	// At phase 0 we cannot eat snakes.
	beq StayAtCurrentLocation
	// Is the next location a snake tail character ? If, so eat the snake
	cmp #$0d
	beq eatSnake
	cmp #$0e
	beq eatSnake
	cmp #$0f
	beq eatSnake
	cmp #$10
	beq eatSnake
StayAtCurrentLocation:
	// We can't eat the snakes. Don't move.
	// Set next location to current one and load VECTOR1 with screen current player location address. 
	lda PLAYER_CURRENT_X
	sta PLAYER_NEXT_X
	sta XLOCATION
	lda PLAYER_CURRENT_Y
	sta PLAYER_NEXT_Y
	sta YLOCATION
	jsr ComputePointersForXY
	jmp DrawPlayer
EatEgg:
	// If so, play sound effect and update score
	jsr EggEaten
UpdatePlayerLocationAndDraw:
	// Update player current coordinates and draw player char on screen
	lda PLAYER_NEXT_X
	sta PLAYER_CURRENT_X
	lda PLAYER_NEXT_Y
	sta PLAYER_CURRENT_Y
DrawPlayer:
	lda PLAYER_CHAR
	jsr SetScreenAtLocation
	rts

eatSnake:
	// VECTOR2 points to the Color RAM copy location of the next player location.
	lda (VECTOR2),y
	// Store snake color in SNAKE_INDEX (it is also the snake index)
	sta SNAKE_INDEX
	dec SNAKES_COUNTER
	// Load eaten snake data bloc address in VECTOR3
	jsr GetDataBlockForSnake
	// Load eaten snake length in SNAKE_TAIL_INDEX
	ldx SNAKE_INDEX
	lda SNAKE_LENGTH,x
	sta SNAKE_TAIL_INDEX

devourTailLoop:   
	// Get Screen address of the current char of snake (starting at the tail)
	ldy SNAKE_TAIL_INDEX
	// Store current tail screen address in VECTOR1
	// The lower byte is stored at $3080 + $80 * snakeIndex + tail
	lda (VECTOR3),y
	sta VECTOR1L
	// The upper byte is stored at $3080 + $80 * snakeIndex + $20 + tail 
	lda SNAKE_TAIL_INDEX
	ora #$20
	tay 
	lda (VECTOR3),y
	sta VECTOR1H
	// Don't forget to animate player face while he eats the snake
	lda PLAYER_CHAR
	// Swap player character code $19 and $20
	eor #$03
	sta PLAYER_CHAR
	// Draw player at the current tail location
	jsr SetScreenAtLocation
	// Play a sound effect and increment score on screen.
	jsr EggEaten
	// Check if the snake has been entirely swallowed
	dec SNAKE_TAIL_INDEX
	beq wholeSnakeEaten
	// If there is more to eat, wait for a small time, clear the current player position 
	// and loop onto the next tail location
	jsr Delay
	jsr Delay
	lda #$00
	jsr SetScreenAtLocation
	jmp devourTailLoop

wholeSnakeEaten:
	lda #$00
	// Load eaten snake index
	ldx SNAKE_INDEX
	// Set snake length to 0
	sta SNAKE_LENGTH,x
	// Set player location to the last snake location (head).
	// Required since player coordinates are not updated during the devouring
	lda SNAKE_X,x
	sta PLAYER_CURRENT_X
	sta PLAYER_NEXT_X
	lda SNAKE_Y,x
	sta PLAYER_CURRENT_Y
	sta PLAYER_NEXT_Y
	rts 
}

/**
 * Fill the top of Screen RAM with 2 rows of data (title)
 *
 * Input : None (Data stored at GameTitleScreenCodes)
 * Output : None (Screen RAM)
 */
*=$0e24
InitScreenTitle: {
	ldx #$0
!:
	lda GameTitleScreenCodes,x
	sta SCREEN_RAM,x
	lda #$01
	sta COLOR_RAM,x
	inx
	cpx #$50
	bne !-
	rts
}

/**
 * Copy 128 ROM characters (1024 bytes) from $dc00-$dfff to $2400-$27ff.
 * It matches the second part of uppercase & lowercase ROM charset (inverted video).
 *
 * Input : None 
 * Output : None 
 */
//*=$0e37
CopyCharactersRom: {
	// In order to copy characters from ROM, CIA A timer must be stopped so that
	// BASIC interupt handler don't try to read ROM instead of I/O and hang CPU
	// Stop Timer A (clear bit 0 of CIACRA)
	lda CIACRA	
	and #%11111110
	sta CIACRA
	// Clear bit 2 of the I/O port to make Char ROM visible to CPU (Character ROM visible at $D000-$DFFF)
	lda R6510
	and #%11111011
	sta R6510
	ldx #$0
!:
	// Copy 128 ROM characters (1024 bytes) from $dc00-$dfff to $2400-$27ff
	lda CHARSET2_ROM+128*8,x	
	sta CHAR_MEMORY+$400,x
	lda CHARSET2_ROM+128*8+$100,x	
	sta CHAR_MEMORY+$500,x
	lda CHARSET2_ROM+128*8+$200,x	
	sta CHAR_MEMORY+$600,x
	lda CHARSET2_ROM+128*8+$300,x	
	sta CHAR_MEMORY+$700,x
	inx
	bne !-
	// Restore memory overlay (I/O area visible at $D000-$DFFF, Dataset motor Off)
	lda R6510
	ora #%00100100
	sta R6510
	// Restart Timer A (set bit 0 of CIACRA)
	lda CIACRA
	ora #%00000001
	sta CIACRA
	rts
}

/**
 * Update the highscore if current score is higher.
 * Note : both values are taken directly from Screen RAM
 *
 * Input : None (Sreen RAM)
 * Output : None (Sreen RAM)
 */
// *=$0e71
UpdateHighscore: {
	// for each digit of score, compare with match digit of highscore
	ldx #$0
!:
	lda SCREEN_RAM,x
	cmp SCREEN_RAM+$22,x
	// if current score digit is lower, then exit
	bmi	end 	// would be better to use BCC (currentCharscore are unsigned !)
	// if current score digit is hight, then replace highscore
	bne update
	// else if digit are equal, check the next digit, until all 6 are processed
	inx
	cpx #$06
	bne !-
end:
	rts
update:
	// replace highscore directly in screen RAM 
	// (X still contains the first digit index to replace)
	lda SCREEN_RAM,x
	sta SCREEN_RAM+$22,x
	inx
	cpx #$06
	bne update
	rts
}

/**
 * Game main routine.
 *
 * Initialize custom Charset
 * Initialize screen (top title and playfield)
 * Initialize SID
 * Initialize snakes (location, state and heading)
 * Initialize player location
 */
// *=$0e8f
Main: {
	jsr InitCustomCharset	// copy custom charset from program data to $2000
	jsr CopyCharactersRom // copy 128 chars from ROM charset to $2400 
	jsr InitScreenTitle // copy character codes of the top 2 rows of screen

	// Clear the SID by setting all registers to 0
	ldx #$18
	lda #$00
!:
	sta SIDSTART,x	
	dex
	bpl !-

	jsr InitVoices

	ldx #$00
!:
	// Initialize current score and high score in first screen row (columns 0-5 and 34-39)
	// Set 6 characters with code 27 (custom '0') 
	lda #$1b
	sta SCREEN_RAM,x
	sta SCREEN_RAM+$22,x
	inx
	cpx #$06
	bne !-

	// Reset end of game flag
RestartGame:
	lda #$00
	sta END_OF_GAME	
	
	// Set volume to max (15), disable filters, voice 3 ON
	// (note : already done in InitVoices)
	lda #%00001111	
	sta SIGVOL    
	
	// Set game speed to minimum (MAX delay)
	lda #$ff
	sta GAME_DELAY	

	lda #$19	// Initialize player char code to 25 (it will alternate between $19 and $1A)
	sta PLAYER_CHAR	

	jsr UpdateHighscore

	// Reset current score 
	ldx #$05	// 6 characters to write
	lda #$1b 	// screen code 27 (custom '0') 
!:
	sta SCREEN_RAM,x
	dex
	bpl !-

NewGame:
	jsr InitPlayfield

	// We start with 15 snakes
	lda #$0f
	sta SNAKES_COUNTER

	// Reset the eggs presence flag
	lda #$01
	sta EGGS_REMAINING	

	// Game starts in phase 0 (we cannot eat snake)
	lda #$00
	sta GAME_PHASE	

	jsr InitVoices

	// Initialize all snakes data area
	// $3001-$301F X and Y coordinates
	// $3021-$302F States
	// $3031-$303F Heading
	// $3080-$30FF Snake 0 datablock (?)
	// ...
	// $3780-$37FF Snake 14 datablock (?)
	ldx #$00
	lda #$00
!:
	sta SNAKE_DATA,x 	
	sta SNAKE_DATA+$100,x
	sta SNAKE_DATA+$200,x
	sta SNAKE_DATA+$300,x
	sta SNAKE_DATA+$400,x
	sta SNAKE_DATA+$500,x
	sta SNAKE_DATA+$600,x
	sta SNAKE_DATA+$700,x
	inx
	bne !-

	lda #$0f 	// Snakes counter (15 loops)
	sta SNAKE_INDEX		// Current snake index

!:
	// Set the zero page pointer VECTOR3 to point to current snake data
	// Snake index is stored at SNAKE_INDEX. Don't know yet what this areas contains (e.g $3080-$307f)
	// Get the datablock for current snake. Block address is stored in VECTOR3
	// (e.g) for snake 2, (VECTOR3.w)=SNAKE2_DATA (=$3100)
	jsr GetDataBlockForSnake 

	// Fill memory at $3001-$300F with playfield X coordinate of all 15 snakes
	// Fill memory at $3011-$301F with playfield Y coordinate of all 15 snakes
	// Fill memory at $3031-$303F with current direction of snakes (0=up, 1=right, 2=down, 3=left)
	ldx SNAKE_INDEX 	// Load current snake index (starting at 15)
	// All snakes starts heading down
	lda #DOWN
	sta SNAKE_HEADING,x 
	// All snakes starts in with a length of 20
	lda #$14
	sta SNAKE_LENGTH,x 
	// load snake start X coordinate and store as current X
	lda InitialSnakesXLocations,x 
	sta SNAKE_X,x 
	sta XLOCATION		// Variable that stores UpperLeft X corner coordinate of nest to draw
	// load snake start Y coordinate and store as current Y
	lda InitialSnakesYLocations,x 
	sta SNAKE_Y,x 
	sta YLOCATION		// Variable that stores UpperLeft Y corner coordinate of nest to draw

	// Let VECTOR1 point to Screem Memory for current snake position
	jsr ComputePointersForXY

	// Store lower byte of current snake screen address in its DATABLOCK, offset $1 (previous snake position)
	ldy #$01
	lda VECTOR1L
	sta (VECTOR3),y 
	// Store upper byte of snake screen address in its DATABLOCK, offset $21 (previous snake position)
	ldy #$21
	lda VECTOR1H
	sta (VECTOR3),y 

	lda #$00	// char screen code (empty chard)
	jsr SetScreenAtLocation // fill current nest, char 1 (Huu.. TEMP1 not initialized ?? Hopefully, it is 0)

	// X++
	inc XLOCATION
	jsr ComputePointersForXY	
	lda #$00
	jsr SetScreenAtLocation // fill current nest, char 2

	// X--
	// Y++
	dec XLOCATION
	inc YLOCATION
	jsr ComputePointersForXY
	lda #$00
	jsr SetScreenAtLocation // fill current nest, char 3

	// X++
	inc XLOCATION
	jsr ComputePointersForXY
	lda #$00
	jsr SetScreenAtLocation // fill current nest, char 4

	// X--
	// Y++
	dec XLOCATION
	inc YLOCATION
	jsr ComputePointersForXY
	lda #$00
	jsr SetScreenAtLocation // fill current nest, char 5

	// X++
	inc XLOCATION
	jsr ComputePointersForXY
	lda #$00
	jsr SetScreenAtLocation // fill current nest, char 6

	// Loop until all 15 snakes are processed
	dec SNAKE_INDEX
	bne	!-

	lda #$03
	sta SNAKE_HEAD_OFFSET  // Really ? an offset of 3 has no meaning for snake head ! (should be 0 or 4) Btw, it is overriden in MainLoop
	sta SNAKE_ANIMATION_COUNTER

	// Initialize snake voice control index
	lda #$14
	sta SNAKE_VOICE_CTRL_INDEX

	lda #$20
	sta XLOCATION 	// X=32 Player Start X
	sta PLAYER_CURRENT_X
	sta PLAYER_NEXT_X

	lda #$11
	sta YLOCATION 	// Y=17 Player Start Y
	sta PLAYER_CURRENT_Y
	sta PLAYER_NEXT_Y

	lda #$0a					// Load player color (light red)
	sta TEMP1
	jsr ComputePointersForXY  	// Put Screen Location of player in VECTOR1
	lda PLAYER_CHAR				// Load current player char code
	jsr SetScreenAtLocation 	// Set player in Screen RAM, Color RAM & RAM Copy 

MainLoop:
	jsr MoveSnakes

	// Compute current snake animation offset ($00=opened mouth, $04=closed)
	lda #$00
    sta SNAKE_HEAD_OFFSET
    dec SNAKE_ANIMATION_COUNTER
    bne !+
    lda #$03
    sta SNAKE_ANIMATION_COUNTER
    lda #$04
    sta SNAKE_HEAD_OFFSET
!:
    jsr Delay
    jsr Delay
    jsr Delay

    jsr CheckForPlayerMove
    // Change game phase and speed if there are no more eggs
    jsr CheckRemainingEggs

    jsr Delay
    jsr Delay
    jsr Delay

    lda SNAKES_COUNTER
    beq NoMoreSnakes

    // If game has ended (eaten up), then wait for 's' key pressed and restart
    lda END_OF_GAME
    beq MainLoop

    // Player has been eaten up. While 's' is not pressed, keep on animating snakes
    // Load last key pressed (updated by kernel)
    lda LSTX	 
    // 's' key pressed ?
    cmp #$0d 	 
    // if not pressed, loop on MainLoop so that snakes can keep on moving
    bne MainLoop
    // else, start a new game
    jmp RestartGame

NoMoreSnakes:
	// turn sound off
    lda #$00
    sta $d418    // select filter mode and volume
WaitForKey:			
    // 's' key pressed ?
    // read last Key pressed
    lda LSTX	
    cmp #$0d
    bne WaitForKey

    jmp NewGame
    jmp MainLoop	// Dead code ???
}

/**
 * Fill Screen RAM (except for the two first rows) with char code $17 (egg).
 * Fill Color RAM (except for the two first rows) with color green.
 * Fill Color RAM Copy (except for the first row) with color green.
 *
 * Input :  None
 * Output : None
 */
//*=$0ffa
InitPlayfield:
	ldx #$00
!:
	// Load Screen code of the 'egg' character
	lda #$17
	sta SCREEN_RAM+$50,x
	sta SCREEN_RAM+$100,x
	sta SCREEN_RAM+$200,x
	sta SCREEN_RAM+$2e8,x
	// Load color of egg characters (green)
	lda #$05
	// Put green in the Color RAM copy
	sta COLORAM_COPY+$28,x 	
	sta COLORAM_COPY+$100,x
	sta COLORAM_COPY+$200,x
	sta COLORAM_COPY+$300,x
	// Put green in color ram for whole playfield (rows 2-24)
	sta COLOR_RAM+$50,x
	sta COLOR_RAM+$100,x
	sta COLOR_RAM+$200,x
	sta COLOR_RAM+$300,x
	inx
	bne !-
	rts

/**
 * Scan the screen to find if at least one egg is remaining.
 * If there are no more eggs, increase game phase (snake eating capability) and game speed.
 *
 * Note : this routine is very suboptimal (we could increase EGGS_REMAINING without accumulator and exit loop directly)
 */
// *=$1028
CheckRemainingEggs: {
	ldx #$00
	// Nothing to do if we already know there are no more eggs
	lda EGGS_REMAINING
	bne !+
	rts 
!:
	// Scan whole playfield, looking for a char with the 'egg' screen code
	stx EGGS_REMAINING
Loop:
	lda SCREEN_RAM,x
	cmp #$17
	bne !+
	lda #$01
	sta EGGS_REMAINING
!:
	lda SCREEN_RAM+$100,x
	cmp #$17
	bne !+
	lda #$01
	sta EGGS_REMAINING
!:
	lda SCREEN_RAM+$200,x
	cmp #$17
	bne !+
	lda #$01
	sta EGGS_REMAINING
!:
	lda SCREEN_RAM+$2e8,x
	cmp #$17
	bne !+
	lda #$01
	sta EGGS_REMAINING
!:
	inx 
	bne Loop
	
	// If there are no more eggs, increase game speed by lowering delay loop time. Also enter game phase 1.
	lda EGGS_REMAINING
	bne !+
	sec 
	lda GAME_DELAY
	sbc #$20
	sta GAME_DELAY
	inc GAME_PHASE
!:
	rts 		
}

/**
 * Snake voice control routine. It is called after the delay loop. 
 * It modifies the content of Voice 2 control register based on precomputed values and an index decreasing at each call.
 */
SoundRoutine: {
	// decrement table index. It ranges in [$1a..$01]
	dec SNAKE_VOICE_CTRL_INDEX	
	bne !+
	lda #$1a
	sta SNAKE_VOICE_CTRL_INDEX
!:
	ldx SNAKE_VOICE_CTRL_INDEX
	lda VoiceCtrlData,x
	sta VCREG2   
	rts 
}

VoiceCtrlData: {
	.byte $81,$81,$81,$81,$81,$81,$81,$81
	.byte $81,$81,$81,$81,$81,$81,$81,$81
	.byte $81,$81,$81,$81,$81,$81,$81,$81
	.byte $00,$00,$00,$00
}

// *=$10a6
InitialSnakesXLocations:
   .byte $00 					// Unused
   .byte $03,    $13			// Snake white, red
   .byte     $0b,$13,$1b,$23	// Snake cyan, purple, green, blue
   .byte $03,$0b,    $1b,$23    // Snake yellow, orange, brown, light red
   .byte $03,$0b,$13,$1b,$23    // Snake dark grey, grey, light green , light blue, light grey

// *=$10b6
InitialSnakesYLocations:
   .byte $00  					// Unused
   .byte $04,    $0c 			// Snake white, Snake red
   .byte     $04,$04,$04,$04    // Snake cyan, purple, green, blue
   .byte $0c,$0c,    $0c,$0c    // Snake yellow, orange, brown, light red
   .byte $14,$14,$14,$14,$14    // Snake dark grey, grey, light green , light blue, light grey

// *=$10c6
// 80 screen characters codes that defines the 2 top lines of screen in CHAR mode
GameTitleScreenCodes:
	.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
    .byte $a0,$a0,$d3,$a0,$ce,$a0,$c1,$a0
    .byte $cb,$a0,$c5,$a0,$ad,$a0,$d0,$a0
    .byte $c9,$a0,$d4,$a0,$a0,$a0,$a0,$a0
    .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
    .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
    .byte $a0,$a0,$82,$99,$a0,$cd,$89,$8b
    .byte $85,$a0,$d3,$89,$8e,$87,$8c,$85
    .byte $94,$8f,$8e,$a0,$a0,$a0,$a0,$a0
    .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0

// *=$1116
InitVoices: {
	// set volume to max (15), disable filters, voice 3 ON
	lda #%00001111	
	sta SIGVOL    
	// VOICE 1 settings
	// set frequency to $089c (C-3, ~129Hz on PAL)
	lda #$08
	sta FREHI1   
	lda #$9c
	sta FRELO1 
	// set release = 0 (6ms), sustain = 15 (full volume) 
	lda #$f0
	sta SUREL1  
	// select sawtooth waveform  
	lda #$20
	sta VCREG1  
	// set decay = 0 (6ms), attack = 4 (38ms)  
	lda #$40
	sta ATDCY1   
	// VOICE 2 settings
	// set frequency to $ffff (Max freq ~3848Hz on PAL)
	lda #$ff
	sta FREHI2    
	sta FRELO2    
	// select noise waveform
	lda #$80
	sta VCREG2    
	// set decay = 11 (2.4s), attack = 4 (38ms)
	lda #$4b
	sta ATDCY2    // voice 2: attack / decay cycle control
	rts 
}

// *=$1147
PlayEatenSoundEffect: {
	// VOICE 1 settings
	// set frequency to $03ff (~60Hz on PAL)
	lda #$03
	sta FREHI1  
	lda #$ff
	sta FRELO1
	// set pulse width to 18.75% ($0300/$0fff)
	lda #$03
	sta PWHI1    
	// set decay = 0 (6ms), attack = 0 (2ms)  
	lda #$00
	sta ATDCY1    
	lda #$f0
	// set release = 0 (6ms), sustain = 15 (full volume) 
	sta SUREL1   
	lda #$41
	// select pulse waveform & gate On (start Attack/Sustain/Release)
	sta VCREG1   
	rts 
}

// *=$1166
// *=$1166
InitCustomCharset: {
	ldx #$0
!:
	lda CharacterSet,x
	sta CHAR_MEMORY,x
	lda CharacterSet+$100,x
	sta CHAR_MEMORY+$100,x
	dex
	bne !-
	rts
}

// *=$1178
CharacterSet:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$46,$46,$4e,$4e,$5a,$5a,$7e
    .byte $00,$fe,$9e,$f8,$e0,$80,$fe,$00
    .byte $7e,$5a,$5a,$72,$72,$62,$62,$00
    .byte $00,$7f,$79,$1f,$07,$01,$7f,$00
    .byte $00,$58,$58,$5c,$5e,$4a,$7a,$7e
    .byte $00,$f0,$98,$fe,$de,$c0,$fe,$00
    .byte $7e,$5e,$52,$7a,$3a,$1a,$1a,$00
    .byte $00,$0f,$19,$7f,$7b,$03,$7f,$00
    .byte $3c,$3e,$1f,$1f,$0f,$03,$00,$00
    .byte $00,$00,$03,$0f,$1f,$1f,$3e,$3c
    .byte $00,$00,$c0,$f0,$f8,$f8,$7c,$3c
    .byte $3c,$7c,$f8,$f8,$f0,$c0,$00,$00
    .byte $3c,$3c,$3c,$18,$18,$18,$18,$00
    .byte $00,$00,$07,$7f,$7f,$07,$00,$00
    .byte $00,$18,$18,$18,$18,$3c,$3c,$3c
    .byte $00,$00,$e0,$fe,$fe,$e0,$00,$00
    .byte $3c,$5e,$ef,$f7,$e7,$ff,$7e,$3c
    .byte $3c,$7e,$fd,$eb,$e7,$ff,$7e,$3c
    .byte $3c,$7e,$ff,$e7,$ef,$f7,$7a,$3c
    .byte $3c,$7e,$ff,$e7,$d7,$bf,$7e,$3c
    .byte $3c,$3c,$3c,$3c,$3c,$3c,$3c,$3c
    .byte $00,$00,$ff,$ff,$ff,$ff,$00,$00
    .byte $00,$3c,$7e,$7e,$7e,$7e,$3c,$00
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $7e,$99,$99,$ff,$bd,$c3,$ff,$7e
    .byte $7e,$99,$ff,$c3,$81,$c3,$e7,$7e
    .byte $c3,$bd,$bd,$bd,$bd,$bd,$c3,$ff
    .byte $f7,$e7,$d7,$f7,$f7,$f7,$c1,$ff
    .byte $c3,$bd,$fd,$f3,$cf,$bf,$81,$ff
    .byte $c3,$bd,$fd,$e3,$fd,$bd,$c3,$ff
    .byte $fb,$f3,$eb,$db,$81,$fb,$fb,$ff
    .byte $81,$bf,$87,$fb,$fd,$bb,$c7,$ff
    .byte $e3,$df,$bf,$83,$bd,$bd,$c3,$ff
    .byte $81,$bd,$fb,$f7,$ef,$ef,$ef,$ff
    .byte $c3,$bd,$bd,$c3,$bd,$bd,$c3,$ff
    .byte $c3,$bd,$bd,$c1,$fd,$fb,$c7,$00
    .byte $ff,$60,$62,$a7


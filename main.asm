*=$0801

// 666 SYS2080 SCI 666
.byte $13, $08, $9A, $02	// next line address ($0813) and line number = $029A = 666
.byte $9E, $32, $30, $38, $30, $20	// SYS basic command, then "2080 "
.byte $53, $43, $49, $20, $36, $36, $36 // "SCI 666"
.byte $00, $00, $00

.fill 46, $EA	// insert NOP instructions until first

Start:
	lda #$00
	sta $d020	// black border
	lda #$0b
	sta $d021	// dark grey background

	sei			// disable maskable IRQs

	lda #$34	// RAM visible in all three areas
	sta $01

	ldx #$d2
loop1:
	// we will copy :
	// $0886-$0957 to $00fa-$01cb 
	// $0958-$09c9 to $0334-$03A5
	lda $0885,x
	sta.a $00f9,x
	cpx #$73		// branch if x is >= #$73
	bcs !+

	lda $0957,x
	sta $0333,x
!:
	dex
	bne loop1		// loop until X=0

	dex				// initialize stack register. SP=$FF
	txs

.break
	ldy #$0
loop3:
	dec $fd			// ($fd) is initially 12, copied from $0889 ()
	dec irqLabel+2
loop2:
	lda ($fc),y		// ($fc) is initially $29, ($fd) is initially $12, y starts at 0
irqLabel:
	sta $ffff,y		// this copies $1129-$1228 to $feff-$fffe (seems to be charset)
	dey
	bne loop2

	ldx $fd
	cpx #$08
	bne loop3
	stx $2e
	iny
	sty $2d
	jmp $100

Unknown1:
	beq !+
	and #$12
!:
	ldy #$f7
	ldx #$03
	jsr $0334  // subroutine copied frol $0958


	rts
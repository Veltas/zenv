; vim:set syntax=z80:

; Current data space pointer
	HEADER h_, "H", 0
	DW create_code
h_:
	DW h_init


; Points to most recently defined symbol
	HEADER sym_last, "SYM-LAST", 0
	DW create_code
sym_last:
	DW sym_last_init


; non-zero while compiling
	HEADER state, "STATE", 0
	DW create_code
state:
	DW 0


	HEADER frames, "FRAMES", 0
	DW create_code
frames:
	DW 0
	DW 0


	HEADER t_attr, "T-ATTR", 0
	DW create_code
t_attr:
	DB t_attr_init


	HEADER t_col, "T-COL", 0
	DW create_code
t_col:
	DB 0


	HEADER t_row, "T-ROW", 0
	DW create_code
t_row:
	DB 0


	HEADER display_file, "DISPLAY-FILE", 0
	DW constant_code
display_file:
	DW display_file_val


	HEADER display_size, "DISPLAY-SIZE", 0
	DW constant_code
display_size:
	DW display_size_val


	HEADER attr_file, "ATTR-FILE", 0
	DW constant_code
attr_file:
	DW attr_file_val


	HEADER attr_size, "ATTR-SIZE", 0
	DW constant_code
attr_size:
	DW attr_size_val


	HEADER tick_int, "'INT", 0
	DW create_code
tick_int:
	DW int-2


	; Default interrupt handler
	; : INT
	HEADER int, "INT", 0
	DW colon_code
int:
	; 1. FRAMES D+! \ increment FRAMES
	DX two_literal_raw-2
	DW 0
	DW 1
	DX frames-2
	DX d_plus_store-2
	; \ KSCAN \ TODO
;	DX kscan-2
	; ;
	DB exit_tok


	; \ Get name string from symbol header address
	; ( sym-addr ) : >SYM ( c-addr n+ )
	HEADER to_sym, ">SYM", 0
	DW colon_code
to_sym:
	; CELL+ CELL+  DUP C@ $7F AND  SWAP CHAR+ SWAP;
	DX cell_plus-2
	DX cell_plus-2
	DB dup_tok
	DB c_fetch_tok
	DB c_literal_tok
	DB 0x7F
	DB and_tok
	DB swap_tok
	DB one_plus_tok
	DB swap_tok
	DB exit_tok


	; \ Type all word names in dictionary to the output device
	; : WORDS
	HEADER words, "WORDS", 0
	DW colon_code
words:
	; SYM-LAST @
	DX sym_last-2
	DB fetch_tok
	; BEGIN
.begin:
	;         \ Print name
	;         DUP >SYM TYPE  BL EMIT
	DB dup_tok
	DX to_sym-2
	DX type-2
	DX bl-2
	DX emit-2
	;         \ Goto next symbol
	;         @
	DB fetch_tok
	; DUP 0= UNTIL DROP
	DB dup_tok
	DB zero_equals_tok
	DB until_raw_tok
	DB .begin-$+256
	DB drop_tok
	; CR ;
	DX cr-2
	DB exit_tok


	; : MAIN
	HEADER main, "MAIN", 0
	DW colon_code
main:
	; \ Clear screen
	; PAGE
	DX page-2
	; \ Display defined words
	; WORDS CR
	DX words-2

	; \ Endlessly get input and print periods
	; BEGIN EKEY DROP '.' EMIT 0 UNTIL
.begin:
	DX ekey-2
	DB drop_tok
	DB c_literal_tok
	DB '.'
	DX emit-2
	DB zero_literal_tok
	DB until_raw_tok
	DB .begin-$+256
	DB exit_tok ; will never be reached


	HEADER zero_literal, "0", 0
	DW $ + 2
zero_literal:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	LD BC, 0
	PUSH BC
	JP next


	HEADER one_literal, "1", 0
	DW $ + 2
one_literal:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	LD BC, 1
	PUSH BC
	JP next


	HEADER literal_raw, "(LITERAL)", 0
	DW $ + 2
literal_raw:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	LD E, (IY+0)
	LD D, (IY+1)
	INC IY
	INC IY
	PUSH DE
	JP next


	; : SCROLL
	HEADER scroll, "SCROLL", 0
	DW colon_code
scroll:
	; T-ROW C@ 8 -  0 MAX  T-ROW C!
	DX t_row-2
	DB c_fetch_tok
	DB c_literal_tok
	DB 8
	DB minus_tok
	DB zero_literal_tok
	DX max-2
	DX t_row-2
	DB c_store_tok
	; DISPLAY-FILE 2048 +  DISPLAY-FILE  4096  CMOVE
	DB literal_raw_tok
	DW display_file_val + 2048
	DX display_file-2
	DB literal_raw_tok
	DW 4096
	DX cmove-2
	; ATTR-FILE 256 +  ATTR-FILE  512  CMOVE
	DB literal_raw_tok
	DW attr_file_val + 256
	DX attr_file-2
	DB literal_raw_tok
	DW 512
	DX cmove-2
	; DISPLAY-FILE 4096 +  2048  ERASE
	DB literal_raw_tok
	DW display_file_val + 4096
	DB literal_raw_tok
	DW 2048
	DX erase-2
	; ATTR-FILE 512 +  256  T-ATTR C@  FILL
	DB literal_raw_tok
	DW attr_file_val + 512
	DB literal_raw_tok
	DW 256
	DX t_attr-2
	DB c_fetch_tok
	DX fill-2
	; EXIT
	DB exit_tok


	; : CR
	HEADER cr, "CR", 0
	DW colon_code
cr:
	; T-ROW C@ 22 > IF SCROLL THEN 1 T-ROW C+!
	DX t_row-2
	DB c_fetch_tok
	DB c_literal_tok
	DB 22
	DB greater_than_tok
	DB if_raw_tok
	DB .cr__if_skip-$-1
	DX scroll-2
.cr__if_skip:
	DB one_literal_tok
	DX t_row-2
	DB c_plus_store_tok
	; 0 T-COL C!
	DB zero_literal_tok
	DX t_col-2
	DB c_store_tok
	; ;
	DB exit_tok


	HEADER emit, "EMIT", 0
	DW $ + 2
emit:
	IF CHECKED
		CALL dat_holds_1
	ENDIF

	IF NARROW_FONT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; C is character
	POP BC
	LD A, C
	CP 0x20
	JR C, .emit__other_char
	CP 0x80
	JR NC, .emit__other_char

	LD A, (t_col)
	CP 64
	JR C, .emit__no_wrap
	PUSH BC
	LD DE, cr
	LD HL, colon_code
	CALL asm_call
	POP BC
.emit__no_wrap:

	LD A, C
	AND 0x7E
	ADD A, A
	LD L, A
	LD H, 0
	ADD HL, HL
	LD DE, font - 4*32
	ADD HL, DE
	; Push glyph pair address
	PUSH HL

	LD A, (t_row)
	LD H, A
	AND 0x7
	RRCA
	RRCA
	RRCA
	LD L, A
	LD A, H
	AND 0x18
	ADD A, display_file_val/256
	LD H, A
	; B is t_col
	LD A, (t_col)
	LD B, A
	OR A
	RRA
	ADD A, L
	LD L, A
	; Push write address
	PUSH HL

	; Calculate if glyph shift needed (set high bit)
	LD A, B
	XOR C
	AND 1
	RRCA
	LD C, A

	; Calculate post-shift glyph mask in B
	BIT 0, B
	LD A, 0xF
	JR NZ, .emit__odd_col
	CPL
.emit__odd_col:
	LD B, A

	; Pop write address
	POP HL
	; Pop font address
	POP DE

.emit__loop:
	; Clear glyph scanline
	LD A, B
	CPL
	AND (HL)
	LD (HL), A
	; Load glyph scanline
	LD A, (DE)
	BIT 7, C
	JR Z, .emit__no_shift
	RLCA
	RLCA
	RLCA
	RLCA
.emit__no_shift:
	AND B
	; Apply glyph to scanline
	OR (HL)
	LD (HL), A
	; Next
	INC DE
	INC H
	INC C
	BIT 3, C
	JR Z, .emit__loop

	LD HL, t_col
	INC (HL)
	JP next


.emit__other_char:
	CP 10

	; Run CR if 10
	LD DE, cr
	LD HL, colon_code
	CALL Z, asm_call

	JP next

	ELSE ; !NARROW_FONT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	POP BC
	LD A, C
	SUB 0x20
	CP 0x7F - 0x20
	JR NC, .non_print
	CP 0x60 - 0x20
	JR Z, .backtick
	ADD A, A
	LD L, A
	LD H, 0
	ADD HL, HL
	ADD HL, HL
	LD BC, font
	ADD HL, BC
	; DE = glyph address
	EX DE, HL
.print_glyph:
	LD A, (t_col)
	CP 32
	JR NC, .next_line
	LD C, A
	LD B, 0
.next_line_done:
	LD A, (t_row)
	LD H, A
	AND 0x7
	RRCA
	RRCA
	RRCA
	LD L, A
	LD A, H
	AND 0x18
	OR 0x40
	LD H, A
	ADD HL, BC
	LD B, 8
.draw_loop:
	LD A, (DE)
	LD (HL), A
	INC DE
	INC H
	DJNZ .draw_loop
	LD HL, t_col
	INC (HL)
	JP next

.next_line:
	PUSH DE
	LD DE, cr
	LD HL, colon_code
	CALL asm_call
	POP DE
	LD BC, 0
	JR .next_line_done

.non_print:
	CP 0x7F - 0x20
	JR Z, .gbp

	CP 0x20 - 0x20
	LD DE, cr
	LD HL, colon_code
	CALL Z, asm_call

	JP next

.gbp:
	LD DE, font + (0x60-0x20)*8
	JR .print_glyph
.backtick:
	LD DE, .backtick_font
	JR .print_glyph

.backtick_font:
	DB %00000000
	DB %00010000
	DB %00001000
	DB %00000000
	DB %00000000
	DB %00000000
	DB %00000000
	DB %00000000

	ENDIF ; NARROW_FONT or not ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	HEADER bye, "BYE", 0
	DW $ + 2
bye:
	RST 0x00


	HEADER store, "!", 0
	DW $ + 2
store:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP BC
	LD (HL), C
	INC HL
	LD (HL), B
	JP next


	; \ Set border to attr
	; ( attr ) : BRDR!
	HEADER brdr_store, "BRDR!", 0
	DW colon_code
brdr_store:
	; 7 AND  ULA P@  0xF8 AND  OR  ULA P! ;
	DB c_literal_tok
	DB 7
	DB and_tok
	DX ula-2
	DX p_fetch-2
	DB c_literal_tok
	DB 0xF8
	DB and_tok
	DB or_tok
	DX ula-2
	DX p_store-2
	DB exit_tok


	; \ Clear screen, reset terminal to top-left
	; : PAGE
	HEADER page, "PAGE", 0
	DW colon_code
page:
	; \ Match border to T-ATTR
	; T-ATTR C@  3 RSHIFT  BRDR!
	DX t_attr-2
	DB c_fetch_tok
	DB c_literal_tok
	DB 3
	DB rshift_tok
	DX brdr_store-2
	; \ Reset terminal col/row
	; 0 0 AT-XY
	DB zero_literal_tok
	DB zero_literal_tok
	DX at_xy-2
	; \ Erase bitmap
	; DISPLAY-FILE DISPLAY-SIZE ERASE
	DX display_file-2
	DX display_size-2
	DX erase-2
	; \ Set attr region to current T-ATTR
	; ATTR-FILE ATTR-SIZE T-ATTR C@ FILL
	DX attr_file-2
	DX attr_size-2
	DX t_attr-2
	DB c_fetch_tok
	DX fill-2
	; ;
	DB exit_tok


	HEADER plus, "+", 0
	DW $ + 2
plus:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP BC
	ADD HL, BC
	PUSH HL
	JP next


	HEADER minus, "-", 0
	DW $ + 2
minus:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP BC
	POP HL
	OR A
	SBC HL, BC
	PUSH HL
	JP next


	HEADER zero_equals, "0=", 0
	DW $ + 2
zero_equals:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	LD A, L
	OR H
	JR NZ, .zero_equals__not_equal
	DEC HL
	PUSH HL
	JP next
.zero_equals__not_equal:
	LD HL, 0
	PUSH HL
	JP next


	HEADER zero_less, "0<", 0
	DW $ + 2
zero_less:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP AF
	AND 0x80
	JR Z, .zero_less__non_negative
	LD HL, 0xFF
	PUSH HL
	JP next
.zero_less__non_negative:
	LD H, A
	LD L, A
	PUSH HL
	JP next


	HEADER one_plus, "1+", 0
	DW $ + 2
one_plus:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	INC HL
	PUSH HL
	JP next


	HEADER char_plus, "CHAR+", 0
	DW one_plus - 2
char_plus:


	HEADER one_minus, "1-", 0
	DW $ + 2
one_minus:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	DEC HL
	PUSH HL
	JP next


	HEADER two_store, "2!", 0
	DW $ + 2
two_store:
	IF CHECKED
		CALL dat_holds_3
	ENDIF
	POP HL
	POP BC
	POP DE
	LD (HL), C
	INC HL
	LD (HL), B
	INC HL
	LD (HL), E
	INC HL
	LD (HL), D
	JP next


	HEADER two_star, "2*", 0
	DW $ + 2
two_star:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	SLA L
	RL H
	PUSH HL
	JP next


	HEADER cells, "CELLS", 0
	DW two_star
cells:


	HEADER exit, "EXIT", 0
	DW $ + 2
exit:
	IF CHECKED
		CALL ret_holds_1
	ENDIF
	LD L, (IX+0)
	LD H, (IX+1)
	INC IX
	INC IX
	PUSH HL
	POP IY
	JP next


	HEADER two_slash, "2/", 0
	DW $ + 2
two_slash:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP DE
	SRA D
	RR E
	PUSH DE
	JP next


	HEADER two_fetch, "2@", 0
	DW $ + 2
two_fetch:
	IF CHECKED
		CALL dat_holds_1_room_1
	ENDIF
	POP HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	INC HL
	LD C, (HL)
	INC HL
	LD B, (HL)
	PUSH BC
	PUSH DE
	JP next


	HEADER two_drop, "2DROP", 0
	DW $ + 2
two_drop:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP HL
	JP next


	HEADER two_dup, "2DUP", 0
	DW $ + 2
two_dup:
	IF CHECKED
		CALL dat_holds_2_room_2
	ENDIF
	POP DE
	POP BC
	PUSH BC
	PUSH DE
	PUSH BC
	PUSH DE
	JP next


	HEADER two_over, "2OVER", 0
	DW $ + 2
two_over:
	IF CHECKED
		CALL dat_holds_4_room_2
	ENDIF
	LD HL, 4
	ADD HL, SP
	LD C, (HL)
	INC HL
	LD B, (HL)
	INC HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	PUSH BC
	JP next


	HEADER to_r, ">R", 0
	DW $ + 2
to_r:
	IF CHECKED
		CALL dat_holds_1_ret_room_1
	ENDIF
	POP BC
	DEC IX
	DEC IX
	LD (IX+0), C
	LD (IX+1), B
	JP next


	HEADER question_dup, "?DUP", 0
	DW $ + 2
question_dup:
	IF CHECKED
		CALL dat_holds_1_room_1
	ENDIF

	POP DE
	PUSH DE
	LD A, E
	OR D
	JR Z, .question_dup__end
	PUSH DE
.question_dup__end:
	JP next


	HEADER abort, "ABORT", 0
	DW $ + 2
abort:
	LD SP, param_stack_top
	LD IX, return_stack_top
	JP quit


	HEADER abs, "ABS", 0
	DW $ + 2
abs:
	POP BC
	LD A, B
	AND 0x80
	JP NZ, .abs__neg
	PUSH BC
	JP next
.abs__neg:
	LD HL, 0
	OR A
	SBC HL, BC
	PUSH HL
	JP next


	HEADER align, "ALIGN", 1
	DW next
align:


	HEADER aligned, "ALIGNED", 1
	DW next
aligned:


	HEADER chars, "CHARS", 1
	DW next
chars:


	HEADER allot, "ALLOT", 0
	DW $ + 2
allot:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP DE
	LD HL, (h_)
	ADD HL, DE
	LD (h_), HL
	JP next


	HEADER and, "AND", 0
	DW $ + 2
and:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP BC
	POP DE
	LD A, B
	AND D
	LD B, A
	LD A, C
	AND E
	LD C, A
	PUSH BC
	JP next


	HEADER or, "OR", 0
	DW $ + 2
or:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP BC
	POP DE
	LD A, B
	OR D
	LD B, A
	LD A, C
	OR E
	LD C, A
	PUSH BC
	JP next


	HEADER xor, "XOR", 0
	DW $ + 2
xor:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP BC
	POP DE
	LD A, B
	XOR D
	LD B, A
	LD A, C
	XOR E
	LD C, A
	PUSH BC
	JP next


	HEADER bl, "BL", 0
	DW constant_code
bl:
	DW ' '


	HEADER c_store, "C!", 0
	DW $ + 2
c_store:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP DE
	LD (HL), E
	JP next


	HEADER c_comma, "C,", 0
	DW $ + 2
c_comma:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP DE
	LD HL, (h_)
	LD (HL), E
	INC HL
	LD (h_), HL
	JP next


	HEADER c_fetch, "C@", 0
	DW $ + 2
c_fetch:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	LD E, (HL)
	LD D, 0
	PUSH DE
	JP next


	HEADER cell_plus, "CELL+", 0
	DW $ + 2
cell_plus:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	INC HL
	INC HL
	PUSH HL
	JP next


	HEADER count, "COUNT", 0
	DW $ + 2
count:
	IF CHECKED
		CALL dat_holds_1_room_1
	ENDIF
	POP HL
	LD E, (HL)
	INC HL
	LD D, 0
	PUSH DE
	PUSH HL
	JP next


	HEADER depth, "DEPTH", 0
	DW colon_code
depth:
	DX s_zero-2
	DX tick_s-2
	DB minus_tok
	DB two_slash_tok
	DB exit_tok


	HEADER drop, "DROP", 0
	DW $ + 2
drop:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	JP next


	HEADER dup, "DUP", 0
	DW $ + 2
dup:
	IF CHECKED
		CALL dat_holds_1_room_1
	ENDIF
	POP HL
	PUSH HL
	PUSH HL
	JP next


	; : MAX 2DUP < IF SWAP THEN DROP ;
	HEADER max, "MAX", 0
	DW colon_code
max:
	DB two_dup_tok
	DB less_than_tok
	DB if_raw_tok
	DB .skip-$-1
	DB swap_tok
.skip:
	DB drop_tok
	DB exit_tok


	HEADER cmove, "CMOVE", 0
	DW $ + 2
cmove:
	IF CHECKED
		CALL dat_holds_3
	ENDIF
	POP BC
	POP DE
	POP HL
	LD A, C
	OR B
	JP Z, next
	LDIR
	JP next


	HEADER cmove_up, "CMOVE>", 0
	DW $ + 2
cmove_up:
	IF CHECKED
		CALL dat_holds_3
	ENDIF
	POP BC
	POP HL
	POP DE
	LD A, C
	OR B
	JP Z, next
	ADD HL, BC
	DEC HL
	EX DE, HL
	ADD HL, BC
	DEC HL
	LDDR
	JP next


	HEADER move, "MOVE", 0
	DW colon_code
move:
	; : MOVE -ROT 2DUP < IF ROT CMOVE> ELSE ROT CMOVE THEN ;
	DB minus_rot_tok
	DB two_dup_tok
	DB less_than_tok
	DB if_raw_tok
	DB .move__else-$-1
	DB rot_tok
	DX cmove_up-2
	DB else_skip_tok
	DB .move__else_skip-$-1
.move__else:
	DB rot_tok
	DX cmove-2
.move__else_skip:
	DB exit_tok


	; : NEGATE  0 SWAP - ;
	HEADER negate, "NEGATE", 0
	DW colon_code
negate:
	DB zero_literal_tok
	DB swap_tok
	DB minus_tok
	DB exit_tok


	HEADER question_do_raw, "(?DO)", 0
	DW $ + 2
question_do_raw:
	IF CHECKED
		CALL ret_room_2
	ENDIF
	INC IY
	; BC = iterator
	POP BC
	; DE = limit
	POP DE
	LD L, E
	LD H, D
	OR A
	SBC HL, BC
	JR Z, .question_do_raw__jump

	; Push limit,iterator to ret stack
	DEC IX
	DEC IX
	LD (IX+0), E
	LD (IX+1), D
	DEC IX
	DEC IX
	LD (IX+0), C
	LD (IX+1), B

	JP next
	
	; If iterator=limit skip loop
.question_do_raw__jump:
	LD E, (IY-1)
	LD D, 0
	ADD IY, DE
	JP next


	HEADER loop_raw, "(LOOP)", 0
	DW $ + 2
loop_raw:
	; DE = iterator
	LD E, (IX+0)
	LD D, (IX+1)
	; HL = limit
	LD L, (IX+2)
	LD H, (IX+3)
	INC DE
	OR A
	SBC HL, DE
	JR NZ, .loop_raw__loop
	LD BC, 4
	ADD IX, BC
	INC IY
	JP next
.loop_raw__loop:
	LD (IX+0), E
	LD (IX+1), D
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
	JP next


	; : TYPE 0 ?DO DUP C@ EMIT 1+ LOOP DROP ;
	HEADER type, "TYPE", 0
	DW colon_code
type:
	DB zero_literal_tok
	DB question_do_raw_tok
	DB .type__skip-$-1
.type__loop:
	DB dup_tok
	DB c_fetch_tok
	DX emit-2
	DB one_plus_tok
	DB loop_raw_tok
	DB .type__loop-$+256
.type__skip:
	DB drop_tok
	DB exit_tok


	HEADER s_quote_raw, '(S")', 0
	DW $ + 2
s_quote_raw:
	IF CHECKED
		CALL dat_room_2
	ENDIF
	PUSH IY
	POP HL
	LD E, (HL)
	LD D, 0
	INC HL
	PUSH HL ; addr
	PUSH DE ; length
	INC DE
	ADD IY, DE
	JP next


	HEADER over, "OVER", 0
	DW $ + 2
over:
	IF CHECKED
		CALL dat_holds_2_room_1
	ENDIF
	LD HL, 2
	ADD HL, SP
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	JP next


	HEADER reset_stacks, "RESET-STACKS", 0
	DW $ + 2
reset_stacks:
	LD IX, return_stack_top
	LD SP, param_stack_top
	JP next


	HEADER quit, "QUIT", 0
	DW colon_code
quit:
	; : QUIT  BEGIN -' IF NUMBER ELSE EXECUTE THEN 0 UNTIL ;
	DX reset_stacks-2
	; DW interpret-2
	DB exit_tok


	; : ERASE 0 FILL ;
	HEADER erase, "ERASE", 0
	DW colon_code
erase:
	DB zero_literal_tok
	DX fill-2
	DB exit_tok


	HEADER fill, "FILL", 0
	DW $ + 2
fill:
	IF CHECKED
		CALL dat_holds_3
	ENDIF
	POP DE
	POP BC
	POP HL
	LD A, B
	OR C
	JR Z, .exit
	LD (HL), E
	DEC BC
	LD A, B
	OR C
	JP Z, .exit
	LD E, L
	LD D, H
	INC DE
	LDIR
.exit:
	JP next


	HEADER fetch, "@", 0
	DW $ + 2
fetch:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	JP next


	HEADER tick_s, "'S", 0
	DW $ + 2
tick_s:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	LD HL, 0
	ADD HL, SP
	PUSH HL
	JP next


	HEADER rot, "ROT", 0
	DW $ + 2
rot:
	IF CHECKED
		CALL dat_holds_3
	ENDIF
	POP HL
	POP DE
	POP BC
	PUSH DE
	PUSH HL
	PUSH BC
	JP next


	HEADER swap, "SWAP", 0
	DW $ + 2
swap:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP DE
	PUSH HL
	PUSH DE
	JP next


	HEADER s_zero, "S0", 0
	DW constant_code
s_zero:
	DW param_stack_top


	HEADER to_in, ">IN", 0
	DW constant_code
to_in:
	; TODO


	HEADER minus_tick, "-'", 0
	DW $ + 2
minus_tick:
	; TODO


	HEADER minus_rot, "-ROT", 0
	DW $ + 2
minus_rot:
	IF CHECKED
		CALL dat_holds_3
	ENDIF
	POP BC
	POP DE
	POP HL
	PUSH BC
	PUSH HL
	PUSH DE
	JP next


	HEADER less_than, "<", 0
	DW $ + 2
less_than:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP DE
	LD BC, 0x8000
	ADD HL, BC
	EX DE, HL
	ADD HL, BC
	OR A
	SBC HL, DE
	LD B, C
	JR C, .less_than__lt
	PUSH BC
	JP next
.less_than__lt:
	DEC BC
	PUSH BC
	JP next


	; : > SWAP < ;
	HEADER greater_than, ">", 0
	DW colon_code
greater_than:
	DB swap_tok
	DB less_than_tok
	DB exit_tok


	HEADER nip, "NIP", 0
	DW $ + 2
nip:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP DE
	PUSH HL
	JP next


	HEADER else_skip, "(ELSE)", 0
	DW $ + 2
else_skip:
	INC IY
	LD E, (IY-1)
	LD D, 0
	ADD IY, DE
	JP next


	HEADER repeat_raw, "(REPEAT)", 0
	DW $ + 2
repeat_raw:
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
	JP next


	HEADER c_plus_store, "C+!", 0
	DW $ + 2
c_plus_store:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP BC
	LD A, (HL)
	ADD A, C
	LD (HL), A
	JP next


	HEADER c_literal, "C-LITERAL", 0
	DW $ + 2
c_literal:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	INC IY
	LD E, (IY-1)
	LD D, 0
	PUSH DE
	JP next


	HEADER if_raw, "(IF)", 0
	DW $ + 2
if_raw:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	INC IY
	POP DE
	LD A, E
	OR D
	JR NZ, .if_raw__end
	LD C, (IY-1)
	LD B, 0
	ADD IY, BC
.if_raw__end:
	JP next


	HEADER ms, "MS", 0
	DW $ + 2
ms:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP DE
.ms__ms_loop:
	LD A, E
	OR D
	JP Z, next
	; waste time
	LD BC, (0x8000)
	; waste time
	LD B, (HL)
	LD BC, 132

.ms__loop:
	DEC BC
	LD A, C
	OR B
	JR NZ, .ms__loop

	DEC DE
	JR .ms__ms_loop


	; \ P@ ( addr -- cx ) \ Read byte from port
	HEADER p_fetch, "P@", 0
	DW $ + 2
p_fetch:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP BC
	IN L, (C)
	LD H, 0
	PUSH HL
	JP next


	; \ P! ( cx addr -- ) \ Write byte to port
	HEADER p_store, "P!", 0
	DW $ + 2
p_store:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP BC
	POP DE
	OUT (C), E
	JP next


	HEADER ula, "ULA", 0
	DW constant_code
ula:
	DW ula_val


	HEADER halt_, "HALT", 0
	DW $ + 2
halt_:
	HALT
	JP next


	; : D+! ( n addr -- ) \ double in addr incremented by 2
	HEADER d_plus_store, "D+!", 0
	DW colon_code
d_plus_store:
	; DUP >R 2@ D+ R> 2! ;
	DB dup_tok
	DB to_r_tok
	DX two_fetch-2
	DX d_plus-2
	DB r_from_tok
	DX two_store-2
	DB exit_tok


	HEADER d_plus, "D+", 0
	DW $ + 2
d_plus:
	IF CHECKED
		CALL dat_holds_4
	ENDIF
	POP AF
	POP BC
	POP DE
	POP HL
	PUSH AF
	ADD HL, BC
	POP BC
	PUSH HL
	EX DE, HL
	ADC HL, BC
	PUSH HL
	JP next


	; : (2LITERAL)  R>  DUP 2 CELLS + >R  2@  ;
	HEADER two_literal_raw, "(2LITERAL)", 0
	DW colon_code
two_literal_raw:
	DB r_from_tok
	DB dup_tok
	DB c_literal_tok
	DB 2
	DX cells-2
	DB plus_tok
	DB to_r_tok
	DX two_fetch-2
	DB exit_tok


	HEADER r_from, "R>", 0
	DW $ + 2
r_from:
	IF CHECKED
		CALL dat_room_1_ret_holds_1
	ENDIF
	LD E, (IX+0)
	LD D, (IX+1)
	INC IX
	INC IX
	PUSH DE
	JP next


	HEADER r_fetch, "R@", 0
	DW $ + 2
r_fetch:
	IF CHECKED
		CALL dat_room_1_ret_holds_1
	ENDIF
	LD E, (IX+0)
	LD D, (IX+1)
	PUSH DE
	JP next


	HEADER _di, "DI", 0
	DW $ + 2
_di:
	DI
	JP next


	HEADER _ei, "EI", 0
	DW $ + 2
_ei:
	EI
	JP next


	; AT-XY ( x y -- ) \ Set next terminal x,y position
	HEADER at_xy, "AT-XY", 0
	DW colon_code
at_xy:
	; ( y ) 0 23 CLAMP T-ROW C!
	DB zero_literal_tok
	DB c_literal_tok
	DB 23
	DX clamp-2
	DX t_row-2
	DB c_store_tok
	; ( x ) 0 63 CLAMP T-COL C!
	DB zero_literal_tok
	DB c_literal_tok
	DB t_width-1
	DX clamp-2
	DX t_col-2
	DB c_store_tok
	; ;
	DB exit_tok


	HEADER keyq_len, "KEYQ-LEN", 0
	DW constant_code
keyq_len:
	DW keyq_len_val


	; keyq items have two parts:
	; byte 1 = scancode which is 8*(4-bit)+(8-hrow_bit)
	;          (this gives an index into Key Table (a) in ROM disassembly)
	; byte 2 = flags: 1=up, 2=shift, 4=sym
	HEADER keyq, "KEYQ", 0
	DW create_code
keyq:
	DS keyq_len_val * 2


	HEADER keyq_s, "KEYQ-S", 0
	DW create_code
keyq_s:
	DB 0


	HEADER keyq_e, "KEYQ-E", 0
	DW create_code
keyq_e:
	DB 0


	; : EKEY? ( -- flags ) \ Is a key event available?
	HEADER ekey_question, "EKEY?", 0
	DW colon_code
ekey_question:
	; KEYQ-E C@ KEYQ-S C@ <> ;
	DX keyq_e-2
	DB c_fetch_tok
	DX keyq_s-2
	DB c_fetch_tok
	DB not_equals_tok
	DB exit_tok


	; 0<> ( n -- flags ) \ true if n is not equal to 0
	HEADER zero_not_equals, "0<>", 0
	DW colon_code
zero_not_equals:
	; 0= INVERT ;
	DB zero_equals_tok
	DB invert_tok
	DB exit_tok


	; <> ( n1 n2 -- flags ) \ true if n1 is not equal to n2
	HEADER not_equals, "<>", 0
	DW colon_code
not_equals:
	; = INVERT ;
	DB equals_tok
	DB invert_tok
	DB exit_tok


	; CLAMP ( n1 n2 n3 -- n ) \ Force n1 to range [n2, n3]
	HEADER clamp, "CLAMP", 0
	DW colon_code
clamp:
	; ROT MIN MAX ;
	DB rot_tok
	DX min-2
	DX max-2
	DB exit_tok


	; : MIN ( n1 n2 -- n ) \ Leave the smaller of n1 and n2
	HEADER min, "MIN", 0
	DW colon_code
min:
	; 2DUP > IF SWAP THEN DROP ;
	DB two_dup_tok
	DB greater_than_tok
	DB if_raw_tok
	DB .skip-$-1
	DB swap_tok
.skip:
	DB drop_tok
	DB exit_tok


	; : INVERT ( x -- y ) \ Invert all bits of x
	HEADER invert, "INVERT", 0
	DW $ + 2
invert:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	LD A, L
	CPL
	LD L, A
	LD A, H
	CPL
	LD H, A
	PUSH HL
	JP next


	; EKEY ( -- x ) \ Push keyboard event when ready
	HEADER ekey, "EKEY", 0
	DW colon_code
ekey:
	; BEGIN EKEY? HALT UNTIL \ Block for event
.begin:
	DX ekey_question-2
	DX halt_-2
	DB until_raw_tok
	DB .begin-$+256
	; KEYQ KEYQ-S C@ CELLS + @ \ Receive event
	DX keyq-2
	DX keyq_s-2
	DB c_fetch_tok
	DB two_star_tok
	DB plus_tok
	DB fetch_tok
	; KEYQ-S C@ 1+ 7 AND KEYQ-S C! \ Increment KEYQ-S (mod 8)
	DX keyq_s-2
	DB c_fetch_tok
	DB one_plus_tok
	DB c_literal_tok
	DB 7
	DB and_tok
	DX keyq_s-2
	DB c_store_tok
	; ;
	DB exit_tok


	HEADER until_raw, "(UNTIL)", 0
	DW $ + 2
until_raw:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	LD A, L
	OR H
	JR Z, .loop
	INC IY
	JP next
.loop:
	LD C, (IY+0)
	LD B, 0xFF
	ADD IY, BC
	JP next


	HEADER equals, "=", 0
	DW $ + 2
equals:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP BC
	XOR A
	SBC HL, BC
	JR Z, .equal
	LD L, A
	LD H, A
	PUSH HL
	JP next
.equal:
	DEC HL
	PUSH HL
	JP next


	HEADER less_than_or_equal, "<=", 0
	DW colon_code
less_than_or_equal:
	DB greater_than_tok
	DB invert_tok
	DB exit_tok


	HEADER greater_than_or_equal, ">=", 0
	DW colon_code
greater_than_or_equal:
	DB less_than_tok
	DB invert_tok
	DB exit_tok


	; CODE 2>R
	HEADER two_to_r, "2>R", 0
	DW $ + 2
two_to_r:
	IF CHECKED
		CALL dat_holds_2_ret_room_2
	ENDIF
	POP HL
	POP DE
	LD BC, -4
	ADD IX, BC
	LD (IX+0), L
	LD (IX+1), H
	LD (IX+2), E
	LD (IX+3), D
	JP next


	; VARIABLE KSHIFT-STATE
	HEADER kshift_state, "KSHIFT-STATE", 0
	DW create_code
kshift_state:
	DW 0


	; : KSCAN \ Update keyboard state
	HEADER kscan, "KSCAN", 0
	DW colon_code
kscan:
	; \ Get state of shift keys for high byte of event
	; $FEFE P@ 1 AND  $7FFE P@ 2 AND  OR  8 LSHIFT  KSHIFT-STATE !
	DB literal_raw_tok
	DW 0xFEFE
	DX p_fetch-2
	DB one_literal_tok
	DB and_tok
	DB literal_raw_tok
	DW 0x7FFE
	DX p_fetch-2
	DB c_literal_tok
	DB 2
	DB and_tok
	DB or_tok
	DB c_literal_tok
	DB 8
	DB lshift_tok
	DX kshift_state
	; \ Call per-row word
	; 8 0 DO I KSCAN-ROW LOOP ;
	DB c_literal_tok
	DB 8
	DB zero_literal_tok
	DB two_to_r_tok
.do:
	DB r_fetch_tok
	DX kscan_row-2
	DB loop_raw_tok
	DB .do-$+256
	DB exit_tok


	; ( n ) : KSCAN-ROW \ Scan+update a given half-row
	HEADER kscan_row, "KSCAN-ROW", 0
	DW colon_code
kscan_row:
	; \ Read row state
	; ( n ) 1  OVER LSHIFT  INVERT  8 LSHIFT ULA OR  P@  $1F AND
	DB one_literal_tok
	DB over_tok
	DB lshift_tok
	DB invert_tok
	DB c_literal_tok
	DB 8
	DB lshift_tok
	DX ula-2
	DB or_tok
	DX p_fetch-2
	DB c_literal_tok
	DB 0x1F
	DB and_tok
	; \ Calculate state address
	; ( n state ) OVER CHARS KSTATE +
	DB over_tok
	DX kstate-2
	DB plus_tok
	; \ Update when state changes
	; ( n state addr ) 2DUP C@ = IF
	DB two_dup_tok
	DB c_fetch_tok
	DB equals_tok
	DB if_raw_tok
	DB .then-$-1
	; THEN
.then:


	; \ Stores scanned key bits from the last scan
	; CREATE KSTATE  8 CHARS  ALLOT
	; KSTATE  8 CHARS  ERASE
	HEADER kstate, "KSTATE", 0
	DW create_code
kstate:
	DS 8


	; \ Stores last key press
	; CREATE KLAST  0 C,
	HEADER klast, "KLAST", 0
	DW create_code
klast:
	DB 0


	; CODE LSHIFT ( x u -- x<<u )
	HEADER lshift, "LSHIFT", 0
	DW $ + 2
lshift:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP DE
	POP HL
	XOR A
	OR E
	JR Z, .finish
	CP 16
	JR NC, .zero
	LD B, E
.loop:
	ADD HL, HL
	DJNZ .loop
.finish:
	PUSH HL
	JP next
.zero:
	LD HL, 0
	JR .finish


	; CODE RSHIFT ( x u -- x>>u )
	HEADER rshift, "RSHIFT", 0
	DW $ + 2
rshift:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP DE
	POP HL
	XOR A
	OR E
	JR Z, .finish
	CP 16
	JR NC, .zero
	LD B, E
.loop:
	SRL H
	RR L
	DJNZ .loop
.finish:
	PUSH HL
	JP next
.zero:
	LD HL, 0
	JR .finish


repeat_wait_init: EQU 45  ; 0.9s
repeat_repeat_init: EQU 5 ; 0.1s
	; Used to time waiting for repeats and repeating, cleared on every new
	; key event.
repeat_timer:
	DB 0

unshifted_key_map:
	; Port 0xFEFE
	DB 0x80, 'z', 'x', 'c', 'v'
	; Port 0xFDFE
	DB 'a', 's', 'd', 'f', 'g'
	; Port 0xFBFE
	DB 'q', 'w', 'e', 'r', 't'
	; Port 0xF7FE
	DB '1', '2', '3', '4', '5'
	; Port 0xEFFE
	DB '0', '9', '8', '7', '6'
	; Port 0xDFFE
	DB 'p', 'o', 'i', 'u', 'y'
	; Port 0xBFFE
	DB '\n', 'l', 'k', 'j', 'h'
	; Port 0x7FFE
	DB ' ', 0x90, 'm', 'n', 'b'

key_up:        EQU 0x11 ; ASCII DC1
key_left:      EQU 0x12 ; ASCII DC2
key_down:      EQU 0x13 ; ASCII DC3
key_right:     EQU 0x14 ; ASCII DC4
key_caps_lock: EQU 0x1C ; ASCII File separator

caps_key_map:
	DB 0x80, 'Z', 'X', 'C', 'V'
	DB 'A', 'S', 'D', 'F', 'G'
	DB 'Q', 'W', 'E', 'R', 'T'
	DB '1', key_caps_lock, '3', '4', key_left
	DB '\b', '9', key_right, key_up, key_down
	DB 'P', 'O', 'I', 'U', 'Y'
	DB '\n', 'L', 'K', 'J', 'H'
	DB ' ', 0x90, 'M', 'N', 'B'

symb_key_map:
	DB 0x80, 'z', 'x', 'c', 'v'
	DB 'a', 's', 'd', 'f', 'g'
	DB 'q', 'w', 'e', 'r', 't'
	DB '1', '2', '3', '4', '5'
	DB '0', '9', '8', '7', '6'
	DB 'p', 'o', 'i', 'u', 'y'
	DB '\n', 'l', 'k', 'j', 'h'
	DB ' ', 0x90, 'm', 'n', 'b'

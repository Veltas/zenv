; vim:set syntax=z80:

; Current data space pointer
	HEADER h_, "H", 0
	DW create_code
h_:
	DW h_init


	; : HERE ( -- addr ) \ Get current address of dictionary end
	HEADER here, "HERE", 0
	DW colon_code
here:
	; H @ ;
	DX h_-2
	DT fetch
	DT exit


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


	HEADER t_w, "T-W", 0
	DW constant_code
t_w:
	DW t_width


	HEADER disp_file, "DISP-FILE", 0
	DW constant_code
disp_file:
	DW disp_file_val


	HEADER disp_size, "DISP-SIZE", 0
	DW constant_code
disp_size:
	DW disp_size_val


	HEADER attr_file, "ATTR-FILE", 0
	DW constant_code
attr_file:
	DW attr_file_val


	HEADER attr_size, "ATTR-SIZE", 0
	DW constant_code
attr_size:
	DW attr_size_val


	HEADER tick_word, "'WORD", 0
	DW constant_code
tick_word:
	DW tick_word_val


	HEADER pad, "PAD", 0
	DW constant_code
pad:
	DW pad_val


	HEADER line_in, "-IN", 0
	DW constant_code
line_in:
	DW line_in_val


	HEADER line_in_size, "-IN#", 0
	DW constant_code
line_in_size:
	DW line_in_size_val


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
	; \ update keyboard state
	; KSCAN
	DX kscan-2
	; ;
	DT exit


	; \ Get name string from symbol header address
	; ( sym-addr ) : >SYM ( c-addr n+ )
	HEADER to_sym, ">SYM", 0
	DW colon_code
to_sym:
	; CELL+ CELL+  DUP C@ $7F AND  SWAP CHAR+ SWAP;
	DX cell_plus-2
	DX cell_plus-2
	DT dup
	DT c_fetch
	DT c_literal
	DB 0x7F
	DT and
	DT swap
	DT one_plus
	DT swap
	DT exit


	; \ Type all word names in dictionary to the output device
	; : WORDS
	HEADER words, "WORDS", 0
	DW colon_code
words:
	; SYM-LAST @
	DX sym_last-2
	DT fetch
	; BEGIN
.begin:
	;         \ Print name
	;         DUP >SYM TYPE  SPACE
	DT dup
	DX to_sym-2
	DX type-2
	DX space-2
	;         \ Goto next symbol
	;         @
	DT fetch
	; DUP 0= UNTIL DROP
	DT dup
	DT zero_equals
	DT until_raw
	DB .begin-$+256
	DT drop
	; CR ;
	DX cr-2
	DT exit


	; : KEY ( -- char )  \ Wait for next input char, using EKEY
	HEADER key, "KEY", 0
	DW colon_code
key:
	; \ Wait for a character key event
	; 0 BEGIN DROP EKEY EKEY>CHAR UNTIL CLICK ;
	DT zero_literal
.begin:
	DT drop
	DX ekey-2
	DX ekey_to_char-2
	DT until_raw
	DB .begin-$+256
	DX click-2
	DT exit


	; : MAIN
	HEADER main, "MAIN", 0
	DW colon_code
main:
	; PAGE
	DX page-2
	; ." ZEnv Forth" CR CR
	DT dot_quote_raw
	DB .s1e-.s1
.s1:
	DM "ZEnv Forth"
.s1e:
	DX cr-2
	DX cr-2

	; \ Run interpreter
	; QUIT ;
	DX quit-2
	DT exit


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


	HEADER false, "FALSE", 0
	DW zero_literal
false:


	HEADER true, "TRUE", 0
	DW $ + 2
true:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	LD BC, -1
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
	DT c_fetch
	DT c_literal
	DB 8
	DT minus
	DT zero_literal
	DX max-2
	DX t_row-2
	DT c_store
	; DISP-FILE 2048 +  DISP-FILE  4096  CMOVE
	DT literal_raw
	DW disp_file_val + 2048
	DX disp_file-2
	DT literal_raw
	DW 4096
	DX cmove-2
	; ATTR-FILE 256 +  ATTR-FILE  512  CMOVE
	DT literal_raw
	DW attr_file_val + 256
	DX attr_file-2
	DT literal_raw
	DW 512
	DX cmove-2
	; DISP-FILE 4096 +  2048  ERASE
	DT literal_raw
	DW disp_file_val + 4096
	DT literal_raw
	DW 2048
	DX erase-2
	; ATTR-FILE 512 +  256  T-ATTR C@  FILL
	DT literal_raw
	DW attr_file_val + 512
	DT literal_raw
	DW 256
	DX t_attr-2
	DT c_fetch
	DX fill-2
	; EXIT
	DT exit


	; : CR
	HEADER cr, "CR", 0
	DW colon_code
cr:
	; T-ROW C@ 22 > IF SCROLL THEN 1 T-ROW C+!
	DX t_row-2
	DT c_fetch
	DT c_literal
	DB 22
	DT greater_than
	DT if_raw
	DB .cr__if_skip-$-1
	DX scroll-2
.cr__if_skip:
	DT one_literal
	DX t_row-2
	DT c_plus_store
	; 0 T-COL C!
	DT zero_literal
	DX t_col-2
	DT c_store
	; ;
	DT exit


	; : BS ( -- ) \ Write a backspace to terminal
	HEADER bs, "BS", 0
	DW colon_code
bs:
	; T-COL C@ ?DUP IF
	DX t_col-2
	DT c_fetch
	DT question_dup
	DT if_raw
	DB .then-$-1
		; ( col )
		; 1-  DUP T-COL C!  SPACE  T-COL C!
		DT one_minus
		DT dup
		DX t_col-2
		DT c_store
		DX space-2
		DX t_col-2
		DT c_store
	; THEN ;
.then:
	DT exit


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
	ADD A, disp_file_val/256
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

	CP 8

	; Run BS if 8
	LD DE, bs
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

	CP 0x0A - 0x20
	LD DE, cr
	LD HL, colon_code
	CALL Z, asm_call

	CP 0x08 - 0x20
	LD DE, bs
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


	; Offset of current digit in 'WORD (pictured number buffer)
	HEADER to_number_sign, ">#", 0
	DW create_code
to_number_sign:
	DB 0


	; \ Initialise pictured number buffer for processing
	; : <# ( -- )
	HEADER less_number_sign, "<#", 0
	DW colon_code
less_number_sign:
	; 256 ># C! ;
	DT literal_raw
	DW 256
	DX to_number_sign-2
	DT c_store
	DT exit


	; \ Prefix pictured numeric string with given character
	; : HOLD ( c -- )
	HEADER hold, "HOLD", 0
	DW colon_code
hold:
	; \ Decrement >#
	; -1 ># C+!
	DT literal_raw
	DW -1
	DX to_number_sign-2
	DT c_plus_store
	; \ Store character at (># + 'WORD)
	; 'WORD ># C@ + C! ;
	DX tick_word-2
	DX to_number_sign-2
	DT c_fetch
	DT plus
	DT c_store
	DT exit


	; \ Add a '-' character to the pictured numeric string if n less than 0
	; : SIGN ( n -- )
	HEADER sign, "SIGN", 0
	DW colon_code
sign:
	; 0< IF '-' HOLD THEN ;
	DT zero_less
	DT if_raw
	DB .then-$-1
	DT c_literal
	DB '-'
	DX hold-2
.then:
	DT exit


	; \ Convert a digit to its character representation
	; : DIGIT ( n -- c )
	HEADER digit, "DIGIT", 0
	DW colon_code
digit:
	; DUP 9 > IF [ 'A' '0' - 10 - ] LITERAL + THEN
	DT dup
	DT c_literal
	DB 9
	DT greater_than
	DT if_raw
	DB .then-$-1
	DT c_literal
	DB 'A' - '0' - 10
	DT plus
.then:
	; '0' + ;
	DT c_literal
	DB '0'
	DT plus
	DT exit


	; \ Divide ud1 by BASE, quotient goes in ud2, remainder converted to
	; \ digit and prefixed to pictured numeric output string.
	; : # ( ud1 -- ud2 )
	HEADER number_sign, "#", 0
	DW colon_code
number_sign:
	; BASE @  DUB/MOD  DIGIT HOLD ;
	DX base-2
	DT fetch
	DX dub_slash_mod-2
	DX digit-2
	DX hold-2
	DT exit


	; \ Drop double cell, get pictured numeric string
	; : #> ( xd -- c-addr u )
	HEADER number_sign_greater, "#>", 0
	DW colon_code
number_sign_greater:
	; 2DROP  ># C@ 'WORD +  256 ># C@ -  ;
	DT two_drop
	DX to_number_sign-2
	DT c_fetch
	DX tick_word-2
	DT plus
	DT literal_raw
	DW 256
	DX to_number_sign-2
	DT c_fetch
	DT minus
	DT exit


	; \ Do # until quotient is zero
	; : #S ( ud1 -- ud2 )
	HEADER number_sign_s, "#S", 0
	DW colon_code
number_sign_s:
	; BEGIN # 2DUP D0= UNTIL
.begin:
	DX number_sign-2
	DT two_dup
	DX d_zero_equals-2
	DT until_raw
	DB .begin-$+256
	DT exit


	; \ Print an unsigned double number in the current BASE
	; : DU. ( ud -- )
	HEADER du_dot, "DU.", 0
	DW colon_code
du_dot:
	; <# #S #> TYPE SPACE ;
	DX less_number_sign-2
	DX number_sign_s-2
	DX number_sign_greater-2
	DX type-2
	DX space-2
	DT exit


	; \ Print a double number in the current BASE
	; : D. ( d -- )
	HEADER d_dot, "D.", 0
	DW colon_code
d_dot:
	; TUCK
	DT tuck
	; <#
	DX less_number_sign-2
	; DUP 0<  IF  0. 2SWAP D-  THEN
	DT dup
	DT zero_less
	DT if_raw
	DB .then-$-1
	DT zero_literal
	DT zero_literal
	DX two_swap-2
	DX d_minus-2
.then:
	; #S
	DX number_sign_s-2
	; ROT SIGN
	DT rot
	DX sign-2
	; #> TYPE
	DX number_sign_greater-2
	DX type-2
	; SPACE ;
	DX space-2
	DT exit


	; \ Print an unsigned number in the current BASE
	; : U. ( u -- )
	HEADER u_dot, "U.", 0
	DW colon_code
u_dot:
	; BASE @ 16 = IF U$. EXIT THEN
	DX base-2
	DT fetch
	DT c_literal
	DB 16
	DT equals
	DT if_raw
	DB .then1-$-1
	DX u_dollar_dot-2
	DT exit
.then1:
	; 0 DU. ;
	DT zero_literal
	DX du_dot-2
	DT exit


	; \ Convert single to double number
	; : S>D ( n -- d )
	HEADER s_to_d, "S>D", 0
	DW colon_code
s_to_d:
	; DUP 0< IF -1 ELSE 0 THEN ;
	DT dup
	DT zero_less
	DT if_raw
	DB .else-$-1
	DT literal_raw
	DW -1
	DT else_skip
	DB .then-$-1
.else:
	DT zero_literal
.then:
	DT exit


	; \ Print number in current BASE
	; : . ( n -- )
	HEADER dot, ".", 0
	DW colon_code
dot:
	; S>D D. ;
	DX s_to_d-2
	DX d_dot-2
	DT exit


	; \ Set border to attr
	; ( attr ) : BRDR!
	HEADER brdr_store, "BRDR!", 0
	DW colon_code
brdr_store:
	; 7 AND  ULA P@  0xF8 AND  OR  ULA P! ;
	DT c_literal
	DB 7
	DT and
	DX ula-2
	DX p_fetch-2
	DT c_literal
	DB 0xF8
	DT and
	DT or
	DX ula-2
	DX p_store-2
	DT exit


	; \ Clear screen, reset terminal to top-left
	; : PAGE
	HEADER page, "PAGE", 0
	DW colon_code
page:
	; \ Match border to T-ATTR
	; T-ATTR C@  3 RSHIFT  BRDR!
	DX t_attr-2
	DT c_fetch
	DT c_literal
	DB 3
	DT rshift
	DX brdr_store-2
	; \ Reset terminal col/row
	; 0 0 AT-XY
	DT zero_literal
	DT zero_literal
	DX at_xy-2
	; \ Erase bitmap
	; DISP-FILE DISP-SIZE ERASE
	DX disp_file-2
	DX disp_size-2
	DX erase-2
	; \ Set attr region to current T-ATTR
	; ATTR-FILE ATTR-SIZE T-ATTR C@ FILL
	DX attr_file-2
	DX attr_size-2
	DX t_attr-2
	DT c_fetch
	DX fill-2
	; ;
	DT exit


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


	HEADER plus_store, "+!", 0
	DW $ + 2
plus_store:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP HL
	POP BC
	LD E, (HL)
	INC HL
	LD D, (HL)
	EX DE, HL
	ADD HL, BC
	EX DE, HL
	LD (HL), D
	DEC HL
	LD (HL), E
	JP next


	; : , ( x -- ) \ Append cell to end of dictionary
	HEADER comma, ",", 0
	DW colon_code
comma:
	; HERE 2 ALLOT ! ;
	DX here-2
	DT c_literal
	DB 2
	DX allot-2
	DT store
	DT exit


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


	HEADER u_dollar_dot, "U$.", 0
	DW $ + 2
u_dollar_dot:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP BC
	; D is non-zero if any digits have been output yet
	LD D, 0
	LD A, B
	RRCA
	RRCA
	RRCA
	RRCA
	CALL .emit_nibble
	LD A, B
	CALL .emit_nibble
	LD A, C
	RRCA
	RRCA
	RRCA
	RRCA
	CALL .emit_nibble
	LD A, C
	INC D
	CALL .emit_nibble
	; Trailing space
	LD HL, ' '
	PUSH HL
	LD HL, emit
	CALL asm_call
	JP next
	; Emit the low nibble of A (if non-zero or D non-zero)
.emit_nibble:
	AND 0xF
	JR NZ, .not_zero
	; If D is 0 as well, skip digit
	OR D
	RET Z
	XOR A
.not_zero:
	INC D
	CP 0xA
	JR C, .digit
	ADD A, 'A'-'0'-0xA
.digit:
	ADD A, '0'
	LD L, A
	LD H, 0
	PUSH BC
	PUSH DE
	PUSH HL
	LD HL, emit
	CALL asm_call
	POP DE
	POP BC
	RET


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
	LD HL, 0xFFFF
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


	HEADER sp_store, "SP!", 0
	DW $ + 2
sp_store:
	POP HL
	LD SP, HL
	JP next


	HEADER rp_store, "RP!", 0
	DW $ + 2
rp_store:
	POP IX
	JP next


	; : ABORT ( -- ) \ Empty data stack and perform QUIT
	HEADER abort, "ABORT", 0
	DW colon_code
abort:
	; S0 SP! QUIT ;
	DX s_zero-2
	DX sp_store-2
	DX quit-2
	DT exit


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


	; : ALLOT ( n -- ) \ Add n to dictionary end pointer
	HEADER allot, "ALLOT", 0
	DW colon_code
allot:
	; H +! ;
	DX h_-2
	DT plus_store
	DT exit


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


	HEADER base, "BASE", 0
	DW create_code
base:
	DW 10


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


	; : C, ( c -- ) \ Append byte to end of dictionary
	HEADER c_comma, "C,", 0
	DW colon_code
c_comma:
	; HERE 1 ALLOT C! ;
	DX here-2
	DT one_literal
	DX allot-2
	DT c_store
	DT exit


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
	PUSH HL
	PUSH DE
	JP next


	; \ Set BASE to 10
	; : DECIMAL ( -- )
	HEADER decimal, "DECIMAL", 0
	DW colon_code
decimal:
	; 10 BASE ! ;
	DT c_literal
	DB 10
	DX base-2
	DT store
	DT exit


	HEADER depth, "DEPTH", 0
	DW colon_code
depth:
	DX s_zero-2
	DX tick_s-2
	DT minus
	DT two_slash
	DT exit


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


	; \ Set BASE to 16
	; : HEX ( -- )
	HEADER hex, "HEX", 0
	DW colon_code
hex:
	; 16 BASE ! ;
	DT c_literal
	DB 16
	DX base-2
	DT store
	DT exit


	; : MAX 2DUP < IF SWAP THEN DROP ;
	HEADER max, "MAX", 0
	DW colon_code
max:
	DT two_dup
	DT less_than
	DT if_raw
	DB .skip-$-1
	DT swap
.skip:
	DT drop
	DT exit


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
	DT minus_rot
	DT two_dup
	DT less_than
	DT if_raw
	DB .move__else-$-1
	DT rot
	DX cmove_up-2
	DT else_skip
	DB .move__else_skip-$-1
.move__else:
	DT rot
	DX cmove-2
.move__else_skip:
	DT exit


	; : NEGATE  0 SWAP - ;
	HEADER negate, "NEGATE", 0
	DW colon_code
negate:
	DT zero_literal
	DT swap
	DT minus
	DT exit


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
	DT zero_literal
	DT question_do_raw
	DB .type__skip-$-1
.type__loop:
	DT dup
	DT c_fetch
	DX emit-2
	DT one_plus
	DT loop_raw
	DB .type__loop-$+256
.type__skip:
	DT drop
	DT exit


	HEADER s_quote_raw, '(S")', 0
	DW colon_code
s_quote_raw:
	; R> COUNT 2DUP + >R ;
	DT r_from
	DX count-2
	DT two_dup
	DT plus
	DT to_r
	DT exit


	HEADER dot_quote_raw, '(.")', 0
	DW colon_code
dot_quote_raw:
	; R> COUNT 2DUP + >R TYPE ;
	DT r_from
	DX count-2
	DT two_dup
	DT plus
	DT to_r
	DX type-2
	DT exit


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


	HEADER over_two, "OVER2", 0
	DW $ + 2
over_two:
	IF CHECKED
		CALL dat_holds_3_room_1
	ENDIF
	LD HL, 4
	ADD HL, SP
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	JP next


	; : -READ ( -- addr u ) \ Read a line of input into -IN
	HEADER line_read, "-READ", 0
	DW colon_code
line_read:
	; 0 BEGIN DUP -IN# < WHILE
	DT zero_literal
.begin:
	DT dup
	DX line_in_size-2
	DT less_than
	DT if_raw
	DB .repeat-$-1
		; ( idx )
		; KEY DUP CASE
		DX key-2
		DT dup
			; ( idx key key )
			; 8  OF EMIT  1-           ENDOF
			DT c_literal
			DB 8
			DT of_raw
			DB .endof1-$-1
			DX emit-2
			DT one_minus
			DT else_skip
			DB .endcase-$-1
.endof1:
			; 10 OF DROP -IN SWAP EXIT ENDOF
			DT c_literal
			DB 10
			DT of_raw
			DB .endof2-$-1
			DT drop
			DX line_in-2
			DT swap
			DT exit
			DT else_skip
			DB .endcase-$-1
.endof2:
			; EMIT OVER -IN + C!  1+
			DX emit-2
			DT over
			DX line_in-2
			DT plus
			DT c_store
			DT one_plus
		; 0 ENDCASE
		DT zero_literal
		DT drop
.endcase:
	; REPEAT
	DT repeat_raw
	DB .begin-$+256
.repeat:
	; ( idx )
	; -IN SWAP ;
	DX line_in-2
	DT swap
	DT exit


	; : QUIT ( -- ) \ Reset return stack then start interpretation
	HEADER quit, "QUIT", 0
	DW colon_code
quit:
	; R0 RP!
	DX r_zero-2
	DX rp_store-2
	; BEGIN
.begin:
		; -READ EVALUATE ."  ok" CR
		DX line_read-2
		DX evaluate-2
		DT dot_quote_raw
		DB .s1e-.s1
.s1:
		DM " ok"
.s1e:
		DX cr-2
	; AGAIN ;
	DT again_raw
	DB .begin-$+256
	DT exit


	; : EVALUATE ( ??? addr u -- ??? )
	HEADER evaluate, "EVALUATE", 0
	DW colon_code
evaluate:
	; BEGIN -' IF NUMBER ELSE EXECUTE THEN AGAIN ;
	DT two_drop
	DT exit


	; : ERASE 0 FILL ;
	HEADER erase, "ERASE", 0
	DW colon_code
erase:
	DT zero_literal
	DX fill-2
	DT exit


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


	HEADER tick_r, "'R", 0
	DW $ + 2
tick_r:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	PUSH IX
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


	HEADER space, "SPACE", 0
	DW colon_code
space:
	DX bl-2
	DX emit-2
	DT exit


	; : SPACES ( n -- ) \ Print n spaces
	HEADER spaces, "SPACES", 0
	DW colon_code
spaces:
	; 0 MAX
	DT zero_literal
	DX max-2
	; 0 ?DO SPACE LOOP ;
	DT zero_literal
	DT question_do_raw
	DB .loop-$-1
.do:
	DX space-2
	DT loop_raw
	DB .do-$+256
.loop:
	DT exit


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


	HEADER r_zero, "R0", 0
	DW constant_code
r_zero:
	DW return_stack_top


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


	HEADER u_less_than, "U<", 0
	DW $ + 2
u_less_than:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP DE
	POP HL
	OR A
	SBC HL, DE
	LD BC, 0
	JR C, .lt
	PUSH BC
	JP next
.lt:
	DEC BC
	PUSH BC
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
	DT swap
	DT less_than
	DT exit


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
	LD E, (IY-1)
	ADD IY, DE
.if_raw__end:
	JP next


	HEADER of_raw, "(OF)", 0
	DW $ + 2
of_raw:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	INC IY
	POP HL
	POP DE
	OR A
	SBC HL, DE
	JR Z, ._end
	LD C, (IY-1)
	LD B, 0
	ADD IY, BC
	PUSH DE
._end:
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


	HEADER tuck, "TUCK", 0
	DW $ + 2
tuck:
	IF CHECKED
		CALL dat_holds_2_room_1
	ENDIF
	POP BC
	POP DE
	PUSH BC
	PUSH DE
	PUSH BC
	JP next


	HEADER tuck2, "TUCK2", 0
	DW $ + 2
tuck2:
	IF CHECKED
		CALL dat_holds_3_room_1
	ENDIF
	POP BC
	POP DE
	POP HL
	PUSH BC
	PUSH HL
	PUSH DE
	PUSH BC
	JP next


	; ( d addr ) : D+! \ double in addr incremented by d
	HEADER d_plus_store, "D+!", 0
	DW colon_code
d_plus_store:
	; TUCK2 2@ D+ ROT 2! ;
	DX tuck2-2
	DX two_fetch-2
	DX d_plus-2
	DT rot
	DX two_store-2
	DT exit


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
	DT r_from
	DT dup
	DT c_literal
	DB 2
	DX cells-2
	DT plus
	DT to_r
	DX two_fetch-2
	DT exit


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
	DT zero_literal
	DT c_literal
	DB 23
	DX clamp-2
	DX t_row-2
	DT c_store
	; ( x ) 0 63 CLAMP T-COL C!
	DT zero_literal
	DT c_literal
	DB t_width-1
	DX clamp-2
	DX t_col-2
	DT c_store
	; ;
	DT exit


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
	DT c_fetch
	DX keyq_s-2
	DT c_fetch
	DT not_equals
	DT exit


	; 0<> ( n -- flags ) \ true if n is not equal to 0
	HEADER zero_not_equals, "0<>", 0
	DW colon_code
zero_not_equals:
	; 0= INVERT ;
	DT zero_equals
	DT invert
	DT exit


	; <> ( n1 n2 -- flags ) \ true if n1 is not equal to n2
	HEADER not_equals, "<>", 0
	DW colon_code
not_equals:
	; = INVERT ;
	DT equals
	DT invert
	DT exit


	; CLAMP ( n1 n2 n3 -- n ) \ Force n1 to range [n2, n3]
	HEADER clamp, "CLAMP", 0
	DW colon_code
clamp:
	; ROT MIN MAX ;
	DT rot
	DX min-2
	DX max-2
	DT exit


	; : MIN ( n1 n2 -- n ) \ Leave the smaller of n1 and n2
	HEADER min, "MIN", 0
	DW colon_code
min:
	; 2DUP > IF SWAP THEN DROP ;
	DT two_dup
	DT greater_than
	DT if_raw
	DB .skip-$-1
	DT swap
.skip:
	DT drop
	DT exit


	; : WITHIN ( n start end -- flags ) \ Is n within [start, end), or (end,
	;                                   \ start] if end is less.
	HEADER within, "WITHIN", 0
	DW colon_code
within:
	; ( n start end )
	; OVER -
	DT over
	DT minus
	; ( n start range-length )
	; -ROT - SWAP
	DT minus_rot
	DT minus
	DT swap
	; ( offset range-length )
	; U< ;
	DX u_less_than-2
	DT exit


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
	DT until_raw
	DB .begin-$+256
	; KEYQ KEYQ-S C@ CELLS + @ \ Receive event
	DX keyq-2
	DX keyq_s-2
	DT c_fetch
	DT two_star
	DT plus
	DT fetch
	; KEYQ-S C@ 1+ 7 AND KEYQ-S C! \ Increment KEYQ-S (mod 8)
	DX keyq_s-2
	DT c_fetch
	DT one_plus
	DT c_literal
	DB 7
	DT and
	DX keyq_s-2
	DT c_store
	; ;
	DT exit


	; \ Address of the character key map in ROM
	; $205 ROM-KMAP CONSTANT
	HEADER rom_kmap, "ROM-KMAP", 0
	DW constant_code
rom_kmap:
	DW 0x205


rom_smap1_val: EQU 0x26A
	; \ Address of one alphabet key map used with symbol shift
	; $26A ROM-SMAP1 CONSTANT
	HEADER rom_smap1, "ROM-SMAP1", 0
	DW constant_code
rom_smap1:
	DW rom_smap1_val


rom_smap2_val: EQU 0x246
	; \ Address of lower priority alphabet key map used with symbol shift
	; $246 ROM-SMAP2 CONSTANT
	HEADER rom_smap2, "ROM-SMAP2", 0
	DW constant_code
rom_smap2:
	DW rom_smap2_val


	; \ Mapping of number keys to symbol shift characters
	; CREATE NSYM-MAP
	HEADER nsym_map, "NSYM-MAP", 0
	DW create_code
nsym_map:
	; '_' C,
	DB '_'
	; '!' C,
	DB '!'
	; '@' C,
	DB '@'
	; '#' C,
	DB '#'
	; '$' C,
	DB '$'
	; '%' C,
	DB '%'
	; '&' C,
	DB '&'
	; ''' C,
	DB "'"
	; '(' C,
	DB '('
	; ')' C,
	DB ')'


	; CREATE KMAP
	HEADER kmap, "KMAP", 0
	DW create_code
kmap:
	; \ Unshifted map
	; 'b' C, 'h' C, 'y' C, '6' C, '5' C, 't' C, 'g' C, 'v' C,
	DB 'b', 'h', 'y', '6', '5', 't', 'g', 'v'
	; 'n' C, 'j' C, 'u' C, '7' C, '4' C, 'r' C, 'f' C, 'c' C,
	DB 'n', 'j', 'u', '7', '4', 'r', 'f', 'c'
	; 'm' C, 'k' C, 'i' C, '8' C, '3' C, 'e' C, 'd' C, 'x' C,
	DB 'm', 'k', 'i', '8', '3', 'e', 'd', 'x'
	; 0 C, 'l' C, 'o' C, '9' C, '2' C, 'w' C, 's' C, 'z' C,
	DB 0, 'l', 'o', '9', '2', 'w', 's', 'z'
	; ' ' C, $0A C, 'p' C, '0' C, '1' C, 'q' C, 'a' C, 0 C,
	DB ' ', 0x0A, 'p', '0', '1', 'q', 'a', 0

	; \ Caps shifted map
	; 'B' C, 'H' C, 'Y' C, 0 C, 0 C, 'T' C, 'G' C, 'V' C,
	DB 'B', 'H', 'Y', 0, 0, 'T', 'G', 'V'
	; 'N' C, 'J' C, 'U' C, 0 C, 0 C, 'R' C, 'F' C, 'C' C,
	DB 'N', 'J', 'U', 0, 0, 'R', 'F', 'C'
	; 'M' C, 'K' C, 'I' C, 0 C, 0 C, 'E' C, 'D' C, 'X' C,
	DB 'M', 'K', 'I', 0, 0, 'E', 'D', 'X'
	; 0 C, 'L' C, 'O' C, 0 C, 0 C, 'W' C, 'S' C, 'Z' C,
	DB 0, 'L', 'O', 0, 0, 'W', 'S', 'Z'
	; $1B C, $0A C, 'P' C, $08 C, 0 C, 'Q' C, 'A' C, 0 C,
	DB 0x1B, 0x0A, 'P', 0x08, 0, 'Q', 'A', 0

	; \ Symbol shifted map
	; '*' C, '^' C, '[' C, '&' C, '%' C, '>' C, '}' C, '/' C,
	DB '*', '^', '[', '&', '%', '>', '}', '/'
	; ',' C, '-' C, ']' C, ''' C, '$' C, '<' C, '{' C, '?' C,
	DB ',', '-', ']', "'", '$', '<', '{', '?'
	; '.' C, '+' C, '`' C, '(' C, '#' C, 0 C, '\' C, $7F C,
	DB '.', '+', '`', '(', '#', 0, 0x5C, 0x7F
	; 0 C, '=' C, ';' C, ')' C, '@' C, 0 C, '|' C, ':' C,
	DB 0, '=', ';', ')', '@', 0, '|', ':'
	; 0 C, $0A C, '"' C, '_' C, '!' C, 0 C, '~' C, 0 C,
	DB 0, 0x0A, '"', '_', '!', 0, '~', 0


	; EKEY>CHAR ( x -- x false | char true )
	HEADER ekey_to_char, "EKEY>CHAR", 0
	DW colon_code
ekey_to_char:
	; \ Get offset byte
	; DUP $FF AND
	DT dup
	DT c_literal
	DB 0xFF
	DT and
	; \ If shift active...
	; 2DUP <> IF
	DT two_dup
	DT not_equals
	DT if_raw
	DB .then1-$-1
		; \ If symbol shift, add 80, otherwise 40
		; OVER $200 AND  80 40 CHOOSE  +
		DT over
		DT literal_raw
		DW 0x200
		DT and
		DT c_literal
		DB 80
		DT c_literal
		DB 40
		DX choose-2
		DT plus
	; THEN
.then1:
	; CHARS KMAP + C@ ?DUP IF
	DX kmap-2
	DT plus
	DT c_fetch
	DT question_dup
	DT if_raw
	DB .else1-$-1
		; NIP TRUE
		DT nip
		DT true
	; ELSE
	DT else_skip
	DB .then2-$-1
.else1:
		; FALSE
		DT zero_literal
	; THEN
.then2:
	; ;
	DT exit


	HEADER until_raw, "(UNTIL)", 0
	DW $ + 2
until_raw:
	IF CHECKED
		CALL dat_holds_1
	ENDIF
	POP HL
	LD A, L
	OR H
	JR Z, until_raw_loop
	INC IY
	JP next
until_raw_loop:
	LD C, (IY+0)
	LD B, 0xFF
	ADD IY, BC
	JP next


	HEADER again_raw, "(AGAIN)", 0
	DW $ + 2
again_raw:
	JR until_raw_loop


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
	DT greater_than
	DT invert
	DT exit


	HEADER greater_than_or_equal, ">=", 0
	DW colon_code
greater_than_or_equal:
	DT less_than
	DT invert
	DT exit


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


	; CREATE KSHIFT-STATE 0 C,
	HEADER kshift_state, "KSHIFT-STATE", 0
	DW create_code
kshift_state:
	DB 0


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


	; \ Update keyboard state
	; CODE KSCAN ( -- )
	HEADER kscan, "KSCAN", 0
	DW $ + 2
kscan:
	PUSH IX
	; If no keys are down, skip
	LD BC, 0x00FE
	IN A, (C)
	CPL
	OR A
	JR NZ, .keys_down
	; Clear kstate
	LD L, A
	LD H, A
	LD (kstate), HL
	LD (kstate+2), HL
	LD (kstate+4), HL
	LD (kstate+6), HL
	POP IX
	JP next

.keys_down
	; Update shift state
	LD B, 0x7F
	IN A, (C)
	OR 0xFD
	LD E, A
	LD B, C
	IN A, (C)
	OR 0xFE
	AND E
	CPL
	LD (kshift_state), A
	; Loop over rows
	; BC is 0xFEFE and will rotate to 7FFE
	; E is counter
	LD E, 7
	; IX is KSTATE pointer
	LD IX, kstate
.loop:
	; If row is empty, clear state and skip
	IN A, (C)
	CPL
	JR NZ, .row_down
	LD (IX+0), A
.next_loop:
	INC IX
	RLC B
	DEC E
	JP P, .loop
	POP IX
	JP next
.row_down:
	; BC is port, E is counter, A is input, IX is kstate+E
	; D is old state
	LD D, (IX+0)
	; H becomes input store, will rotate to get all needed bits
	LD H, A
	; If same as old state, skip
	CP D
	JR Z, .next_loop
	; Store inverted old state in D
	LD A, D
	CPL
	LD D, A
	; L is counter for bit loop
	LD L, 4
.bit_loop:
	; If new state is down and old state is up...
	LD A, H
	AND D
	AND 1
	JR Z, .bit_skip
	; ... generate an EKEY
	PUSH BC
	PUSH DE
	PUSH HL
	; L = bit, E = row
	; Is there space in the queue?
	; C is keyq offset to store at
	LD A, (keyq_s)
	LD B, A
	LD A, (keyq_e)
	LD C, A
	INC A
	AND 7
	CP B
	JR Z, .inner_skip
	; Save next keyq_e value
	LD (keyq_e), A
	; Make HL new EKEY value
	LD A, L
	ADD A, A
	ADD A, A
	ADD A, A
	ADD A, E
	LD L, A
	LD A, (kshift_state)
	LD H, A
	; Store in queue
	EX DE, HL
	LD HL, keyq
	LD B, 0
	ADD HL, BC
	ADD HL, BC
	LD (HL), E
	INC HL
	LD (HL), D
.inner_skip:
	POP HL
	POP DE
	POP BC
.bit_skip:
	RRC H
	RRC D
	DEC L
	JP P, .bit_loop
	; All bits considered, save input state
	LD A, H
	RRCA
	RRCA
	RRCA
	LD (IX+0), A
	JR .next_loop


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


	; : BIT ( x n -- x&(1<<n) ) \ Mask all but nth bit of x
	HEADER bit_, "BIT", 0
	DW colon_code
bit_:
	; 1 SWAP LSHIFT AND ;
	DT one_literal
	DT swap
	DT lshift
	DT and
	DT exit


	; : CHOOSE ( flags x1 x2 -- x1|x2 ) \ Select x1 if flags true, o/w x2
	HEADER choose, "CHOOSE", 0
	DW colon_code
choose:
	; ROT IF SWAP THEN NIP ;
	DT rot
	DT if_raw
	DB .then-$-1
	DT swap
.then:
	DT nip
	DT exit


	; : 2OR ( d1 d2 -- d1|d2 ) \ OR of doubles
	HEADER two_or, "2OR", 0
	DW colon_code
two_or:
	; ROT OR -ROT OR SWAP ;
	DT rot
	DT or
	DT minus_rot
	DT or
	DT swap
	DT exit


	; : D0< ( d -- flags ) \ Is a double integer negative?
	HEADER d_zero_less, "D0<", 0
	DW colon_code
d_zero_less:
	; NIP 0< ;
	DT nip
	DT zero_less
	DT exit


	; CODE D2*
	HEADER d_two_star, "D2*", 0
	DW $ + 2
d_two_star:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP BC
	POP DE
	SLA E
	RL D
	RL C
	RL B
	PUSH DE
	PUSH BC
	JP next


	; CODE DU2/
	HEADER du_two_slash, "DU2/", 0
	DW $ + 2
du_two_slash:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	POP BC
	POP DE
	SRL B
	RR C
	RR D
	RR E
	PUSH DE
	PUSH BC
	JP next


	; CODE 2SWAP
	HEADER two_swap, "2SWAP", 0
	DW $ + 2
two_swap:
	IF CHECKED
		CALL dat_holds_4
	ENDIF
	POP AF
	POP BC
	POP DE
	POP HL
	PUSH BC
	PUSH AF
	PUSH HL
	PUSH DE
	JP next


	; : 2ROT ( d1 d2 d3 -- d2 d3 d1 )
	HEADER two_rot, "2ROT", 0
	DW colon_code
two_rot:
	; 2>R 2SWAP 2R> 2SWAP ;
	DT two_to_r
	DX two_swap-2
	DX two_r_from-2
	DX two_swap-2
	DT exit


	; : 2-ROT ( d1 d2 d3 -- d3 d1 d2 )
	HEADER two_minus_rot, "2-ROT", 0
	DW colon_code
two_minus_rot:
	; 2SWAP 2>R 2SWAP 2R> ;
	DX two_swap-2
	DT two_to_r
	DX two_swap-2
	DX two_r_from-2
	DT exit


	HEADER du_less_than, "DU<", 0
	DW $ + 2
du_less_than:
	IF CHECKED
		CALL dat_holds_4
	ENDIF
	POP BC
	POP DE
	POP HL
	OR A
	SBC HL, BC
	JR Z, .equal
	JR C, .true
	LD HL, 0
	EX (SP), HL
	JP next
.true:
	LD HL, -1
	EX (SP), HL
	JP next
.equal:
	POP HL
	OR A
	SBC HL, DE
	JR C, .true2
	LD HL, 0
	PUSH HL
	JP next
.true2:
	LD HL, -1
	PUSH HL
	JP next


	; : D<
	HEADER d_less_than, "D<", 0
	DW colon_code
d_less_than:
	; 2SWAP $80000000. D+ 2SWAP $80000000. D+ DU< ;
	DX two_swap-2
	DX two_literal_raw-2
	DW 0x8000
	DW 0
	DX d_plus-2
	DX two_swap-2
	DX two_literal_raw-2
	DW 0x8000
	DW 0
	DX d_plus-2
	DX du_less_than-2
	DT exit


	; CODE D-
	HEADER d_minus, "D-", 0
	DW $ + 2
d_minus:
	IF CHECKED
		CALL dat_holds_4
	ENDIF
	POP AF
	POP BC
	POP DE
	POP HL
	PUSH AF
	OR A
	SBC HL, BC
	EX DE, HL
	POP BC
	SBC HL, BC
	PUSH DE
	PUSH HL
	JP next


	; CODE 2R>
	HEADER two_r_from, "2R>", 0
	DW $ + 2
two_r_from:
	IF CHECKED
		CALL dat_room_2_ret_holds_2
	ENDIF
	LD E, (IX+0)
	LD D, (IX+1)
	LD C, (IX+2)
	LD B, (IX+3)
	PUSH BC
	PUSH DE
	LD BC, 4
	ADD IX, BC
	JP next


	; : 2OVER2 ( d1 d2 d3 -- d1 d2 d3 d1 )
	HEADER two_over_two, "2OVER2", 0
	DW colon_code
two_over_two:
	; 'S 8 + 2@ ;
	DX tick_s-2
	DT c_literal
	DB 8
	DT plus
	DX two_fetch-2
	DT exit


	; : D0=
	HEADER d_zero_equals, "D0=", 0
	DW colon_code
d_zero_equals:
	; OR 0= ;
	DT or
	DT zero_equals
	DT exit


	; \ Divide and produce remainder in double integers
	; : DU/MOD ( ud1 ud2 -- ud3 ud4 ) \ Where ud1 is numerator, ud2 is
	;                                 \ denominator, ud3 is result, ud4 is
	;                                 \ remainder.
	HEADER du_slash_mod, "DU/MOD", 0
	DW colon_code
du_slash_mod:
	; \ Refuse to divide by 0
	; \ TODO 2DUP D0= IF ABORT" Div by 0" THEN
	; 1. 2-ROT
	DT one_literal
	DT zero_literal
	DX two_minus_rot-2
	; ( rem den unit )
	; \ Shift den and unit, while den smaller than rem,
	; \ and until before den overflows.
	; BEGIN 2OVER 2OVER 2SWAP DU< IF 2DUP D0< INVERT ELSE FALSE THEN WHILE
.begin1:
	DX two_over-2
	DX two_over-2
	DX two_swap-2
	DX du_less_than-2
	DT if_raw
	DB .else-$-1
	DX two_dup-2
	DX d_zero_less-2
	DT invert
	DT else_skip
	DB .then2-$-1
.else:
	DT zero_literal
.then2:
	DT if_raw
	DB .repeat1-$-1
		; 2ROT D2* 2-ROT D2*
		DX two_rot-2
		DX d_two_star-2
		DX two_minus_rot-2
		DX d_two_star-2
	; REPEAT
	DT repeat_raw
	DB .begin1-$+256
.repeat1:

	; 0. 2>R  2SWAP
	DT zero_literal
	DT zero_literal
	DT two_to_r
	DX two_swap-2
	; ( unit div rem ) ( R:result )
	; BEGIN
.begin2:
		; \ If remainder at least divisor, sub and OR unit to result
		; 2OVER 2OVER 2SWAP DU< INVERT IF
		DX two_over-2
		DX two_over-2
		DX two_swap-2
		DX du_less_than-2
		DT invert
		DT if_raw
		DB .then-$-1
			; 2OVER D-
			DX two_over-2
			DX d_minus-2
			; 2ROT 2DUP 2R> 2OR 2>R 2-ROT
			DX two_rot-2
			DX two_dup-2
			DX two_r_from-2
			DX two_or-2
			DT two_to_r
			DX two_minus_rot-2
		; THEN
.then:

		; \ Shift right each step until divisor gone
		; 2ROT DU2/ 2ROT DU2/ 2ROT
		DX two_rot-2
		DX du_two_slash-2
		DX two_rot-2
		DX du_two_slash-2
		DX two_rot-2
	; 2OVER2 D0= UNTIL
	DX two_over_two-2
	DX d_zero_equals-2
	DT until_raw
	DB .begin2-$+256

	; \ Return result + remainder
	; 2-ROT 2DROP 2DROP 2R> 2SWAP ;
	DX two_minus_rot-2
	DT two_drop
	DT two_drop
	DX two_r_from-2
	DX two_swap-2
	DT exit


	; : UM/MOD ( ud u1 -- u2 u3 ) \ Divide by u1, quo u2 rem u3
	HEADER um_slash_mod, "UM/MOD", 0
	DW colon_code
um_slash_mod:
	; 0 DU/MOD DROP NIP ;
	DT zero_literal
	DX du_slash_mod-2
	DT drop
	DT nip
	DT exit


	; CODE DUB/MOD ( ud byte -- ud2 rem-byte ) \ Divide ud by unsigned byte
	HEADER dub_slash_mod, "DUB/MOD", 0
	DW $ + 2
dub_slash_mod:
	IF CHECKED
		CALL dat_holds_3
	ENDIF
	POP BC
	POP DE
	POP HL
	LD B, 32
	XOR A
.loop:
	ADD HL, HL
	RL E
	RL D
	RLA
	CP C
	JR C, .div_larger
	INC L
	SUB C
.div_larger:
	DJNZ .loop
	LD C, A
	PUSH HL
	PUSH DE
	PUSH BC
	JP next


	; CODE ITONE ( u1 u2 -- ) \ half-oscillations period
	HEADER itone, "ITONE", 0
	DW $ + 2
itone:
	IF CHECKED
		CALL dat_holds_2
	ENDIF
	LD A, (t_attr)
	RRCA
	RRCA
	RRCA
	AND 7
	POP DE
	LD HL, 0
	SBC HL, DE
	; DE = period
	EX DE, HL
	POP BC
	LD HL, 0
	OR A
	; HL = half-oscillations
	SBC HL, BC
	; B = OUT value
	LD B, A
	; Quit if HL = 0
	LD A, L
	OR A, H
	JP Z, next
	; Push period
	PUSH DE
	LD A, B
.loop1_with_delay:
	ADD A, 0
	NOP
.loop1:
	OUT (ula_val), A
	XOR 1<<4
.loop2_with_delay:
	ADD A, 0
	NOP
.loop2:
	INC E
	JR NZ, .loop2_with_delay
	INC D
	JR NZ, .loop2

	POP DE
	PUSH DE
	INC L
	JR NZ, .loop1_with_delay
	INC H
	JR NZ, .loop1

	POP HL
	JP next


	; : TONE  ( len period -- ) \ Make an accurate tone, masking interrupts
	HEADER tone, "TONE", 0
	DW colon_code
tone:
	; DI ITONE EI ;
	DX _di-2
	DX itone-2
	DX _ei-2
	DT exit


	; : CLICK ( -- ) \ Make a 'click' noise
	HEADER click, "CLICK", 0
	DW colon_code
click:
	; 4 30 ITONE ;
	DT c_literal
	DB 4
	DT c_literal
	DB 30
	DX itone-2
	DT exit


repeat_wait_init: EQU 45  ; 0.9s
repeat_repeat_init: EQU 5 ; 0.1s
	; Used to time waiting for repeats and repeating, cleared on every new
	; key event.
repeat_timer:
	DB 0


key_up:        EQU 0x11 ; ASCII DC1
key_left:      EQU 0x12 ; ASCII DC2
key_down:      EQU 0x13 ; ASCII DC3
key_right:     EQU 0x14 ; ASCII DC4
key_caps_lock: EQU 0x1C ; ASCII File separator

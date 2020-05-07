; vim:set syntax=z80:

; Current data space pointer
	HEADER forth_h, "H"
	DW forth_create_code
forth_h:
	DW forth_h_init


; non-zero while compiling
	HEADER forth_state, "STATE"
	DW forth_create_code
forth_state:
	DW 0


	HEADER frames, "FRAMES"
	DW forth_create_code
frames:
	DW 0
	DW 0


	HEADER t_attr, "T-ATTR"
	DW forth_create_code
t_attr:
	DB 7


	HEADER t_col, "T-COL"
	DW forth_create_code
t_col:
	DB 0


	HEADER t_row, "T-ROW"
	DW forth_create_code
t_row:
	DB 0


	HEADER display_file, "DISPLAY-FILE"
	DW forth_constant_code
display_file:
	DW display_file_val


	HEADER display_size, "DISPLAY-SIZE"
	DW forth_constant_code
display_size:
	DW display_size_val


	HEADER attr_file, "ATTR-FILE"
	DW forth_constant_code
attr_file:
	DW attr_file_val


	HEADER attr_size, "ATTR-SIZE"
	DW forth_constant_code
attr_size:
	DW attr_size_val


	HEADER tick_int, "'INT"
	DW forth_create_code
tick_int:
	DW int-2


	; Default interrupt handler
	; : INT
	HEADER int, "INT"
	DW forth_colon_code
int:
	; 1. FRAMES D+! \ increment FRAMES
	DX forth_two_literal_raw-2
	DW 0
	DW 1
	DX frames-2
	DX forth_d_plus_store-2
	; \ KSCAN \ TODO
;	DX kscan-2
	; ;
	DB forth_exit_tok


	; : MAIN
	HEADER forth_main, "MAIN"
	DW forth_colon_code
forth_main:
	; PAGE \ Clear screen
	DX forth_page-2
	; 50 0 ?DO S" ..." TYPE LOOP S" ..." TYPE   \ test program
	DB forth_c_literal_tok
	DB 50
	DB forth_zero_literal_tok
	DB forth_question_do_raw_tok
	DB .main__loop_skip-$-1
.main__loop:
	DB forth_s_quote_raw_tok
	DB .main__s1_end-$-1
	DM ": FILL ROT ROT 0 ?DO 2DUP C! 1+ LOOP 2DROP ; ok "
.main__s1_end:
	DX forth_type-2
	DB forth_loop_raw_tok
	DB .main__loop-$+256
.main__loop_skip:
	DB forth_s_quote_raw_tok
	DB .main__s2_end-$-1
	DM "Hello, world!\n\n"
.main__s2_end:
	DX forth_type-2
	; BEGIN EKEY DROP '.' EMIT 0 UNTIL \ Endlessly get input and print periods
.begin:
	DX ekey-2
	DB forth_drop_tok
	DB forth_c_literal_tok
	DB '.'
	DX forth_emit-2
	DB forth_zero_literal_tok
	DB until_raw_tok
	DB $-1-.begin
	DB forth_exit_tok ; will never be reached


	HEADER forth_zero_literal, "0"
	DW $ + 2
forth_zero_literal:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD BC, 0
	PUSH BC
	JP forth_next


	HEADER forth_one_literal, "1"
	DW $ + 2
forth_one_literal:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD BC, 1
	PUSH BC
	JP forth_next


	HEADER forth_literal_raw, "(LITERAL)"
	DW $ + 2
forth_literal_raw:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD E, (IY+0)
	LD D, (IY+1)
	INC IY
	INC IY
	PUSH DE
	JP forth_next


	; : SCROLL
	HEADER scroll, "SCROLL"
	DW forth_colon_code
scroll:
	; T-ROW C@ 8 -  0 MAX  T-ROW C!
	DX t_row-2
	DB forth_c_fetch_tok
	DB forth_c_literal_tok
	DB 8
	DB forth_minus_tok
	DB forth_zero_literal_tok
	DX forth_max-2
	DX t_row-2
	DB forth_c_store_tok
	; DISPLAY-FILE 2048 +  DISPLAY-FILE  4096  CMOVE
	DB forth_literal_raw_tok
	DW display_file_val + 2048
	DX display_file-2
	DB forth_literal_raw_tok
	DW 4096
	DX forth_cmove-2
	; ATTR-FILE 256 +  ATTR-FILE  512  CMOVE
	DB forth_literal_raw_tok
	DW attr_file_val + 256
	DX attr_file-2
	DB forth_literal_raw_tok
	DW 512
	DX forth_cmove-2
	; DISPLAY-FILE 4096 +  2048  ERASE
	DB forth_literal_raw_tok
	DW display_file_val + 4096
	DB forth_literal_raw_tok
	DW 2048
	DX forth_erase-2
	; ATTR-FILE 512 +  256  T-ATTR C@  FILL
	DB forth_literal_raw_tok
	DW attr_file_val + 512
	DB forth_literal_raw_tok
	DW 256
	DX t_attr-2
	DB forth_c_fetch_tok
	DX forth_fill-2
	; EXIT
	DB forth_exit_tok


	; : CR
	HEADER forth_cr, "CR"
	DW forth_colon_code
forth_cr:
	; T-ROW C@ 22 > IF SCROLL THEN 1 T-ROW C+!
	DX t_row-2
	DB forth_c_fetch_tok
	DB forth_c_literal_tok
	DB 22
	DB forth_greater_than_tok
	DB forth_if_raw_tok
	DB .cr__if_skip-$-1
	DX scroll-2
.cr__if_skip:
	DB forth_one_literal_tok
	DX t_row-2
	DB forth_c_plus_store_tok
	; 0 T-COL C!
	DB forth_zero_literal_tok
	DX t_col-2
	DB forth_c_store_tok
	; ;
	DB forth_exit_tok


	HEADER forth_emit, "EMIT"
	DW $ + 2
forth_emit:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	; C is character
	POP BC
	LD A, C
	CP 0x20
	JR C, .forth_emit__other_char
	CP 0x80
	JR NC, .forth_emit__other_char

	LD A, (t_col)
	CP 64
	JR C, .forth_emit__no_wrap
	PUSH BC
	LD DE, forth_cr
	LD HL, forth_colon_code
	CALL forth_asm_call
	POP BC
.forth_emit__no_wrap:

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
	JR NZ, .forth_emit__odd_col
	CPL
.forth_emit__odd_col:
	LD B, A

	; Pop write address
	POP HL
	; Pop font address
	POP DE

.forth_emit__loop:
	; Clear glyph scanline
	LD A, B
	CPL
	AND (HL)
	LD (HL), A
	; Load glyph scanline
	LD A, (DE)
	BIT 7, C
	JR Z, .forth_emit__no_shift
	RLCA
	RLCA
	RLCA
	RLCA
.forth_emit__no_shift:
	AND B
	; Apply glyph to scanline
	OR (HL)
	LD (HL), A
	; Next
	INC DE
	INC H
	INC C
	BIT 3, C
	JR Z, .forth_emit__loop

	LD HL, t_col
	INC (HL)
	JP forth_next


.forth_emit__other_char:
	CP 10

	; Run CR if 10
	LD DE, forth_cr
	LD HL, forth_colon_code
	CALL Z, forth_asm_call

	JP forth_next


	HEADER forth_bye, "BYE"
	DW $ + 2
forth_bye:
	RST 0x00


	HEADER forth_store, "!"
	DW $ + 2
forth_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP BC
	LD (HL), C
	INC HL
	LD (HL), B
	JP forth_next


	; : PAGE
	HEADER forth_page, "PAGE"
	DW forth_colon_code
forth_page:
	; 0 T-COL C!
	DB forth_zero_literal_tok
	DX t_col-2
	DB forth_c_store_tok
	; 0 T-ROW C!
	DB forth_zero_literal_tok
	DX t_row-2
	DB forth_c_store_tok
	; DISPLAY-FILE DISPLAY-SIZE ERASE
	DX display_file-2
	DX display_size-2
	DX forth_erase-2
	; ATTR-FILE ATTR-SIZE T-ATTR C@ FILL
	DX attr_file-2
	DX attr_size-2
	DX t_attr-2
	DB forth_c_fetch_tok
	DX forth_fill-2
	; ;
	DB forth_exit_tok


	HEADER forth_plus, "+"
	DW $ + 2
forth_plus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP BC
	ADD HL, BC
	PUSH HL
	JP forth_next


	HEADER forth_minus, "-"
	DW $ + 2
forth_minus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP BC
	POP HL
	OR A
	SBC HL, BC
	PUSH HL
	JP forth_next


	HEADER forth_zero_equals, "0="
	DW $ + 2
forth_zero_equals:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	LD A, L
	OR H
	JR NZ, .forth_zero_equals__not_equal
	DEC HL
	PUSH HL
	JP forth_next
.forth_zero_equals__not_equal:
	LD HL, 0
	PUSH HL
	JP forth_next


	HEADER forth_zero_less, "0<"
	DW $ + 2
forth_zero_less:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP AF
	AND 0x80
	JR Z, .forth_zero_less__non_negative
	LD HL, 0xFF
	PUSH HL
	JP forth_next
.forth_zero_less__non_negative:
	LD H, A
	LD L, A
	PUSH HL
	JP forth_next


	HEADER forth_one_plus, "1+"
	DW $ + 2
forth_one_plus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	INC HL
	PUSH HL
	JP forth_next


	HEADER forth_char_plus, "CHAR+"
	DW forth_one_plus - 2
forth_char_plus:


	HEADER forth_one_minus, "1-"
	DW $ + 2
forth_one_minus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	DEC HL
	PUSH HL
	JP forth_next


	HEADER forth_two_store, "2!"
	DW $ + 2
forth_two_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_3
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
	JP forth_next


	HEADER forth_two_star, "2*"
	DW $ + 2
forth_two_star:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	SLA L
	RL H
	PUSH HL
	JP forth_next


	HEADER forth_cells, "CELLS"
	DW forth_two_star
forth_cells:


	HEADER forth_exit, "EXIT"
	DW $ + 2
forth_exit:
	IF FORTH_CHECKED
		CALL forth_ret_holds_1
	ENDIF
	LD L, (IX+0)
	LD H, (IX+1)
	INC IX
	INC IX
	PUSH HL
	POP IY
	JP forth_next


	HEADER forth_two_slash, "2/"
	DW $ + 2
forth_two_slash:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP DE
	SRA D
	RR E
	PUSH DE
	JP forth_next


	HEADER forth_two_fetch, "2@"
	DW $ + 2
forth_two_fetch:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1_room_1
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
	JP forth_next


	HEADER forth_two_drop, "2DROP"
	DW $ + 2
forth_two_drop:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP HL
	JP forth_next


	HEADER forth_two_dup, "2DUP"
	DW $ + 2
forth_two_dup:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2_room_2
	ENDIF
	POP DE
	POP BC
	PUSH BC
	PUSH DE
	PUSH BC
	PUSH DE
	JP forth_next


	HEADER forth_two_over, "2OVER"
	DW $ + 2
forth_two_over:
	IF FORTH_CHECKED
		CALL forth_dat_holds_4_room_2
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
	JP forth_next


	HEADER forth_to_r, ">R"
	DW $ + 2
forth_to_r:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1_ret_room_1
	ENDIF
	POP BC
	DEC IX
	DEC IX
	LD (IX+0), C
	LD (IX+1), B
	JP forth_next


	HEADER forth_question_dup, "?DUP"
	DW $ + 2
forth_question_dup:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1_room_1
	ENDIF

	POP DE
	PUSH DE
	LD A, E
	OR D
	JR Z, .forth_question_dup__end
	PUSH DE
.forth_question_dup__end:
	JP forth_next


	HEADER forth_abort, "ABORT"
	DW $ + 2
forth_abort:
	LD SP, param_stack_top
	LD IX, return_stack_top
	JP forth_quit


	HEADER forth_abs, "ABS"
	DW $ + 2
forth_abs:
	POP BC
	LD A, B
	AND 0x80
	JP NZ, .forth_abs__neg
	PUSH BC
	JP forth_next
.forth_abs__neg:
	LD HL, 0
	OR A
	SBC HL, BC
	PUSH HL
	JP forth_next


	HEADER forth_align, "ALIGN"
	DW forth_next
forth_align:


	HEADER forth_aligned, "ALIGNED"
	DW forth_next
forth_aligned:


	HEADER forth_chars, "CHARS"
	DW forth_next
forth_chars:


	HEADER forth_allot, "ALLOT"
	DW $ + 2
forth_allot:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP DE
	LD HL, (forth_h)
	ADD HL, DE
	LD (forth_h), HL
	JP forth_next


	HEADER forth_and, "AND"
	DW $ + 2
forth_and:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
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
	JP forth_next


	HEADER forth_or, "OR"
	DW $ + 2
forth_or:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
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
	JP forth_next


	HEADER forth_xor, "XOR"
	DW $ + 2
forth_xor:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
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
	JP forth_next


	HEADER forth_bl, "BL"
	DW forth_constant_code
forth_bl:
	DW ' '


	HEADER forth_c_store, "C!"
	DW $ + 2
forth_c_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP DE
	LD (HL), E
	JP forth_next


	HEADER forth_c_comma, "C,"
	DW $ + 2
forth_c_comma:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP DE
	LD HL, (forth_h)
	LD (HL), E
	INC HL
	LD (forth_h), HL
	JP forth_next


	HEADER forth_c_fetch, "C@"
	DW $ + 2
forth_c_fetch:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	LD E, (HL)
	LD D, 0
	PUSH DE
	JP forth_next


	HEADER forth_cell_plus, "CELL+"
	DW $ + 2
forth_cell_plus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	INC HL
	INC HL
	PUSH HL
	JP forth_next


	HEADER forth_count, "COUNT"
	DW $ + 2
forth_count:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1_room_1
	ENDIF
	POP HL
	LD E, (HL)
	INC HL
	LD D, 0
	PUSH DE
	PUSH HL
	JP forth_next


	HEADER forth_depth, "DEPTH"
	DW forth_colon_code
forth_depth:
	DX forth_s_zero-2
	DX forth_tick_s-2
	DB forth_minus_tok
	DB forth_two_slash_tok
	DB forth_exit_tok


	HEADER forth_drop, "DROP"
	DW $ + 2
forth_drop:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	JP forth_next


	HEADER forth_dup, "DUP"
	DW $ + 2
forth_dup:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1_room_1
	ENDIF
	POP HL
	PUSH HL
	PUSH HL
	JP forth_next


	; : MAX 2DUP < IF SWAP THEN DROP ;
	HEADER forth_max, "MAX"
	DW forth_colon_code
forth_max:
	DB forth_two_dup_tok
	DB forth_less_than_tok
	DB forth_if_raw_tok
	DB .skip-$-1
	DB forth_swap_tok
.skip:
	DB forth_drop_tok
	DB forth_exit_tok


	HEADER forth_cmove, "CMOVE"
	DW $ + 2
forth_cmove:
	IF FORTH_CHECKED
		CALL forth_dat_holds_3
	ENDIF
	POP BC
	POP DE
	POP HL
	LD A, C
	OR B
	JP Z, forth_next
	LDIR
	JP forth_next


	HEADER forth_cmove_up, "CMOVE>"
	DW $ + 2
forth_cmove_up:
	IF FORTH_CHECKED
		CALL forth_dat_holds_3
	ENDIF
	POP BC
	POP HL
	POP DE
	LD A, C
	OR B
	JP Z, forth_next
	ADD HL, BC
	DEC HL
	EX DE, HL
	ADD HL, BC
	DEC HL
	LDDR
	JP forth_next


	HEADER forth_move, "MOVE"
	DW forth_colon_code
forth_move:
	; : MOVE -ROT 2DUP < IF ROT CMOVE> ELSE ROT CMOVE THEN ;
	DB forth_minus_rot_tok
	DB forth_two_dup_tok
	DB forth_less_than_tok
	DB forth_if_raw_tok
	DB .move__else-$-1
	DB forth_rot_tok
	DX forth_cmove_up-2
	DB forth_else_skip_tok
	DB .move__else_skip-$-1
.move__else:
	DB forth_rot_tok
	DX forth_cmove-2
.move__else_skip:
	DB forth_exit_tok


	; : NEGATE  0 SWAP - ;
	HEADER forth_negate, "NEGATE"
	DW forth_colon_code
forth_negate:
	DB forth_zero_literal_tok
	DB forth_swap_tok
	DB forth_minus_tok
	DB forth_exit_tok


	HEADER forth_question_do_raw, "(?DO)"
	DW $ + 2
forth_question_do_raw:
	IF FORTH_CHECKED
		CALL forth_ret_room_2
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
	JR Z, .forth_question_do_raw__jump

	; Push limit,iterator to ret stack
	DEC IX
	DEC IX
	LD (IX+0), E
	LD (IX+1), D
	DEC IX
	DEC IX
	LD (IX+0), C
	LD (IX+1), B

	JP forth_next
	
	; If iterator=limit skip loop
.forth_question_do_raw__jump:
	LD E, (IY-1)
	LD D, 0
	ADD IY, DE
	JP forth_next


	HEADER forth_loop_raw, "(LOOP)"
	DW $ + 2
forth_loop_raw:
	; DE = iterator
	LD E, (IX+0)
	LD D, (IX+1)
	; HL = limit
	LD L, (IX+2)
	LD H, (IX+3)
	INC DE
	OR A
	SBC HL, DE
	JR NZ, .forth_loop_raw__loop
	LD BC, 4
	ADD IX, BC
	INC IY
	JP forth_next
.forth_loop_raw__loop:
	LD (IX+0), E
	LD (IX+1), D
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
	JP forth_next


	; : TYPE 0 ?DO DUP C@ EMIT 1+ LOOP DROP ;
	HEADER forth_type, "TYPE"
	DW forth_colon_code
forth_type:
	DB forth_zero_literal_tok
	DB forth_question_do_raw_tok
	DB .forth_type__skip-$-1
.forth_type__loop:
	DB forth_dup_tok
	DB forth_c_fetch_tok
	DX forth_emit-2
	DB forth_one_plus_tok
	DB forth_loop_raw_tok
	DB .forth_type__loop-$+256
.forth_type__skip:
	DB forth_drop_tok
	DB forth_exit_tok


	HEADER forth_s_quote_raw, '(S")'
	DW $ + 2
forth_s_quote_raw:
	IF FORTH_CHECKED
		CALL forth_dat_room_2
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
	JP forth_next


	HEADER forth_over, "OVER"
	DW $ + 2
forth_over:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2_room_1
	ENDIF
	LD HL, 2
	ADD HL, SP
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	JP forth_next


	HEADER forth_reset_stacks, "RESET-STACKS"
	DW $ + 2
forth_reset_stacks:
	LD IX, return_stack_top
	LD SP, param_stack_top
	JP forth_next


	HEADER forth_quit, "QUIT"
	DW forth_colon_code
forth_quit:
	; : QUIT  BEGIN -' IF NUMBER ELSE EXECUTE THEN 0 UNTIL ;
	DX forth_reset_stacks-2
	; DW forth_interpret-2
	DB forth_exit_tok


	; : ERASE 0 FILL ;
	HEADER forth_erase, "ERASE"
	DW forth_colon_code
forth_erase:
	DB forth_zero_literal_tok
	DX forth_fill-2
	DB forth_exit_tok


	HEADER forth_fill, "FILL"
	DW $ + 2
forth_fill:
	IF FORTH_CHECKED
		CALL forth_dat_holds_3
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
	JP forth_next


	HEADER forth_fetch, "@"
	DW $ + 2
forth_fetch:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	JP forth_next


	HEADER forth_tick_s, "'S"
	DW $ + 2
forth_tick_s:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD HL, 0
	ADD HL, SP
	PUSH HL
	JP forth_next


	HEADER forth_rot, "ROT"
	DW $ + 2
forth_rot:
	IF FORTH_CHECKED
		CALL forth_dat_holds_3
	ENDIF
	POP HL
	POP DE
	POP BC
	PUSH DE
	PUSH HL
	PUSH BC
	JP forth_next


	HEADER forth_swap, "SWAP"
	DW $ + 2
forth_swap:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP DE
	PUSH HL
	PUSH DE
	JP forth_next


	HEADER forth_s_zero, "S0"
	DW forth_constant_code
forth_s_zero:
	DW param_stack_top


	HEADER forth_to_in, ">IN"
	DW forth_constant_code
forth_to_in:
	; TODO


	HEADER forth_minus_tick, "-'"
	DW $ + 2
forth_minus_tick:
	; TODO


	HEADER forth_minus_rot, "-ROT"
	DW $ + 2
forth_minus_rot:
	IF FORTH_CHECKED
		CALL forth_dat_holds_3
	ENDIF
	POP BC
	POP DE
	POP HL
	PUSH BC
	PUSH HL
	PUSH DE
	JP forth_next


	HEADER forth_less_than, "<"
	DW $ + 2
forth_less_than:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
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
	JP forth_next
.less_than__lt:
	DEC BC
	PUSH BC
	JP forth_next


	; : > SWAP < ;
	HEADER forth_greater_than, ">"
	DW forth_colon_code
forth_greater_than:
	DB forth_swap_tok
	DB forth_less_than_tok
	DB forth_exit_tok


	HEADER forth_nip, "NIP"
	DW $ + 2
forth_nip:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP DE
	PUSH HL
	JP forth_next


	HEADER forth_else_skip, "(ELSE)"
	DW $ + 2
forth_else_skip:
	INC IY
	LD E, (IY-1)
	LD D, 0
	ADD IY, DE
	JP forth_next


	HEADER repeat_raw, "(REPEAT)"
	DW $ + 2
repeat_raw:
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
	JP forth_next


	HEADER forth_c_plus_store, "C+!"
	DW $ + 2
forth_c_plus_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP BC
	LD A, (HL)
	ADD A, C
	LD (HL), A
	JP forth_next


	HEADER forth_c_literal, "C-LITERAL"
	DW $ + 2
forth_c_literal:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	INC IY
	LD E, (IY-1)
	LD D, 0
	PUSH DE
	JP forth_next


	HEADER forth_if_raw, "(IF)"
	DW $ + 2
forth_if_raw:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
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
	JP forth_next


	HEADER forth_ms, "MS"
	DW $ + 2
forth_ms:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP DE
.ms__ms_loop:
	LD A, E
	OR D
	JP Z, forth_next
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
	HEADER p_fetch, "P@"
	DW $ + 2
p_fetch:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP BC
	IN L, (C)
	LD H, 0
	PUSH HL
	JP forth_next


	; \ P! ( cx addr -- ) \ Write byte to port
	HEADER p_store, "P!"
	DW $ + 2
p_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP BC
	POP DE
	OUT (C), E
	JP forth_next


	HEADER ula, "ULA"
	DW forth_constant_code
ula:
	DW ula_val


	HEADER halt_, "HALT"
	DW $ + 2
halt_:
	HALT
	JP forth_next


	; : D+! ( n addr -- ) \ double in addr incremented by 2
	HEADER forth_d_plus_store, "D1+!"
	DW forth_colon_code
forth_d_plus_store:
	; DUP >R 2@ D+ R> 2! ;
	DB forth_dup_tok
	DB forth_to_r_tok
	DX forth_two_fetch-2
	DX forth_d_plus-2
	DB forth_r_from_tok
	DX forth_two_store-2
	DB forth_exit_tok


	HEADER forth_d_plus, "D+"
	DW $ + 2
forth_d_plus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_4
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
	JP forth_next


	; : (2LITERAL)  R>  DUP 2 CELLS + >R  2@  ;
	HEADER forth_two_literal_raw, "(2LITERAL)"
	DW forth_colon_code
forth_two_literal_raw:
	DB forth_r_from_tok
	DB forth_dup_tok
	DB forth_c_literal_tok
	DB 2
	DX forth_cells-2
	DB forth_plus_tok
	DB forth_to_r_tok
	DX forth_two_fetch-2
	DB forth_exit_tok


	HEADER forth_r_from, "R>"
	DW $ + 2
forth_r_from:
	IF FORTH_CHECKED
		CALL forth_dat_room_1_ret_holds_1
	ENDIF
	LD E, (IX+0)
	LD D, (IX+1)
	INC IX
	INC IX
	PUSH DE
	JP forth_next


	HEADER _di, "DI"
	DW $ + 2
_di:
	DI
	JP forth_next


	HEADER _ei, "EI"
	DW $ + 2
_ei:
	EI
	JP forth_next


	; AT-XY ( x y -- ) \ Set next terminal x,y position
	HEADER forth_at_xy, "AT-XY"
	DW forth_colon_code
forth_at_xy:
	; ( y ) 0 23 CLAMP T-ROW C!
	DB forth_zero_literal_tok
	DB forth_c_literal_tok
	DB 23
	DX clamp-2
	DX t_row-2
	DB forth_c_store_tok
	; ( x ) 0 63 CLAMP T-COL C!
	DB forth_zero_literal_tok
	DB forth_c_literal_tok
	DB 63
	DX clamp-2
	DX t_col-2
	DB forth_c_store_tok
	; ;
	DB forth_exit_tok


	HEADER keyq_len, "KEYQ-LEN"
	DW forth_constant_code
keyq_len:
	DW keyq_len_val


	; keyq items have two parts:
	; byte 1 = scancode which is 8*(4-bit)+(8-hrow_bit)
	;          (this gives an index into Key Table (a) in ROM disassembly)
	; byte 2 = flags: 1=up, 2=shift, 4=sym
	HEADER keyq, "KEYQ"
	DW forth_create_code
keyq:
	DS keyq_len_val * 2


	HEADER keyq_s, "KEYQ-S"
	DW forth_create_code
keyq_s:
	DB 0


	HEADER keyq_e, "KEYQ-E"
	DW forth_create_code
keyq_e:
	DB 0


	; : EKEY? ( -- flags ) \ Is a key event available?
	HEADER ekey_question, "EKEY?"
	DW forth_colon_code
ekey_question:
	; KEYQ-E C@ KEYQ-S C@ <> ;
	DX keyq_e-2
	DB forth_c_fetch_tok
	DX keyq_s-2
	DB forth_c_fetch_tok
	DB forth_not_equals_tok
	DB forth_exit_tok


	; 0<> ( n -- flags ) \ true if n is not equal to 0
	HEADER forth_zero_not_equals, "0<>"
	DW forth_colon_code
forth_zero_not_equals:
	; 0= INVERT ;
	DB forth_zero_equals_tok
	DB forth_invert_tok
	DB forth_exit_tok


	; <> ( n1 n2 -- flags ) \ true if n1 is not equal to n2
	HEADER forth_not_equals, "<>"
	DW forth_colon_code
forth_not_equals:
	; = INVERT ;
	DB forth_equals_tok
	DB forth_invert_tok
	DB forth_exit_tok


	; CLAMP ( n1 n2 n3 -- n ) \ Force n1 to range [n2, n3]
	HEADER clamp, "CLAMP"
	DW forth_colon_code
clamp:
	; ROT MIN MAX ;
	DB forth_rot_tok
	DX forth_min-2
	DX forth_max-2
	DB forth_exit_tok


	; : MIN ( n1 n2 -- n ) \ Leave the smaller of n1 and n2
	HEADER forth_min, "MIN"
	DW forth_colon_code
forth_min:
	; 2DUP > IF SWAP THEN DROP ;
	DB forth_two_dup_tok
	DB forth_greater_than_tok
	DB forth_if_raw_tok
	DB .skip-$-1
	DB forth_swap_tok
.skip:
	DB forth_drop_tok
	DB forth_exit_tok


	; : INVERT ( x -- y ) \ Invert all bits of x
	HEADER forth_invert, "INVERT"
	DW $ + 2
forth_invert:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	LD A, L
	CPL
	LD L, A
	LD A, H
	CPL
	LD H, A
	PUSH HL
	JP forth_next


	; EKEY ( -- x ) \ Push keyboard event when ready
	HEADER ekey, "EKEY"
	DW forth_colon_code
ekey:
	; BEGIN EKEY? HALT UNTIL \ Block for event
.begin:
	DX ekey_question-2
	DX halt_-2
	DB until_raw_tok
	DB $-1-.begin
	; KEYQ KEYQ-S C@ CELLS + @ \ Receive event
	DX keyq-2
	DX keyq_s-2
	DB forth_c_fetch_tok
	DB forth_two_star_tok
	DB forth_plus_tok
	DB forth_fetch_tok
	; KEYQ-S C@ 1+ 7 AND KEYQ-S C! \ Increment KEYQ-S (mod 8)
	DX keyq_s-2
	DB forth_c_fetch_tok
	DB forth_one_plus_tok
	DB forth_c_literal_tok
	DB 7
	DB forth_and_tok
	DX keyq_s-2
	DB forth_c_store_tok
	; ;
	DB forth_exit_tok


	HEADER until_raw, "(UNTIL)"
	DW $ + 2
until_raw:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	LD A, L
	OR H
	JR Z, .loop
	INC IY
	JP forth_next
.loop:
	LD C, (IY+0)
	XOR A
	LD B, A
	LD HL, 0
	INC BC
	SBC HL, BC
	EX DE, HL
	ADD IY, DE
	JP forth_next


	HEADER forth_equals, "="
	DW $ + 2
forth_equals:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP BC
	XOR A
	SBC HL, BC
	JR Z, .equal
	LD L, A
	LD H, A
	PUSH HL
	JP forth_next
.equal:
	DEC HL
	PUSH HL
	JP forth_next


	HEADER less_than_or_equal, "<="
	DW forth_colon_code
less_than_or_equal:
	DB forth_greater_than_tok
	DB forth_invert_tok
	DB forth_exit_tok


	HEADER greater_than_or_equal, ">="
	DW forth_colon_code
greater_than_or_equal:
	DB forth_less_than_tok
	DB forth_invert_tok
	DB forth_exit_tok


	; CODE 2>R
	HEADER two_to_r, "2>R"
	DW $ + 2
two_to_r:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2_ret_room_2
	ENDIF
	POP HL
	POP DE
	LD BC, -4
	ADD IX, BC
	LD (IX+0), L
	LD (IX+1), H
	LD (IX+2), E
	LD (IX+3), D
	JP forth_next


	; VARIABLE KSHIFT-STATE
	HEADER kshift_state, "KSHIFT-STATE"
	DW forth_create_code
kshift_state:
	DW 0


	; : KSCAN \ Update keyboard state
	HEADER kscan, "KSCAN"
	DW forth_colon_code
kscan:
	; \ Get state of shift keys for high byte of event
	; $FEFE P@ 1 AND  $7FFE P@ 2 AND  OR  8 LSHIFT  KSHIFT-STATE !
	DB forth_literal_raw_tok
	DW 0xFEFE
	DX p_fetch-2
	DB forth_one_literal_tok
	DB forth_and_tok
	DB forth_literal_raw_tok
	DW 0x7FFE
	DX p_fetch-2
	DB forth_c_literal_tok
	DB 2
	DB forth_and_tok
	DB forth_or_tok
	DB forth_c_literal_tok
	DB 8
	DB lshift_tok
	DX kshift_state
	; \ Call per-row word
	; 8 0 DO I KSCAN-ROW LOOP ;
	DB forth_c_literal_tok
	DB 8
	DB forth_zero_literal_tok
	DB two_to_r_tok
.do:
	DB forth_r_from_tok
	DX kscan_row-2
	DB forth_loop_raw_tok
	DB .do-$+256
	DB forth_exit_tok


	; ( n ) : KSCAN-ROW \ Scan+update a given half-row
	HEADER kscan_row, "KSCAN-ROW"
	DW forth_colon_code
kscan_row:
	; \ Read row state
	; ( n ) 1  OVER LSHIFT  INVERT  8 LSHIFT ULA OR  P@  $1F AND
	DB forth_one_literal_tok
	DB over_tok
	DB lshift_tok
	DB forth_invert_tok
	DB forth_c_literal_tok
	DB 8
	DB lshift_tok
	DX ula-2
	DB forth_or_tok
	DX p_fetch-2
	DB forth_c_literal_tok
	DB 0x1F
	DB forth_and_tok
	; \ Calculate state address
	; ( n state ) OVER CHARS KSTATE +
	DB over_tok
	DX kstate-2
	DB forth_plus_tok
	; \ Update when state changes
	; ( n state addr ) 2DUP C@ XOR
	DB forth_two_dup_tok
	DB forth_c_fetch_tok
	DB forth_xor_tok
	; ( n state addr xor'd )  BEGIN ?DUP WHILE
.begin:
	DB question_dup_tok
	DB forth_if_raw_tok
	DB .repeat-$-1
	;         \ Get lowest bit 
	; REPEAT
	DB repeat_raw_tok
	DB .begin-$+256
.repeat:


	; \ Stores scanned key bits from the last scan
	; CREATE KSTATE  8 CHARS  ALLOT
	; KSTATE  8 CHARS  ERASE
	HEADER kstate, "KSTATE"
	DW forth_create_code
kstate:
	DS 8


	; \ Stores last key press
	; CREATE KLAST  0 C,
	HEADER klast, "KLAST"
	DW forth_create_code
klast:
	DB 0


	; CODE LSHIFT ( x u -- x<<u )
	HEADER lshift, "LSHIFT"
	DW $ + 2
lshift:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
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
	JP forth_next
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

; vim:set syntax=z80:

; Current data space pointer
	DW forth_create_code
forth_h:
	DW forth_h_init


; non-zero while compiling
	DW forth_create_code
forth_state:
	DW 0


	DW forth_create_code
frames:
	DW 0
	DW 0


	DW forth_create_code
t_attr:
	DB 7


	DW forth_create_code
t_col:
	DB 0


	DW forth_create_code
t_row:
	DB 0


	DW forth_constant_code
display_file:
	DW display_file_val


	DW forth_constant_code
display_size:
	DW display_size_val


	DW forth_constant_code
attr_file:
	DW attr_file_val


	DW forth_constant_code
attr_size:
	DW attr_size_val


	DW forth_create_code
tick_int:
	DW int-2


	; Default interrupt handler
	; : INT
	DW forth_colon_code
int:
	; 1. FRAMES D+! \ increment FRAMES
	DX forth_two_literal_raw-2
	DW 0
	DW 1
	DX frames-2
	DX forth_d_plus_store-2
	; ;
	DB forth_exit_tok


	; : MAIN  PAGE
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
	DB $-1-.main__loop
.main__loop_skip:
	DB forth_s_quote_raw_tok
	DB .main__s2_end-$-1
	DM "Hello, world!\n\n"
.main__s2_end:
	DX forth_type-2
	; (hang) ;  \ do not return, TODO: replace with forth loop
	DX forth_hang-2
	DB forth_exit_tok ; will never be reached


	DW $ + 2
forth_zero_literal:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD BC, 0
	PUSH BC
	JP forth_next


	DW $ + 2
forth_one_literal:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD BC, 1
	PUSH BC
	JP forth_next


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


	DW $ + 2
forth_bye:
	RST 0x00


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


	DW $ + 2
forth_one_plus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	INC HL
	PUSH HL
	JP forth_next

	DW forth_one_plus - 2
forth_char_plus:


	DW $ + 2
forth_one_minus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	DEC HL
	PUSH HL
	JP forth_next


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


	DW forth_two_star
forth_cells:


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


	DW $ + 2
forth_two_drop:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP HL
	JP forth_next


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


	DW $ + 2
forth_abort:
	LD SP, param_stack_top
	LD IX, return_stack_top
	JP forth_quit


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


	DW forth_next
forth_align:


	DW forth_next
forth_aligned:


	DW forth_next
forth_chars:


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


	DW forth_constant_code
forth_bl:
	DW ' '


	DW $ + 2
forth_c_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP DE
	LD (HL), E
	JP forth_next


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


	DW forth_colon_code
forth_depth:
	DX forth_s_zero-2
	DX forth_tick_s-2
	DB forth_minus_tok
	DB forth_two_slash_tok
	DB forth_exit_tok


	DW $ + 2
forth_drop:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	JP forth_next


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
	DW forth_colon_code
forth_negate:
	DB forth_zero_literal_tok
	DB forth_swap_tok
	DB forth_minus_tok
	DB forth_exit_tok


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
	LD A, (IY+0)
	NEG
	LD E, A
	LD D, 0xFF
	DEC DE
	ADD IY, DE
	JP forth_next


	; : TYPE 0 ?DO DUP C@ EMIT 1+ LOOP DROP ;
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
	DB $-1-.forth_type__loop
.forth_type__skip:
	DB forth_drop_tok
	DB forth_exit_tok


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


	DW $ + 2
forth_reset_stacks:
	LD IX, return_stack_top
	LD SP, param_stack_top
	JP forth_next


	DW forth_colon_code
forth_quit:
	; : QUIT  BEGIN -' IF NUMBER ELSE EXECUTE THEN 0 UNTIL ;
	DX forth_reset_stacks-2
	; DW forth_interpret-2
	DB forth_exit_tok


	; : ERASE 0 FILL ;
	DW forth_colon_code
forth_erase:
	DB forth_zero_literal_tok
	DX forth_fill-2
	DB forth_exit_tok


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
	JP Z, forth_next
	LD (HL), E
	DEC BC
	LD A, B
	OR C
	JP Z, forth_next
	LD E, L
	LD D, H
	INC DE
	LDIR
	JP forth_next


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


	DW $ + 2
forth_tick_s:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD HL, 0
	ADD HL, SP
	PUSH HL
	JP forth_next


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


	DW forth_constant_code
forth_s_zero:
	DW param_stack_top


	DW forth_constant_code
in_buf:
	DW in_buf_val


	DW forth_constant_code
in_size:
	DW in_size_val


	DW forth_constant_code
forth_to_in:
	; TODO

	DW $ + 2
forth_minus_tick:
	; TODO


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
	DW forth_colon_code
forth_greater_than:
	DB forth_swap_tok
	DB forth_less_than_tok
	DB forth_exit_tok


	DW $ + 2
forth_nip:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP DE
	PUSH HL
	JP forth_next


	DW $ + 2
forth_else_skip:
	INC IY
	LD E, (IY-1)
	LD D, 0
	ADD IY, DE
	JP forth_next


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


	DW $ + 2
forth_p_fetch:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP BC
	IN L, (C)
	LD H, 0
	PUSH HL
	JP forth_next


	DW $ + 2
forth_p_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP BC
	POP DE
	OUT (C), E
	JP forth_next


	DW forth_constant_code
ula:
	DW ula_val


	DW $ + 2
halt_:
	HALT
	JP forth_next


	; : D+! ( n addr -- ) \ double in addr incremented by 2
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


	DW $ + 2
_di:
	DI
	JP forth_next


	DW $ + 2
_ei:
	EI
	JP forth_next


	; AT-XY ( x y -- ) \ Set next terminal x,y position
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


	DW forth_constant_code
keyq_len:
	DW keyq_len_val


	; keyq items have two parts:
	; byte 1 = scancode which is 8*(4-bit)+(8-hrow_bit)
	;          (this gives an index into Key Table (a) in ROM disassembly)
	; byte 2 = flags: 1=up, 2=shift, 4=sym
	DW forth_create_code
keyq:
	DS keyq_len_val * 2


	DW forth_create_code
keyq_s:
	DB 0


	DW forth_create_code
keyq_e:
	DB 0


	; : EKEY? ( -- flags ) \ Is a key event available?
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
	DW forth_colon_code
forth_zero_not_equals:
	; 0= INVERT ;
	DB forth_zero_equals_tok
	DB forth_invert_tok
	DB forth_exit_tok


	; <> ( n1 n2 -- flags ) \ true if n1 is not equal to n2
	DW forth_colon_code
forth_not_equals:
	; = INVERT ;
	DB forth_equals_tok
	DB forth_invert_tok
	DB forth_exit_tok


	; CLAMP ( n1 n2 n3 -- n ) \ Force n1 to range [n2, n3]
	DW forth_colon_code
clamp:
	; ROT MIN MAX ;
	DB forth_rot_tok
	DX forth_min-2
	DX forth_max-2
	DB forth_exit_tok


	; : MIN ( n1 n2 -- n ) \ Leave the smaller of n1 and n2
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
	DW forth_colon_code
ekey:
	; BEGIN EKEY? UNTIL \ Wait for event
.begin:
	DX ekey_question-2
	DB until_raw_tok
	DB $-1-.begin
	; KEYQ-S >R R@ C@ CELLS  KEYQ  +  @
	DX keyq_s-2
	DB forth_c_fetch_tok
	DB forth_two_star_tok
	DX keyq-2
	DB forth_plus_tok
	DB forth_fetch_tok
	; R@ C@ 1+ $7 AND R> C! ;


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


	DW forth_colon_code
less_than_or_equal:
	DB forth_greater_than_tok
	DB forth_invert_tok
	DB forth_exit_tok


	DW forth_colon_code
greater_than_or_equal:
	DB forth_less_than_tok
	DB forth_invert_tok
	DB forth_exit_tok

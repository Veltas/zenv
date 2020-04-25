; vim:set syntax=z80:

; ZENV - bootstrapping a Forth system

DX: MACRO addr
	DB (addr) >> 8
	DB (addr) & 0xFF
	ENDM

; Globals

FORTH_CHECKED: EQU 1
display_file_val: EQU 0x4000
display_size_val: EQU 0x1800
attr_file_val: EQU 0x5800
attr_size_val: EQU 0x300
ula_val: EQU 0xFE
in_buf_val: EQU 0x5B00
in_size_val: EQU 255
keyq_len_val: EQU 8
	; Address data stack starts at
param_stack_top: EQU 0x90F0
param_stack_size: EQU 0xE0
	; Address return stack starts at
return_stack_top: EQU 0x91F0
return_stack_size: EQU 0xE0

;throw_addr: EQU 11
;throw_stack: EQU 13
;throw_ret_stack: EQU 15

	ORG 0x8000

	; Init interrupt handler
	DI
	LD A, 0x18 ; JR (to make 0xFFFF: JR 0xFFF4)
	LD (0xFFFF), A

	; Set interrupt vector to 257 bytes of 0xFF, i.e. vector to 0xFFFF
	IM 2
	LD A, 0x3B
	LD I, A

	; Init 0xFFF4 code to jump to our handler code
	LD HL, 0xFFF4
	; Compile "JP interrupt"
	LD (HL), 0xC3
	INC HL
	LD (HL), interrupt & 0xFF
	INC HL
	LD (HL), interrupt >> 8

	; Enable interrupts
	EI

	; Init vars
	; FIXME

	; Init parameter stack
	LD SP, param_stack_top

	; Init return stack
	LD IX, return_stack_top

	; Run forth
	LD IY, forth_main
	JP forth_next


	; Interrupt handling body
interrupt:
	EX AF, AF'
	EXX
	PUSH IX
	PUSH IY
	LD (.interrupt__save_sp), SP
	LD BC, -4
	ADD IX, BC ; Leave room on return stack, less likely to clobber bad asm

	; Call forth interrupt routine

	; Restore context, return from interrupt
	LD SP, (.interrupt__save_sp)
	POP IY
	POP IX
	EXX
	EX AF, AF'
	EI
	RETI


.interrupt__save_sp:
	DW 0

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

	; Stores scanned key bits from the last scan
scan_state:
	DS 8

	; Auto-repeat control
	; Bit 0: enable auto-repeat
	; Bit 1: auto-repeat repeating
	; Bit 2: last event was press
auto_repeat:
	DB 0x01

	; Stores last key press
last_key:
	DB 0

repeat_wait_init: EQU 45  ; 0.9s
repeat_repeat_init: EQU 5 ; 0.1s
	; Used to time waiting for repeats and repeating, cleared on every new
	; key event.
repeat_timer:
	DB 0


forth_next:
	LD A, (IY+0)
	INC IY
	CP 0x80
	JR NC, .next__not_token
	ADD A, A
	LD L, A
	LD H, 0
	LD BC, tokens
	ADD HL, BC
	; Get token code pointer address
	LD E, (HL)
	INC HL
	LD D, (HL)
	EX DE, HL
.next__got_code_ptr:
	; DE = param, HL = code pointer
	LD E, (HL)
	INC HL
	LD D, (HL)
	INC HL
	EX DE, HL
	; Execute code
	JP (HL)
.next__not_token:
	; HL = code pointer address
	LD H, A
	LD L, (IY+0)
	INC IY
	JR .next__got_code_ptr


forth_colon_code:
	IF FORTH_CHECKED
		CALL forth_ret_room_1
	ENDIF
	PUSH DE
	EX (SP), IY
	POP DE
	DEC IX
	DEC IX
	LD (IX+0), E
	LD (IX+1), D
	JR forth_next


forth_do_does:
	; Push parameter field address to stack, DE = asm snippet
	EX DE, HL
	PUSH HL
	IF FORTH_CHECKED
		CALL forth_dat_room_0
	ENDIF
	; Forward DE to colon def
	INC DE
	INC DE
	INC DE
	; Call that colon def
	JR forth_colon_code


forth_constant_code:
forth_constant_does:
	JP forth_do_does
	DB forth_fetch_tok
	DB forth_exit_tok


forth_two_constant_does:
	JP forth_do_does
	DX forth_two_fetch-2
	DB forth_exit_tok


;forth_constant_code:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	EX DE, HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	JR forth_next


forth_2constant_code:
	IF FORTH_CHECKED
		CALL forth_dat_room_2
	ENDIF
	EX DE, HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	INC HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	PUSH DE
	JR forth_next


forth_create_code:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	PUSH DE
	JR forth_next


	; CALL this address, with HL set to asm code to call, uses forth_next
	; to return to calling code.
forth_asm_call:
	IF FORTH_CHECKED
		PUSH HL
		CALL forth_ret_room_2
		POP HL
	ENDIF
	; Push IP to ret stack
	PUSH IY
	POP BC
	DEC IX
	DEC IX
	LD (IX+0), C
	LD (IX+1), B
	; Move return pointer to ret stack
	POP BC
	DEC IX
	DEC IX
	LD (IX+0), C
	LD (IX+1), B
	; Set IP to instruction below
	LD IY, forth_asm_call_param
	; Jump to provided HL
	JP (HL)
forth_asm_call_param:
	DX forth_asm_exit_cp
forth_asm_exit_cp:
	DW $ + 2
forth_asm_exit:
	IF FORTH_CHECKED
		CALL forth_ret_holds_1
	ENDIF
	LD L, (IX+0)
	LD H, (IX+1)
	INC IX
	INC IX
	LD C, (IX+0)
	LD B, (IX+1)
	INC IX
	INC IX
	PUSH BC
	POP IY
	JP (HL)


forth_dat_room_0:
forth_dat_room_1:
forth_dat_room_2:
forth_dat_holds_1:
forth_dat_holds_1_room_1:
forth_dat_holds_2_room_1:
forth_dat_holds_1_ret_room_1:
forth_dat_room_1_ret_holds_1:
forth_dat_holds_2:
forth_dat_holds_2_room_2:
forth_dat_holds_3:
forth_dat_holds_4:
forth_dat_holds_4_room_2:
forth_ret_room_1:
forth_ret_room_2:
forth_ret_holds_1:
forth_ret_holds_2:
	RET

	DW $+2
forth_hang:
	JR $

tokens:
forth_literal_raw_tok: EQU ($-tokens)/2
	DW forth_literal_raw-2
forth_c_literal_tok: EQU ($-tokens)/2
	DW forth_c_literal-2
forth_zero_literal_tok: EQU ($-tokens)/2
	DW forth_zero_literal-2
forth_one_literal_tok: EQU ($-tokens)/2
	DW forth_one_literal-2
forth_dup_tok: EQU ($-tokens)/2
	DW forth_dup-2
forth_swap_tok: EQU ($-tokens)/2
	DW forth_swap-2
forth_rot_tok: EQU ($-tokens)/2
	DW forth_rot-2
forth_fetch_tok: EQU ($-tokens)/2
	DW forth_fetch-2
forth_store_tok: EQU ($-tokens)/2
	DW forth_store-2
forth_c_fetch_tok: EQU ($-tokens)/2
	DW forth_c_fetch-2
forth_c_store_tok: EQU ($-tokens)/2
	DW forth_c_store-2
forth_s_quote_raw_tok: EQU ($-tokens)/2
	DW forth_s_quote_raw-2
forth_dot_quote_raw_tok: EQU ($-tokens)/2
	DW 0 ; DW forth_dot_quote_raw-2
forth_exit_tok: EQU ($-tokens)/2
	DW forth_exit-2
forth_loop_raw_tok: EQU ($-tokens)/2
	DW forth_loop_raw-2
forth_one_plus_tok: EQU ($-tokens)/2
	DW forth_one_plus-2
forth_question_do_raw_tok: EQU ($-tokens)/2
	DW forth_question_do_raw-2
forth_plus_tok: EQU ($-tokens)/2
	DW forth_plus-2
forth_minus_tok: EQU ($-tokens)/2
	DW forth_minus-2
forth_two_slash_tok: EQU ($-tokens)/2
	DW forth_two_slash-2
forth_if_raw_tok: EQU ($-tokens)/2
	DW forth_if_raw-2
forth_else_skip_tok: EQU ($-tokens)/2
	DW forth_else_skip-2
forth_two_dup_tok: EQU ($-tokens)/2
	DW forth_two_dup-2
forth_nip_tok: EQU ($-tokens)/2
	DW forth_nip-2
forth_less_than_tok: EQU ($-tokens)/2
	DW forth_less_than-2
forth_greater_than_tok: EQU ($-tokens)/2
	DW forth_greater_than-2
forth_minus_rot_tok: EQU ($-tokens)/2
	DW forth_minus_rot-2
forth_drop_tok: EQU ($-tokens)/2
	DW forth_drop-2
forth_c_plus_store_tok: EQU ($-tokens)/2
	DW forth_c_plus_store-2
forth_r_from_tok: EQU ($-tokens)/2
	DW forth_r_from-2
forth_to_r_tok: EQU ($-tokens)/2
	DW forth_to_r-2
forth_zero_equals_tok: EQU ($-tokens)/2
	DW forth_zero_equals-2
forth_zero_not_equals_tok: EQU ($-tokens)/2
	DW forth_zero_not_equals-2
forth_equals_tok: EQU ($-tokens)/2
	DW forth_equals-2
forth_not_equals_tok: EQU ($-tokens)/2
	DW forth_not_equals-2
forth_invert_tok: EQU ($-tokens)/2
	DW forth_invert-2
forth_two_star_tok: EQU ($-tokens)/2
	DW forth_two_star-2
until_raw_tok: EQU ($-tokens)/2
	DW until_raw-2

	DS tokens+128*2 - $


	INCLUDE "font.asm"


dictionary_start:


; Current data space pointer
forth_h__dict:
	DW 0
	DB 1
	DM "H"
	DW forth_create_code
forth_h:
	DW forth_h_init


; non-zero while compiling
forth_state__dict:
	DW forth_h__dict
	DB 5
	DM "STATE"
	DW forth_create_code
forth_state:
	DW 0


frames__dict:
	DW forth_state__dict
	DB 6
	DM "FRAMES"
	DW forth_create_code
frames:
	DW 0
	DW 0


t_attr__dict:
	DW frames__dict
	DB 6
	DM "T-ATTR"
	DW forth_create_code
t_attr:
	DB 7


t_col__dict:
	DW t_attr__dict
	DB 5
	DM "T-COL"
	DW forth_create_code
t_col:
	DB 0


t_row__dict:
	DW t_col__dict
	DB 5
	DM "T-ROW"
	DW forth_create_code
t_row:
	DB 0


display_file__dict:
	DW t_row__dict
	DB 12
	DM "DISPLAY-FILE"
	DW forth_constant_code
display_file:
	DW display_file_val


display_size__dict:
	DW display_file__dict
	DB 12
	DM "DISPLAY-SIZE"
	DW forth_constant_code
display_size:
	DW display_size_val


attr_file__dict:
	DW display_size__dict
	DB 12
	DM "ATTR-FILE"
	DW forth_constant_code
attr_file:
	DW attr_file_val


attr_size__dict:
	DW attr_file__dict
	DB 12
	DM "ATTR-SIZE"
	DW forth_constant_code
attr_size:
	DW attr_size_val


tick_int__dict:
	DW attr_size__dict
	DB 4
	DM "'INT"
	DW forth_create_code
tick_int:
	DW int


	; Default interrupt handler
	; : INT
int__dict:
	DW tick_int__dict
	DB 3
	DM "INT"
	DW forth_colon_code
int:
	; 1. FRAMES D+! \ increment FRAMES
	DX forth_two_literal_raw-2
	DW 1
	DW 0
	DX frames-2
	DX forth_d_plus_store-2
	; ;
	DB forth_exit_tok


	; : MAIN  PAGE
forth_main__dict:
	DW int__dict
	DB 4
	DM "MAIN"
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


forth_zero_literal__dict:
	DW forth_main__dict
	DB 1
	DM "0"
	DW $ + 2
forth_zero_literal:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD BC, 0
	PUSH BC
	JP forth_next


forth_one_literal__dict:
	DW forth_zero_literal__dict
	DB 1
	DM "1"
	DW $ + 2
forth_one_literal:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD BC, 1
	PUSH BC
	JP forth_next


forth_literal_raw__dict:
	DW forth_one_literal__dict
	DB 9
	DM "(LITERAL)"
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
scroll__dict:
	DW forth_literal_raw__dict
	DB 5
	DM "SCROLL"
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
forth_cr__dict:
	DW scroll__dict
	DB 2
	DM "CR"
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


forth_emit__dict:
	DW forth_cr__dict
	DB 4
	DM "EMIT"
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


forth_bye__dict:
	DW forth_emit__dict
	DB 3
	DM "BYE"
	DW $ + 2
forth_bye__param:
	RST 0x00


forth_store__dict:
	DW forth_main__dict
	DB 1
	DM "!"
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
forth_page__dict:
	DW forth_store__dict
	DB 4
	DM "PAGE"
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


forth_plus__dict:
	DW forth_page__dict
	DB 1
	DM "+"
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


forth_minus__dict:
	DW forth_plus__dict
	DB 1
	DM "-"
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


forth_zero_equals__dict:
	DW forth_minus__dict
	DB 2
	DM "0="
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


forth_zero_less__dict:
	DW forth_zero_equals__dict
	DB 2
	DM "0<"
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


forth_one_plus__dict:
	DW forth_zero_less__dict
	DB 2
	DM "1+"
	DW $ + 2
forth_one_plus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	INC HL
	PUSH HL
	JP forth_next


forth_char_plus__dict:
	DW forth_one_plus__dict
	DB 5
	DM "CHAR+"
	DW forth_one_plus - 2


forth_one_minus__dict:
	DW forth_char_plus__dict
	DB 2
	DM "1-"
	DW $ + 2
forth_one_minus:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	DEC HL
	PUSH HL
	JP forth_next


forth_two_store__dict:
	DW forth_one_minus__dict
	DB 2
	DM "2!"
	DW $ + 2
forth_two_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_3
	ENDIF
	POP HL
	POP BC
	POP DE
	LD (HL), E
	INC HL
	LD (HL), D
	INC HL
	LD (HL), C
	INC HL
	LD (HL), B
	JP forth_next


forth_two_star__dict:
	DW forth_two_store__dict
	DB 2
	DM "2*"
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


forth_cells__dict:
	DW forth_two_star__dict
	DB 5
	DM "CELLS"
	DW forth_two_star
forth_cells:


forth_exit__dict:
	DW forth_cells__dict
	DB 4
	DM "EXIT"
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


forth_two_slash__dict:
	DW forth_exit__dict
	DB 2
	DM "2/"
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


forth_two_fetch__dict:
	DW forth_two_slash__dict
	DB 2
	DM "2@"
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
	PUSH DE
	PUSH BC
	JP forth_next


forth_two_drop__dict:
	DW forth_two_fetch__dict
	DB 5
	DM "2DROP"
	DW $ + 2
forth_two_drop:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP HL
	JP forth_next


forth_two_dup__dict:
	DW forth_two_drop__dict
	DB 4
	DM "2DUP"
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


forth_two_over__dict:
	DW forth_two_dup__dict
	DB 5
	DM "2OVER"
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


forth_to_r__dict:
	DW forth_two_over__dict
	DB 2
	DM ">R"
	DW $ + 2
forth_to_r:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1_ret_room_1
	ENDIF
	POP BC
	DEC IX
	DEC IX
	LD (IX+0), C
	LD (IX+1), D
	JP forth_next


forth_question_dup__dict:
	DW forth_to_r__dict
	DB 4
	DM "?DUP"
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


forth_abort__dict:
	DW forth_question_dup__dict
	DB 5
	DM "ABORT"
	DW $ + 2
forth_abort:
	LD SP, param_stack_top
	LD IX, return_stack_top
	JP forth_quit


forth_abs__dict:
	DW forth_abort__dict
	DB 3
	DM "ABS"
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


forth_align__dict:
	DW forth_abs__dict
	DB 0x85
	DM "ALIGN"
	DW forth_next


forth_aligned__dict:
	DW forth_align__dict
	DB 0x87
	DM "ALIGNED"
	DW forth_next


forth_chars__dict:
	DW forth_aligned__dict
	DB 0x85
	DM "CHARS"
	DW forth_next


forth_allot__dict:
	DW forth_chars__dict
	DB 5
	DM "ALLOT"
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


forth_and__dict:
	DW forth_allot__dict
	DB 3
	DM "AND"
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

forth_or__dict:
	DW forth_and__dict
	DB 2
	DM "OR"
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

forth_xor__dict:
	DW forth_or__dict
	DB 3
	DM "XOR"
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


forth_bl__dict:
	DW forth_xor__dict
	DB 2
	DM "BL"
	DW forth_constant_code
forth_bl:
	DW ' '


forth_c_store__dict:
	DW forth_bl__dict
	DB 2
	DM "C!"
	DW $ + 2
forth_c_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP DE
	LD (HL), E
	JP forth_next


forth_c_comma__dict:
	DW forth_c_store__dict
	DB 2
	DM "C,"
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


forth_c_fetch__dict:
	DW forth_c_comma__dict
	DB 2
	DM "C@"
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


forth_cell_plus__dict:
	DW forth_c_fetch__dict
	DB 5
	DM "CELL+"
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


forth_count__dict:
	DW forth_cell_plus__dict
	DB 5
	DM "COUNT"
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


forth_depth__dict:
	DW forth_count__dict
	DB 5
	DM "DEPTH"
	DW forth_colon_code
forth_depth:
	DX forth_s_zero-2
	DX forth_tick_s-2
	DB forth_minus_tok
	DB forth_two_slash_tok
	DB forth_exit_tok


forth_drop__dict:
	DW forth_depth__dict
	DB 4
	DM "DROP"
	DW $ + 2
forth_drop:
	IF FORTH_CHECKED
		CALL forth_dat_holds_1
	ENDIF
	POP HL
	JP forth_next


forth_dup__dict:
	DW forth_drop__dict
	DB 3
	DM "DUP"
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
forth_max__dict:
	DW forth_dup__dict
	DB 3
	DM "MAX"
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


forth_cmove__dict:
	DW forth_max__dict
	DB 5
	DM "CMOVE"
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


forth_cmove_up__dict:
	DW forth_cmove__dict
	DB 6
	DM "CMOVE>"
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


forth_move__dict:
	DW forth_cmove_up__dict
	DB 4
	DM "MOVE"
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
forth_negate__dict:
	DW forth_move__dict
	DB 6
	DM "NEGATE"
	DW forth_colon_code
forth_negate:
	DB forth_zero_literal_tok
	DB forth_swap_tok
	DB forth_minus_tok
	DB forth_exit_tok


forth_question_do_raw__dict:
	DW forth_negate__dict
	DB 4
	DM "(?DO)"
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


forth_loop_raw__dict:
	DW forth_question_do_raw__dict
	DB 6
	DM "(LOOP)"
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
forth_type__dict:
	DW forth_loop_raw__dict
	DB 4
	DM "TYPE"
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


forth_s_quote_raw__dict:
	DW forth_type__dict
	DB 4
	DM '(S")'
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


forth_over__dict:
	DW forth_s_quote_raw__dict
	DB 4
	DM "OVER"
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


forth_reset_stacks__dict:
	DW forth_over__dict
	DB 12
	DM "RESET-STACKS"
	DW $ + 2
forth_reset_stacks:
	LD IX, return_stack_top
	LD SP, param_stack_top
	JP forth_next


forth_quit__dict:
	DW forth_reset_stacks__dict
	DB 4
	DM "QUIT"
	DW forth_colon_code
forth_quit:
	; : QUIT  BEGIN -' IF NUMBER ELSE EXECUTE THEN 0 UNTIL ;
	DX forth_reset_stacks-2
	; DW forth_interpret-2
	DB forth_exit_tok


	; : ERASE 0 FILL ;
forth_erase__dict:
	DW forth_quit__dict
	DB 5
	DM "ERASE"
	DW forth_colon_code
forth_erase:
	DB forth_zero_literal_tok
	DX forth_fill-2
	DB forth_exit_tok


forth_fill__dict:
	DW forth_erase__dict
	DB 4
	DM "FILL"
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


forth_fetch__dict:
	DW forth_fill__dict
	DB 1
	DM "@"
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


forth_tick_s__dict:
	DW forth_fetch__dict
	DB 2
	DM "'S"
	DW $ + 2
forth_tick_s:
	IF FORTH_CHECKED
		CALL forth_dat_room_1
	ENDIF
	LD HL, 0
	ADD HL, SP
	PUSH HL
	JP forth_next


forth_rot__dict:
	DW forth_tick_s__dict
	DB 3
	DM "ROT"
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


forth_swap__dict:
	DW forth_rot__dict
	DB 4
	DM "SWAP"
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


forth_s_zero__dict:
	DW forth_swap__dict
	DB 2
	DM "S0"
	DW forth_constant_code
forth_s_zero:
	DW param_stack_top


in_buf__dict:
	DW forth_s_zero__dict
	DB 6
	DM "IN-BUF"
	DW forth_constant_code
in_buf:
	DW in_buf_val


in_size__dict:
	DW in_buf__dict
	DB 7
	DM "IN-SIZE"
	DW forth_constant_code
in_size:
	DW in_size_val


forth_to_in__dict:
	DW in_size__dict
	DB 3
	DM ">IN"
	DW forth_constant_code
forth_to_in:
	; TODO

forth_minus_tick__dict:
	DW forth_to_in__dict
	DB 2
	DM "-'"
	DW $ + 2
forth_minus_tick:
	; TODO


forth_minus_rot__dict:
	DW forth_minus_tick__dict
	DB 4
	DM "-ROT"
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


forth_less_than__dict:
	DW forth_minus_rot__dict
	DB 1
	DM "<"
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
forth_greater_than__dict:
	DW forth_less_than__dict
	DB 1
	DM ">"
	DW forth_colon_code
forth_greater_than:
	DB forth_swap_tok
	DB forth_less_than_tok
	DB forth_exit_tok


forth_nip__dict:
	DW forth_greater_than__dict
	DB 3
	DM "NIP"
	DW $ + 2
forth_nip:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP HL
	POP DE
	PUSH HL
	JP forth_next


forth_else_skip__dict:
	DW forth_nip__dict
	DB 6
	DM "(ELSE)"
	DW $ + 2
forth_else_skip:
	INC IY
	LD E, (IY-1)
	LD D, 0
	ADD IY, DE
	JP forth_next


forth_c_plus_store__dict:
	DW forth_else_skip__dict
	DB 3
	DM "C+!"
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


forth_c_literal__dict:
	DW forth_c_plus_store__dict
	DB 9
	DM "C-LITERAL"
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


forth_if_raw__dict:
	DW forth_c_literal__dict
	DB 4
	DM "(IF)"
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


forth_ms__dict:
	DW forth_if_raw__dict
	DB 2
	DM "MS"
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


forth_p_fetch__dict:
	DW forth_ms__dict
	DB 3
	DM "P@"
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


forth_p_store__dict:
	DW forth_p_fetch__dict
	DB 3
	DM "P!"
	DW $ + 2
forth_p_store:
	IF FORTH_CHECKED
		CALL forth_dat_holds_2
	ENDIF
	POP BC
	POP DE
	OUT (C), E
	JP forth_next


ula__dict:
	DW forth_p_store__dict
	DB 3
	DM "ULA"
	DW forth_constant_code
ula:
	DW ula_val


halt__dict:
	DW ula__dict
	DB 4
	DM "HALT"
	DW $ + 2
halt_:
	HALT
	JP forth_next


	; : D+! ( n addr -- ) \ double in addr incremented by 2
forth_d_plus_store__dict:
	DW halt__dict
	DB 4
	DM "D1+!"
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


forth_d_plus__dict:
	DW forth_d_plus_store__dict
	DB 2
	DM "D+"
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
forth_two_literal_raw__dict:
	DW forth_d_plus__dict
	DB 10
	DM "(2LITERAL)"
	DW $ + 2
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


forth_r_from__dict:
	DW forth_two_literal_raw__dict
	DB 2
	DM "R>"
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


_di__dict:
	DW forth_r_from__dict
	DB 2
	DM "DI"
	DW $ + 2
_di:
	DI
	JP forth_next


_ei__dict:
	DW _di__dict
	DB 2
	DM "EI"
	DW $ + 2
_ei:
	EI
	JP forth_next


	; AT-XY ( x y -- ) \ Set next terminal x,y position
forth_at_xy__dict:
	DW _ei__dict
	DB 5
	DM "AT-XY"
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


keyq_len__dict:
	DW forth_at_xy__dict
	DB 8
	DM "KEYQ-LEN"
	DW forth_constant_code
keyq_len:
	DW keyq_len_val


	; keyq items have two parts:
	; byte 1 = scancode which is 8*(4-bit)+(8-hrow_bit)
	;          (this gives an index into Key Table (a) in ROM disassembly)
	; byte 2 = flags: 1=up, 2=shift, 4=sym
keyq__dict:
	DW keyq_len__dict
	DB 4
	DM "KEYQ"
	DW forth_create_code
keyq:
	DS keyq_len_val * 2


keyq_s__dict:
	DW keyq__dict
	DB 6
	DM "KEYQ-S"
	DW forth_create_code
keyq_s:
	DB 0


keyq_e__dict:
	DW keyq_s__dict
	DB 6
	DM "KEYQ-E"
	DW forth_create_code
keyq_e:
	DB 0


	; : EKEY? ( -- flags ) \ Is a key event available?
ekey_question__dict:
	DW keyq_e__dict
	DB 5
	DM "EKEY?"
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
forth_zero_not_equals__dict:
	DW ekey_question__dict
	DB 3
	DM "0<>"
	DW forth_colon_code
forth_zero_not_equals:
	; 0= INVERT ;
	DB forth_zero_equals_tok
	DB forth_invert_tok
	DB forth_exit_tok


	; <> ( n1 n2 -- flags ) \ true if n1 is not equal to n2
forth_not_equals__dict:
	DW forth_zero_not_equals__dict
	DB 2
	DM "<>"
	DW forth_colon_code
forth_not_equals:
	; = INVERT ;
	DB forth_equals_tok
	DB forth_invert_tok
	DB forth_exit_tok


	; CLAMP ( n1 n2 n3 -- n ) \ Force n1 to range [n2, n3]
clamp__dict:
	DW forth_not_equals__dict
	DB 5
	DM "CLAMP"
	DW forth_colon_code
clamp:
	; ROT MIN MAX ;
	DB forth_rot_tok
	DX forth_min-2
	DX forth_max-2
	DB forth_exit_tok


	; : MIN ( n1 n2 -- n ) \ Leave the smaller of n1 and n2
forth_min__dict:
	DW clamp__dict
	DB 3
	DM "MIN"
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
forth_invert__dict:
	DW forth_min__dict
	DB 6
	DM "INVERT"
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
ekey__dict:
	DW forth_invert__dict
	DB 4
	DM "EKEY"
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


until_raw__dict:
	DW ekey__dict
	DB 7
	DM "(UNTIL)"
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


forth_equals__dict:
	DW until_raw__dict
	DB 1
	DM "="
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


less_than_or_equal__dict:
	DW forth_equals__dict
	DB 2
	DM "<="
	DW forth_colon_code
less_than_or_equal:
	DB forth_greater_than_tok
	DB forth_invert_tok
	DB forth_exit_tok


greater_than_or_equal__dict:
	DW less_than_or_equal__dict
	DB 2
	DM ">="
	DW forth_colon_code
greater_than_or_equal:
	DB forth_less_than_tok
	DB forth_invert_tok
	DB forth_exit_tok


forth_h_init:

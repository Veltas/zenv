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
symbols: EQU attr_file_val + attr_size_val
symbols_size: EQU symbols + 3*1024
	; Address data stack starts at
param_stack_top: EQU 0xFDF0
param_stack_size: EQU 0xE0
	; Address return stack starts at
return_stack_top: EQU 0xFEF0
return_stack_size: EQU 0xE0

;throw_addr: EQU 11
;throw_stack: EQU 13
;throw_ret_stack: EQU 15

	ORG 0x8000

	DI

	; Move symbols
	LD DE, symbols
	LD HL, builtin_symbols_start
	LD BC, builtin_symbols_end - builtin_symbols_start
	LDIR

	; Init interrupt handler
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
	LD HL, (tick_int)
	LD E, (HL)
	INC HL
	LD D, (HL)
	INC HL
	EX DE, HL
	CALL forth_asm_call

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
	INCLUDE "words.asm"
forth_h_init:


builtin_symbols_start:
	DISP symbols
	INCLUDE "symbols.asm"
	ENT
builtin_symbols_end:

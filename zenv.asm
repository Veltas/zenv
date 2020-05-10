; vim:set syntax=z80:

; ZENV - bootstrapping a Forth system

	MACRO DX ___addr
	DB (___addr) >> 8, (___addr) & 0xFF
	ENDM


NARROW_FONT: EQU 0
CHECKED: EQU 1
	IF NARROW_FONT
t_width: EQU 64
	ELSE
t_width: EQU 32
	ENDIF
t_attr_init: EQU 0x7
load_addr: EQU 0x8000
display_file_val: EQU 0x4000
display_size_val: EQU 0x1800
attr_file_val: EQU 0x5800
attr_size_val: EQU 0x300
ula_val: EQU 0xFE
keyq_len_val: EQU 8
symbols: EQU attr_file_val + attr_size_val
symbols_size: EQU symbols + 3*1024
	; Address data stack starts at
param_stack_top: EQU 0xFDF0
param_stack_size: EQU 0xE0
	; Address return stack starts at
return_stack_top: EQU 0xFEF0
return_stack_size: EQU 0xE0


this_header = 0
symb_pos = symbols

	MACRO HEADER ___symbol, ___text, ___immediate
main_pos = $
	ORG symb_pos
prev_header = this_header
this_header = $
	DW prev_header
	DW ___symbol - 2
	DB (.se - .ss) | (___immediate << 7)
.ss:
	DM ___text
.se:
symb_pos = $
	ORG main_pos
	ENDM


	DEVICE ZXSPECTRUM48

	ORG load_addr

	DI

	; Move symbols
	LD DE, symbols
	LD HL, h_init
	LD BC, symbols_len
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
	LD IY, main
	JP next


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
	CALL asm_call

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


next:
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


colon_code:
	IF CHECKED
		CALL ret_room_1
	ENDIF
	PUSH DE
	EX (SP), IY
	POP DE
	DEC IX
	DEC IX
	LD (IX+0), E
	LD (IX+1), D
	JR next


do_does:
	; Push parameter field address to stack, DE = asm snippet
	EX DE, HL
	PUSH HL
	IF CHECKED
		CALL dat_room_0
	ENDIF
	; Forward DE to colon def
	INC DE
	INC DE
	INC DE
	; Call that colon def
	JR colon_code


constant_code:
constant_does:
	JP do_does
	DB fetch_tok
	DB exit_tok


two_constant_does:
	JP do_does
	DX two_fetch-2
	DB exit_tok


create_code:
	IF CHECKED
		CALL dat_room_1
	ENDIF
	PUSH DE
	JR next


	; CALL this address, with HL set to asm code to call, uses next
	; to return to calling code.
asm_call:
	IF CHECKED
		PUSH HL
		CALL ret_room_2
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
	LD IY, asm_call_param
	; Jump to provided HL
	JP (HL)
asm_call_param:
	DX asm_exit_cp
asm_exit_cp:
	DW $ + 2
asm_exit:
	IF CHECKED
		CALL ret_holds_1
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


dat_room_0:
dat_room_1:
dat_room_2:
dat_holds_1:
dat_holds_2:
dat_holds_3:
dat_holds_4:
dat_holds_1_room_1:
dat_holds_2_room_1:
dat_holds_3_room_1:
dat_holds_2_room_2:
dat_holds_4_room_2:
ret_room_1:
ret_room_2:
ret_holds_1:
ret_holds_2:
dat_holds_1_ret_room_1:
dat_holds_2_ret_room_2:
dat_room_1_ret_holds_1:
	RET

	DW $+2
hang:
	JR $

tokens:
	INCLUDE "tokens.asm"
	DS tokens+128*2 - $


	IF NARROW_FONT
	INCLUDE "font.asm"
	ELSE
font: EQU 0x3D00
	ENDIF

dictionary_start:
	INCLUDE "words.asm"
h_init:

sym_last_init: EQU this_header

symbols_len: EQU symb_pos - symbols

	SAVEBIN "zenv-code.bin", load_addr, h_init-load_addr
	SAVEBIN "zenv-syms.bin", symbols, symbols_len

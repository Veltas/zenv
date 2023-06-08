; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021-2023 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Master assembly/definitions file

tokenised: EQU 1

t_attr_init: EQU 0x38
load_addr: EQU 0x8000
disp_file_val: EQU 0x4000
disp_size_val: EQU 0x1800
attr_file_val: EQU 0x5800
attr_size_val: EQU 0x300
ula_val: EQU 0xFE
keyq_len_val: EQU 8
	; Address of PAD
pad_val: EQU 0xFB00
	; Address of line input buffer
line_in_val: EQU 0xFC80
line_in_size_val: EQU 32; 82
	; Address WORD is stored in
tick_word_val: EQU 0xFD80
tick_word_size: EQU 0x81
	; Address data stack starts at, SP must remain within one page
param_stack_top: EQU 0xFE78
param_stack_size: EQU 0x70
	; Address return stack starts at, RP must remain within one page
return_stack_top: EQU 0xFEF8
return_stack_size: EQU 0x70
	; Address of built-in font
font: EQU 0x3D00


this_header = 0

	MACRO HEADER ___symbol, ___text, ___immediate
prev_header = this_header
this_header = $
	DW prev_header
	DB (.se - .ss) | (___immediate << 7)
.ss:
	DM ___text
.se:
	ENDM

	IF tokenised
		MACRO DX ___val
			IF EXIST ___val_tok
				DB (___val_tok - tokens) / 2
			ELSE
				DB (___val) >> 8, (___val) & 0xFF
			ENDIF
		ENDM
	ELSE
		MACRO DX ___val
			DW ___val
		ENDM
	ENDIF

	ORG load_addr

	DI

	; Init interrupt handler
	LD A, 0x18
	LD (0xFFFF), A

	IM 2
	LD A, 0x3B
	LD I, A

	; Init 0xFFF4 code to jump to our handler code
	LD HL, 0xFFF4
	; Compile "JP interrupt"
	LD (HL), 0xC3
	INC HL
	LD DE, interrupt
	LD (HL), E
	INC HL
	LD (HL), D

	; Enable interrupts
	EI

	; Init stacks
	LD SP, param_stack_top
	LD IX, return_stack_top

	; Run forth
	JP main


; CALL <does> in instance code
; CALL does_code in <does>
does_code:
	POP AF
	POP DE
	PUSH HL
	EX DE, HL
	PUSH AF
	; fall through


; CALL colon_code in code
colon_code:
	; Push current PC to RS
	DEC IXL
	DEC IXL
	PUSH IY
	POP BC
	LD (IX+0), C
	LD (IX+1), B
	; Get new PC from stack
pop_pc_next:
	POP IY
drop: EQU pop_pc_next + 1 ; 'POP IY' is 'DB 0xFD \ POP HL', so +1 is 'drop'
	; fall through


; JP next at end of code
next:
	IF tokenised
		; Load byte, advance PC
		LD A, (IY+0)
		INC IY
		CP 0x80
		JR NC, .not_token
		; If token, load token address and jump
		ADD A, A
		LD E, A
		LD D, tokens >> 8
		LD A, (DE)
		LD C, A
		INC DE
		LD A, (DE)
		LD B, A
		PUSH BC
		RET
.not_token:
		; If not token, jump to big endian address
		LD D, A
		LD E, (IY+0)
		INC IY
		PUSH DE
		RET
	ELSE
		; Load address, advance PC
		LD E, (IY+0)
		INC IY
		LD D, (IY+0)
		INC IY
		; Jump to address
		PUSH DE
		RET
	ENDIF


	; CALL this address, with DE set to code to call, uses next
	; to return to calling code.
asm_call:
	; Push IP to ret stack
	LD BC, -4
	ADD IX, BC
	PUSH IY
	POP BC
	LD (IX+2), C
	LD (IX+3), B
	; Move return pointer to ret stack
	POP BC
	LD (IX+0), C
	LD (IX+1), B
	; Set IP to instruction below
	LD IY, asm_call_param
	; Jump to provided DE
	PUSH DE
	RET
asm_call_param:
	DX asm_exit
asm_exit:
	LD E, (IX+0)
	LD D, (IX+1)
	LD C, (IX+2)
	LD B, (IX+3)
	PUSH BC
	POP IY
	LD BC, 4
	ADD IX, BC
	PUSH DE
	RET


	; Interrupt handling body
interrupt:
	PUSH AF
	PUSH BC
	PUSH DE
	PUSH HL
	EX AF, AF'
	PUSH AF
	EXX
	PUSH BC
	PUSH DE
	PUSH HL
	PUSH IX
	PUSH IY
	LD (.save_sp), SP
	LD DE, -4
	ADD IX, DE

	; Call forth interrupt routine
	LD DE, (tick_int+3)
	CALL asm_call

	; Restore context, return from interrupt
	LD SP, (.save_sp)
	POP IY
	POP IX
	POP HL
	POP DE
	POP BC
	POP AF
	EXX
	POP HL
	POP DE
	POP BC
	EX AF, AF'
	POP AF
	EI
	RETI

.save_sp:
	DW 0


constant_code:
	POP DE
	PUSH HL
	EX DE, HL
	LD E, (HL)
	INC HL
	LD D, (HL)
	EX DE, HL
	JP next


create_code:
	POP DE
	PUSH HL
	EX DE, HL
	JP next


dictionary_start:


	INCLUDE "stack.asm"
	INCLUDE "memory.asm"
	INCLUDE "maths.asm"
	INCLUDE "binary.asm"
	INCLUDE "control.asm"
	INCLUDE "tools.asm"
	INCLUDE "dict.asm"


	HEADER false, "FALSE", 0
false:
	JP zero_literal


	HEADER literal_raw, "(LITERAL)", 0
literal_raw:
	PUSH HL
	LD L, (IY+0)
	INC IY
	LD H, (IY+0)
	INC IY
	JP next


	HEADER bye, "BYE", 0
bye:
	RST 0x00


	HEADER execute, "EXECUTE", 0
execute:
	POP DE
	EX DE, HL
	PUSH DE
	RET


	HEADER one_plus, "1+", 0
one_plus:
	INC HL
	JP next


	HEADER char_plus, "CHAR+", 0
char_plus:
	JR one_plus


	HEADER one_minus, "1-", 0
one_minus:
	DEC HL
	JP next


	HEADER two_store, "2!", 0
two_store:
	POP BC
	POP DE
	LD (HL), C
	INC HL
	LD (HL), B
	INC HL
	LD (HL), E
	INC HL
	LD (HL), D
	JP drop


	HEADER two_star, "2*", 0
two_star:
	SLA L
	RL H
	JP next


	HEADER cells, "CELLS", 0
cells:
	JR two_star


	HEADER two_slash, "2/", 0
two_slash:
	SRA H
	RR L
	JP next


	HEADER two_fetch, "2@", 0
two_fetch:
	LD E, (HL)
	INC HL
	LD D, (HL)
	INC HL
	LD C, (HL)
	INC HL
	LD B, (HL)
	PUSH BC
	EX DE, HL
	JP next


	HEADER two_over, "2OVER", 0
two_over:
	PUSH HL
	LD HL, 4
	ADD HL, SP
	LD E, (HL)
	INC L
	LD D, (HL)
	INC L
	LD C, (HL)
	INC L
	LD B, (HL)
	PUSH BC
	EX DE, HL
	JP next


	HEADER question_dup, "?DUP", 0
question_dup:
	LD A, L
	OR H
	JR Z, .no_dup
	PUSH HL
.no_dup:
	JP next


	HEADER sp_store, "SP!", 0
sp_store:
	LD SP, HL
	JP next


	HEADER rp_store, "RP!", 0
rp_store:
	PUSH HL
	POP IX
	JP drop


	HEADER _abs, "ABS", 0
_abs:
	LD A, H
	AND 0x80
	JP Z, next
	EX DE, HL
	LD HL, 0
	OR A
	SBC HL, DE
.next:
	JP next


	HEADER align, "ALIGN", 1
align:
	JR _abs.next


	HEADER aligned, "ALIGNED", 1
aligned:
	JR _abs.next


	HEADER chars, "CHARS", 1
chars:
	JR _abs.next


	HEADER cell_plus, "CELL+", 0
cell_plus:
	INC HL
	INC HL
	JP next


	HEADER cmove, "CMOVE", 0
cmove:
	LD C, L
	LD B, H
	POP DE
	POP HL
	LD A, C
	OR B
	JR Z, .skip
	LDIR
.skip:
	JP drop


	HEADER cmove_up, "CMOVE>", 0
cmove_up:
	LD C, L
	LD B, H
	POP HL
	POP DE
	LD A, C
	OR B
	JR Z, .skip
	ADD HL, BC
	DEC HL
	EX DE, HL
	ADD HL, BC
	DEC HL
	LDDR
.skip:
	JP drop


	HEADER loop_raw, "(LOOP)", 0
loop_raw:
	PUSH HL
	; DE = iterator
	LD E, (IX+0)
	LD D, (IX+1)
	; HL = limit
	LD L, (IX+2)
	LD H, (IX+3)
	INC DE
	OR A
	SBC HL, DE
	JR NZ, .loop
	LD BC, 6
	ADD IX, BC
	INC IY
	JP drop
.loop:
	LD (IX+0), E
	LD (IX+1), D
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
	JP drop


	HEADER plus_loop_raw, "(+LOOP)", 0
plus_loop_raw:
	; DE = iterator
	LD E, (IX+0)
	LD D, (IX+1)
	; BC = limit
	LD C, (IX+2)
	LD B, (IX+3)
	; HL = increment
	EX DE, HL
	; HL = iterator
	; DE = increment
	; HL = HL-BC
	OR A
	SBC HL, BC
	LD A, D
	AND 0x80
	ADD HL, DE
	JR Z, .non_negative
	JR C, .next_loop
	JR .end_loop
.non_negative:
	; End loop when iterator-limit + increment carries
	JR C, .end_loop
.next_loop:
	; HL = new iterator
	ADD HL, BC
	LD (IX+0), L
	LD (IX+1), H
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
.exit:
	JP drop
.end_loop:
	LD BC, 6
	ADD IX, BC
	INC IY
	JR .exit


	HEADER over_two, "OVER2", 0
over_two:
	POP AF
	POP DE
	PUSH DE
	PUSH AF
	PUSH HL
	EX DE, HL
	JP next


	HEADER fill, "FILL", 0
fill:
	EX DE, HL
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
	JP drop


	HEADER tick_s, "'S", 0
tick_s:
	PUSH HL
	LD HL, 0
	ADD HL, SP
	JP next


	HEADER tick_r, "'R", 0
tick_r:
	PUSH HL
	PUSH IX
	JP drop


	HEADER u_less_than, "U<", 0
u_less_than:
	EX DE, HL
	POP HL
common_less_than:
	OR A
	SBC HL, DE
	LD HL, 0
	JR C, .lt
	JP next
.lt:
	DEC HL
	JP next


	HEADER less_than, "<", 0
less_than:
	POP DE
	LD BC, 0x8000
	ADD HL, BC
	EX DE, HL
	ADD HL, BC
	JR common_less_than


	HEADER else_skip, "(ELSE)", 0
else_skip:
	INC IY
	LD E, (IY-1)
	LD D, 0
	ADD IY, DE
	JP next


	HEADER again_raw, "(AGAIN)", 0
again_raw:
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
	JP next


	HEADER c_plus_store, "C+!", 0
c_plus_store:
	POP DE
	LD A, (HL)
	ADD A, E
	LD (HL), A
	JP drop


	HEADER raw_char, "(CHAR)", 0
raw_char:
	PUSH HL
	LD L, (IY+0)
	LD H, 0
	INC IY
	JP next


	HEADER if_raw, "(IF)", 0
if_raw:
	INC IY
	LD A, L
	OR H
	JR NZ, .end
	EX DE, HL
	LD E, (IY-1)
	ADD IY, DE
.end:
	JP drop


	HEADER of_raw, "(OF)", 0
of_raw:
	INC IY
	POP DE
	OR A
	SBC HL, DE
	JR NZ, .skip
	JP drop
.skip:
	LD C, (IY-1)
	LD B, 0
	ADD IY, BC
	EX DE, HL
	JP next


	HEADER ms, "MS", 0
ms:
	EX DE, HL
.ms_loop:
	LD A, E
	OR D
	JP Z, .end
	; waste time
	LD BC, (0x8000)
	; waste time
	LD B, (HL)
	LD BC, 132
.loop:
	DEC BC
	LD A, C
	OR B
	JR NZ, .loop
	DEC DE
	JR .ms_loop
.end:
	JP drop


	; CODE P@ ( addr -- cx ) \ Read byte from port
	HEADER p_fetch, "P@", 0
p_fetch:
	LD C, L
	LD B, H
	IN L, (C)
	LD H, 0
p_fetch_next:
	JP next


	; CODE P! ( cx addr -- ) \ Write byte to port
	HEADER p_store, "P!", 0
p_store:
	LD C, L
	LD B, H
	POP DE
	OUT (C), E
	POP HL
	JR p_fetch_next


	HEADER halt_, "HALT", 0
halt_:
	HALT
	JR p_fetch_next


	HEADER tuck2, "TUCK2", 0
tuck2:
	POP DE
	POP BC
	PUSH HL
	PUSH BC
	PUSH DE
	JP next


	HEADER d_plus, "D+", 0
d_plus:
	PUSH HL
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
	JP next


	HEADER _di, "DI", 0
_di:
	DI
	JP next


	HEADER _ei, "EI", 0
_ei:
	EI
	JP next


	HEADER until_raw, "(UNTIL)", 0
until_raw:
	LD A, L
	OR H
	POP HL
	JR NZ, until_raw_done
until_raw_loop:
	LD C, (IY+0)
	LD B, 0xFF
	ADD IY, BC
until_raw_next:
	JP next
until_raw_done:
	INC IY
	JR until_raw_next


	HEADER equals, "=", 0
equals:
	POP BC
	XOR A
	SBC HL, BC
	JR Z, .equal
	LD L, A
	LD H, A
	JP next
.equal:
	DEC HL
	JP next


	HEADER lshift, "LSHIFT", 0
lshift:
	EX DE, HL
	POP HL
	XOR A
	OR E
	JR Z, lshift_finish
	LD B, E
.loop:
	ADD HL, HL
	DJNZ .loop
lshift_finish:
	JP next


	HEADER rshift, "RSHIFT", 0
rshift:
	EX DE, HL
	POP HL
	XOR A
	OR E
	JR Z, lshift_finish
	LD B, E
.loop:
	SRL H
	RR L
	DJNZ .loop
	JR lshift_finish


	HEADER d_two_star, "D2*", 0
d_two_star:
	POP DE
	SLA E
	RL D
	RL L
	RL H
	PUSH DE
	JP next


	HEADER du_two_slash, "DU2/", 0
du_two_slash:
	POP DE
	SRL H
	RR L
	RR D
	RR E
	PUSH DE
	JP next


	HEADER du_less_than, "DU<", 0
du_less_than:
	LD C, L
	LD B, H
	POP DE
	POP HL
	OR A
	SBC HL, BC
	POP HL
	JR Z, .equal
	LD HL, 0
	JR NC, .false2
	DEC HL
.false:
	JR .false2
.equal:
	OR A
	SBC HL, DE
	LD HL, 0
	JR NC, .false2
	DEC HL
.false2:
	JP next


	; CODE D-
	HEADER d_minus, "D-", 0
d_minus:
	PUSH HL
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
	JP next


	; CODE DUB/MOD ( ud byte -- ud2 rem-byte ) \ Divide ud by unsigned byte
	HEADER dub_slash_mod, "DUB/MOD", 0
dub_slash_mod:
	LD C, L
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
	PUSH HL
	PUSH DE
	LD L, A
	LD H, B
	JP next


	; CODE ITONE ( u1 u2 -- ) \ half-oscillations period
	HEADER itone, "ITONE", 0
itone:
	LD A, (t_attr+3)
	RRCA
	RRCA
	RRCA
	AND 7
	EX DE, HL
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
	JP NZ, .skip
	JP drop
.skip:
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
	JP drop


	; \ -1 if s1<s2, 1 if s1>s2, 0 if s1=s2
	; CODE COMPARE ( a1 u1 a2 u2 -- n )
	HEADER compare, "COMPARE", 0
compare:
	LD C, L
	LD B, H
	POP DE
	POP HL
	PUSH HL
	OR A
	SBC HL, BC
	JR C, .u2_larger
	JR Z, .u1_u2_equal
	POP HL
	LD C, L
	LD B, H
	LD A, 1
	JR .cont
.u1_u2_equal:
	POP HL
	LD A, 0
	JR .cont
.u2_larger:
	POP HL
	LD A, -1
.cont:
	POP HL
	PUSH AF
	; HL = a1, DE = a2, BC = min(u1,u2)
	LD A, C
	OR B
	JR Z, .skip_loop
.loop:
	LD A, (DE)
	INC DE
	CPI
	JR C, .s1_larger
	JR NZ, .s2_larger
	JP PE, .loop
.skip_loop:
	POP AF
	LD L, A
	LD H, 0
	JP next

.s1_larger:
	LD HL, 1
	JR .cont2

.s2_larger:
	LD HL, -1
.cont2:
	POP DE
	JP next


	; ( n1 n2 -- d)
	; CODE UM*
	HEADER um_star, "UM*", 0
um_star:
	EX DE, HL
	POP BC
	LD HL, 0
	LD A, 16
.loop:
	ADD HL, HL
	RL E
	RL D
	JR NC, .skip
	ADD HL, BC
	JR NC, .skip
	INC DE
.skip:
	DEC A
	JR NZ, .loop
	EX DE, HL
	PUSH DE
	JP next


	; ( d n1 n2 -- d)
	; CODE UM*/
	HEADER um_star_slash, "UM*/", 0
um_star_slash:
	POP BC
	POP DE
	POP AF
	; Save IY and n2
	PUSH IY
	PUSH HL
	PUSH AF
	POP HL

	; DEHL = d, BC = n1
	LD IY, 0
	LD A, 32
.loop:
	ADD IY, IY
	RL L
	RL H
	RL E
	RL D
	JR NC, .skip
	ADD IY, BC
	JR NC, .skip
	INC L
	JR NZ, .skip
	INC H
	JR NZ, .skip
	INC E
	JR NZ, .skip
	INC D
.skip:
	DEC A
	JR NZ, .loop
	; DEHLIY = result

	EXX
	POP BC
	LD HL, 0
	EXX
	; BC' = n2
	; HL' = rem

	LD B, 48
.loop2:
	ADD IY, IY
	ADC HL, HL
	RL E
	RL D
	EXX
	ADC HL, HL
	OR A
	SBC HL, BC
	JR C, .skip2
	EXX
	INC IY
	JR .skip3
.skip2:
	ADD HL, BC
	EXX
.skip3:
	DJNZ .loop2

	; Restore IY, result MSB in HL, result LSB on stack
.end:
	EX (SP), IY
	JP next


	; ( ud u -- urem uquo)
	; CODE UM/MOD
	HEADER um_slash_mod, "UM/MOD", 0
um_slash_mod:
	EX DE, HL
	POP BC
	EX (SP), IY
	; BCIY = numerator
	; HL = remainder
	; DE = denominator
	LD HL, 0
	LD A, 32
.loop:
	ADD IY, IY
	RL C
	RL B
	ADC HL, HL
	JR NC, .skip3
	OR A
	SBC HL, DE
	INC IY
	JR .skip2
.skip3:
	SBC HL, DE
	JR C, .skip
	INC IY
	JP .skip2
.skip:
	ADD HL, DE
.skip2:
	DEC A
	JP NZ, .loop
	; IY = quotient
	; HL = remainder
	EX (SP), IY
	EX DE, HL
	POP HL
	PUSH DE
	JP next


	; ( num den -- rem quo)
	; CODE U/MOD
	HEADER u_slash_mod, "U/MOD", 0
u_slash_mod:
	EX DE, HL
	POP BC
	LD A, 16
	LD HL, 0
	JP .loop_entry
.loop_and_add:
	ADD HL, DE
.loop:
	DEC A
	JP Z, .end
.loop_entry:
	SLA C
	RL B
	ADC HL, HL
	SBC HL, DE
	JR C, .loop_and_add
	INC C
	JP .loop
.end:
	PUSH HL
	LD L, C
	LD H, B
	JP next


	; : > SWAP < ;
	HEADER greater_than, ">", 0
greater_than:
	CALL colon_code
	DX swap
	DX less_than
	DX exit


	; -1 CONSTANT TRUE
	HEADER true, "TRUE", 0
true:
	CALL constant_code
	DW -1


	; 1 CELLS CONSTANT CELL
	HEADER cell, "CELL", 0
cell:
	CALL constant_code
	DW 2


	; Current data space pointer
	HEADER h_, "H", 0
h_:
	CALL create_code
	DW h_init


	; : HERE ( -- addr ) \ Get current address of dictionary end
	HEADER here, "HERE", 0
here:
	CALL colon_code
	; H @ ;
	DX h_
	DX fetch
	DX exit


; Points to most recently defined symbol
	HEADER sym_last, "SYM-LAST", 0
sym_last:
	CALL create_code
	DW sym_last_init


; non-zero while compiling
	HEADER state, "STATE", 0
state:
	CALL create_code
	DW 0


	HEADER frames, "FRAMES", 0
frames:
	CALL create_code
	DW 0
	DW 0


	HEADER t_attr, "T-ATTR", 0
t_attr:
	CALL create_code
	DB t_attr_init


	HEADER t_col, "T-COL", 0
t_col:
	CALL create_code
	DB 0


	HEADER t_row, "T-ROW", 0
t_row:
	CALL create_code
	DB 0


	HEADER disp_file, "DISP-FILE", 0
disp_file:
	CALL constant_code
	DW disp_file_val


	HEADER disp_size, "DISP-SIZE", 0
disp_size:
	CALL constant_code
	DW disp_size_val


	HEADER attr_file, "ATTR-FILE", 0
attr_file:
	CALL constant_code
	DW attr_file_val


	HEADER attr_size, "ATTR-SIZE", 0
attr_size:
	CALL constant_code
	DW attr_size_val


	HEADER tick_word, "'WORD", 0
tick_word:
	CALL constant_code
	DW tick_word_val


	HEADER pad, "PAD", 0
pad:
	CALL constant_code
	DW pad_val


	; Current input buffer
	HEADER tick_in, "'IN", 0
tick_in:
	CALL create_code
	DW 0


	; Current input buffer size
	HEADER in_size, "IN#", 0
in_size:
	CALL create_code
	DW 0


	; \ Get address and length of current input buff
	; ( -- addr u )
	; : SOURCE
	HEADER source, "SOURCE", 0
source:
	CALL colon_code
	; 'IN @ IN# @ ;
	DX tick_in
	DX fetch
	DX in_size
	DX fetch
	DX exit


	HEADER line_in, "-IN", 0
line_in:
	CALL constant_code
	DW line_in_val


	HEADER line_in_size, "-IN#", 0
line_in_size:
	CALL constant_code
	DW line_in_size_val


	HEADER tick_int, "'INT", 0
tick_int:
	CALL create_code
	DW int


	; Default interrupt handler
	; : INT
	HEADER int, "INT", 0
int:
	CALL colon_code
	; 1. FRAMES D+! \ increment FRAMES
	DX two_literal_raw
	DW 0
	DW 1
	DX frames
	DX d_plus_store
	; \ update keyboard state
	; KSCAN ;
	DX kscan
	DX exit


	; \ Get name string from symbol header address
	; : >SYM ( sym-addr -- c-addr n+ )
	HEADER to_sym, ">SYM", 0
to_sym:
	CALL colon_code
	; CELL+  DUP C@ $7F AND  SWAP CHAR+ SWAP ;
	DX cell_plus
	DX dup
	DX c_fetch
	DX raw_char
	DB 0x7F
	DX and
	DX swap
	DX one_plus
	DX swap
	DX exit


	; ( str len -- )
	; : TYPE
	HEADER type, "TYPE", 0
type:
	CALL colon_code
	; OVER +
	DX over
	DX plus
	; ( str end )
	; SWAP ?DO I C@ EMIT LOOP ;
	DX swap
	DX question_do_raw
	DB .loop-$-1
.do:
	DX r_fetch
	DX c_fetch
	DX emit
	DX loop_raw
	DB .do-$+256
.loop:
	DX exit


	; \ Type all word names in dictionary to the output device
	; : WORDS
	HEADER words, "WORDS", 0
words:
	CALL colon_code
	; SYM-LAST @
	DX sym_last
	DX fetch
	; BEGIN
.begin:
		; \ Print name
		; DUP >SYM TYPE  SPACE
		DX dup
		DX to_sym
		DX type
		DX space
		; \ Goto next symbol
		; @
		DX fetch
	; ?DUP 0= UNTIL
	DX question_dup
	DX zero_equals
	DX until_raw
	DB .begin-$+256
	; CR ;
	DX cr
	DX exit


	; : KEY ( -- char )  \ Wait for next input char, using EKEY
	HEADER key, "KEY", 0
key:
	CALL colon_code
	; \ Wait for a character key event
	; BEGIN
.begin:
		; ( )
		; EKEY EKEY>CHAR 0= WHILE DROP
		DX ekey
		DX ekey_to_char
		DX zero_equals
		DX if_raw
		DB .repeat-$-1
		DX drop
	; REPEAT
	DX again_raw
	DB .begin-$+256
.repeat:
	; CLICK ;
	DX click
	DX exit


	; : MAIN
	HEADER main, "MAIN", 0
main:
	CALL colon_code
	; \ Clear screen
	; PAGE
	DX page
	; \ Greeting
	; ." HI"
	DX dot_quote_raw
	DB .s1e-.s1
.s1:
	DM "HI"
.s1e:
	; \ Run interpreter
	; ABORT ; -? ALLOT
	DX abort


	; : WITHIN ( n start end -- flags ) \ Is n within [start, end), or (end,
	;                                   \ start] if end is less.
	HEADER within, "WITHIN", 0
within:
	CALL colon_code
	; ( n start end )
	; OVER -
	DX over
	DX minus
	; ( n start range-length )
	; -ROT - SWAP
	DX minus_rot
	DX minus
	DX swap
	; ( offset range-length )
	; U< ;
	DX u_less_than
	DX exit


	; : , ( x -- ) \ Append cell to end of dictionary
	HEADER comma, ",", 0
comma:
	CALL colon_code
	; HERE CELL ALLOT ! ;
	DX here
	DX cell
	DX allot
	DX store
	DX exit


	; \ Compile colon code to put x on the stack
	; : LITERAL ( x -- )
	HEADER literal, "LITERAL", 1
literal:
	CALL colon_code
	; DUP 0 256 WITHIN IF
	DX dup
	DX zero_literal
	DX literal_raw
	DW 256
	DX within
	DX if_raw
	DB .else-$-1
		; POSTPONE (CHAR) C,
		DX postpone_raw
		DW raw_char
		DX c_comma
	; ELSE
	DX else_skip
	DB .then-$-1
.else:
		; POSTPONE (LITERAL) ,
		DX postpone_raw
		DW literal_raw
		DX comma
	; THEN ;
.then:
	DX exit


	; \ Compile colon code to put dx on the stack
	; ( dx --)
	; : 2LITERAL
	HEADER two_literal, "2LITERAL", 1
two_literal:
	CALL colon_code
	; 2DUP D0= IF
	DX two_dup
	DX d_zero_equals
	DX if_raw
	DB .else-$-1
		; 2DROP POSTPONE 0 POSTPONE 0
		DX two_drop
		DX postpone_raw
		DW zero_literal
		DX postpone_raw
		DW zero_literal
	; ELSE
	DX else_skip
	DB .then-$-1
.else:
		; POSTPONE (2LITERAL)
		DX postpone_raw
		DW two_literal_raw
		; , ,
		DX comma
		DX comma
	; THEN ;
.then:
	DX exit


	; : SCROLL
	HEADER scroll, "SCROLL", 0
scroll:
	CALL colon_code
	; T-ROW C@ 8 -  0 MAX  T-ROW C!
	DX t_row
	DX c_fetch
	DX raw_char
	DB 8
	DX minus
	DX zero_literal
	DX max
	DX t_row
	DX c_store
	; [ DISP-FILE 2048 + ] LITERAL  DISP-FILE  4096  CMOVE
	DX literal_raw
	DW disp_file_val + 2048
	DX disp_file
	DX literal_raw
	DW 4096
	DX cmove
	; [ ATTR-FILE 256 + ] LITERAL  ATTR-FILE  512  CMOVE
	DX literal_raw
	DW attr_file_val + 256
	DX attr_file
	DX literal_raw
	DW 512
	DX cmove
	; [ DISP-FILE 4096 + ] LITERAL  2048  ERASE
	DX literal_raw
	DW disp_file_val + 4096
	DX literal_raw
	DW 2048
	DX erase
	; [ ATTR-FILE 512 + ] LITERAL  256  T-ATTR C@  FILL
	DX literal_raw
	DW attr_file_val + 512
	DX literal_raw
	DW 256
	DX t_attr
	DX c_fetch
	DX fill
	; ;
	DX exit

	; : CR
	HEADER cr, "CR", 0
cr:
	CALL colon_code
	; T-ROW C@ 22 > IF SCROLL THEN 1 T-ROW C+!
	DX t_row
	DX c_fetch
	DX raw_char
	DB 22
	DX greater_than
	DX if_raw
	DB .cr__if_skip-$-1
	DX scroll
.cr__if_skip:
	DX one_literal
	DX t_row
	DX c_plus_store
	; 0 T-COL C!
	DX zero_literal
	DX t_col
	DX c_store
	; ;
	DX exit

	; : BS ( -- ) \ Write a backspace to terminal
	HEADER bs, "BS", 0
bs:
	CALL colon_code
	; T-COL C@ ?DUP IF
	DX t_col
	DX c_fetch
	DX question_dup
	DX if_raw
	DB .then-$-1
		; ( col )
		; 1-  DUP T-COL C!  SPACE  T-COL C!
		DX one_minus
		DX dup
		DX t_col
		DX c_store
		DX space
		DX t_col
		DX c_store
	; THEN ;
.then:
	DX exit

	HEADER emit, "EMIT", 0
emit:
	LD C, L
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
	LD A, (t_col+3)
	CP 32
	JR NC, .next_line
	LD C, A
	LD B, 0
.next_line_done:
	LD A, (t_row+3)
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
	LD HL, t_col+3
	INC (HL)
	JP drop

.next_line:
	PUSH DE
	LD DE, cr
	CALL asm_call
	POP DE
	LD BC, 0
	JR .next_line_done

.non_print:
	CP 0x7F - 0x20
	JR Z, .gbp

	CP 0x0A - 0x20
	LD DE, cr
	CALL Z, asm_call

	CP 0x08 - 0x20
	LD DE, bs
	CALL Z, asm_call

	JP drop

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

	; Offset of current digit in 'WORD (pictured number buffer)
	HEADER to_number_sign, ">#", 0
to_number_sign:
	CALL create_code
	DB 0

	; \ Initialise pictured number buffer for processing
	; : <# ( -- )
	HEADER less_number_sign, "<#", 0
less_number_sign:
	CALL colon_code
	; 128 ># C! ;
	DX raw_char
	DB 128
	DX to_number_sign
	DX c_store
	DX exit

	; \ Prefix pictured numeric string with given character
	; : HOLD ( c -- )
	HEADER hold, "HOLD", 0
hold:
	CALL colon_code
	; \ Decrement >#
	; -1 ># C+!
	DX true
	DX to_number_sign
	DX c_plus_store
	; \ Store character at (># + 'WORD)
	; 'WORD ># C@ + C! ;
	DX tick_word
	DX to_number_sign
	DX c_fetch
	DX plus
	DX c_store
	DX exit

	; \ Add a '-' character to the pictured numeric string if n less than 0
	; : SIGN ( n -- )
	HEADER sign, "SIGN", 0
sign:
	CALL colon_code
	; 0< IF [CHAR] - HOLD THEN ;
	DX zero_less
	DX if_raw
	DB .then-$-1
	DX raw_char
	DB '-'
	DX hold
.then:
	DX exit


	; \ Convert a digit to its character representation
	; : DIGIT ( n -- c )
	HEADER digit, "DIGIT", 0
digit:
	CALL colon_code
	; DUP 9 > IF [ CHAR A CHAR 0 - 10 - ] LITERAL + THEN
	DX dup
	DX raw_char
	DB 9
	DX greater_than
	DX if_raw
	DB .then-$-1
	DX raw_char
	DB 'A' - '0' - 10
	DX plus
.then:
	; [CHAR] 0 + ;
	DX raw_char
	DB '0'
	DX plus
	DX exit

	; \ Divide ud1 by BASE, quotient goes in ud2, remainder converted to
	; \ digit and prefixed to pictured numeric output string.
	; : # ( ud1 -- ud2 )
	HEADER number_sign, "#", 0
number_sign:
	CALL colon_code
	; BASE @  DUB/MOD  DIGIT HOLD ;
	DX base
	DX fetch
	DX dub_slash_mod
	DX digit
	DX hold
	DX exit

	; \ Drop double cell, get pictured numeric string
	; : #> ( xd -- c-addr u )
	HEADER number_sign_greater, "#>", 0
number_sign_greater:
	CALL colon_code
	; 2DROP  ># C@ 'WORD +  128 ># C@ -  ;
	DX two_drop
	DX to_number_sign
	DX c_fetch
	DX tick_word
	DX plus
	DX raw_char
	DB 128
	DX to_number_sign
	DX c_fetch
	DX minus
	DX exit

	; \ Do # until quotient is zero
	; : #S ( ud1 -- ud2 )
	HEADER number_sign_s, "#S", 0
number_sign_s:
	CALL colon_code
	; BEGIN # 2DUP D0= UNTIL
.begin:
	DX number_sign
	DX two_dup
	DX d_zero_equals
	DX until_raw
	DB .begin-$+256
	DX exit


	; \ Print an unsigned double number in the current BASE
	; : DU. ( ud -- )
	HEADER du_dot, "DU.", 0
du_dot:
	CALL colon_code
	; <# #S #> TYPE SPACE ;
	DX less_number_sign
	DX number_sign_s
	DX number_sign_greater
	DX type
	DX space
	DX exit


	; \ Print a double number in the current BASE, field width n
	;  ( d n --)
	; : D.R
	HEADER d_dot_r, "DR.", 0
d_dot_r:
	CALL colon_code
	; >R
	DX to_r
	; ( d) ( R:n)
	; TUCK
	DX tuck
	; ( high d)
	; <#
	DX less_number_sign
	; DUP 0<  IF  0. 2SWAP D-  THEN
	DX dup
	DX zero_less
	DX if_raw
	DB .then-$-1
	DX zero_literal
	DX zero_literal
	DX two_swap
	DX d_minus
.then:
	; #S
	DX number_sign_s
	; ROT SIGN
	DX rot
	DX sign
	; ( d)
	; #> R>
	DX number_sign_greater
	DX r_from
	; ( addr u n) ( R:)
	; \ Print padding spaces
	; 2DUP < IF OVER - SPACES ELSE DROP THEN
	DX two_dup
	DX less_than
	DX if_raw
	DB .else_spaces-$-1
	DX over
	DX minus
	DX spaces
	DX else_skip
	DB .then_spaces-$-1
.else_spaces:
	DX drop
.then_spaces:
	; ( addr u)
	; \ Print number
	; TYPE ;
	DX type
	DX exit


	; \ Print a double number in the current BASE
	; : D. ( d -- ) 0 D.R SPACE ;
	HEADER d_dot, "D.", 0
d_dot:
	CALL colon_code
	DX zero_literal
	DX d_dot_r
	DX space
	DX exit


	; \ Print an unsigned number in the current BASE
	; : U. ( u -- )
	HEADER u_dot, "U.", 0
u_dot:
	CALL colon_code
	; BASE @ 16 = IF U$. EXIT THEN
	DX base
	DX fetch
	DX raw_char
	DB 16
	DX equals
	DX if_raw
	DB .then1-$-1
	DX u_dollar_dot
	DX exit
.then1:
	; 0 DU. ;
	DX zero_literal
	DX du_dot
	DX exit


	; \ Convert single to double number
	; : S>D ( n -- d )
	HEADER s_to_d, "S>D", 0
s_to_d:
	CALL colon_code
	; DUP 0< IF -1 ELSE 0 THEN ;
	DX dup
	DX zero_less
	DX if_raw
	DB .else-$-1
	DX true
	DX else_skip
	DB .then-$-1
.else:
	DX zero_literal
.then:
	DX exit


	; \ Print number in current BASE, followed by space
	; : . ( n -- )
	HEADER dot, ".", 0
dot:
	CALL colon_code
	; S>D D. ;
	DX s_to_d
	DX d_dot
	DX exit


	; \ Print number in current BASE, with given min. field size
	; : .R ( n m -- ) SWAP S>D ROT D.R ;
	HEADER dot_r, ".R", 0
dot_r:
	CALL colon_code
	DX swap
	DX s_to_d
	DX rot
	DX d_dot_r
	DX exit


	; \ Set border to attr
	; ( attr ) : BRDR!
	HEADER brdr_store, "BRDR!", 0
brdr_store:
	CALL colon_code
	; 7 AND  ULA P@  0xF8 AND  OR  ULA P! ;
	DX raw_char
	DB 7
	DX and
	DX ula
	DX p_fetch
	DX raw_char
	DB 0xF8
	DX and
	DX or
	DX ula
	DX p_store
	DX exit


	; \ Clear screen, reset terminal to top-left
	; : PAGE
	HEADER page, "PAGE", 0
page:
	CALL colon_code
	; \ Match border to T-ATTR
	; T-ATTR C@  3 RSHIFT  BRDR!
	DX t_attr
	DX c_fetch
	DX raw_char
	DB 3
	DX rshift
	DX brdr_store
	; \ Reset terminal col/row
	; 0 0 AT-XY
	DX zero_literal
	DX zero_literal
	DX at_xy
	; \ Erase bitmap
	; DISP-FILE DISP-SIZE ERASE
	DX disp_file
	DX disp_size
	DX erase
	; \ Set attr region to current T-ATTR
	; ATTR-FILE ATTR-SIZE T-ATTR C@ FILL
	DX attr_file
	DX attr_size
	DX t_attr
	DX c_fetch
	DX fill
	; ;
	DX exit


	HEADER u_dollar_dot, "U$.", 0
u_dollar_dot:
	LD C, L
	LD B, H
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
	LD DE, emit
	LD HL, ' '
	PUSH HL
	CALL asm_call
	JP drop
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
	LD DE, emit
	CALL asm_call
	POP DE
	POP BC
	RET


	; \ Empty data stack and perform QUIT
	; ( --)
	; : ABORT S0 SP! DROP QUIT ; -2 ALLOT
	HEADER abort, "ABORT", 0
abort:
	CALL colon_code
	DX s_zero
	DX sp_store
	DX drop
	DX quit


	; ( flags --)
	; : (ABORT")
	HEADER abort_quote_raw, '(ABORT")', 0
abort_quote_raw:
	CALL colon_code
	; R> COUNT ROT
	DX r_from
	DX count
	DX rot
	; ( addr u cond)
	; IF TYPE ABORT THEN
	DX if_raw
	DB .then-$-1
	DX type
	DX abort
.then:
	; ( addr u)
	; + >R
	DX plus
	DX to_r
	; ;
	DX exit


	; : ALLOT ( n -- ) \ Add n to dictionary end pointer
	HEADER allot, "ALLOT", 0
allot:
	CALL colon_code
	; H +! ;
	DX h_
	DX plus_store
	DX exit


	HEADER base, "BASE", 0
base:
	CALL create_code
	DW 10


	HEADER bl, "BL", 0
bl:
	CALL constant_code
	DW ' '


	; : C, ( c -- ) \ Append byte to end of dictionary
	HEADER c_comma, "C,", 0
c_comma:
	CALL colon_code
	; HERE 1 ALLOT C! ;
	DX here
	DX one_literal
	DX allot
	DX c_store
	DX exit


	; : COUNT ( addr -- addr2 u ) \ Get string in counted string
	HEADER count, "COUNT", 0
count:
	CALL colon_code
	; DUP 1+ SWAP C@ ;
	DX dup
	DX one_plus
	DX swap
	DX c_fetch
	DX exit


	; \ Set BASE to 10
	; : DECIMAL ( -- )
	HEADER decimal, "DECIMAL", 0
decimal:
	CALL colon_code
	; 10 BASE ! ;
	DX raw_char
	DB 10
	DX base
	DX store
	DX exit


	HEADER depth, "DEPTH", 0
depth:
	CALL colon_code
	DX s_zero
	DX tick_s
	DX minus
	DX two_slash
	DX one_minus
	DX exit


	; \ Set BASE to 16
	; : HEX ( -- )
	HEADER hex, "HEX", 0
hex:
	CALL colon_code
	; 16 BASE ! ;
	DX raw_char
	DB 16
	DX base
	DX store
	DX exit


	; : MAX 2DUP < IF SWAP THEN DROP ;
	HEADER max, "MAX", 0
max:
	CALL colon_code
	DX two_dup
	DX less_than
	DX if_raw
	DB .skip-$-1
	DX swap
.skip:
	DX drop
	DX exit


	HEADER move, "MOVE", 0
move:
	CALL colon_code
	; : MOVE -ROT 2DUP < IF ROT CMOVE> ELSE ROT CMOVE THEN ;
	DX minus_rot
	DX two_dup
	DX less_than
	DX if_raw
	DB .move__else-$-1
	DX rot
	DX cmove_up
	DX else_skip
	DB .move__else_skip-$-1
.move__else:
	DX rot
	DX cmove
.move__else_skip:
	DX exit


	; : NEGATE  0 SWAP - ;
	HEADER negate, "NEGATE", 0
negate:
	CALL colon_code
	DX zero_literal
	DX swap
	DX minus
	DX exit


	; : DNEGATE  0. 2SWAP D- ;
	HEADER dnegate, "DNEGATE", 0
dnegate:
	CALL colon_code
	DX zero_literal
	DX zero_literal
	DX two_swap
	DX d_minus
	DX exit


	HEADER s_quote_raw, '(S")', 0
s_quote_raw:
	CALL colon_code
	; R> COUNT 2DUP + >R ;
	DX r_from
	DX count
	DX two_dup
	DX plus
	DX to_r
	DX exit


	HEADER dot_quote_raw, '(.")', 0
dot_quote_raw:
	CALL colon_code
	; R> COUNT 2DUP + >R TYPE ;
	DX r_from
	DX count
	DX two_dup
	DX plus
	DX to_r
	DX type
	DX exit


	; \ Read a line of input into buffer, return length of line read
	; : ACCEPT ( buf size -- n )
	HEADER accept, "ACCEPT", 0
accept:
	CALL colon_code
	; SWAP >R 0
	DX swap
	DX to_r
	DX zero_literal
	; ( size idx ) ( R: buf )
	; BEGIN 2DUP > WHILE
.begin:
	DX two_dup
	DX greater_than
	DX if_raw
	DB .repeat-$-1
		; ( size idx ) ( R: buf )
		; KEY DUP CASE
		DX key
		DX dup
			; ( size idx key key ) ( R: buf )
			; \ Delete: remove character
			; 8  OF EMIT  1- 0 MAX        ENDOF
			DX raw_char
			DB 8
			DX of_raw
			DB .endof1-$-1
			DX emit
			DX one_minus
			DX zero_literal
			DX max
			DX else_skip
			DB .endcase-$-1
.endof1:
			; \ Enter: finish input
			; 10 OF DROP NIP R> DROP EXIT ENDOF
			DX raw_char
			DB 10
			DX of_raw
			DB .endof2-$-1
			DX drop
			DX nip
			DX r_from
			DX drop

			DX exit
			DX else_skip
			DB .endcase-$-1
.endof2:
			; \ default: output character
			; EMIT OVER R@ + C!  1+
			DX emit
			DX over
			DX r_fetch
			DX plus
			DX c_store
			DX one_plus
		; 0 ENDCASE
		DX zero_literal
		DX drop
.endcase:
	; REPEAT
	DX again_raw
	DB .begin-$+256
.repeat:
	; ( size idx ) ( R: buf )
	; R> DROP NIP ;
	DX r_from
	DX drop
	DX nip
	DX exit


	; \ Read a line of input into -IN
	; : -READ ( -- addr u )
	HEADER line_read, "-READ", 0
line_read:
	CALL colon_code
	; -IN DUP -IN# ACCEPT ;
	DX line_in
	DX dup
	DX line_in_size
	DX accept
	DX exit


	; ( addr u -- addr u 0 | xt 1 | xt -1 )
	; CODE SFIND
	HEADER sfind, "SFIND", 0
sfind:
	POP DE
	DEC DE
	PUSH DE
	; B = size of name
	LD B, L
	; Load first symbol
	LD HL, (sym_last+3)
	; Start loop
	JR .loop_cond
.loop:
	; Compare sizes
	INC HL
	INC HL
	LD A, (HL)
	AND 0x7F
	CP B
	JP NZ, .not_equal
	; Loop over characters to compare strings if same size
	POP DE
	PUSH DE
	PUSH HL
	PUSH BC
.loop2:
	INC HL
	LD C, (HL)
	INC DE
	LD A, (DE)
	CP C
	JP NZ, .not_equal2
	DJNZ .loop2
	; If fall through loop then strings match
	POP BC
	POP HL
	POP DE
	LD A, (HL)
	AND 0x80
	LD DE, 1
	JR NZ, .immediate
	LD DE, -1
.immediate:
	LD C, B
	LD B, 0
	ADD HL, BC
	INC HL
	PUSH HL
	EX DE, HL
	JP next
.not_equal2:
	POP BC
	POP HL
.not_equal:
	DEC HL
	DEC HL
	; Next symbol
	LD E, (HL)
	INC HL
	LD D, (HL)
	EX DE, HL
.loop_cond:
	; Loop while not 0
	LD A, H
	OR L
	JP NZ, .loop
	; If loop falls through then no match is found
	POP DE
	INC DE
	PUSH DE
	LD E, B
	LD D, 0
	PUSH DE
	JP next


	; ( c-addr -- c-addr 0 | xt 1 | xt -1)
	; : FIND
	HEADER find, "FIND", 0
find:
	CALL colon_code
	; COUNT SFIND
	DX count
	DX sfind
	; ( addr u 0 | xt 1 | xt -1)
	; ?DUP 0= IF
	DX question_dup
	DX zero_equals
	DX if_raw
	DB .then-$-1
		; ( addr u)
		; DROP 1- 0
		DX drop
		DX one_minus
		DX zero_literal
	; THEN ;
.then:
	DX exit


	; \ See next parse character or 0 if parse area empty
	; : PPEEK ( -- c | 0 )
	HEADER ppeek, "PPEEK", 0
ppeek:
	CALL colon_code
	; >IN @
	DX to_in
	DX fetch
	; ( >IN-val)
	; DUP  IN# @  < IF
	DX dup
	DX in_size
	DX fetch
	DX less_than
	DX if_raw
	DB .else-$-1
		; 'IN @ + C@
		DX tick_in
		DX fetch
		DX plus
		DX c_fetch
		; ( c)
	; ELSE
	DX else_skip
	DB .then-$-1
.else:
		; DROP 0
		DX drop
		DX zero_literal
		; ( 0)
	; THEN ;
.then:
	DX exit


	; \ Parse character or 0 if parse area empty
	; : PCHAR ( "c" -- c | 0 )
	HEADER pchar, "PCHAR", 0
pchar:
	CALL colon_code
	; PPEEK DUP IF 1 >IN +! THEN ;
	DX ppeek
	DX dup
	DX if_raw
	DB .then-$-1
	DX one_literal
	DX to_in
	DX plus_store
.then:
	DX exit


	; \ Parse a string from parse area, and return address/length of it in
	; \ parse area.
	; ( c "...<c>" -- addr u)
	; : PARSE
	HEADER parse, "PARSE", 0
parse:
	CALL colon_code
	; 'IN @ >IN @ +
	DX tick_in
	DX fetch
	DX to_in
	DX fetch
	DX plus
	; ( c addr)
	; 0 ROT
	DX zero_literal
	DX rot
	; ( addr u c)
	; BEGIN
.begin:
		; \ Stop when empty
		; PCHAR ?DUP WHILE
		DX pchar
		DX question_dup
		DX if_raw
		DB .repeat-$-1
		; ( addr u c p)
		; \ Stop when delimiter
		; OVER <> WHILE
		DX over
		DX not_equals
		DX if_raw
		DB .repeat-$-1
		; ( addr u c)
		; \ Increment length
		; SWAP 1+ SWAP
		DX swap
		DX one_plus
		DX swap
	; REPEAT THEN
	DX again_raw
	DB .begin-$+256
.repeat:
	; DROP ;
	DX drop
	DX exit

	; \ Parse and skip a string of delimiters
	; ( c "<c's>" --)
	; : PSKIP
	HEADER pskip, "PSKIP", 0
pskip:
	CALL colon_code
	; BEGIN
.begin:
		; PPEEK ?DUP WHILE
		DX ppeek
		DX question_dup
		DX if_raw
		DB .then-$-1
		; OVER = WHILE
		DX over
		DX equals
		DX if_raw
		DB .then-$-1
		; 1 >IN +!
		DX one_literal
		DX to_in
		DX plus_store
	; REPEAT THEN
	DX again_raw
	DB .begin-$+256
.then:
	; DROP ;
	DX drop
	DX exit


	; \ Parse a string from parse area, skipping preceding delimiters, and
	; \ return address/length of it in parse area.
	; ( c "<c's>...<c>" -- addr u)
	; : PARSE-WORD
	HEADER parse_word, "PARSE-WORD", 0
parse_word:
	CALL colon_code
	; \ Skip preceding delimiters
	; DUP PSKIP
	DX dup
	DX pskip
	; \ Parse rest
	; PARSE ;
	DX parse
	DX exit


	; \ Parse a name from parse area, and return address/length of it in
	; \ parse area.
	; ( "<spaces>...<space>" -- addr u)
	; : PARSE-NAME BL PARSE-WORD ;
	HEADER parse_name, "PARSE-NAME", 0
parse_name:
	CALL colon_code
	DX bl
	DX parse_word
	DX exit


	; \ Parse counted string www, terminated by c
	; : CPARSE ( c "www<c>" -- addr )
	HEADER cparse, "CPARSE", 0
cparse:
	CALL colon_code
	; PARSE
	DX parse
	; ( addr u)
	; DUP 256 >= ABORT" long name"
	DX dup
	DX literal_raw
	DW 256
	DX greater_than_or_equal
	DX abort_quote_raw
	DB .e1-.s1
.s1:
	DM "long name"
.e1:
	; TUCK
	DX tuck
	; ( u addr u)
	; 'WORD 1+ SWAP
	DX tick_word
	DX one_plus
	DX swap
	; ( u addr addr2 u)
	; CMOVE
	DX cmove
	; ( u)
	; 'WORD C!
	DX tick_word
	DX c_store
	; ( )
	; 'WORD ;
	DX tick_word
	DX exit


	; \ Parse counted string www, delimited by c
	; ( c "<c...>www<c>" -- addr )
	; : WORD
	HEADER word, "WORD", 0
word:
	CALL colon_code
	; \ Ignore initial delimiters
	; DUP PSKIP
	DX dup
	DX pskip
	; \ Parse
	; CPARSE ;
	DX cparse
	DX exit


	; \ Check if a character is a valid double number punctuator
	; ( c -- ?)
	; : NUM-PUNC?
	HEADER num_punc_question, "NUM-PUNC?", 0
num_punc_question:
	CALL colon_code
	; CASE
		; [CHAR] , OF TRUE ENDOF
		DX raw_char
		DB ','
		DX of_raw
		DB .endof1-$-1
		DX true
		DX else_skip
		DB .endcase-$-1
.endof1:
		; [CHAR] . OF TRUE ENDOF
		DX raw_char
		DB '.'
		DX of_raw
		DB .endof2-$-1
		DX true
		DX else_skip
		DB .endcase-$-1
.endof2:
		; [CHAR] + OF TRUE ENDOF
		DX raw_char
		DB '+'
		DX of_raw
		DB .endof3-$-1
		DX true
		DX else_skip
		DB .endcase-$-1
.endof3:
		; [CHAR] - OF TRUE ENDOF
		DX raw_char
		DB '-'
		DX of_raw
		DB .endof4-$-1
		DX true
		DX else_skip
		DB .endcase-$-1
.endof4:
		; [CHAR] / OF TRUE ENDOF
		DX raw_char
		DB '/'
		DX of_raw
		DB .endof5-$-1
		DX true
		DX else_skip
		DB .endcase-$-1
.endof5:
		; [CHAR] : OF TRUE ENDOF
		DX raw_char
		DB ':'
		DX of_raw
		DB .endof6-$-1
		DX true
		DX else_skip
		DB .endcase-$-1
.endof6:
		; FALSE SWAP
		DX false
		DX swap
	; ENDCASE ;
	DX drop
.endcase:
	DX exit


	; : DEFER
	HEADER defer, "DEFER", 0
defer:
	CALL colon_code
	; CREATE 0 , DOES> @ EXECUTE ;
	DX create: DX zero_literal: DX comma: DX does_raw: CALL does_code
	DX fetch: DX execute: DX exit


	; : IS' ( xt)
	HEADER is_tick, "IS'", 0
is_tick:
	CALL colon_code
	; ' >BODY ! ;
	DX tick: DX to_body: DX store: DX exit


	; : (IS) ( xt)
	HEADER is_raw, "(IS)", 0
is_raw:
	CALL colon_code
	; R> TUCK  @ >BODY !  CELL+ >R ;
	DX r_from: DX tuck: DX fetch: DX to_body: DX store: DX cell_plus
	DX to_r: DX exit


	; : IS
	HEADER is, "IS", 1
is:
	CALL colon_code
	; STATE @ IF ' POSTPONE (IS) , ELSE IS' THEN ; IMMEDIATE
	DX state: DX fetch: DX if_raw: DB .else-$-1: DX tick: DX postpone_raw
	DW is_raw: DX comma: DX else_skip: DB .then-$-1
.else:
	DX is_tick
.then:
	DX exit


	; ( ud addr u -- ud2 addr2 u2)
	; : >NUMBER
	HEADER to_number, ">NUMBER", 0
to_number:
	CALL colon_code
	; \ Loop while chars remaining
	; DUP IF BEGIN
	DX dup
	DX if_raw
	DB .then_skip-$-1
.begin:
		; \ Get char
		; OVER C@
		DX over
		DX c_fetch
		; ( ud addr u c)
		; \ Handle 0-9 / a-z apart
		; DUP [CHAR] 0 [ CHAR 9 1+ ] LITERAL WITHIN IF
		DX dup
		DX raw_char
		DB '0'
		DX raw_char
		DB '9'+1
		DX within
		DX if_raw
		DB .else-$-1
			; \ Convert char to number
			; [CHAR] 0 -
			DX raw_char
			DB '0'
			DX minus
			; ( ud addr u n)
		; ELSE
		DX else_skip
		DB .then_apart-$-1
.else:
			; \ Convert char to number
			; [ CHAR A 10 - ] -
			DX raw_char
			DB 'A'-10
			DX minus
			; ( ud addr u n)
		; THEN
.then_apart:
		; \ Must be within current base
		; DUP BASE @ 0 WITHIN IF DROP EXIT THEN
		DX dup
		DX base
		DX fetch
		DX zero_literal
		DX within
		DX if_raw
		DB .then_base-$-1
		DX drop
		DX exit
.then_base:
		; 0 2ROT
		DX zero_literal
		DX two_rot
		; ( addr u dn ud)
		; \ Multiply current number by base
		; BASE @ 1 M*/
		DX base
		DX fetch
		DX one_literal
		DX m_star_slash
		; \ Add digit to current number
		; D+ 2SWAP
		DX d_plus
		DX two_swap
		; ( ud addr u)
		; \ Next char
		; SWAP 1+ SWAP 1-
		DX swap
		DX one_plus
		DX swap
		DX one_minus
	; DUP 0= UNTIL THEN ;
	DX dup
	DX zero_equals
	DX until_raw
	DB .begin-$+256
.then_skip:
	DX exit


	; \ Type a string with question mark and abort
	; ( addr u --)
	; : WHAT? TYPE TRUE ABORT" ?" ; -? ALLOT
	HEADER what_question, "WHAT?", 0
what_question:
	CALL colon_code
	DX type
	DX true
	DX abort_quote_raw
	DB .s2-.s1
.s1:
	DM "?"
.s2:


	; \ Type a counted string with question mark and abort
	; ( c-addr --)
	; : CWHAT? COUNT WHAT? ; -? ALLOT
	HEADER cwhat_question, "CWHAT?", 0
cwhat_question:
	CALL colon_code
	DX count
	DX what_question


	; \ Parse single or double number from counted string
	; ( c-addr -- n|d is-double?)
	; : NUMBER
	HEADER number, "NUMBER", 0
number:
	CALL colon_code
	; DUP COUNT
	DX dup
	DX count
	; ( c-addr addr u)
	; \ Fail on empty string
	; ?DUP 0= IF DROP CWHAT? THEN
	DX question_dup
	DX zero_equals
	DX if_raw
	DB .then_empty-$-1
	DX drop
	DX cwhat_question
.then_empty:
	; \ Ignore first char addr/u if '-'
	; OVER C@ [CHAR] - = IF
	DX over
	DX c_fetch
	DX raw_char
	DB '-'
	DX equals
	DX if_raw
	DB .then_ignore_minus-$-1
		; SWAP 1+ SWAP 1-
		DX swap
		DX one_plus
		DX swap
		DX one_minus
	; THEN
.then_ignore_minus:
	; \ Fail on empty string
	; ?DUP 0= IF DROP CWHAT? THEN
	DX question_dup
	DX zero_equals
	DX if_raw
	DB .then_empty2-$-1
	DX drop
	DX cwhat_question
.then_empty2:
	; 0. 2SWAP
	DX zero_literal
	DX zero_literal
	DX two_swap
	; ( c-addr d-num addr u)
	; \ Attempt conversion
	; >NUMBER ?DUP IF
	DX to_number
	DX question_dup
	DX if_raw
	DB .else_single-$-1
		; ( c-addr d-num addr u)
		; \ Characters remain: double or bad char
		; BEGIN
.begin:
			; \ Check for bad char
			; OVER C@ NUM-PUNC? 0= IF
			DX over
			DX c_fetch
			DX num_punc_question
			DX zero_equals
			DX if_raw
			DB .then_bad-$-1
				; 2DROP 2DROP CWHAT?
				DX two_drop
				DX two_drop
				DX cwhat_question
			; THEN
.then_bad:
			; \ Advance char
			; SWAP 1+ SWAP 1-
			DX swap
			DX one_plus
			DX swap
			DX one_minus
			; \ Check remaining
			; ?DUP WHILE
			DX question_dup
			DX if_raw
			DB .repeat-$-1
			; \ Convert more and check remaining
			; >NUMBER ?DUP WHILE
			DX to_number
			DX question_dup
			DX if_raw
			DB .repeat-$-1
		; REPEAT THEN
		DX again_raw
		DB .begin-$+256
.repeat:
		; ( c-addr d-num addr)
		; DROP ROT
		DX drop
		DX rot
		; ( d-num c-addr)
		; \ If first char '-' negate
		; 1+ C@ [CHAR] - = IF DNEGATE THEN
		DX one_plus
		DX c_fetch
		DX raw_char
		DB '-'
		DX equals
		DX if_raw
		DB .then_negate-$-1
		DX dnegate
.then_negate:
		; ( d-num)
		; TRUE
		; ( d-num true)
		DX true
	; ELSE
	DX else_skip
	DB .then_single-$-1
.else_single:
		; ( c-addr d-num addr)
		; \ No chars remain: single
		; 2DROP SWAP
		DX two_drop
		DX swap
		; ( num c-addr)
		; \ Check for '-'
		; 1+ C@ [CHAR] - = IF NEGATE THEN
		DX one_plus
		DX c_fetch
		DX raw_char
		DB '-'
		DX equals
		DX if_raw
		DB .then_negate2-$-1
		DX negate
.then_negate2:
		; ( num)
		; FALSE
		; ( d-num false)
		DX false
	; THEN ;
.then_single:
	DX exit


	if tokenised
	; \ Start of tokens vector
	; ??? TOKS CONSTANT
	HEADER toks, "TOKS", 0
toks:
	CALL constant_code
	DW tokens
	endif


	; ( xt -- )
	; : COMPILE,
	HEADER compile_comma, "COMPILE,", 0
compile_comma:
	CALL colon_code
	IF tokenised
		; \ Search for xt in tokens vector
		; [ TOKS 128 CELLS + ] LITERAL TOKS DO
		DX literal_raw: DW tokens + 256: DX toks: DX do_raw: DB .loop-$-1
.do:
			; \ If in tokens vector then compile the index
			; DUP I @ = IF
			DX dup: DX r_fetch: DX fetch: DX equals: DX if_raw: DB .then-$-1
				; I TOKS - 2/ C,
				DX r_fetch: DX toks: DX minus: DX two_slash: DX c_comma
				; DROP UNLOOP EXIT
				DX drop: DX unloop: DX exit
			; THEN
.then:
		; CELL +LOOP
		DX cell: DX plus_loop_raw: DB .do-$+256
.loop:
		; \ Otherwise compile xt as big-endian
		; DUP 8 RSHIFT C, C, ;
		DX dup: DX raw_char: DB 8: DX rshift: DX c_comma: DX c_comma: DX exit
	ELSE
		; , ;
		DX comma: DX exit
	ENDIF


	; : lower ( C -- c )
	HEADER lower, "LOWER", 0
lower:
	CALL colon_code
	; dup [CHAR] A [ CHAR Z 1+ ] LITERAL within IF
	DX dup
	DX raw_char
	DB 'A'
	DX raw_char
	DB 'Z' + 1
	DX within
	DX if_raw
	DB .then-$-1
		; [ CHAR A CHAR a - ] LITERAL +
		DX raw_char
		DB 'a' - 'A'
		DX plus
	; THEN ;
.then:
	DX exit


	; \ Convert lowercase letters to uppercase (or do nothing)
	; : upper ( c -- C )
	HEADER upper, "UPPER", 0
upper:
	CALL colon_code
	; DUP [CHAR] a [ CHAR z 1+ ] LITERAL within IF
	DX dup
	DX raw_char
	DB 'a'
	DX raw_char
	DB 'z' + 1
	DX within
	DX if_raw
	DB .then-$-1
		; [ 'A' 'a' - ] LITERAL +
		DX literal_raw
		DW 'A' - 'a'
		DX plus
	; THEN ;
.then:
	DX exit


	; : ERASE 0 FILL ;
	HEADER erase, "ERASE", 0
erase:
	CALL colon_code
	DX zero_literal
	DX fill
	DX exit


	HEADER space, "SPACE", 0
space:
	CALL colon_code
	DX bl
	DX emit
	DX exit


	; : SPACES ( n -- ) \ Print n spaces
	HEADER spaces, "SPACES", 0
spaces:
	CALL colon_code
	; 0 MAX
	DX zero_literal
	DX max
	; 0 ?DO SPACE LOOP ;
	DX zero_literal
	DX question_do_raw
	DB .loop-$-1
.do:
	DX space
	DX loop_raw
	DB .do-$+256
.loop:
	DX exit


	HEADER s_zero, "S0", 0
s_zero:
	CALL constant_code
	DW param_stack_top-2


	HEADER r_zero, "R0", 0
r_zero:
	CALL constant_code
	DW return_stack_top


	HEADER to_in, ">IN", 0
to_in:
	CALL create_code
	DW 0


	HEADER ula, "ULA", 0
ula:
	CALL constant_code
	DW ula_val


	; ( d addr ) : D+! \ double in addr incremented by d
	HEADER d_plus_store, "D+!", 0
d_plus_store:
	CALL colon_code
	; TUCK2 2@ D+ ROT 2! ;
	DX tuck2
	DX two_fetch
	DX d_plus
	DX rot
	DX two_store
	DX exit


	; : (2LITERAL)  R>  DUP [ 2 CELLS ] LITERAL + >R  2@  ;
	HEADER two_literal_raw, "(2LITERAL)", 0
two_literal_raw:
	CALL colon_code
	DX r_from
	DX dup
	DX raw_char
	DB 4
	DX plus
	DX to_r
	DX two_fetch
	DX exit


	; AT-XY ( x y -- ) \ Set next terminal x,y position
	HEADER at_xy, "AT-XY", 0
at_xy:
	CALL colon_code
	; ( y ) 0 23 CLAMP T-ROW C!
	DX zero_literal
	DX raw_char
	DB 23
	DX clamp
	DX t_row
	DX c_store
	; ( x ) 0 31 CLAMP T-COL C!
	DX zero_literal
	DX raw_char
	DB 31
	DX clamp
	DX t_col
	DX c_store
	; ;
	DX exit


	HEADER keyq_len, "KEYQ-LEN", 0
keyq_len:
	CALL constant_code
	DW keyq_len_val


	; keyq items have two parts:
	; byte 1 = scancode which is 8*(4-bit)+(8-hrow_bit)
	;          (this gives an index into Key Table (a) in ROM disassembly)
	; byte 2 = flags: 1=up, 2=shift, 4=sym
	HEADER keyq, "KEYQ", 0
keyq:
	CALL create_code
	DS keyq_len_val * 2


	HEADER keyq_s, "KEYQ-S", 0
keyq_s:
	CALL create_code
	DB 0


	HEADER keyq_e, "KEYQ-E", 0
keyq_e:
	CALL create_code
	DB 0


	; : EKEY? ( -- flags ) \ Is a key event available?
	HEADER ekey_question, "EKEY?", 0
ekey_question:
	CALL colon_code
	; KEYQ-E C@ KEYQ-S C@ <> ;
	DX keyq_e
	DX c_fetch
	DX keyq_s
	DX c_fetch
	DX not_equals
	DX exit


	; 0<> ( n -- flags ) \ true if n is not equal to 0
	HEADER zero_not_equals, "0<>", 0
zero_not_equals:
	CALL colon_code
	; 0= INVERT ;
	DX zero_equals
	DX invert
	DX exit


	; <> ( n1 n2 -- flags ) \ true if n1 is not equal to n2
	HEADER not_equals, "<>", 0
not_equals:
	CALL colon_code
	; = INVERT ;
	DX equals
	DX invert
	DX exit


	; CLAMP ( n1 n2 n3 -- n ) \ Force n1 to range [n2, n3]
	HEADER clamp, "CLAMP", 0
clamp:
	CALL colon_code
	; ROT MIN MAX ;
	DX rot
	DX min
	DX max
	DX exit


	; : MIN ( n1 n2 -- n ) \ Leave the smaller of n1 and n2
	HEADER min, "MIN", 0
min:
	CALL colon_code
	; 2DUP > IF SWAP THEN DROP ;
	DX two_dup
	DX greater_than
	DX if_raw
	DB .skip-$-1
	DX swap
.skip:
	DX drop
	DX exit


	; \ Store CAPS LOCK state
	; CREATE CAPS 1 C,
	HEADER caps, "CAPS", 0
caps:
	CALL create_code
	DB 255


	; EKEY ( -- x ) \ Push keyboard event when ready
	HEADER ekey, "EKEY", 0
ekey:
	CALL colon_code
	; BEGIN EKEY? HALT UNTIL \ Block for event
.begin:
	DX ekey_question
	DX halt_
	DX until_raw
	DB .begin-$+256
	; KEYQ KEYQ-S C@ CELLS + @ \ Receive event
	DX keyq
	DX keyq_s
	DX c_fetch
	DX two_star
	DX plus
	DX fetch
	; KEYQ-S C@ 1+ 7 AND KEYQ-S C! \ Increment KEYQ-S (mod 8)
	DX keyq_s
	DX c_fetch
	DX one_plus
	DX raw_char
	DB 7
	DX and
	DX keyq_s
	DX c_store
	; \ If CAPS LOCK then toggle CAPS
	; DUP $11C = IF
	DX dup
	DX literal_raw
	DW 0x11C
	DX equals
	DX if_raw
	DB .then-$-1
		; CAPS C@ 0= CAPS C!
		DX caps
		DX c_fetch
		DX zero_equals
		DX caps
		DX c_store
	; THEN
.then:
	; ;
	DX exit


	; \ Address of the character key map in ROM
	; $205 ROM-KMAP CONSTANT
	HEADER rom_kmap, "ROM-KMAP", 0
rom_kmap:
	CALL constant_code
	DW 0x205


rom_smap1_val: EQU 0x26A
	; \ Address of one alphabet key map used with symbol shift
	; $26A ROM-SMAP1 CONSTANT
	HEADER rom_smap1, "ROM-SMAP1", 0
rom_smap1:
	CALL constant_code
	DW rom_smap1_val


rom_smap2_val: EQU 0x246
	; \ Address of lower priority alphabet key map used with symbol shift
	; $246 ROM-SMAP2 CONSTANT
	HEADER rom_smap2, "ROM-SMAP2", 0
rom_smap2:
	CALL constant_code
	DW rom_smap2_val


	; \ Mapping of number keys to symbol shift characters
	; CREATE NSYM-MAP
	HEADER nsym_map, "NSYM-MAP", 0
nsym_map:
	CALL create_code
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
kmap:
	CALL create_code
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
ekey_to_char:
	CALL colon_code
	; \ Get offset byte
	; DUP $FF AND
	DX dup
	DX raw_char
	DB 0xFF
	DX and
	; \ If shift active...
	; 2DUP <> IF
	DX two_dup
	DX not_equals
	DX if_raw
	DB .then1-$-1
		; \ If symbol shift, add 80, otherwise 40
		; OVER $200 AND  80 40 CHOOSE  +
		DX over
		DX literal_raw
		DW 0x200
		DX and
		DX raw_char
		DB 80
		DX raw_char
		DB 40
		DX choose
		DX plus
	; THEN
.then1:
	; CHARS KMAP + C@ ?DUP IF
	DX kmap
	DX plus
	DX c_fetch
	DX question_dup
	DX if_raw
	DB .else1-$-1
		; NIP
		DX nip
		; \ Invert case if CAPS
		; CAPS C@ IF
		DX caps
		DX c_fetch
		DX if_raw
		DB .then_caps-$-1
			; DUP [CHAR] A [ CHAR Z 1+ ] LITERAL WITHIN IF
			DX dup
			DX raw_char
			DB 'A'
			DX raw_char
			DB 'Z' + 1
			DX within
			DX if_raw
			DB .else_invert_case-$-1
				; [ CHAR a CHAR A - ] LITERAL +
				DX raw_char
				DB 'a' - 'A'
				DX plus
			; ELSE DUP [CHAR] a [ CHAR z 1+ ] LITERAL WITHIN IF
			DX else_skip
			DB .then_invert_case-$-1
.else_invert_case:
			DX dup
			DX raw_char
			DB 'a'
			DX raw_char
			DB 'z' + 1
			DX within
			DX if_raw
			DB .then_invert_case-$-1
				; [ CHAR A CHAR a - ] LITERAL +
				DX literal_raw
				DW 'A' - 'a'
				DX plus
			; THEN THEN
.then_invert_case:
		; THEN
.then_caps:
		; TRUE
		DX true
	; ELSE
	DX else_skip
	DB .then2-$-1
.else1:
		; FALSE
		DX zero_literal
	; THEN
.then2:
	; ;
	DX exit


	HEADER less_than_or_equal, "<=", 0
less_than_or_equal:
	CALL colon_code
	DX greater_than
	DX zero_equals
	DX exit


	HEADER greater_than_or_equal, ">=", 0
greater_than_or_equal:
	CALL colon_code
	DX less_than
	DX zero_equals
	DX exit


	; CREATE KSHIFT-STATE 0 C,
	HEADER kshift_state, "KSHIFT-STATE", 0
kshift_state:
	CALL create_code
	DB 0


	; \ Stores scanned key bits from the last scan
	; CREATE KSTATE  8 CHARS  ALLOT
	; KSTATE  8 CHARS  ERASE
	HEADER kstate, "KSTATE", 0
kstate:
	CALL create_code
	DS 8


	; \ Stores last key press
	; CREATE KLAST  0 C,
	HEADER klast, "KLAST", 0
klast:
	CALL create_code
	DB 0


	; \ Update keyboard state
	; CODE KSCAN ( -- )
	HEADER kscan, "KSCAN", 0
kscan:
	PUSH IY
	PUSH HL
	; If no keys are down, skip
	LD BC, 0x00FE
	IN A, (C)
	CPL
	OR A
	JR NZ, .keys_down
	; Clear kstate
	LD L, A
	LD H, A
	LD (kstate+3), HL
	LD (kstate+3+2), HL
	LD (kstate+3+4), HL
	LD (kstate+3+6), HL
	POP HL
	JP pop_pc_next

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
	LD (kshift_state+3), A
	; Loop over rows
	; BC is 0xFEFE and will rotate to 7FFE
	; E is counter
	LD E, 7
	; IY is KSTATE pointer
	LD IY, kstate+3
.loop:
	; If row is empty, clear state and skip
	IN A, (C)
	CPL
	JR NZ, .row_down
	LD (IY+0), A
.next_loop:
	INC IY
	RLC B
	DEC E
	JP P, .loop
	POP HL
	JP pop_pc_next
.row_down:
	; BC is port, E is counter, A is input, IY is kstate+E
	; D is old state
	LD D, (IY+0)
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
	LD A, (keyq_s+3)
	LD B, A
	LD A, (keyq_e+3)
	LD C, A
	INC A
	AND 7
	CP B
	JR Z, .inner_skip
	; Save next keyq_e value
	LD (keyq_e+3), A
	; Make HL new EKEY value
	LD A, L
	ADD A, A
	ADD A, A
	ADD A, A
	ADD A, E
	LD L, A
	LD A, (kshift_state+3)
	LD H, A
	; Store in queue
	EX DE, HL
	LD HL, keyq+3
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
	LD (IY+0), A
	JR .next_loop


	; : BIT ( x n -- x&(1<<n) ) \ Mask all but nth bit of x
	HEADER bit_, "BIT", 0
bit_:
	CALL colon_code
	; 1 SWAP LSHIFT AND ;
	DX one_literal
	DX swap
	DX lshift
	DX and
	DX exit


	; : CHOOSE ( flags x1 x2 -- x1|x2 ) \ Select x1 if flags true, o/w x2
	HEADER choose, "CHOOSE", 0
choose:
	CALL colon_code
	; ROT IF SWAP THEN NIP ;
	DX rot
	DX if_raw
	DB .then-$-1
	DX swap
.then:
	DX nip
	DX exit


	; : 2OR ( d1 d2 -- d1|d2 ) \ OR of doubles
	HEADER two_or, "2OR", 0
two_or:
	CALL colon_code
	; ROT OR -ROT OR SWAP ;
	DX rot
	DX or
	DX minus_rot
	DX or
	DX swap
	DX exit


	; : D0< ( d -- flags ) \ Is a double integer negative?
	HEADER d_zero_less, "D0<", 0
d_zero_less:
	CALL colon_code
	; NIP 0< ;
	DX nip
	DX zero_less
	DX exit


	; : 2ROT ( d1 d2 d3 -- d2 d3 d1 )
	HEADER two_rot, "2ROT", 0
two_rot:
	CALL colon_code
	; 2>R 2SWAP 2R> 2SWAP ;
	DX two_to_r
	DX two_swap
	DX two_r_from
	DX two_swap
	DX exit


	; : 2-ROT ( d1 d2 d3 -- d3 d1 d2 )
	HEADER two_minus_rot, "2-ROT", 0
two_minus_rot:
	CALL colon_code
	; 2SWAP 2>R 2SWAP 2R> ;
	DX two_swap
	DX two_to_r
	DX two_swap
	DX two_r_from
	DX exit


	; : D<
	HEADER d_less_than, "D<", 0
d_less_than:
	CALL colon_code
	; 2SWAP $8000 + 2SWAP $8000 + DU< ;
	DX two_swap
	DX literal_raw
	DW 0x8000
	DX plus
	DX two_swap
	DX literal_raw
	DW 0x8000
	DX plus
	DX du_less_than
	DX exit


	; : D0= OR 0= ;
	HEADER d_zero_equals, "D0=", 0
d_zero_equals:
	CALL colon_code
	DX or
	DX zero_equals
	DX exit


	; : TONE  ( len period -- ) \ Make an accurate tone, masking interrupts
	HEADER tone, "TONE", 0
tone:
	CALL colon_code
	; DI ITONE EI ;
	DX _di
	DX itone
	DX _ei
	DX exit


	; : CLICK ( -- ) \ Make a 'click' noise
	HEADER click, "CLICK", 0
click:
	CALL colon_code
	; 4 30 ITONE ;
	DX raw_char
	DB 4
	DX raw_char
	DB 30
	DX itone
	DX exit


	; \ Print current state of stack
	; : .S ( -- )
	HEADER dot_s, ".S", 0
dot_s:
	CALL colon_code
	; \ Print size of stack in brackets
	; ." <"  DEPTH 0 .R  ." > "
	DX dot_quote_raw
	DB .e1-.s1
.s1:
	DM "<"
.e1:
	DX depth
	DX zero_literal
	DX dot_r
	DX dot_quote_raw
	DB .e2-.s2
.s2:
	DM "> "
.e2:
	; 's s0 < IF
	DX tick_s
	DX s_zero
	DX less_than
	DX if_raw
	DB .then-$-1
		; \ Print contents of stack
		; 's  s0 2 -  DO
		DX tick_s
		DX s_zero
		DX raw_char
		DB 2
		DX minus
		DX do_raw
		DB .loop-$-1
.do:
			; i @ .
			DX r_fetch
			DX fetch
			DX dot
		; -2 +LOOP
		DX literal_raw
		DW -2
		DX plus_loop_raw
		DB .do-$+256
.loop:
	; THEN ;
.then:
	DX exit


	; \ Print current state of return stack
	; : .RS ( -- )
	HEADER dot_rs, ".RS", 0
dot_rs:
	CALL colon_code
	; \ Print size of stack in brackets
	; 'r r0 swap - 2/ 1-
	DX tick_r
	DX r_zero
	DX swap
	DX minus
	DX two_slash
	DX one_minus
	; ." <"
	DX dot_quote_raw
	DB .s1e-.s1
.s1:
	DM "<"
.s1e:
	; 0 .R
	DX zero_literal
	DX dot_r
	; ." > "
	DX dot_quote_raw
	DB .s2e-.s2
.s2:
	DM "> "
.s2e:
	; 'r cell+ r0 < INVERT IF EXIT THEN
	DX tick_r
	DX cell_plus
	DX r_zero
	DX less_than
	DX invert
	DX if_raw
	DB .then-$-1
	DX exit
.then:
	; \ Print contents of stack
	; 'r cell+ r0 2 -  DO
	DX tick_r
	DX cell_plus
	DX r_zero
	DX raw_char
	DB 2
	DX minus
	DX do_raw
	DB .loop-$-1
.do:
		; i @ u.
		DX r_fetch
		DX fetch
		DX u_dot
	; -2 +LOOP ;
	DX literal_raw
	DW -2
	DX plus_loop_raw
	DB .do-$+256
.loop:
	DX exit


	; \ Print immediate string
	; ( "...<r-paren>" --)
	; : .( [CHAR] ) PARSE TYPE ; IMMEDIATE
	HEADER dot_paren, ".(", 1
dot_paren:
	CALL colon_code
	DX raw_char
	DB ')'
	DX parse
	DX type
	DX exit


	; \ Inline comment
	; ( "...<r-paren>" --)
	; : ( [CHAR] ) PARSE 2DROP ; IMMEDIATE
	HEADER paren, "(", 1
paren:
	CALL colon_code
	DX raw_char
	DB ')'
	DX parse
	DX two_drop
	DX exit


	; \ Line comment
	; ( "......" --)
	; : \ IN# @ >IN ! ; IMMEDIATE
	HEADER backslash, "\\", 1
backslash:
	CALL colon_code
	DX in_size
	DX fetch
	DX to_in
	DX store
	DX exit


	; \ Compile a Z80 CALL instruction
	; ( addr --)
	; HEX : CALL CD C, , ; DECIMAL
	HEADER _call, "CALL", 0
_call:
	CALL colon_code
	DX raw_char
	DB 0xCD
	DX c_comma
	DX comma
	DX exit


	; \ Compile string as counted string
	; ( addr u --)
	; : CSTR,
	HEADER cstr_comma, "CSTR,", 0
cstr_comma:
	CALL colon_code
	; \ Store length
	; DUP C,
	DX dup
	DX c_comma
	; \ Store string
	; OVER + SWAP ?DO I C@ C, LOOP ;
	DX over
	DX plus
	DX swap
	DX question_do_raw
	DB .loop-$-1
.do:
	DX r_fetch
	DX c_fetch
	DX c_comma
	DX loop_raw
	DB .do-$+256
.loop:
	DX exit


	; \ Create new symbol from parsed name
	; ( " name" --)
	HEADER sym_comma, "SYM,", 0
sym_comma:
	CALL colon_code
	; \ Write back-link
	; SYM-LAST @ ,
	DX sym_last
	DX fetch
	DX comma
	; \ Parse the name
	; PARSE-NAME
	DX parse_name
	; ( str-addr str-u)
	; \ Must be non-empty
	; DUP 0= ABORT" missing name"
	DX dup
	DX zero_equals
	DX abort_quote_raw
	DB .e1-.s1
.s1:
	DM "missing name"
.e1:
	; \ Must be smaller than 64 chars
	; DUP 64 >= ABORT" long name"
	DX dup
	DX raw_char
	DB 64
	DX greater_than_or_equal
	DX abort_quote_raw
	DB .e2-.s2
.s2:
	DM "long name"
.e2:
	; \ Compile counted string
	; CSTR, ;
	DX cstr_comma
	DX exit


	; \ Create new definition, and make it findable
	; \ At runtime: ( -- data-addr)
	; ( " name" --)
	; HEX : CREATE
	HEADER create, "CREATE", 0
create:
	CALL colon_code
	; \ Retain address of new symbol
	; HERE
	DX here
	; ( addr " name")
	; \ Create new symbol with parsed name
	; SYM,
	DX sym_comma
	; ( addr)
	; \ Write 'CALL create-code' instruction
	; ??? CALL
	DX literal_raw
	DW create_code
	DX _call
	; \ Make symbol findable
	; SYM-LAST ! ; DECIMAL
	DX sym_last
	DX store
	DX exit


	; : VARIABLE CREATE CELL ALLOT ;
	HEADER variable, "VARIABLE", 0
variable:
	CALL colon_code
	DX create
	DX cell
	DX allot
	DX exit


	; \ Switch to compilation mode
	; ( --)
	; : ] TRUE STATE ! ;
	HEADER right_bracket, "]", 0
right_bracket:
	CALL colon_code
	DX true
	DX state
	DX store
	DX exit


	; \ Switch to interpreter mode immediately
	; ( --)
	; : [ FALSE STATE ! ; IMMEDIATE
	HEADER left_bracket, "[", 1
left_bracket:
	CALL colon_code
	DX false
	DX state
	DX store
	DX exit


	; \ Use to remember stack depth before/after colon def
	; VARIABLE :DEPTH
	HEADER colon_depth, ":DEPTH", 0
colon_depth:
	CALL create_code
	DW 0


	; \ Use to remember start of current colon def
	; VARIABLE :START
	HEADER colon_start, ":START", 0
colon_start:
	CALL create_code
	DW 0


	; \ Start compiling a colon definition with given name
	; ( " name" -- colon-sys)
	; : :
	HEADER colon, ":", 0
colon:
	CALL colon_code
	; \ Retain address of symbol
	; HERE
	DX here
	; \ Remember stack depth
	; DEPTH :DEPTH !
	DX depth
	DX colon_depth
	DX store
	; ( colon-sys " name")
	; \ Write symbol header
	; SYM,
	DX sym_comma
	; ( colon-sys)
	; \ Remember code address
	; HERE :START !
	DX here
	DX colon_start
	DX store
	; \ Write CALL colon-code
	; ??? CALL
	DX literal_raw
	DW colon_code
	DX _call
	; \ Start compiling
	; ] ;
	DX right_bracket
	DX exit


	; \ End : or DOES> definition
	; ( --)
	; : ;
	HEADER semicolon, ";", 1
semicolon:
	CALL colon_code
	; \ Current code exits if not already
	; POSTPONE EXIT
	DX postpone_raw
	DW exit
	; \ Check stack depth
	; DEPTH :DEPTH @ <> ABORT" unbalanced"
	DX depth
	DX colon_depth
	DX fetch
	DX not_equals
	DX abort_quote_raw
	DB .e1-.s1
.s1:
	DM "unbalanced"
.e1:
	; \ Make definition findable
	; SYM-LAST !
	DX sym_last
	DX store
	; \ Return to interpreter mode
	; POSTPONE [ ; IMMEDIATE
	DX left_bracket
	DX exit


	; ( -- orig)
	; : IF
	HEADER _if, "IF", 1
_if:
	CALL colon_code
	; POSTPONE (IF)
	DX postpone_raw
	DW if_raw
	; HERE
	DX here
	; ( orig)
	; 1 ALLOT ; IMMEDIATE
	DX one_literal
	DX allot
	DX exit


	; ( orig --)
	; : THEN DUP 1+ HERE SWAP - SWAP C! ; IMMEDIATE
	HEADER then, "THEN", 1
then:
	CALL colon_code
	; DUP 1+
	; ( orig orig+1)
	DX dup
	DX one_plus
	; HERE SWAP -
	DX here
	DX swap
	DX minus
	; ( orig jump-len)
	; SWAP C! ; IMMEDAITE
	DX swap
	DX c_store
	DX exit


	; : ELSE
	HEADER _else, "ELSE", 1
_else:
	CALL colon_code
	; POSTPONE (ELSE)
	DX postpone_raw
	DW else_skip
	; HERE
	DX here
	; ( orig)
	; 1 ALLOT
	DX one_literal
	DX allot
	; SWAP POSTPONE THEN
	DX swap
	DX then
	; ; IMMEDIATE
	DX exit


	; ( -- dest)
	; : BEGIN HERE ; IMMEDIATE
	HEADER begin, "BEGIN", 1
begin:
	JP here


	; ( dest --)
	; : AGAIN POSTPONE (AGAIN) HERE - C, ; IMMEDIATE
	HEADER again, "AGAIN", 1
again:
	CALL colon_code
	DX postpone_raw
	DW again_raw
	DX here
	DX minus
	DX c_comma
	DX exit


	; ( dest --)
	; : UNTIL POSTPONE (UNTIL) HERE - C, ; IMMEDIATE
	HEADER until, "UNTIL", 1
until:
	CALL colon_code
	DX postpone_raw
	DW until_raw
	DX here
	DX minus
	DX c_comma
	DX exit


	; ( dest -- orig dest)
	; : WHILE POSTPONE IF SWAP ; IMMEDIATE
	HEADER while, "WHILE", 1
while:
	CALL colon_code
	DX _if
	DX swap
	DX exit


	; ( orig dest --)
	; : REPEAT POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
	HEADER repeat, "REPEAT", 1
repeat:
	CALL colon_code
	DX again
	DX then
	DX exit


	; ( -- dest)
	; : DO POSTPONE (DO) 1 ALLOT HERE ; IMMEDIATE
	HEADER do, "DO", 1
do:
	CALL colon_code
	DX postpone_raw
	DW do_raw
	DX one_literal
	DX allot
	DX here
	DX exit


	; ( -- dest)
	; : ?DO POSTPONE (?DO) 1 ALLOT HERE ; IMMEDIATE
	HEADER question_do, "?DO", 1
question_do:
	CALL colon_code
	DX postpone_raw
	DW question_do_raw
	DX one_literal
	DX allot
	DX here
	DX exit


	; ( do-sys --)
	; : LOOP
	HEADER loop, "LOOP", 1
loop:
	CALL colon_code
	; POSTPONE (LOOP)
	DX postpone_raw
	DW loop_raw
	; DUP HERE - C,
	DX dup
	DX here
	DX minus
	DX c_comma
	; DUP 1- SWAP HERE SWAP - SWAP C!
	DX dup
	DX one_minus
	DX swap
	DX here
	DX swap
	DX minus
	DX swap
	DX c_store
	; ; IMMEDIATE
	DX exit


	; ( do-sys --)
	; : +LOOP
	HEADER plus_loop, "+LOOP", 1
plus_loop:
	CALL colon_code
	; POSTPONE (+LOOP)
	DX postpone_raw
	DW plus_loop_raw
	; HERE - C,
	DX here
	DX minus
	DX c_comma
	; ; IMMEDIATE
	DX exit


	; : I POSTPONE R@ ; IMMEDIATE
	HEADER _i, "I", 1
_i:
	CALL colon_code
	DX postpone_raw
	DW r_fetch
	DX exit


	; \ Interpret input buffer until empty
	; : INTERPRET ( ? -- ? )
	HEADER interpret, "INTERPRET", 0
interpret:
	CALL colon_code
	; BEGIN BL WORD DUP C@ WHILE
.begin:
	DX bl
	DX word
	DX dup
	DX c_fetch
	DX if_raw
	DB .repeat-$-1
		; FIND
		DX find
		; ?DUP 0= IF
		DX question_dup
		DX zero_equals
		DX if_raw
		DB .else-$-1
			; NUMBER  STATE @  IF
			DX number
			DX state
			DX fetch
			DX if_raw
			DB .else2-$-1
				; IF
				DX if_raw
				DB .else4-$-1
					; POSTPONE 2LITERAL
					DX two_literal
				; ELSE
				DX else_skip
				DB .then4-$-1
.else4:
					; POSTPONE LITERAL
					DX literal
				; THEN
.then4:
			; ELSE
			DX else_skip
			DB .then2-$-1
.else2:
				; DROP
				DX drop
			; THEN
.then2:
		; ELSE
		DX else_skip
		DB .then-$-1
.else:
			; 0<  STATE @  AND  IF COMPILE, ELSE EXECUTE THEN
			DX zero_less
			DX state
			DX fetch
			DX and
			DX if_raw
			DB .else3-$-1
			DX compile_comma
			DX else_skip
			DB .then3-$-1
.else3:
			DX execute
.then3:
		; THEN
.then:
	; REPEAT DROP ;
	DX again_raw
	DB .begin-$+256
.repeat:
	DX drop
	DX exit


	; : QUIT ( -- ) \ Reset return stack then start interpretation
	HEADER quit, "QUIT", 0
quit:
	CALL colon_code
	; R0 RP! CR
	DX r_zero
	DX rp_store
	DX cr
	; POSTPONE [
	DX left_bracket
	; BEGIN
.begin:
		; -READ  0 >IN !  IN# !  'IN !  SPACE INTERPRET ." ok" CR
		DX line_read
		DX zero_literal
		DX to_in
		DX store
		DX in_size
		DX store
		DX tick_in
		DX store
		DX space
		DX interpret
		DX dot_quote_raw
		DB .s1e-.s1
.s1:
		DM "ok"
.s1e:
		DX cr
	; AGAIN ; -? ALLOT
	DX again_raw
	DB .begin-$+256


	; : EVALUATE ( ??? addr u -- ??? )
	HEADER evaluate, "EVALUATE", 0
evaluate:
	CALL colon_code
	; \ Update input state to new addr, u, and save old state
	; 0 >IN DUP @ >R !
	DX zero_literal
	DX to_in
	DX dup
	DX fetch
	DX to_r
	DX store
	; IN# DUP @ >R !
	DX in_size
	DX dup
	DX fetch
	DX to_r
	DX store
	; 'IN DUP @ >R !
	DX tick_in
	DX dup
	DX fetch
	DX to_r
	DX store
	; \ Interpret code
	; INTERPRET
	DX interpret
	; \ Restore input state
	; R> 'IN !  R> IN# !  R> >IN ! ;
	DX r_from
	DX tick_in
	DX store
	DX r_from
	DX in_size
	DX store
	DX r_from
	DX to_in
	DX store
	DX exit


	; ( "...<quote>"--)
	; : ."
	HEADER dot_quote, ".\"", 1
dot_quote:
	CALL colon_code
	; POSTPONE (.")
	DX postpone_raw
	DW dot_quote_raw
	; [CHAR] " PARSE
	DX raw_char
	DB '"'
	DX parse
	; ( addr u)
	; CSTR, ;
	DX cstr_comma
	DX exit


	; ( "...<quote>"--)
	; : S"
	HEADER s_quote, "S\"", 1
s_quote:
	CALL colon_code
	; POSTPONE (S")
	DX postpone_raw
	DW s_quote_raw
	; [CHAR] " PARSE
	DX raw_char
	DB '"'
	DX parse
	; ( addr u)
	; CSTR, ;
	DX cstr_comma
	DX exit


	; ( "<spaces>name" -- xt)
	; : '
	HEADER tick, "'", 0
tick:
	CALL colon_code
	; PARSE-NAME
	DX parse_name
	; ( addr u)
	; SFIND
	DX sfind
	; ( addr u 0 | xt 1 | xt -1)
	; ?DUP 0= IF
	DX question_dup
	DX zero_equals
	DX if_raw
	DB .then-$-1
		; ( addr u)
		; WHAT?
		DX what_question
	; THEN
.then:
	; ( xt 1 | xt -1)
	; DROP ;
	DX drop
	DX exit


	; ( "<spaces>name" --)
	; ( -- xt) \ runtime
	; : ['] ' POSTPONE LITERAL ; IMMEDIATE
	HEADER bracket_tick, "[']", 1
bracket_tick:
	CALL colon_code
	DX tick
	DX literal
	DX exit


	; ( d -- d)
	; : DABS
	HEADER dabs, "DABS", 0
dabs:
	CALL colon_code
	; DUP 0< IF
	DX dup
	DX zero_less
	DX if_raw
	DB .then-$-1
		; 0. 2SWAP D-
		DX zero_literal
		DX zero_literal
		DX two_swap
		DX d_minus
	; THEN ;
.then:
	DX exit


	; ( d n -- rem quo)
	; : SM/REM
	HEADER sm_slash_rem, "SM/REM", 0
sm_slash_rem:
	CALL colon_code
	; \ retain signs
	; 2DUP 2>R
	DX two_dup
	DX two_to_r
	; ( R: d-high n)
	; \ absolute values
	; -ROT DABS ROT ABS
	DX minus_rot
	DX dabs
	DX rot
	DX _abs
	; \ perform unsigned div
	; UM/MOD SWAP
	DX um_slash_mod
	DX swap
	; ( uquo urem)
	; \ remainder has sign of d
	; I' 0< IF NEGATE THEN
	DX i_tick
	DX zero_less
	DX if_raw
	DB .then1-$-1
	DX negate
.then1:
	; SWAP
	DX swap
	; ( urem uquo)
	; \ quo sign is sign1*sign2
	; 2R> XOR 0< IF NEGATE THEN ;
	DX two_r_from
	DX xor
	DX zero_less
	DX if_raw
	DB .then2-$-1
	DX negate
.then2:
	DX exit


	; ( x m d -- r q)
	; : */MOD -ROT M* ROT SM/REM ;
	HEADER star_slash_mod, "*/MOD", 0
star_slash_mod:
	CALL colon_code
	DX minus_rot
	DX m_star
	DX rot
	DX sm_slash_rem
	DX exit


	; ( x m d -- q)
	; : */ */MOD NIP ;
	HEADER star_slash, "*/", 0
star_slash:
	CALL colon_code
	DX star_slash_mod
	DX nip
	DX exit


	; ( n1 n2 -- d)
	; HEX : M*
	HEADER m_star, "M*", 0
m_star:
	CALL colon_code
	; 2DUP
	DX two_dup
	; ( n1 n2 n1 n2)
	; XOR 0< -ROT
	DX xor
	DX zero_less
	DX minus_rot
	; ( sign n1 n2)
	; SWAP ABS SWAP ABS
	DX swap
	DX _abs
	DX swap
	DX _abs
	; ( sign an1 an2)
	; UM*
	DX um_star
	; ( sign ad)
	; ROT IF DNEGATE THEN
	DX rot
	DX if_raw
	DB .then-$-1
	DX dnegate
.then:
	; ;
	DX exit


	; ( d n1 n2 -- d)
	; : M*/
	HEADER m_star_slash, "M*/", 0
m_star_slash:
	CALL colon_code
	; ( dl dh n1 n2)
	; -ROT 2DUP
	DX minus_rot
	DX two_dup
	; ( dl n2 dh n1 dh n1)
	; XOR 0<
	DX xor
	DX zero_less
	; ( dl n2 dh n1 sign)
	; >R
	DX to_r
	; ( dl n2 dh n1) ( R:sign)
	; ABS ROT
	DX _abs
	DX rot
	; ( dl dh an1 n2)
	; 2SWAP DABS 2SWAP
	DX two_swap
	DX dabs
	DX two_swap
	; ( ad an1 n2)
	; UM*/
	DX um_star_slash
	; ( ad2)
	; R> IF DNEGATE THEN
	DX r_from
	DX if_raw
	DB .then-$-1
	DX dnegate
.then:
	; ;
	DX exit


	; ( num den -- rem quo)
	; : /MOD
	HEADER slash_mod, "/MOD", 0
slash_mod:
	CALL colon_code
	; 2DUP 2>R
	DX two_dup
	DX two_to_r
	; ( n d) ( R: n d)
	; SWAP ABS SWAP ABS
	DX swap
	DX _abs
	DX swap
	DX _abs
	; ( un ud)
	; U/MOD
	DX u_slash_mod
	; ( ur uq)
	; SWAP I' 0< IF NEGATE THEN
	DX swap
	DX i_tick
	DX zero_less
	DX if_raw
	DB .then-$-1
	DX negate
.then:
	; ( uq r)
	; SWAP 2R> XOR 0< IF NEGATE THEN ;
	DX swap
	DX two_r_from
	DX xor
	DX zero_less
	DX if_raw
	DB .then2-$-1
	DX negate
.then2:
	DX exit


	; ( num den -- quo)
	; : /
	HEADER slash, "/", 0
slash:
	CALL colon_code
	; /MOD NIP ;
	DX slash_mod
	DX nip
	DX exit


	; ( num den -- rem)
	; : MOD
	HEADER _mod, "MOD", 0
_mod:
	CALL colon_code
	; /MOD DROP ;
	DX slash_mod
	DX drop
	DX exit


	; ( xt -- data-addr)
	; : >BODY
	HEADER to_body, ">BODY", 0
to_body:
	CALL colon_code
	; 3 + ;
	DX raw_char
	DB 3
	DX plus
	DX exit


	; ( "message<quote>" --)
	; ( cond --) \ runtime
	; : ABORT"
	HEADER abort_quote, "ABORT\"", 1
abort_quote:
	CALL colon_code
	; POSTPONE (ABORT")
	DX postpone_raw
	DW abort_quote_raw
	; [CHAR] " PARSE CSTR, ;
	DX raw_char
	DB '"'
	DX parse
	DX cstr_comma
	DX exit


	; ( "name " -- c)
	; : CHAR
	HEADER char, "CHAR", 0
char:
	CALL colon_code
	; PARSE-NAME
	DX parse_name
	; ( addr u)
	; DUP 0= ABORT" expect char"
	DX dup
	DX zero_equals
	DX abort_quote_raw
	DB .e1-.s1
.s1:
	DM "expect char"
.e1:
	; DROP C@ ;
	DX drop
	DX c_fetch
	DX exit


	; ( "name " --)
	; ( -- c) \ runtime
	; : [CHAR]
	HEADER bracket_char, "[CHAR]", 1
bracket_char:
	CALL colon_code
	; CHAR
	DX char
	; ( c)
	; POSTPONE (CHAR)
	DX postpone_raw
	DW raw_char
	; C, ;
	DX c_comma
	DX exit


	; \ Make last defined symbol call/jump to xt instead
	; ( xt --)
	; : INSTEAD
	HEADER instead, "INSTEAD", 0
instead:
	CALL colon_code
	; SYM-LAST @ >SYM + 1+ ! ;
	DX sym_last
	DX fetch
	DX to_sym
	DX plus
	DX one_plus
	DX store
	DX exit


	; ( x "name " --)
	; : CONSTANT
	HEADER constant, "CONSTANT", 0
constant:
	CALL colon_code
	; CREATE ,
	DX create
	DX comma
	; ??? INSTEAD ;
	DX literal_raw
	DW constant_code
	DX instead
	DX exit


	; : (DOES)
	HEADER does_raw, "(DOES)", 0
does_raw:
	CALL colon_code
	; R> INSTEAD ;
	DX r_from
	DX instead
	DX exit


	; : DOES>
	HEADER does, "DOES>", 1
does:
	CALL colon_code
	; POSTPONE (DOES)
	DX postpone_raw
	DW does_raw
	; ??? CALL ;
	DX literal_raw
	DW does_code
	DX _call
	DX exit


	; : FM/MOD
	HEADER fm_slash_mod, "FM/MOD", 0
fm_slash_mod:
	CALL colon_code
	; SM/REM
	DX sm_slash_rem
	; ( rem quo)
	; OVER IF DUP 0< IF
	DX over
	DX if_raw
	DB .then-$-1
	DX dup
	DX zero_less
	DX if_raw
	DB .then-$-1
		; SWAP
		DX swap
		; ( quo rem)
		; DUP 0< IF
		DX dup
		DX zero_less
		DX if_raw
		DB .else2-$-1
			; 1- NEGATE
			DX one_minus
			DX negate
		; ELSE
		DX else_skip
		DB .then2-$-1
.else2:
			; NEGATE 1-
			DX negate
			DX one_minus
		; THEN
.then2:
		; SWAP 1-
		DX swap
		DX one_minus
		; ( rem quo)
	; THEN THEN ;
.then:
	DX exit


	; HEX : IMMEDIATE
	HEADER immediate, "IMMEDIATE", 0
immediate:
	CALL colon_code
	; SYM-LAST @ CELL+
	DX sym_last
	DX fetch
	DX cell_plus
	; ( addr)
	; DUP C@ 80 OR SWAP C!
	DX dup
	DX c_fetch
	DX raw_char
	DB 0x80
	DX or
	DX swap
	DX c_store
	; ( )
	; ; DECIMAL
	DX exit


	; \ Compile literal xt
	; : (PP)
	HEADER postpone_raw, "(PP)", 0
postpone_raw:
	CALL colon_code
	; R>
	DX r_from
	; ( addr)
	; DUP @
	DX dup
	DX fetch
	; ( addr xt)
	; COMPILE,
	DX compile_comma
	; ( addr)
	; CELL+ >R ;
	DX cell_plus
	DX to_r
	DX exit


	; \ Compile compilation semantics of name
	; ( " name" --)
	; : POSTPONE
	HEADER postpone, "POSTPONE", 1
postpone:
	CALL colon_code
	; PARSE-NAME SFIND
	DX parse_name
	DX sfind
	; ( addr u 0 | xt 1 | xt -1)
	; ?DUP 0= IF WHAT? THEN
	DX question_dup
	DX zero_equals
	DX if_raw
	DB .then-$-1
	DX what_question
.then:
	; ( imm-xt 1 | xt -1)
	; 0< IF
	DX zero_less
	DX if_raw
	DB .else2-$-1
		; ( xt)
		; POSTPONE (PP)
		DX postpone_raw
		DW postpone_raw
		; ,
		DX comma
		; ( )
	; ELSE
	DX else_skip
	DB .then2-$-1
.else2:
		; ( imm-xt)
		; COMPILE,
		DX compile_comma
		; ( )
	; THEN ; IMMEDIATE
.then2:
	DX exit


	; : RECURSE
	HEADER recurse, "RECURSE", 1
recurse:
	CALL colon_code
	; :START @ COMPILE,
	DX colon_start
	DX fetch
	DX compile_comma
	; ; IMMEDIATE
	DX exit


	; \ Remove u2 characters from string
	; ( addr1 u1 u2 -- addr2 u3)
	; : /STRING
	HEADER slash_string, "/STRING", 0
slash_string:
	CALL colon_code
	; TUCK
	DX tuck
	; ( addr1 u2 u1 u2)
	; -
	DX minus
	; ( addr1 u2 u3)
	; SWAP ROT
	DX swap
	DX rot
	; ( u3 u2 addr1)
	; +
	DX plus
	; ( u3 addr2)
	; SWAP ;
	DX swap
	DX exit


repeat_wait_init: EQU 45  ; 0.9s
repeat_repeat_init: EQU 5 ; 0.1s


key_up:        EQU 0x11 ; ASCII DC1
key_left:      EQU 0x12 ; ASCII DC2
key_down:      EQU 0x13 ; ASCII DC3
key_right:     EQU 0x14 ; ASCII DC4
key_caps_lock: EQU 0x1C ; ASCII File separator

h_init:

sym_last_init: EQU this_header

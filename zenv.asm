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
	; Address data stack starts at
param_stack_top: EQU 0xFE78
param_stack_size: EQU 0x70
	; Address return stack starts at
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


; CALL <does-code> in instance code
; CALL colon_code in <does-code>
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
	DEC IX
	DEC IX
	PUSH IY
	POP BC
	LD (IX+0), C
	LD (IX+1), B
	; Get new PC from stack
	POP IY
	; fall through


; JP next at end of code
next:
	; Load address, advance PC
	LD E, (IY+0)
	INC IY
	LD D, (IY+0)
	INC IY
	; Jump to address
	PUSH DE
	RET


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
	DW asm_exit
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
	JR next


create_code:
	POP DE
	PUSH HL
	EX DE, HL
	JP next


dictionary_start:


	HEADER exit, "EXIT", 0
exit:
	LD C, (IX+0)
	LD B, (IX+1)
	INC IX
	INC IX
	PUSH BC
	POP IY
	JP next


	HEADER drop, "DROP", 0
drop:
	POP HL
	JP next


	HEADER two_drop, "2DROP", 0
two_drop:
	POP HL
	POP HL
	JP next


	HEADER dup, "DUP", 0
dup:
	PUSH HL
	JP next


	HEADER two_dup, "2DUP", 0
two_dup:
	POP DE
	PUSH DE
	PUSH HL
	PUSH DE
	JP next


	HEADER swap, "SWAP", 0
swap:
	POP DE
	PUSH HL
	EX DE, HL
	JP next


	HEADER two_swap, "2SWAP", 0
two_swap:
	POP BC
	POP DE
	POP AF
	PUSH BC
	PUSH HL
	PUSH AF
	EX DE, HL
	JP next


	HEADER over, "OVER", 0
over:
	POP DE
	PUSH DE
	PUSH HL
	EX DE, HL
	JP next


	HEADER tuck, "TUCK", 0
tuck:
	POP DE
	PUSH HL
	PUSH DE
	JP next


	HEADER nip, "NIP", 0
nip:
	POP DE
	JP next


	HEADER rot, "ROT", 0
rot:
	POP AF
	POP DE
	PUSH AF
	PUSH HL
	EX DE, HL
	JP next


	HEADER minus_rot, "-ROT", 0
minus_rot:
	POP DE
	POP AF
	PUSH HL
	PUSH AF
	EX DE, HL
	JP next


	HEADER fetch, "@", 0
fetch:
	LD E, (HL)
	INC HL
	LD D, (HL)
	EX DE, HL
	JP next


	HEADER c_fetch, "C@", 0
c_fetch:
	LD E, (HL)
	LD D, 0
	EX DE, HL
	JP next


	HEADER store, "!", 0
store:
	POP DE
	LD (HL), E
	INC HL
	LD (HL), D
	POP HL
	JP next


	HEADER c_store, "C!", 0
c_store:
	POP DE
	LD (HL), E
	POP HL
	JP next


	HEADER plus, "+", 0
plus:
	POP DE
	ADD HL, DE
	JP next


	HEADER plus_store, "+!", 0
plus_store:
	POP DE
	LD C, (HL)
	INC HL
	LD B, (HL)
	EX DE, HL
	ADD HL, BC
	EX DE, HL
	LD (HL), D
	DEC HL
	LD (HL), E
	POP HL
	JP next


	HEADER minus, "-", 0
minus:
	POP DE
	EX DE, HL
	OR A
	SBC HL, DE
	JP next


	HEADER and, "AND", 0
and:
	POP DE
	LD A, E
	AND L
	LD L, A
	LD A, D
	AND H
	LD H, A
	JP next


	HEADER or, "OR", 0
or:
	POP DE
	LD A, E
	OR L
	LD L, A
	LD A, D
	OR H
	LD H, A
	JP next


	HEADER xor, "XOR", 0
xor:
	POP DE
	LD A, E
	XOR L
	LD L, A
	LD A, D
	XOR H
	LD H, A
	JP next


	HEADER invert, "INVERT", 0
invert:
	LD A, L
	CPL
	LD L, A
	LD A, H
	CPL
	LD H, A
	JP next


	HEADER r_fetch, "R@", 0
r_fetch:
	PUSH HL
	LD L, (IX+0)
	LD H, (IX+1)
	JP next


	HEADER two_r_fetch, "2R@", 0
two_r_fetch:
	PUSH HL
	LD L, (IX+0)
	LD H, (IX+1)
	LD E, (IX+2)
	LD D, (IX+3)
	PUSH DE
	JP next


	HEADER r_from, "R>", 0
r_from:
	PUSH HL
	LD L, (IX+0)
	LD H, (IX+1)
	INC IX
	INC IX
	JP next


	HEADER two_r_from, "2R>", 0
two_r_from:
	PUSH HL
	LD L, (IX+0)
	LD H, (IX+1)
	LD E, (IX+2)
	LD D, (IX+3)
	PUSH DE
	LD DE, 4
	ADD IX, DE
	JP next


	HEADER to_r, ">R", 0
to_r:
	DEC IX
	DEC IX
	LD (IX+0), L
	LD (IX+1), H
	POP HL
	JP next


	HEADER two_to_r, "2>R", 0
two_to_r:
	LD DE, -4
	ADD IX, DE
	POP DE
	LD (IX+0), L
	LD (IX+1), H
	LD (IX+2), E
	LD (IX+3), D
	POP HL
	JP next


	HEADER zero_literal, "0", 0
zero_literal:
	PUSH HL
	LD HL, 0
	JP next


	HEADER one_literal, "1", 0
one_literal:
	PUSH HL
	LD HL, 1
	JP next


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


	HEADER star, "*", 0
star:
	POP DE
	LD C, L
	LD A, H
	LD B, 16
.loop:
	ADD HL, HL
	SLA C
	RLA
	JR NC, .skip_add
	ADD HL, DE
.skip_add:
	DJNZ .loop
	JP next


	HEADER zero_equals, "0=", 0
zero_equals:
	LD A, L
	OR H
	JR NZ, .not_equal
	DEC HL
	JP next
.not_equal:
	LD HL, 0
	JP next


	HEADER zero_less, "0<", 0
zero_less:
	LD A, H
	AND 0x80
	JR Z, .zero_less__non_negative
	LD HL, 0xFFFF
	JP next
.zero_less__non_negative:
	LD H, A
	LD L, A
	JP next


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
	POP HL
	JP next


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
	INC HL
	LD D, (HL)
	INC HL
	LD C, (HL)
	INC HL
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
	POP HL
	JP next


	HEADER abs, "ABS", 0
abs:
	LD A, H
	AND 0x80
	JP Z, next
	EX DE, HL
	LD HL, 0
	OR A
	SBC HL, DE
	JP next


	HEADER align, "ALIGN", 1
align:
	JP next


	HEADER aligned, "ALIGNED", 1
aligned:
	JP next


	HEADER chars, "CHARS", 1
chars:
	JP next


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
	POP HL
	JP next


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
	POP HL
	JP next


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
	LD BC, 4
	ADD IX, BC
	INC IY
	POP HL
	JP next
.loop:
	LD (IX+0), E
	LD (IX+1), D
	LD E, (IY+0)
	LD D, 0xFF
	ADD IY, DE
	POP HL
	JP next


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
	JR Z, .non_negative
	ADD HL, DE
	JP C, .next_loop
	JR .end_loop
.non_negative:
	; End loop when iterator-limit + increment carries
	ADD HL, DE
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
	POP HL
	JP next
.end_loop:
	LD BC, 4
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
	POP HL
	JP next


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
	POP HL
	JP next


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
	POP HL
	JP next


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
	POP HL
	JP next


	HEADER of_raw, "(OF)", 0
of_raw:
	INC IY
	POP DE
	OR A
	SBC HL, DE
	JR NZ, .skip
	POP HL
	JP next
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
	POP HL
	JP next


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
	LD B, H
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
	LD L, C
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
	POP HL
	JP next
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
	POP HL
	JP next


	; \ -1 if s1<s2, 1 if s1>s2, 0 if s1=s2
	; CODE COMPARE ( s1-addr s1-u s2-addr s2-u -- n )
	HEADER compare, "COMPARE", 0
compare:
	LD C, L
	LD B, H
	POP DE
	POP HL
	PUSH HL
	OR A
	SBC HL, BC
	JR C, .s2_u_larger
	JR Z, .s1_u_s2_u_equal
	POP HL
	LD C, L
	LD B, H
	LD A, 1
	JR .cont
.s1_u_s2_u_equal:
	POP HL
	LD A, 0
	JR .cont
.s2_u_larger:
	POP HL
	LD A, -1
.cont:
	POP HL
	PUSH AF
	; HL = s1, DE = s2, BC = min(s1-u,s2-u)
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
	; CODE M*
	HEADER m_star, "M*", 0
m_star:
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
	; CODE M*/
	HEADER m_star_slash, "M*/", 0
m_star_slash:
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

	POP BC
	; BC = n2

	; FIXME: high byte of n2 discarded
	LD B, 48
	XOR A
.loop2:
	ADD IY, IY
	ADC HL, HL
	RL E
	RL D
	RLA
	CP C
	JR C, .skip2
	INC IY
	SUB C
.skip2:
	DJNZ .loop2

	; Restore IY, result MSB in HL, result LSB on stack
.end:
	EX (SP), IY
	JP next


	; : > SWAP < ;
	HEADER greater_than, ">", 0
greater_than:
	CALL colon_code
	DW swap
	DW less_than
	DW exit


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
	DW h_
	DW fetch
	DW exit


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
	DW tick_in
	DW fetch
	DW in_size
	DW fetch
	DW exit


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
	DW two_literal_raw
	DW 0
	DW 1
	DW frames
	DW d_plus_store
	; \ update keyboard state
	; KSCAN ;
	DW kscan
	DW exit


	; \ Get name string from symbol header address
	; : >SYM ( sym-addr -- c-addr n+ )
	HEADER to_sym, ">SYM", 0
to_sym:
	CALL colon_code
	; CELL+  DUP C@ $7F AND  SWAP CHAR+ SWAP ;
	DW cell_plus
	DW dup
	DW c_fetch
	DW raw_char
	DB 0x7F
	DW and
	DW swap
	DW one_plus
	DW swap
	DW exit


	; ( str len -- )
	; : TYPE
	HEADER type, "TYPE", 0
type:
	CALL colon_code
	; OVER +
	DW over
	DW plus
	; ( str end )
	; SWAP ?DO I C@ EMIT LOOP ;
	DW swap
	DW question_do_raw
	DB .loop-$-1
.do:
	DW r_fetch
	DW c_fetch
	DW emit
	DW loop_raw
	DB .do-$+256
.loop:
	DW exit


	; \ Type all word names in dictionary to the output device
	; : WORDS
	HEADER words, "WORDS", 0
words:
	CALL colon_code
	; SYM-LAST @
	DW sym_last
	DW fetch
	; BEGIN
.begin:
		; \ Print name
		; DUP >SYM TYPE  SPACE
		DW dup
		DW to_sym
		DW type
		DW space
		; \ Goto next symbol
		; @
		DW fetch
	; ?DUP 0= UNTIL DROP
	DW question_dup
	DW zero_equals
	DW until_raw
	DB .begin-$+256
	; CR ;
	DW cr
	DW exit


	; : KEY ( -- char )  \ Wait for next input char, using EKEY
	HEADER key, "KEY", 0
key:
	CALL colon_code
	; \ Wait for a character key event
	; BEGIN
.begin:
		; ( )
		; EKEY EKEY>CHAR 0= WHILE DROP
		DW ekey
		DW ekey_to_char
		DW zero_equals
		DW if_raw
		DB .repeat-$-1
		DW drop
	; REPEAT
	DW again_raw
	DB .begin-$+256
.repeat:
	; CLICK ;
	DW click
	DW exit


	; : MAIN
	HEADER main, "MAIN", 0
main:
	CALL colon_code
	; \ Clear screen
	; PAGE
	DW page
	; \ Greeting
	; ." HI"
	DW dot_quote_raw
	DB .s1e-.s1
.s1:
	DM "HI"
.s1e:
	; \ Run interpreter
	; ABORT ;
	DW abort
	DW exit


	; : WITHIN ( n start end -- flags ) \ Is n within [start, end), or (end,
	;                                   \ start] if end is less.
	HEADER within, "WITHIN", 0
within:
	CALL colon_code
	; ( n start end )
	; OVER -
	DW over
	DW minus
	; ( n start range-length )
	; -ROT - SWAP
	DW minus_rot
	DW minus
	DW swap
	; ( offset range-length )
	; U< \
	DW u_less_than
	DW exit


	; : , ( x -- ) \ Append cell to end of dictionary
	HEADER comma, ",", 0
comma:
	CALL colon_code
	; HERE CELL ALLOT ! \
	DW here
	DW cell
	DW allot
	DW store
	DW exit


	; \ Compile colon code to put x on the stack
	; : LITERAL ( x -- )
	HEADER literal, "LITERAL", 1
literal:
	CALL colon_code
	; DUP 0 256 WITHIN IF
	DW dup
	DW zero_literal
	DW literal_raw
	DW 256
	DW within
	DW if_raw
	DB .else-$-1
		; POSTPONE (CHAR) C,
		DW literal_raw
		DW raw_char
		DW compile_comma
		DW c_comma
	; ELSE
	DW else_skip
	DB .then-$-1
.else:
		; POSTPONE (LITERAL) ,
		DW literal_raw
		DW literal_raw
		DW compile_comma
		DW comma
	; THEN \
.then:
	DW exit


	; \ Compile colon code to put dx on the stack
	; ( dx --)
	; : 2LITERAL
	HEADER two_literal, "2LITERAL", 1
two_literal:
	CALL colon_code
	; 2DUP D0= IF
	DW two_dup
	DW d_zero_equals
	DW if_raw
	DB .else-$-1
		; 2DROP POSTPONE 0 POSTPONE 0
		DW two_drop
		DW literal_raw
		DW zero_literal
		DW compile_comma
		DW literal_raw
		DW zero_literal
		DW compile_comma
	; ELSE
	DW else_skip
	DB .then-$-1
.else:
		; POSTPONE (2LITERAL)
		DW literal_raw
		DW two_literal_raw
		DW compile_comma
		; SWAP , ,
		DW swap
		DW comma
		DW comma
	; THEN ;
.then:
	DW exit


	; : SCROLL
	HEADER scroll, "SCROLL", 0
scroll:
	CALL colon_code
	; T-ROW C@ 8 -  0 MAX  T-ROW C!
	DW t_row
	DW c_fetch
	DW raw_char
	DB 8
	DW minus
	DW zero_literal
	DW max
	DW t_row
	DW c_store
	; [ DISP-FILE 2048 + ] LITERAL  DISP-FILE  4096  CMOVE
	DW literal_raw
	DW disp_file_val + 2048
	DW disp_file
	DW literal_raw
	DW 4096
	DW cmove
	; [ ATTR-FILE 256 + ] LITERAL  ATTR-FILE  512  CMOVE
	DW literal_raw
	DW attr_file_val + 256
	DW attr_file
	DW literal_raw
	DW 512
	DW cmove
	; [ DISP-FILE 4096 + ] LITERAL  2048  ERASE
	DW literal_raw
	DW disp_file_val + 4096
	DW literal_raw
	DW 2048
	DW erase
	; [ ATTR-FILE 512 + ] LITERAL  256  T-ATTR C@  FILL
	DW literal_raw
	DW attr_file_val + 512
	DW literal_raw
	DW 256
	DW t_attr
	DW c_fetch
	DW fill
	; \
	DW exit

	; : CR
	HEADER cr, "CR", 0
cr:
	CALL colon_code
	; T-ROW C@ 22 > IF SCROLL THEN 1 T-ROW C+!
	DW t_row
	DW c_fetch
	DW raw_char
	DB 22
	DW greater_than
	DW if_raw
	DB .cr__if_skip-$-1
	DW scroll
.cr__if_skip:
	DW one_literal
	DW t_row
	DW c_plus_store
	; 0 T-COL C!
	DW zero_literal
	DW t_col
	DW c_store
	; \
	DW exit

	; : BS ( -- ) \ Write a backspace to terminal
	HEADER bs, "BS", 0
bs:
	CALL colon_code
	; T-COL C@ ?DUP IF
	DW t_col
	DW c_fetch
	DW question_dup
	DW if_raw
	DB .then-$-1
		; ( col )
		; 1-  DUP T-COL C!  SPACE  T-COL C!
		DW one_minus
		DW dup
		DW t_col
		DW c_store
		DW space
		DW t_col
		DW c_store
	; THEN \
.then:
	DW exit

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
	POP HL
	JP next

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

	POP HL
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
	; 0 ># C! \
	DW zero_literal
	DW to_number_sign
	DW c_store
	DW exit

	; \ Prefix pictured numeric string with given character
	; : HOLD ( c -- )
	HEADER hold, "HOLD", 0
hold:
	CALL colon_code
	; \ Decrement >#
	; -1 ># C+!
	DW literal_raw
	DW -1
	DW to_number_sign
	DW c_plus_store
	; \ Store character at (># + 'WORD)
	; 'WORD ># C@ + C! \
	DW tick_word
	DW to_number_sign
	DW c_fetch
	DW plus
	DW c_store
	DW exit

	; \ Add a '-' character to the pictured numeric string if n less than 0
	; : SIGN ( n -- )
	HEADER sign, "SIGN", 0
sign:
	CALL colon_code
	; 0< IF [CHAR] - HOLD THEN \
	DW zero_less
	DW if_raw
	DB .then-$-1
	DW raw_char
	DB '-'
	DW hold
.then:
	DW exit


	; \ Convert a digit to its character representation
	; : DIGIT ( n -- c )
	HEADER digit, "DIGIT", 0
digit:
	CALL colon_code
	; DUP 9 > IF [ CHAR A CHAR 0 - 10 - ] LITERAL + THEN
	DW dup
	DW raw_char
	DB 9
	DW greater_than
	DW if_raw
	DB .then-$-1
	DW raw_char
	DB 'A' - '0' - 10
	DW plus
.then:
	; [CHAR] 0 + \
	DW raw_char
	DB '0'
	DW plus
	DW exit

	; \ Divide ud1 by BASE, quotient goes in ud2, remainder converted to
	; \ digit and prefixed to pictured numeric output string.
	; : # ( ud1 -- ud2 )
	HEADER number_sign, "#", 0
number_sign:
	CALL colon_code
	; BASE @  DUB/MOD  DIGIT HOLD \
	DW base
	DW fetch
	DW dub_slash_mod
	DW digit
	DW hold
	DW exit

	; \ Drop double cell, get pictured numeric string
	; : #> ( xd -- c-addr u )
	HEADER number_sign_greater, "#>", 0
number_sign_greater:
	CALL colon_code
	; 2DROP  ># C@ 'WORD +  256 ># C@ -  \
	DW two_drop
	DW to_number_sign
	DW c_fetch
	DW tick_word
	DW plus
	DW literal_raw
	DW 256
	DW to_number_sign
	DW c_fetch
	DW minus
	DW exit

	; \ Do # until quotient is zero
	; : #S ( ud1 -- ud2 )
	HEADER number_sign_s, "#S", 0
number_sign_s:
	CALL colon_code
	; BEGIN # 2DUP D0= UNTIL
.begin:
	DW number_sign
	DW two_dup
	DW d_zero_equals
	DW until_raw
	DB .begin-$+256
	DW exit


	; \ Print an unsigned double number in the current BASE
	; : DU. ( ud -- )
	HEADER du_dot, "DU.", 0
du_dot:
	CALL colon_code
	; <# #S #> TYPE SPACE ;
	DW less_number_sign
	DW number_sign_s
	DW number_sign_greater
	DW type
	DW space
	DW exit


	; \ Print a double number in the current BASE, field width n
	;  ( d n --)
	; : D.R
	HEADER d_dot_r, "DR.", 0
d_dot_r:
	CALL colon_code
	; >R
	DW to_r
	; ( d) ( R:n)
	; TUCK
	DW tuck
	; ( high d)
	; <#
	DW less_number_sign
	; DUP 0<  IF  0. 2SWAP D-  THEN
	DW dup
	DW zero_less
	DW if_raw
	DB .then-$-1
	DW zero_literal
	DW zero_literal
	DW two_swap
	DW d_minus
.then:
	; #S
	DW number_sign_s
	; ROT SIGN
	DW rot
	DW sign
	; ( d)
	; #> R>
	DW number_sign_greater
	DW r_from
	; ( addr u n) ( R:)
	; \ Print padding spaces
	; 2DUP < IF OVER - SPACES ELSE DROP THEN
	DW two_dup
	DW less_than
	DW if_raw
	DB .else_spaces-$-1
	DW over
	DW minus
	DW spaces
	DW else_skip
	DB .then_spaces-$-1
.else_spaces:
	DW drop
.then_spaces:
	; ( addr u)
	; \ Print number
	; TYPE ;
	DW type
	DW exit


	; \ Print a double number in the current BASE
	; : D. ( d -- ) 0 D.R SPACE ;
	HEADER d_dot, "D.", 0
d_dot:
	CALL colon_code
	DW zero_literal
	DW d_dot_r
	DW space
	DW exit


	; \ Print an unsigned number in the current BASE
	; : U. ( u -- )
	HEADER u_dot, "U.", 0
u_dot:
	CALL colon_code
	; BASE @ 16 = IF U$. EXIT THEN
	DW base
	DW fetch
	DW raw_char
	DB 16
	DW equals
	DW if_raw
	DB .then1-$-1
	DW u_dollar_dot
	DW exit
.then1:
	; 0 DU. \
	DW zero_literal
	DW du_dot
	DW exit


	; \ Convert single to double number
	; : S>D ( n -- d )
	HEADER s_to_d, "S>D", 0
s_to_d:
	CALL colon_code
	; DUP 0< IF -1 ELSE 0 THEN \
	DW dup
	DW zero_less
	DW if_raw
	DB .else-$-1
	DW literal_raw
	DW -1
	DW else_skip
	DB .then-$-1
.else:
	DW zero_literal
.then:
	DW exit


	; \ Print number in current BASE, followed by space
	; : . ( n -- )
	HEADER dot, ".", 0
dot:
	CALL colon_code
	; S>D D. ;
	DW s_to_d
	DW d_dot
	DW exit


	; \ Print number in current BASE, with given min. field size
	; : .R ( n m -- ) SWAP S>D ROT D.R ;
	HEADER dot_r, ".R", 0
dot_r:
	CALL colon_code
	DW swap
	DW s_to_d
	DW rot
	DW d_dot_r
	DW exit


	; \ Set border to attr
	; ( attr ) : BRDR!
	HEADER brdr_store, "BRDR!", 0
brdr_store:
	CALL colon_code
	; 7 AND  ULA P@  0xF8 AND  OR  ULA P! \
	DW raw_char
	DB 7
	DW and
	DW ula
	DW p_fetch
	DW raw_char
	DB 0xF8
	DW and
	DW or
	DW ula
	DW p_store
	DW exit


	; \ Clear screen, reset terminal to top-left
	; : PAGE
	HEADER page, "PAGE", 0
page:
	CALL colon_code
	; \ Match border to T-ATTR
	; T-ATTR C@  3 RSHIFT  BRDR!
	DW t_attr
	DW c_fetch
	DW raw_char
	DB 3
	DW rshift
	DW brdr_store
	; \ Reset terminal col/row
	; 0 0 AT-XY
	DW zero_literal
	DW zero_literal
	DW at_xy
	; \ Erase bitmap
	; DISP-FILE DISP-SIZE ERASE
	DW disp_file
	DW disp_size
	DW erase
	; \ Set attr region to current T-ATTR
	; ATTR-FILE ATTR-SIZE T-ATTR C@ FILL
	DW attr_file
	DW attr_size
	DW t_attr
	DW c_fetch
	DW fill
	; \
	DW exit


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
	POP HL
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
	DW s_zero
	DW sp_store
	DW drop
	DW quit


	; : (ABORT") TYPE ABORT ; -2 ALLOT
	HEADER abort_quote_raw, '(ABORT")', 0
abort_quote_raw:
	CALL colon_code
	DW type
	DW abort


	; : ALLOT ( n -- ) \ Add n to dictionary end pointer
	HEADER allot, "ALLOT", 0
allot:
	CALL colon_code
	; H +! \
	DW h_
	DW plus_store
	DW exit


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
	; HERE 1 ALLOT C! \
	DW here
	DW one_literal
	DW allot
	DW c_store
	DW exit


	; : COUNT ( addr -- addr2 u ) \ Get string in counted string
	HEADER count, "COUNT", 0
count:
	CALL colon_code
	; DUP 1+ SWAP C@ \
	DW dup
	DW one_plus
	DW swap
	DW c_fetch
	DW exit


	; \ Set BASE to 10
	; : DECIMAL ( -- )
	HEADER decimal, "DECIMAL", 0
decimal:
	CALL colon_code
	; 10 BASE ! \
	DW raw_char
	DB 10
	DW base
	DW store
	DW exit


	HEADER depth, "DEPTH", 0
depth:
	CALL colon_code
	DW s_zero
	DW tick_s
	DW minus
	DW two_slash
	DW one_minus
	DW exit


	; \ Set BASE to 16
	; : HEX ( -- )
	HEADER hex, "HEX", 0
hex:
	CALL colon_code
	; 16 BASE ! \
	DW raw_char
	DB 16
	DW base
	DW store
	DW exit


	; : MAX 2DUP < IF SWAP THEN DROP \
	HEADER max, "MAX", 0
max:
	CALL colon_code
	DW two_dup
	DW less_than
	DW if_raw
	DB .skip-$-1
	DW swap
.skip:
	DW drop
	DW exit


	HEADER move, "MOVE", 0
move:
	CALL colon_code
	; : MOVE -ROT 2DUP < IF ROT CMOVE> ELSE ROT CMOVE THEN \
	DW minus_rot
	DW two_dup
	DW less_than
	DW if_raw
	DB .move__else-$-1
	DW rot
	DW cmove_up
	DW else_skip
	DB .move__else_skip-$-1
.move__else:
	DW rot
	DW cmove
.move__else_skip:
	DW exit


	; : NEGATE  0 SWAP - ;
	HEADER negate, "NEGATE", 0
negate:
	CALL colon_code
	DW zero_literal
	DW swap
	DW minus
	DW exit


	; : DNEGATE  0. 2SWAP D- ;
	HEADER dnegate, "DNEGATE", 0
dnegate:
	CALL colon_code
	DW two_literal_raw
	DW 0
	DW 0
	DW two_swap
	DW d_minus
	DW exit


	; \ If n1 = n2 skip the ?DO..LOOP, otherwise move n1+n2 to return stack
	; : (?DO) ( n1 n2 -- ) ( R: -- | n1 n2 )
	HEADER question_do_raw, "(?DO)", 0
question_do_raw:
	CALL colon_code
	; r> 1+ -rot 2dup = IF 2drop dup 1- c@ + ELSE 2>r THEN >r \
	DW r_from
	DW one_plus
	DW minus_rot
	DW two_dup
	DW equals
	DW if_raw
	DB .else-$-1
	DW two_drop
	DW dup
	DW one_minus
	DW c_fetch
	DW plus
	DW else_skip
	DB .then-$-1
.else:
	DW two_to_r
.then:
	DW to_r
	DW exit


	HEADER s_quote_raw, '(S")', 0
s_quote_raw:
	CALL colon_code
	; R> COUNT 2DUP + >R \
	DW r_from
	DW count
	DW two_dup
	DW plus
	DW to_r
	DW exit


	HEADER dot_quote_raw, '(.")', 0
dot_quote_raw:
	CALL colon_code
	; R> COUNT 2DUP + >R TYPE \
	DW r_from
	DW count
	DW two_dup
	DW plus
	DW to_r
	DW type
	DW exit


	; \ Read a line of input into buffer, return length of line read
	; : ACCEPT ( buf size -- n )
	HEADER accept, "ACCEPT", 0
accept:
	CALL colon_code
	; SWAP >R 0
	DW swap
	DW to_r
	DW zero_literal
	; ( size idx ) ( R: buf )
	; BEGIN 2DUP > WHILE
.begin:
	DW two_dup
	DW greater_than
	DW if_raw
	DB .repeat-$-1
		; ( size idx ) ( R: buf )
		; KEY DUP CASE
		DW key
		DW dup
			; ( size idx key key ) ( R: buf )
			; \ Delete: remove character
			; 8  OF EMIT  1- 0 MAX        ENDOF
			DW raw_char
			DB 8
			DW of_raw
			DB .endof1-$-1
			DW emit
			DW one_minus
			DW zero_literal
			DW max
			DW else_skip
			DB .endcase-$-1
.endof1:
			; \ Enter: finish input
			; 10 OF DROP NIP R> DROP EXIT ENDOF
			DW raw_char
			DB 10
			DW of_raw
			DB .endof2-$-1
			DW drop
			DW nip
			DW r_from
			DW drop

			DW exit
			DW else_skip
			DB .endcase-$-1
.endof2:
			; \ default: output character
			; EMIT OVER R@ + C!  1+
			DW emit
			DW over
			DW r_fetch
			DW plus
			DW c_store
			DW one_plus
		; 0 ENDCASE
		DW zero_literal
		DW drop
.endcase:
	; REPEAT
	DW again_raw
	DB .begin-$+256
.repeat:
	; ( size idx ) ( R: buf )
	; R> DROP NIP \
	DW r_from
	DW drop
	DW nip
	DW exit


	; \ Read a line of input into -IN
	; : -READ ( -- addr u )
	HEADER line_read, "-READ", 0
line_read:
	CALL colon_code
	; -IN DUP -IN# ACCEPT \
	DW line_in
	DW dup
	DW line_in_size
	DW accept
	DW exit


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
	DW count
	DW sfind
	; ( addr u 0 | xt 1 | xt -1)
	; ?DUP 0= IF
	DW question_dup
	DW zero_equals
	DW if_raw
	DB .then-$-1
		; ( addr u)
		; DROP 1- 0
		DW drop
		DW one_minus
		DW zero_literal
	; THEN ;
.then:
	DW exit


	; \ See next parse character or 0 if parse area empty
	; : PPEEK ( -- c | 0 )
	HEADER ppeek, "PPEEK", 0
ppeek:
	CALL colon_code
	; >IN @
	DW to_in
	DW fetch
	; ( >IN-val)
	; DUP  IN# @  < IF
	DW dup
	DW in_size
	DW fetch
	DW less_than
	DW if_raw
	DB .else-$-1
		; 'IN @ + C@
		DW tick_in
		DW fetch
		DW plus
		DW c_fetch
		; ( c)
	; ELSE
	DW else_skip
	DB .then-$-1
.else:
		; DROP 0
		DW drop
		DW zero_literal
		; ( 0)
	; THEN ;
.then:
	DW exit


	; \ Parse character or 0 if parse area empty
	; : PCHAR ( "c" -- c | 0 )
	HEADER pchar, "PCHAR", 0
pchar:
	CALL colon_code
	; PPEEK DUP IF 1 >IN +! THEN ;
	DW ppeek
	DW dup
	DW if_raw
	DB .then-$-1
	DW one_literal
	DW to_in
	DW plus_store
.then:
	DW exit


	; \ Parse a string from parse area, and return address/length of it in
	; \ parse area.
	; ( c "...<c>" -- addr u)
	; : PARSE
	HEADER parse, "PARSE", 0
parse:
	CALL colon_code
	; 'IN @ >IN @ +
	DW tick_in
	DW fetch
	DW to_in
	DW fetch
	DW plus
	; ( c addr)
	; 0 ROT
	DW zero_literal
	DW rot
	; ( addr u c)
	; BEGIN
.begin:
		; \ Stop when empty
		; PCHAR ?DUP WHILE
		DW pchar
		DW question_dup
		DW if_raw
		DB .repeat-$-1
		; ( addr u c p)
		; \ Stop when delimiter
		; OVER <> WHILE
		DW over
		DW not_equals
		DW if_raw
		DB .repeat-$-1
		; ( addr u c)
		; \ Increment length
		; SWAP 1+ SWAP
		DW swap
		DW one_plus
		DW swap
	; REPEAT THEN
	DW again_raw
	DB .begin-$+256
.repeat:
	; DROP ;
	DW drop
	DW exit

	; \ Parse and skip a string of delimiters
	; ( c "<c's>" --)
	; : PSKIP
	HEADER pskip, "PSKIP", 0
pskip:
	CALL colon_code
	; BEGIN
.begin:
		; PPEEK ?DUP WHILE
		DW ppeek
		DW question_dup
		DW if_raw
		DB .then-$-1
		; OVER = WHILE
		DW over
		DW equals
		DW if_raw
		DB .then-$-1
		; 1 >IN +!
		DW one_literal
		DW to_in
		DW plus_store
	; REPEAT THEN
	DW again_raw
	DB .begin-$+256
.then:
	; DROP ;
	DW drop
	DW exit


	; \ Parse a string from parse area, skipping preceding delimiters, and
	; \ return address/length of it in parse area.
	; ( c "<c's>...<c>" -- addr u)
	; : PARSE-WORD
	HEADER parse_word, "PARSE-WORD", 0
parse_word:
	CALL colon_code
	; \ Skip preceding delimiters
	; DUP PSKIP
	DW dup
	DW pskip
	; \ Parse rest
	; PARSE ;
	DW parse
	DW exit


	; \ Parse a name from parse area, and return address/length of it in
	; \ parse area.
	; ( "<spaces>...<space>" -- addr u)
	; : PARSE-NAME BL PARSE-WORD ;
	HEADER parse_name, "PARSE-NAME", 0
parse_name:
	CALL colon_code
	DW bl
	DW parse_word
	DW exit


	; \ Parse counted string www, terminated by c
	; : CPARSE ( c "www<c>" -- addr )
	HEADER cparse, "CPARSE", 0
cparse:
	CALL colon_code
	; PARSE
	DW parse
	; ( addr u)
	; DUP 256 >= IF ABORT" long name" THEN
	DW dup
	DW literal_raw
	DW 256
	DW greater_than_or_equal
	DW if_raw
	DB .then-$-1
	DW s_quote_raw
	DB .e1-.s1
.s1:
	DM "long name"
.e1:
	DW abort_quote_raw
.then:
	; TUCK
	DW tuck
	; ( u addr u)
	; 'WORD 1+ SWAP
	DW tick_word
	DW one_plus
	DW swap
	; ( u addr addr2 u)
	; CMOVE
	DW cmove
	; ( u)
	; 'WORD C!
	DW tick_word
	DW c_store
	; ( )
	; 'WORD ;
	DW tick_word
	DW exit


	; \ Parse counted string www, delimited by c
	; ( c "<c...>www<c>" -- addr )
	; : WORD
	HEADER word, "WORD", 0
word:
	CALL colon_code
	; \ Ignore initial delimiters
	; DUP PSKIP
	DW dup
	DW pskip
	; \ Parse
	; CPARSE ;
	DW cparse
	DW exit


	; \ Check if a character is a valid double number punctuator
	; ( c -- ?)
	; : NUM-PUNC?
	HEADER num_punc_question, "NUM-PUNC?", 0
num_punc_question:
	CALL colon_code
	; CASE
		; [CHAR] , OF TRUE ENDOF
		DW raw_char
		DB ','
		DW of_raw
		DB .endof1-$-1
		DW true
		DW else_skip
		DB .endcase-$-1
.endof1:
		; [CHAR] . OF TRUE ENDOF
		DW raw_char
		DB '.'
		DW of_raw
		DB .endof2-$-1
		DW true
		DW else_skip
		DB .endcase-$-1
.endof2:
		; [CHAR] + OF TRUE ENDOF
		DW raw_char
		DB '+'
		DW of_raw
		DB .endof3-$-1
		DW true
		DW else_skip
		DB .endcase-$-1
.endof3:
		; [CHAR] - OF TRUE ENDOF
		DW raw_char
		DB '-'
		DW of_raw
		DB .endof4-$-1
		DW true
		DW else_skip
		DB .endcase-$-1
.endof4:
		; [CHAR] / OF TRUE ENDOF
		DW raw_char
		DB '/'
		DW of_raw
		DB .endof5-$-1
		DW true
		DW else_skip
		DB .endcase-$-1
.endof5:
		; [CHAR] : OF TRUE ENDOF
		DW raw_char
		DB ':'
		DW of_raw
		DB .endof6-$-1
		DW true
		DW else_skip
		DB .endcase-$-1
.endof6:
		; FALSE SWAP
		DW false
		DW swap
	; ENDCASE ;
	DW drop
.endcase:
	DW exit


	; ( ud addr u -- ud2 addr2 u2)
	; : >NUMBER
	HEADER to_number, ">NUMBER", 0
to_number:
	CALL colon_code
	; \ Loop while chars remaining
	; DUP IF BEGIN
	DW dup
	DW if_raw
	DB .then_skip-$-1
.begin:
		; \ Get char
		; OVER C@
		DW over
		DW c_fetch
		; ( ud addr u c)
		; \ Handle 0-9 / a-z apart
		; DUP [CHAR] 0 [ CHAR 9 1+ ] LITERAL WITHIN IF
		DW dup
		DW raw_char
		DB '0'
		DW raw_char
		DB '9'+1
		DW within
		DW if_raw
		DB .else-$-1
			; \ Convert char to number
			; [CHAR] 0 -
			DW raw_char
			DB '0'
			DW minus
			; ( ud addr u n)
		; ELSE
		DW else_skip
		DB .then_apart-$-1
.else:
			; \ Convert char to number
			; [ CHAR A 10 - ] -
			DW raw_char
			DB 'A'-10
			DW minus
			; ( ud addr u n)
		; THEN
.then_apart:
		; \ Must be within current base
		; DUP BASE @ 0 WITHIN IF DROP EXIT THEN
		DW dup
		DW base
		DW fetch
		DW zero_literal
		DW within
		DW if_raw
		DB .then_base-$-1
		DW drop
		DW exit
.then_base:
		; 0 2ROT
		DW zero_literal
		DW two_rot
		; ( addr u dn ud)
		; \ Multiply current number by base
		; BASE @ 1 M*/
		DW base
		DW fetch
		DW one_literal
		DW m_star_slash
		; \ Add digit to current number
		; D+ 2SWAP
		DW d_plus
		DW two_swap
		; ( ud addr u)
		; \ Next char
		; SWAP 1+ SWAP 1-
		DW swap
		DW one_plus
		DW swap
		DW one_minus
	; DUP 0= UNTIL THEN ;
	DW dup
	DW zero_equals
	DW until_raw
	DB .begin-$+256
.then_skip:
	DW exit


	; \ Type a string with question mark and abort
	; ( addr u --)
	; : WHAT? TYPE ABORT" ?" ; -2 ALLOT
	HEADER what_question, "WHAT?", 0
what_question:
	CALL colon_code
	DW type
	DW s_quote_raw
	DB .s2-.s1
.s1:
	DM "?"
.s2:
	DW abort_quote_raw


	; \ Type a counted string with question mark and abort
	; ( c-addr --)
	; : CWHAT? COUNT WHAT? ; -2 ALLOT
	HEADER cwhat_question, "CWHAT?", 0
cwhat_question:
	CALL colon_code
	DW count
	DW what_question


	; \ Parse single or double number from counted string
	; ( c-addr -- n|d is-double?)
	; : NUMBER
	HEADER number, "NUMBER", 0
number:
	CALL colon_code
	; DUP COUNT
	DW dup
	DW count
	; ( c-addr addr u)
	; \ Fail on empty string
	; ?DUP 0= IF DROP ABORT-TYPE THEN
	DW question_dup
	DW zero_equals
	DW if_raw
	DB .then_empty-$-1
	DW drop
	DW cwhat_question
.then_empty:
	; \ Ignore first char addr/u if '-'
	; OVER C@ [CHAR] - = IF
	DW over
	DW c_fetch
	DW raw_char
	DB '-'
	DW equals
	DW if_raw
	DB .then_ignore_minus-$-1
		; SWAP 1+ SWAP 1-
		DW swap
		DW one_plus
		DW swap
		DW one_minus
	; THEN
.then_ignore_minus:
	; \ Fail on empty string
	; ?DUP 0= IF DROP ABORT-TYPE THEN
	DW question_dup
	DW zero_equals
	DW if_raw
	DB .then_empty2-$-1
	DW drop
	DW cwhat_question
.then_empty2:
	; 0. 2SWAP
	DW zero_literal
	DW zero_literal
	DW two_swap
	; ( c-addr d-num addr u)
	; \ Attempt conversion
	; >NUMBER ?DUP IF
	DW to_number
	DW question_dup
	DW if_raw
	DB .else_single-$-1
		; ( c-addr d-num addr u)
		; \ Characters remain: double or bad char
		; BEGIN
.begin:
			; \ Check for bad char
			; OVER C@ NUM-PUNC? 0= IF
			DW over
			DW c_fetch
			DW num_punc_question
			DW zero_equals
			DW if_raw
			DB .then_bad-$-1
				; 2DROP 2DROP ABORT-TYPE
				DW two_drop
				DW two_drop
				DW cwhat_question
			; THEN
.then_bad:
			; \ Advance char
			; SWAP 1+ SWAP 1-
			DW swap
			DW one_plus
			DW swap
			DW one_minus
			; \ Check remaining
			; ?DUP WHILE
			DW question_dup
			DW if_raw
			DB .repeat-$-1
			; \ Convert more and check remaining
			; >NUMBER ?DUP WHILE
			DW to_number
			DW question_dup
			DW if_raw
			DB .repeat-$-1
		; REPEAT THEN
		DW again_raw
		DB .begin-$+256
.repeat:
		; ( c-addr d-num addr)
		; DROP ROT
		DW drop
		DW rot
		; ( d-num c-addr)
		; \ If first char '-' negate
		; 1+ C@ [CHAR] - = IF DNEGATE THEN
		DW one_plus
		DW c_fetch
		DW raw_char
		DB '-'
		DW equals
		DW if_raw
		DB .then_negate-$-1
		DW dnegate
.then_negate:
		; ( d-num)
		; TRUE
		; ( d-num true)
		DW true
	; ELSE
	DW else_skip
	DB .then_single-$-1
.else_single:
		; ( c-addr d-num addr)
		; \ No chars remain: single
		; 2DROP SWAP
		DW two_drop
		DW swap
		; ( num c-addr)
		; \ Check for '-'
		; 1+ C@ [CHAR] - = IF NEGATE THEN
		DW one_plus
		DW c_fetch
		DW raw_char
		DB '-'
		DW equals
		DW if_raw
		DB .then_negate2-$-1
		DW negate
.then_negate2:
		; ( num)
		; FALSE
		; ( d-num false)
		DW false
	; THEN ;
.then_single:
	DW exit


	; ( xt -- )
	; : COMPILE, , ;
	HEADER compile_comma, "COMPILE,", 0
compile_comma:
	CALL colon_code
	DW comma
	DW exit


	; \ Use before exiting from a DO..LOOP
	; : unloop ( R: x1 x2 -- )
	HEADER unloop, "UNLOOP", 0
unloop:
	CALL colon_code
	; r> 2r> 2drop >r \
	DW r_from
	DW two_r_from
	DW two_drop
	DW to_r
	DW exit


	; : lower ( C -- c )
	HEADER lower, "LOWER", 0
lower:
	CALL colon_code
	; dup [CHAR] A [ CHAR Z 1+ ] LITERAL within IF
	DW dup
	DW raw_char
	DB 'A'
	DW raw_char
	DB 'Z' + 1
	DW within
	DW if_raw
	DB .then-$-1
		; [ CHAR A CHAR a - ] LITERAL +
		DW raw_char
		DB 'a' - 'A'
		DW plus
	; THEN \
.then:
	DW exit


	; \ Convert lowercase letters to uppercase (or do nothing)
	; : upper ( c -- C )
	HEADER upper, "UPPER", 0
upper:
	CALL colon_code
	; DUP [CHAR] a [ CHAR z 1+ ] LITERAL within IF
	DW dup
	DW raw_char
	DB 'a'
	DW raw_char
	DB 'z' + 1
	DW within
	DW if_raw
	DB .then-$-1
		; [ 'A' 'a' - ] LITERAL +
		DW literal_raw
		DW 'A' - 'a'
		DW plus
	; THEN \
.then:
	DW exit


	; : ERASE 0 FILL \
	HEADER erase, "ERASE", 0
erase:
	CALL colon_code
	DW zero_literal
	DW fill
	DW exit


	HEADER space, "SPACE", 0
space:
	CALL colon_code
	DW bl
	DW emit
	DW exit


	; : SPACES ( n -- ) \ Print n spaces
	HEADER spaces, "SPACES", 0
spaces:
	CALL colon_code
	; 0 MAX
	DW zero_literal
	DW max
	; 0 ?DO SPACE LOOP \
	DW zero_literal
	DW question_do_raw
	DB .loop-$-1
.do:
	DW space
	DW loop_raw
	DB .do-$+256
.loop:
	DW exit


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
	; TUCK2 2@ D+ ROT 2! \
	DW tuck2
	DW two_fetch
	DW d_plus
	DW rot
	DW two_store
	DW exit


	; : (2LITERAL)  R>  DUP [ 2 CELLS ] LITERAL + >R  2@  \
	HEADER two_literal_raw, "(2LITERAL)", 0
two_literal_raw:
	CALL colon_code
	DW r_from
	DW dup
	DW raw_char
	DB 4
	DW plus
	DW to_r
	DW two_fetch
	DW exit


	; AT-XY ( x y -- ) \ Set next terminal x,y position
	HEADER at_xy, "AT-XY", 0
at_xy:
	CALL colon_code
	; ( y ) 0 23 CLAMP T-ROW C!
	DW zero_literal
	DW raw_char
	DB 23
	DW clamp
	DW t_row
	DW c_store
	; ( x ) 0 31 CLAMP T-COL C!
	DW zero_literal
	DW raw_char
	DB 31
	DW clamp
	DW t_col
	DW c_store
	; \
	DW exit


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
	; KEYQ-E C@ KEYQ-S C@ <> \
	DW keyq_e
	DW c_fetch
	DW keyq_s
	DW c_fetch
	DW not_equals
	DW exit


	; 0<> ( n -- flags ) \ true if n is not equal to 0
	HEADER zero_not_equals, "0<>", 0
zero_not_equals:
	CALL colon_code
	; 0= INVERT \
	DW zero_equals
	DW invert
	DW exit


	; <> ( n1 n2 -- flags ) \ true if n1 is not equal to n2
	HEADER not_equals, "<>", 0
not_equals:
	CALL colon_code
	; = INVERT \
	DW equals
	DW invert
	DW exit


	; CLAMP ( n1 n2 n3 -- n ) \ Force n1 to range [n2, n3]
	HEADER clamp, "CLAMP", 0
clamp:
	CALL colon_code
	; ROT MIN MAX \
	DW rot
	DW min
	DW max
	DW exit


	; : MIN ( n1 n2 -- n ) \ Leave the smaller of n1 and n2
	HEADER min, "MIN", 0
min:
	CALL colon_code
	; 2DUP > IF SWAP THEN DROP \
	DW two_dup
	DW greater_than
	DW if_raw
	DB .skip-$-1
	DW swap
.skip:
	DW drop
	DW exit


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
	DW ekey_question
	DW halt_
	DW until_raw
	DB .begin-$+256
	; KEYQ KEYQ-S C@ CELLS + @ \ Receive event
	DW keyq
	DW keyq_s
	DW c_fetch
	DW two_star
	DW plus
	DW fetch
	; KEYQ-S C@ 1+ 7 AND KEYQ-S C! \ Increment KEYQ-S (mod 8)
	DW keyq_s
	DW c_fetch
	DW one_plus
	DW raw_char
	DB 7
	DW and
	DW keyq_s
	DW c_store
	; \ If CAPS LOCK then toggle CAPS
	; DUP $11C = IF
	DW dup
	DW literal_raw
	DW 0x11C
	DW equals
	DW if_raw
	DB .then-$-1
		; CAPS C@ 0= CAPS C!
		DW caps
		DW c_fetch
		DW zero_equals
		DW caps
		DW c_store
	; THEN
.then:
	; ;
	DW exit


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
	DW dup
	DW raw_char
	DB 0xFF
	DW and
	; \ If shift active...
	; 2DUP <> IF
	DW two_dup
	DW not_equals
	DW if_raw
	DB .then1-$-1
		; \ If symbol shift, add 80, otherwise 40
		; OVER $200 AND  80 40 CHOOSE  +
		DW over
		DW literal_raw
		DW 0x200
		DW and
		DW raw_char
		DB 80
		DW raw_char
		DB 40
		DW choose
		DW plus
	; THEN
.then1:
	; CHARS KMAP + C@ ?DUP IF
	DW kmap
	DW plus
	DW c_fetch
	DW question_dup
	DW if_raw
	DB .else1-$-1
		; NIP
		DW nip
		; \ Invert case if CAPS
		; CAPS C@ IF
		DW caps
		DW c_fetch
		DW if_raw
		DB .then_caps-$-1
			; DUP [CHAR] A [ CHAR Z 1+ ] LITERAL WITHIN IF
			DW dup
			DW raw_char
			DB 'A'
			DW raw_char
			DB 'Z' + 1
			DW within
			DW if_raw
			DB .else_invert_case-$-1
				; [ CHAR a CHAR A - ] LITERAL +
				DW raw_char
				DB 'a' - 'A'
				DW plus
			; ELSE DUP [CHAR] a [ CHAR z 1+ ] LITERAL WITHIN IF
			DW else_skip
			DB .then_invert_case-$-1
.else_invert_case:
			DW dup
			DW raw_char
			DB 'a'
			DW raw_char
			DB 'z' + 1
			DW within
			DW if_raw
			DB .then_invert_case-$-1
				; [ CHAR A CHAR a - ] LITERAL +
				DW literal_raw
				DW 'A' - 'a'
				DW plus
			; THEN THEN
.then_invert_case:
		; THEN
.then_caps:
		; TRUE
		DW true
	; ELSE
	DW else_skip
	DB .then2-$-1
.else1:
		; FALSE
		DW zero_literal
	; THEN
.then2:
	; ;
	DW exit


	HEADER less_than_or_equal, "<=", 0
less_than_or_equal:
	CALL colon_code
	DW greater_than
	DW zero_equals
	DW exit


	HEADER greater_than_or_equal, ">=", 0
greater_than_or_equal:
	CALL colon_code
	DW less_than
	DW zero_equals
	DW exit


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
	POP IY
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
	POP IY
	JP next
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
	; 1 SWAP LSHIFT AND \
	DW one_literal
	DW swap
	DW lshift
	DW and
	DW exit


	; : CHOOSE ( flags x1 x2 -- x1|x2 ) \ Select x1 if flags true, o/w x2
	HEADER choose, "CHOOSE", 0
choose:
	CALL colon_code
	; ROT IF SWAP THEN NIP \
	DW rot
	DW if_raw
	DB .then-$-1
	DW swap
.then:
	DW nip
	DW exit


	; : 2OR ( d1 d2 -- d1|d2 ) \ OR of doubles
	HEADER two_or, "2OR", 0
two_or:
	CALL colon_code
	; ROT OR -ROT OR SWAP \
	DW rot
	DW or
	DW minus_rot
	DW or
	DW swap
	DW exit


	; : D0< ( d -- flags ) \ Is a double integer negative?
	HEADER d_zero_less, "D0<", 0
d_zero_less:
	CALL colon_code
	; NIP 0< \
	DW nip
	DW zero_less
	DW exit


	; : 2ROT ( d1 d2 d3 -- d2 d3 d1 )
	HEADER two_rot, "2ROT", 0
two_rot:
	CALL colon_code
	; 2>R 2SWAP 2R> 2SWAP \
	DW two_to_r
	DW two_swap
	DW two_r_from
	DW two_swap
	DW exit


	; : 2-ROT ( d1 d2 d3 -- d3 d1 d2 )
	HEADER two_minus_rot, "2-ROT", 0
two_minus_rot:
	CALL colon_code
	; 2SWAP 2>R 2SWAP 2R> \
	DW two_swap
	DW two_to_r
	DW two_swap
	DW two_r_from
	DW exit


	; : D<
	HEADER d_less_than, "D<", 0
d_less_than:
	CALL colon_code
	; 2SWAP $80000000. D+ 2SWAP $80000000. D+ DU< \
	DW two_swap
	DW two_literal_raw
	DW 0x8000
	DW 0
	DW d_plus
	DW two_swap
	DW two_literal_raw
	DW 0x8000
	DW 0
	DW d_plus
	DW du_less_than
	DW exit


	; : 2OVER2 ( d1 d2 d3 -- d1 d2 d3 d1 )
	HEADER two_over_two, "2OVER2", 0
two_over_two:
	CALL colon_code
	; 'S 8 + 2@ \
	DW tick_s
	DW raw_char
	DB 8
	DW plus
	DW two_fetch
	DW exit


	; : D0= OR 0= ;
	HEADER d_zero_equals, "D0=", 0
d_zero_equals:
	CALL colon_code
	DW or
	DW zero_equals
	DW exit


	; \ Divide and produce remainder in double integers
	; : DU/MOD ( ud1 ud2 -- ud3 ud4 ) \ Where ud1 is numerator, ud2 is
	;                                 \ denominator, ud3 is result, ud4 is
	;                                 \ remainder.
	HEADER du_slash_mod, "DU/MOD", 0
du_slash_mod:
	CALL colon_code
	; \ Refuse to divide by 0
	; \ TODO 2DUP D0= IF ABORT" Div by 0" THEN
	; 1. 2-ROT
	DW one_literal
	DW zero_literal
	DW two_minus_rot
	; ( rem den unit )
	; \ Shift den and unit, while den smaller than rem,
	; \ and until before den overflows.
	; BEGIN 2OVER 2OVER 2SWAP DU< IF 2DUP D0< INVERT ELSE FALSE THEN WHILE
.begin1:
	DW two_over
	DW two_over
	DW two_swap
	DW du_less_than
	DW if_raw
	DB .else-$-1
	DW two_dup
	DW d_zero_less
	DW invert
	DW else_skip
	DB .then2-$-1
.else:
	DW zero_literal
.then2:
	DW if_raw
	DB .repeat1-$-1
		; 2ROT D2* 2-ROT D2*
		DW two_rot
		DW d_two_star
		DW two_minus_rot
		DW d_two_star
	; REPEAT
	DW again_raw
	DB .begin1-$+256
.repeat1:

	; 0. 2>R  2SWAP
	DW zero_literal
	DW zero_literal
	DW two_to_r
	DW two_swap
	; ( unit div rem ) ( R:result )
	; BEGIN
.begin2:
		; \ If remainder at least divisor, sub and OR unit to result
		; 2OVER 2OVER 2SWAP DU< INVERT IF
		DW two_over
		DW two_over
		DW two_swap
		DW du_less_than
		DW invert
		DW if_raw
		DB .then-$-1
			; 2OVER D-
			DW two_over
			DW d_minus
			; 2ROT 2DUP 2R> 2OR 2>R 2-ROT
			DW two_rot
			DW two_dup
			DW two_r_from
			DW two_or
			DW two_to_r
			DW two_minus_rot
		; THEN
.then:

		; \ Shift right each step until divisor gone
		; 2ROT DU2/ 2ROT DU2/ 2ROT
		DW two_rot
		DW du_two_slash
		DW two_rot
		DW du_two_slash
		DW two_rot
	; 2OVER2 D0= UNTIL
	DW two_over_two
	DW d_zero_equals
	DW until_raw
	DB .begin2-$+256

	; \ Return result + remainder
	; 2-ROT 2DROP 2DROP 2R> 2SWAP \
	DW two_minus_rot
	DW two_drop
	DW two_drop
	DW two_r_from
	DW two_swap
	DW exit


	; : UM/MOD ( ud u1 -- u2 u3 ) \ Divide by u1, quo u2 rem u3
	HEADER um_slash_mod, "UM/MOD", 0
um_slash_mod:
	CALL colon_code
	; 0 DU/MOD DROP NIP \
	DW zero_literal
	DW du_slash_mod
	DW drop
	DW nip
	DW exit


	; : TONE  ( len period -- ) \ Make an accurate tone, masking interrupts
	HEADER tone, "TONE", 0
tone:
	CALL colon_code
	; DI ITONE EI \
	DW _di
	DW itone
	DW _ei
	DW exit


	; : CLICK ( -- ) \ Make a 'click' noise
	HEADER click, "CLICK", 0
click:
	CALL colon_code
	; 4 30 ITONE ;
	DW raw_char
	DB 4
	DW raw_char
	DB 30
	DW itone
	DW exit


	; \ Print current state of stack
	; : .S ( -- )
	HEADER dot_s, ".S", 0
dot_s:
	CALL colon_code
	; \ Print size of stack in brackets
	; ." <"  DEPTH 0 .R  ." > "
	DW dot_quote_raw
	DB .e1-.s1
.s1:
	DM "<"
.e1:
	DW depth
	DW zero_literal
	DW dot_r
	DW dot_quote_raw
	DB .e2-.s2
.s2:
	DM "> "
.e2:
	; 's s0 < IF
	DW tick_s
	DW s_zero
	DW less_than
	DW if_raw
	DB .then-$-1
		; \ Print contents of stack
		; 's  s0 2 -  DO
		DW tick_s
		DW s_zero
		DW raw_char
		DB 2
		DW minus
		DW two_to_r
.do:
			; i @ .
			DW r_fetch
			DW fetch
			DW dot
		; -2 +LOOP
		DW literal_raw
		DW -2
		DW plus_loop_raw
		DB .do-$+256
.loop:
	; THEN ;
.then:
	DW exit


	; \ Print current state of return stack
	; : .RS ( -- )
	HEADER dot_rs, ".RS", 0
dot_rs:
	CALL colon_code
	; \ Print size of stack in brackets
	; 'r r0 swap - 2/ 1-
	DW tick_r
	DW r_zero
	DW swap
	DW minus
	DW two_slash
	DW one_minus
	; ." <"
	DW dot_quote_raw
	DB .s1e-.s1
.s1:
	DM "<"
.s1e:
	; 0 .R
	DW zero_literal
	DW dot_r
	; ." > "
	DW dot_quote_raw
	DB .s2e-.s2
.s2:
	DM "> "
.s2e:
	; 'r cell+ r0 < INVERT IF EXIT THEN
	DW tick_r
	DW cell_plus
	DW r_zero
	DW less_than
	DW invert
	DW if_raw
	DB .then-$-1
	DW exit
.then:
	; \ Print contents of stack
	; 'r cell+ r0 2 -  DO
	DW tick_r
	DW cell_plus
	DW r_zero
	DW raw_char
	DB 2
	DW minus
	DW two_to_r
.do:
		; i @ u.
		DW r_fetch
		DW fetch
		DW u_dot
	; -2 +LOOP \
	DW literal_raw
	DW -2
	DW plus_loop_raw
	DB .do-$+256
.loop:
	DW exit


	; \ Print immediate string
	; ( "...<r-paren>" --)
	; : .( [CHAR] ) PARSE TYPE ; IMMEDIATE
	HEADER dot_paren, ".(", 1
dot_paren:
	CALL colon_code
	DW raw_char
	DB ')'
	DW parse
	DW type
	DW exit


	; \ Inline comment
	; ( "...<r-paren>" --)
	; : ( [CHAR] ) PARSE 2DROP ; IMMEDIATE
	HEADER paren, "(", 1
paren:
	CALL colon_code
	DW raw_char
	DB ')'
	DW parse
	DW two_drop
	DW exit


	; \ Line comment
	; ( "......" --)
	; : \ IN# @ >IN ! ; IMMEDIATE
	HEADER backslash, "\\", 1
backslash:
	CALL colon_code
	DW in_size
	DW fetch
	DW to_in
	DW store
	DW exit


	; \ Compile a Z80 CALL instruction
	; ( addr --)
	; HEX : CALL CD C, , ; DECIMAL
	HEADER _call, "CALL", 0
_call:
	CALL colon_code
	DW raw_char
	DB 0xCD
	DW c_comma
	DW comma
	DW exit


	; \ Compile string as counted string
	; ( addr u --)
	; : CSTR,
	HEADER cstr_comma, "CSTR,", 0
cstr_comma:
	CALL colon_code
	; \ Store length
	; DUP C,
	DW dup
	DW c_comma
	; \ Store string
	; OVER + SWAP ?DO I C@ C, LOOP ;
	DW over
	DW plus
	DW swap
	DW question_do_raw
	DB .loop-$-1
.do:
	DW r_fetch
	DW c_fetch
	DW c_comma
	DW loop_raw
	DB .do-$+256
.loop:
	DW exit


	; \ Create new symbol from parsed name
	; ( " name" --)
	HEADER sym_comma, "SYM,", 0
sym_comma:
	CALL colon_code
	; \ Write back-link
	; SYM-LAST @ ,
	DW sym_last
	DW fetch
	DW comma
	; \ Parse the name
	; PARSE-NAME
	DW parse_name
	; ( str-addr str-u)
	; \ Must be non-empty
	; DUP 0= IF ABORT" missing name" THEN
	DW dup
	DW zero_equals
	DW if_raw
	DB .then-$-1
	DW s_quote_raw
	DB .e1-.s1
.s1:
	DM "missing name"
.e1:
	DW abort_quote_raw
.then:
	; \ Must be smaller than 64 chars
	; DUP 64 >= IF ABORT" long name" THEN
	DW dup
	DW raw_char
	DB 64
	DW greater_than_or_equal
	DW if_raw
	DB .then2-$-1
	DW s_quote_raw
	DB .e2-.s2
.s2:
	DM "long name"
.e2:
	DW abort_quote_raw
.then2:
	; \ Compile counted string
	; CSTR, ;
	DW cstr_comma
	DW exit


	; \ Create new definition, and make it findable
	; \ At runtime: ( -- data-addr)
	; ( " name" --)
	; HEX : CREATE
	HEADER create, "CREATE", 0
create:
	CALL colon_code
	; \ Retain address of new symbol
	; HERE
	DW here
	; ( addr " name")
	; \ Create new symbol with parsed name
	; SYM,
	DW sym_comma
	; ( addr)
	; \ Write 'CALL create-code' instruction
	; ??? CALL
	DW literal_raw
	DW create_code
	DW _call
	; \ Make symbol findable
	; SYM-LAST ! ; DECIMAL
	DW sym_last
	DW store
	DW exit


	; : VARIABLE CREATE CELL ALLOT ;
	HEADER variable, "VARIABLE", 0
variable:
	CALL colon_code
	DW create
	DW cell
	DW allot
	DW exit


	; \ Switch to compilation mode
	; ( --)
	; : ] TRUE STATE ! ;
	HEADER right_bracket, "]", 0
right_bracket:
	CALL colon_code
	DW true
	DW state
	DW store
	DW exit


	; \ Switch to interpreter mode immediately
	; ( --)
	; : [ FALSE STATE ! ; IMMEDIATE
	HEADER left_bracket, "[", 1
left_bracket:
	CALL colon_code
	DW false
	DW state
	DW store
	DW exit


	; \ Use to remember stack depth before/after colon def
	; VARIABLE :DEPTH
	HEADER colon_depth, ":DEPTH", 0
colon_depth:
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
	DW here
	; \ Remember stack depth
	; DEPTH :DEPTH !
	DW depth
	DW colon_depth
	DW store
	; ( colon-sys " name")
	; \ Write symbol header
	; SYM,
	DW sym_comma
	; ( colon-sys)
	; \ Write CALL colon-code
	; ??? CALL
	DW literal_raw
	DW colon_code
	DW _call
	; \ Start compiling
	; ] ;
	DW right_bracket
	DW exit


	; \ End : or DOES> definition
	; ( --)
	; : ;
	HEADER semicolon, ";", 1
semicolon:
	CALL colon_code
	; \ Current code exits if not already
	; POSTPONE EXIT
	DW literal_raw
	DW exit
	DW compile_comma
	; \ Check stack depth
	; DEPTH :DEPTH @ <> IF ABORT" unbalanced" THEN
	DW depth
	DW colon_depth
	DW fetch
	DW not_equals
	DW if_raw
	DB .then-$-1
	DW s_quote_raw
	DB .e1-.s1
.s1:
	DM "unbalanced"
.e1:
	DW abort_quote_raw
.then:
	; \ Make definition findable
	; SYM-LAST !
	DW sym_last
	DW store
	; \ Return to interpreter mode
	; POSTPONE [ ; IMMEDIATE
	DW left_bracket
	DW exit


	; ( -- orig)
	; : IF
	HEADER _if, "IF", 1
_if:
	CALL colon_code
	; POSTPONE (IF)
	DW literal_raw
	DW if_raw
	DW compile_comma
	; HERE
	DW here
	; ( orig)
	; 1 ALLOT ; IMMEDIATE
	DW one_literal
	DW allot
	DW exit


	; ( orig --)
	; : THEN DUP 1+ HERE SWAP - SWAP C! ; IMMEDIATE
	HEADER then, "THEN", 1
then:
	CALL colon_code
	; DUP 1+
	; ( orig orig+1)
	DW dup
	DW one_plus
	; HERE SWAP -
	DW here
	DW swap
	DW minus
	; ( orig jump-len)
	; SWAP C! ; IMMEDAITE
	DW swap
	DW c_store
	DW exit


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
	DW literal_raw
	DW again_raw
	DW compile_comma
	DW here
	DW minus
	DW c_comma
	DW exit


	; ( dest --)
	; : UNTIL POSTPONE (UNTIL) HERE - C, ; IMMEDIATE
	HEADER until, "UNTIL", 1
until:
	CALL colon_code
	DW literal_raw
	DW until_raw
	DW compile_comma
	DW here
	DW minus
	DW c_comma
	DW exit


	; ( dest -- orig dest)
	; : WHILE POSTPONE IF SWAP ; IMMEDIATE
	HEADER while, "WHILE", 1
while:
	CALL colon_code
	DW _if
	DW swap
	DW exit


	; ( orig dest --)
	; : REPEAT POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
	HEADER repeat, "REPEAT", 1
repeat:
	CALL colon_code
	DW again
	DW then
	DW exit


	; ( -- dest false)
	; : DO POSTPONE 2>R HERE FALSE ; IMMEDIATE
	HEADER do, "DO", 1
do:
	CALL colon_code
	DW literal_raw
	DW two_to_r
	DW compile_comma
	DW here
	DW false
	DW exit


	; ( -- dest true)
	; : ?DO POSTPONE (?DO) 1 ALLOT HERE TRUE ; IMMEDIATE
	HEADER question_do, "?DO", 1
question_do:
	CALL colon_code
	DW literal_raw
	DW question_do_raw
	DW compile_comma
	DW one_literal
	DW allot
	DW here
	DW true
	DW exit


	; ( do-sys --)
	; : LOOP
	HEADER loop, "LOOP", 1
loop:
	CALL colon_code
	; IF
	DW if_raw
	DB .else-$-1
		; POSTPONE (LOOP)
		DW literal_raw
		DW loop_raw
		DW compile_comma
		; DUP HERE - C,
		DW dup
		DW here
		DW minus
		DW c_comma
		; DUP 1- SWAP HERE SWAP - SWAP C!
		DW dup
		DW one_minus
		DW swap
		DW here
		DW swap
		DW minus
		DW swap
		DW c_store
	; ELSE
	DW else_skip
	DB .then-$-1
.else:
		; POSTPONE (LOOP)
		DW literal_raw
		DW loop_raw
		DW compile_comma
		; HERE - C,
		DW here
		DW minus
		DW c_comma
	; THEN
.then:
	; ; IMMEDIATE
	DW exit


	; : I POSTPONE R@ ; IMMEDIATE
	HEADER _i, "I", 1
_i:
	CALL colon_code
	DW literal_raw
	DW r_fetch
	DW compile_comma
	DW exit


	; \ Interpret input buffer until empty
	; : INTERPRET ( ? -- ? )
	HEADER interpret, "INTERPRET", 0
interpret:
	CALL colon_code
	; BEGIN BL WORD DUP C@ WHILE
.begin:
	DW bl
	DW word
	DW dup
	DW c_fetch
	DW if_raw
	DB .repeat-$-1
		; FIND
		DW find
		; ?DUP 0= IF
		DW question_dup
		DW zero_equals
		DW if_raw
		DB .else-$-1
			; NUMBER  STATE @  IF
			DW number
			DW state
			DW fetch
			DW if_raw
			DB .else2-$-1
				; IF
				DW if_raw
				DB .else4-$-1
					; POSTPONE 2LITERAL
					DW two_literal
				; ELSE
				DW else_skip
				DB .then4-$-1
.else4:
					; POSTPONE LITERAL
					DW literal
				; THEN
.then4:
			; ELSE
			DW else_skip
			DB .then2-$-1
.else2:
				; DROP
				DW drop
			; THEN
.then2:
		; ELSE
		DW else_skip
		DB .then-$-1
.else:
			; 0<  STATE @  AND  IF COMPILE, ELSE EXECUTE THEN
			DW zero_less
			DW state
			DW fetch
			DW and
			DW if_raw
			DB .else3-$-1
			DW compile_comma
			DW else_skip
			DB .then3-$-1
.else3:
			DW execute
.then3:
		; THEN
.then:
	; REPEAT DROP ;
	DW again_raw
	DB .begin-$+256
.repeat:
	DW drop
	DW exit


	; : QUIT ( -- ) \ Reset return stack then start interpretation
	HEADER quit, "QUIT", 0
quit:
	CALL colon_code
	; R0 RP! CR
	DW r_zero
	DW rp_store
	DW cr
	; POSTPONE [
	DW left_bracket
	; BEGIN
.begin:
		; -READ  0 >IN !  IN# !  'IN !  SPACE INTERPRET ." ok" CR
		DW line_read
		DW zero_literal
		DW to_in
		DW store
		DW in_size
		DW store
		DW tick_in
		DW store
		DW space
		DW interpret
		DW dot_quote_raw
		DB .s1e-.s1
.s1:
		DM "ok"
.s1e:
		DW cr
	; AGAIN ; -2 ALLOT
	DW again_raw
	DB .begin-$+256


	; : EVALUATE ( ??? addr u -- ??? )
	HEADER evaluate, "EVALUATE", 0
evaluate:
	CALL colon_code
	; \ Update input state to new addr, u, and save old state
	; 0 >IN DUP @ >R !
	DW zero_literal
	DW to_in
	DW dup
	DW fetch
	DW to_r
	DW store
	; IN# DUP @ >R !
	DW in_size
	DW dup
	DW fetch
	DW to_r
	DW store
	; 'IN DUP @ >R !
	DW tick_in
	DW dup
	DW fetch
	DW to_r
	DW store
	; \ Interpret code
	; INTERPRET
	DW interpret
	; \ Restore input state
	; R> 'IN !  R> IN# !  R> >IN ! ;
	DW r_from
	DW tick_in
	DW store
	DW r_from
	DW in_size
	DW store
	DW r_from
	DW to_in
	DW store
	DW exit


	; ( "...<quote>"--)
	; : ."
	HEADER dot_quote, ".\"", 1
dot_quote:
	CALL colon_code
	; POSTPONE (.")
	DW literal_raw
	DW dot_quote_raw
	DW compile_comma
	; [CHAR] " PARSE
	DW raw_char
	DB '"'
	DW parse
	; ( addr u)
	; CSTR, ;
	DW cstr_comma
	DW exit


	; ( "...<quote>"--)
	; : S"
	HEADER s_quote, "S\"", 1
s_quote:
	CALL colon_code
	; POSTPONE (S")
	DW literal_raw
	DW s_quote_raw
	DW compile_comma
	; [CHAR] " PARSE
	DW raw_char
	DB '"'
	DW parse
	; ( addr u)
	; CSTR, ;
	DW cstr_comma
	DW exit


	; ( "<spaces>name" -- xt)
	; : '
	HEADER tick, "'", 0
tick:
	CALL colon_code
	; PARSE-NAME
	DW parse_name
	; ( addr u)
	; SFIND
	DW sfind
	; ( addr u 0 | xt 1 | xt -1)
	; ?DUP 0= IF
	DW question_dup
	DW zero_equals
	DW if_raw
	DB .then-$-1
		; ( addr u)
		; WHAT?
		DW what_question
	; THEN
.then:
	; ( xt 1 | xt -1)
	; DROP ;
	DW drop
	DW exit


	; ( "<spaces>name" --)
	; ( -- xt) \ runtime
	; : ['] ' POSTPONE LITERAL ; IMMEDIATE
	HEADER bracket_tick, "[']", 1
bracket_tick:
	CALL colon_code
	DW tick
	DW literal
	DW exit


repeat_wait_init: EQU 45  ; 0.9s
repeat_repeat_init: EQU 5 ; 0.1s


key_up:        EQU 0x11 ; ASCII DC1
key_left:      EQU 0x12 ; ASCII DC2
key_down:      EQU 0x13 ; ASCII DC3
key_right:     EQU 0x14 ; ASCII DC4
key_caps_lock: EQU 0x1C ; ASCII File separator

h_init:

sym_last_init: EQU this_header

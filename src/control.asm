; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Control flow words


	HEADER leave, "LEAVE", 0
leave:
	LD E, (IX+4)
	LD D, (IX+5)
	PUSH DE
	POP IY
	LD DE, 6
	ADD IX, DE
	JP next


	HEADER do_raw, "(DO)", 0
do_raw:
	POP DE
.common:
	LD BC, -6
	ADD IX, BC
	LD (IX+0), L
	LD (IX+1), H
	LD (IX+2), E
	LD (IX+3), D
	PUSH IY
	INC IY
	POP HL
	LD E, (HL)
	LD D, 0
	INC HL
	ADD HL, DE
	LD (IX+4), L
	LD (IX+5), H
	POP HL
	JP next


	HEADER question_do_raw, "(?DO)", 0
question_do_raw:
	POP DE
	OR A
	SBC HL, DE
	JR NZ, .not_equal
	PUSH IY
	POP HL
	LD E, (HL)
	LD D, 0
	INC HL
	ADD HL, DE
	PUSH HL
	POP IY
	POP HL
	JP next
.not_equal:
	ADD HL, DE
	JR do_raw.common


	HEADER unloop, "UNLOOP", 0
unloop:
	LD DE, 6
	ADD IX, DE
	JP next


	HEADER i_tick, "I'", 0
i_tick:
	PUSH HL
	LD L, (IX+2)
	LD H, (IX+3)
	JP next


	HEADER i_leave, "I>", 0
i_leave:
	PUSH HL
	LD L, (IX+4)
	LD H, (IX+5)
	JP next


	HEADER _j, "J", 0
j:
	PUSH HL
	LD L, (IX+6)
	LD H, (IX+7)
	JP next


	HEADER _k, "K", 0
_k:
	PUSH HL
	LD L, (IX+12)
	LD H, (IX+13)
	JP next

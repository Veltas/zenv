; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021-2024 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Mathematical word definitions


	HEADER plus, "+", 0
plus:
	POP DE
	ADD HL, DE
	JP next


	HEADER minus, "-", 0
minus:
	POP DE
	EX DE, HL
	OR A
	SBC HL, DE
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
	ADD HL, HL
	SBC HL, HL
	JP next

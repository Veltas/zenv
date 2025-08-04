; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021-2025 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Memory manipulating words


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
	JP drop


	HEADER c_store, "C!", 0
c_store:
	POP DE
	LD (HL), E
	JP drop


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
	JP drop

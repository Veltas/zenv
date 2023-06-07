; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Stack operation words


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


	; Token vector is included here as a space optimisation, it is page-aligned
	INCLUDE "tokens.asm"


	HEADER swap, "SWAP", 0
swap:
	POP DE
	PUSH HL
	EX DE, HL
	JP next


	HEADER two_dup, "2DUP", 0
two_dup:
	POP DE
	PUSH DE
	PUSH HL
	PUSH DE
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
	INC IXL
	INC IXL
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
	DEC IXL
	DEC IXL
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


	; ( x_u ... x_0 u | x_u ... x_0 x_u)
	; CODE PICK
	HEADER pick, "PICK", 0
pick:
	ADD HL, HL
	ADD HL, SP
	LD E, (HL)
	INC L
	LD D, (HL)
	EX DE, HL
	JP next

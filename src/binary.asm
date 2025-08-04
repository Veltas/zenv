; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021-2025 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Binary operations


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

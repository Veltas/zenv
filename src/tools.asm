; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021-2024 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Tools / utils


	; \ Print hexdump of memory
	; ( a u --)
	; : HEXDUMP
hexdump:
	CALL colon_code
	; BASE @ -ROT  HEX
	DX base: DX fetch: DX minus_rot: DX hex
	; ( base a u)
	; OVER + SWAP ?DO
	DX over: DX plus: DX swap: DX question_do_raw: DB .loop-$-1
.do:
		; ( base)
		; I C@ 0 <# # # #> TYPE SPACE
		DX r_fetch: DX c_fetch: DX zero_literal: DX less_number_sign: DX number_sign
		DX number_sign: DX number_sign_greater: DX type: DX space
	; LOOP
	DX loop_raw: DB .do-$+256
.loop:
	; BASE ! ;
	DX base: DX store: DX exit


	; \ Print preview of memory, the character or dot
	; ( a u --)
	; : PREVIEW
	HEADER preview, "PREVIEW", 0
preview:
	CALL colon_code
	; OVER + SWAP ?DO
	DX over: DX plus: DX swap: DX question_do_raw: DB .loop-$-1
.do:
		; ( )
		; I C@
		DX r_fetch: DX c_fetch
		; ( c)
		; DUP 128 33 WITHIN IF DROP [CHAR] . THEN
		DX dup: DX literal_raw: DW 128: DX raw_char: DB 33: DX within
		DX if_raw: DB .then-$-1: DX drop: DX raw_char: DB '.'
.then:
		; ( c|'.')
		; EMIT
		DX emit
	; LOOP ;
	DX loop_raw: DB .do-$+256
.loop:
	DX exit


	; \ Print hexdump and preview for specified memory
	; ( a u --)
	; : DUMP
	HEADER dump, "DUMP", 0
dump:
	CALL colon_code
	; CR
	DX cr
	; TUCK
	DX tuck
	; ( u a u)
	; OVER + SWAP ?DO
	DX over: DX plus: DX swap: DX question_do_raw: DB .loop-$-1
.do:
		; ( u)
		; I OVER 8 MIN
		DX r_fetch: DX over: DX raw_char: DB 8: DX min
		; ( u a n)
		; 2DUP HEXDUMP
		DX two_dup: DX hexdump
		; 24 T-COL C!
		DX raw_char: DB 24: DX t_col: DX c_store
		; PREVIEW
		DX preview
		; 1+
		DX one_plus
	; 8 +LOOP
	DX raw_char: DB 8: DX plus_loop_raw: DB .do-$+256
.loop:
	; DROP CR ;
	DX drop: DX cr: DX exit

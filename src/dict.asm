; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Dictionary words


	; \ Remove named word and everything in dictionary after
	; ( " name" --)
	; : FORGET
	HEADER forget, "FORGET", 0
forget:
	CALL colon_code
	; BL WORD
	DX bl: DX word
	; ( str)
	; SYM-LAST @
	DX sym_last: DX fetch
	; ( str sym)
	; BEGIN
.begin:
		; ?DUP 0= IF CWHAT? THEN
		DX question_dup: DX zero_equals: DX if_raw: DB .then-$-1: DX cwhat_question
.then:
		; 2DUP >SYM ROT COUNT COMPARE WHILE
		DX two_dup: DX to_sym: DX rot: DX count: DX compare: DX if_raw
		DB .repeat-$-1
		; @
		DX fetch
	; REPEAT
	DX again_raw
	DB .begin-$+256
.repeat:
	; NIP
	DX nip
	; ( sym)
	; DUP @ SWAP
	DX dup: DX fetch: DX swap
	; ( prev-sym sym)
	; H !
	DX h_: DX store
	; ( prev-sym)
	; SYM-LAST ! ;
	DX sym_last: DX store: DX exit

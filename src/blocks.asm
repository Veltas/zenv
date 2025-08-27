; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021-2025 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Block support

	; TODO: only one block, no saving yet


	; HEX : BLOCK ( u -- addr )   ( TODO ) DROP 5B00 ; DECIMAL
	HEADER block, "BLOCK", 0
block:
	CALL colon_code
	DX drop: DX literal_raw: DW 0x5B00: DX exit


	; : BUFFER  ( TODO ) BLOCK ;
	HEADER buffer, "BUFFER", 0
buffer:
	CALL colon_code
	DX block: DX exit


	; : UPDATE  ( TODO ) ;
	HEADER update, "UPDATE", 0
update:
	CALL colon_code
	DX exit


	; : EMPTY-BUFFERS  ( TODO ) ;
	HEADER empty_buffers, "EMPTY-BUFFERS", 0
empty_buffers:
	CALL colon_code
	DX exit


	; : SAVE-BUFFERS  ( TODO ) ;
	HEADER save_buffers, "SAVE-BUFFERS", 0
save_buffers:
	CALL colon_code
	DX exit


	; : FLUSH  ( TODO ) ;
	HEADER flush, "FLUSH", 0
flush:
	CALL colon_code
	DX exit


	; VARIABLE SCR
	HEADER scr, "SCR", 0
scr:
	CALL create_code
	DW 0


	; VARIABLE LIN
	HEADER lin, "LIN", 0
lin:
	CALL create_code
	DW 0


	; : VIEW ( n-a)  5 LSHIFT  SCR @ BLOCK  + ;
	HEADER view, "VIEW", 0
view:
	CALL colon_code
	DX raw_char: DB 5: DX lshift: DX scr: DX fetch: DX block: DX plus
	DX exit


	; : LINE. ( n)
		; T-ATTR C@ >R
		; DUP LIN @ = IF  120 T-ATTR C!  THEN
		; CR
		; VIEW 32 TYPE
		; R> T-ATTR C! ;
	HEADER line_dot, "LINE.", 0
line_dot:
	CALL colon_code
	DX t_attr: DX c_fetch: DX to_r
	DX dup: DX lin: DX fetch: DX equals: DX if_raw: DB .then-$-1
	DX raw_char: DB 120: DX t_attr: DX c_store
.then:
	DX cr
	DX view: DX raw_char: DB 32: DX type
	DX r_from: DX t_attr: DX c_store: DX exit


	; : LIST' ( RS)  PAGE TYPE SPACE  SCR @ U.  ?DO I LINE. LOOP ;
	HEADER list_tick, "LIST'", 0
list_tick:
	CALL colon_code
	DX page: DX type: DX space: DX scr: DX fetch: DX u_dot
	DX question_do_raw: DB .loop-$-1
.do:
	DX r_fetch: DX line_dot: DX loop_raw: DB .do-$+256
.loop:
	DX exit


	; : LINE ( n-a)  DUP LIN !  VIEW UPDATE ;
	HEADER line, "LINE", 0
line:
	CALL colon_code
	DX dup: DX lin: DX store: DX view: DX update: DX exit


	; : WIPE  0 LINE 1024 BLANK ;
	HEADER wipe, "WIPE", 0
wipe:
	CALL colon_code
	DX zero_literal: DX line: DX literal_raw: DW 1024: DX blank: DX exit


	; : L ( n)  LIN @ + 0 MAX 31 MIN
		; DUP LIN !
		; 15 > IF  32 16 S" Lower"
		; ELSE  16. S" Upper"
		; THEN LIST' ;
	HEADER l_, "L", 0
l_:
	CALL colon_code
	DX lin: DX fetch: DX plus: DX zero_literal: DX max: DX raw_char: DB 31
	DX min
	DX dup: DX lin: DX store
	DX raw_char: DB 15: DX greater_than: DX if_raw
	DB .else-$-1
	DX raw_char: DB 32: DX raw_char: DB 16: DX s_quote_raw: DB .s2-.s1
.s1:
	DM "Lower"
.s2:
	DX else_skip: DB .then-$-1
.else:
	DX two_literal_raw: DW 0: DW 16: DX s_quote_raw: DB .s4-.s3
.s3:
	DM "Upper"
.s4:
.then:
	DX list_tick: DX exit


	; : >PAD  REFILL DROP  0 PARSE 32 MIN  PAD 32 BLANK  PAD SWAP MOVE ;
	HEADER to_pad, ">PAD", 0
to_pad:
	CALL colon_code
	DX refill: DX drop
	DX zero_literal: DX parse: DX raw_char: DB 32: DX min: DX pad
	DX raw_char: DB 32: DX blank: DX pad: DX swap: DX move: DX exit


	; : P  PAD  LIN @ LINE  32 MOVE  1 L ;
	HEADER p_, "P", 0
p_:
	CALL colon_code
	DX pad: DX lin: DX fetch: DX line: DX raw_char: DB 32: DX move
	DX one_literal: DX l_
	DX exit


	; : Y  LIN @ VIEW  PAD  32 MOVE ;
	HEADER y, "Y", 0
y:
	CALL colon_code
	DX lin: DX fetch: DX view: DX pad: DX raw_char: DB 32: DX move: DX exit


	; : C  >PAD P ;
	HEADER c_, "C", 0
c_:
	CALL colon_code
	DX to_pad: DX p_: DX exit


	; : N  1 SCR +!  0 L ;
	HEADER n, "N", 0
n:
	CALL colon_code
	DX one_literal: DX scr: DX plus_store: DX zero_literal: DX l_: DX exit


	; : B  -1 SCR +!  0 L ;
	HEADER b_, "B", 0
b_:
	CALL colon_code
	DX true: DX scr: DX plus_store: DX zero_literal: DX l_: DX exit


	; : LIST ( u)  SCR !  -32 L ;
	HEADER list, "LIST", 0
list:
	CALL colon_code
	DX scr: DX store: DX literal_raw: DW -32: DX l_: DX exit

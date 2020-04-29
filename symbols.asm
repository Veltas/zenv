; vim:set syntax=z80:

this_header DEFL 0

	MACRO HEADER symbol, text
prev_header DEFL this_header
this_header DEFL $
	DW prev_header
	DB .se - .ss
.ss:
	DM text
.se:
	DW symbol - 2
	ENDM

	HEADER forth_h, "H"

	HEADER forth_state, "STATE"

	HEADER frames, "FRAMES"

	HEADER t_attr, "T-ATTR"

	HEADER t_col, "T-COL"

	HEADER t_row, "T-ROW"

	HEADER display_file, "DISPLAY-FILE"

	HEADER display_size, "DISPLAY-SIZE"

	HEADER attr_file, "ATTR-FILE"

	HEADER attr_size, "ATTR-SIZE"

	HEADER tick_int, "'INT"

	HEADER int, "INT"

	HEADER forth_main, "MAIN"

	HEADER forth_zero_literal, "0"

	HEADER forth_one_literal, "1"

	HEADER forth_literal_raw, "(LITERAL)"

	HEADER scroll, "SCROLL"

	HEADER forth_cr, "CR"

	HEADER forth_emit, "EMIT"

	HEADER forth_bye, "BYE"

	HEADER forth_store, "!"

	HEADER forth_page, "PAGE"

	HEADER forth_plus, "+"

	HEADER forth_minus, "-"

	HEADER forth_zero_equals, "0="

	HEADER forth_zero_less, "0<"

	HEADER forth_one_plus, "1+"

	HEADER forth_char_plus, "CHAR+"

	HEADER forth_one_minus, "1-"

	HEADER forth_two_store, "2!"

	HEADER forth_two_star, "2*"

	HEADER forth_cells, "CELLS"

	HEADER forth_exit, "EXIT"

	HEADER forth_two_slash, "2/"

	HEADER forth_two_fetch, "2@"

	HEADER forth_two_drop, "2DROP"

	HEADER forth_two_dup, "2DUP"

	HEADER forth_two_over, "2OVER"

	HEADER forth_to_r, ">R"

	HEADER forth_question_dup, "?DUP"

	HEADER forth_abort, "ABORT"

	HEADER forth_abs, "ABS"

	HEADER forth_align, "ALIGN"

	HEADER forth_aligned, "ALIGNED"

	HEADER forth_chars, "CHARS"

	HEADER forth_allot, "ALLOT"

	HEADER forth_and, "AND"

	HEADER forth_or, "OR"

	HEADER forth_xor, "XOR"

	HEADER forth_bl, "BL"

	HEADER forth_c_store, "C!"

	HEADER forth_c_comma, "C,"

	HEADER forth_c_fetch, "C@"

	HEADER forth_cell_plus, "CELL+"

	HEADER forth_count, "COUNT"

	HEADER forth_depth, "DEPTH"

	HEADER forth_drop, "DROP"

	HEADER forth_dup, "DUP"

	HEADER forth_max, "MAX"

	HEADER forth_cmove, "CMOVE"

	HEADER forth_cmove_up, "CMOVE>"

	HEADER forth_move, "MOVE"

	HEADER forth_negate, "NEGATE"

	HEADER forth_question_do_raw, "(?DO)"

	HEADER forth_loop_raw, "(LOOP)"

	HEADER forth_type, "TYPE"

	HEADER forth_s_quote_raw, '(S")'

	HEADER forth_over, "OVER"

	HEADER forth_reset_stacks, "RESET-STACKS"

	HEADER forth_quit, "QUIT"

	HEADER forth_erase, "ERASE"

	HEADER forth_fill, "FILL"

	HEADER forth_fetch, "@"

	HEADER forth_tick_s, "'S"

	HEADER forth_rot, "ROT"

	HEADER forth_swap, "SWAP"

	HEADER forth_s_zero, "S0"

	HEADER forth_to_in, ">IN"

	HEADER forth_minus_tick, "-'"

	HEADER forth_minus_rot, "-ROT"

	HEADER forth_less_than, "<"

	HEADER forth_greater_than, ">"

	HEADER forth_nip, "NIP"

	HEADER forth_else_skip, "(ELSE)"

	HEADER forth_c_plus_store, "C+!"

	HEADER forth_c_literal, "C-LITERAL"

	HEADER forth_if_raw, "(IF)"

	HEADER forth_ms, "MS"

	HEADER p_fetch, "P@"

	HEADER p_store, "P!"

	HEADER ula, "ULA"

	HEADER halt_, "HALT"

	HEADER forth_d_plus_store, "D1+!"

	HEADER forth_d_plus, "D+"

	HEADER forth_two_literal_raw, "(2LITERAL)"

	HEADER forth_r_from, "R>"

	HEADER _di, "DI"

	HEADER _ei, "EI"

	HEADER forth_at_xy, "AT-XY"

	HEADER keyq_len, "KEYQ-LEN"

	HEADER keyq, "KEYQ"

	HEADER keyq_s, "KEYQ-S"

	HEADER keyq_e, "KEYQ-E"

	HEADER ekey_question, "EKEY?"

	HEADER forth_zero_not_equals, "0<>"

	HEADER forth_not_equals, "<>"

	HEADER clamp, "CLAMP"

	HEADER forth_min, "MIN"

	HEADER forth_invert, "INVERT"

	HEADER ekey, "EKEY"

	HEADER until_raw, "(UNTIL)"

	HEADER forth_equals, "="

	HEADER less_than_or_equal, "<="

	HEADER greater_than_or_equal, ">="

	HEADER two_to_r, "2>R"

	HEADER kscan, "KSCAN"

	HEADER kscan_row, "KSCAN-ROW"

	HEADER kstate, "KSTATE"

	HEADER klast, "KLAST"

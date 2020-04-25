; vim:set syntax=z80:

forth_h__dict:
	DW 0
	DB 1
	DM "H"
	DW forth_h - 2


forth_state__dict:
	DW forth_h__dict
	DB 5
	DM "STATE"
	DW forth_state - 2


frames__dict:
	DW forth_state__dict
	DB 6
	DM "FRAMES"
	DW frames - 2


t_attr__dict:
	DW frames__dict
	DB 6
	DM "T-ATTR"
	DW t_attr - 2


t_col__dict:
	DW t_attr__dict
	DB 5
	DM "T-COL"
	DW t_col - 2


t_row__dict:
	DW t_col__dict
	DB 5
	DM "T-ROW"
	DW t_row - 2


display_file__dict:
	DW t_row__dict
	DB 12
	DM "DISPLAY-FILE"
	DW display_file - 2


display_size__dict:
	DW display_file__dict
	DB 12
	DM "DISPLAY-SIZE"
	DW display_size - 2


attr_file__dict:
	DW display_size__dict
	DB 12
	DM "ATTR-FILE"
	DW attr_file - 2


attr_size__dict:
	DW attr_file__dict
	DB 12
	DM "ATTR-SIZE"
	DW attr_size - 2


tick_int__dict:
	DW attr_size__dict
	DB 4
	DM "'INT"
	DW tick_int - 2


int__dict:
	DW tick_int__dict
	DB 3
	DM "INT"
	DW int - 2


forth_main__dict:
	DW int__dict
	DB 4
	DM "MAIN"
	DW forth_main - 2


forth_zero_literal__dict:
	DW forth_main__dict
	DB 1
	DM "0"
	DW forth_zero_literal - 2


forth_one_literal__dict:
	DW forth_zero_literal__dict
	DB 1
	DM "1"
	DW forth_one_literal - 2


forth_literal_raw__dict:
	DW forth_one_literal__dict
	DB 9
	DM "(LITERAL)"
	DW forth_literal_raw - 2


scroll__dict:
	DW forth_literal_raw__dict
	DB 5
	DM "SCROLL"
	DW scroll - 2


forth_cr__dict:
	DW scroll__dict
	DB 2
	DM "CR"
	DW forth_cr - 2


forth_emit__dict:
	DW forth_cr__dict
	DB 4
	DM "EMIT"
	DW forth_emit - 2


forth_bye__dict:
	DW forth_emit__dict
	DB 3
	DM "BYE"
	DW forth_bye - 2


forth_store__dict:
	DW forth_main__dict
	DB 1
	DM "!"
	DW forth_store - 2


forth_page__dict:
	DW forth_store__dict
	DB 4
	DM "PAGE"
	DW forth_page - 2


forth_plus__dict:
	DW forth_page__dict
	DB 1
	DM "+"
	DW forth_plus - 2


forth_minus__dict:
	DW forth_plus__dict
	DB 1
	DM "-"
	DW forth_minus - 2


forth_zero_equals__dict:
	DW forth_minus__dict
	DB 2
	DM "0="
	DW forth_zero_equals - 2


forth_zero_less__dict:
	DW forth_zero_equals__dict
	DB 2
	DM "0<"
	DW forth_zero_less - 2


forth_one_plus__dict:
	DW forth_zero_less__dict
	DB 2
	DM "1+"
	DW forth_one_plus - 2


forth_char_plus__dict:
	DW forth_one_plus__dict
	DB 5
	DM "CHAR+"
	DW forth_char_plus -2


forth_one_minus__dict:
	DW forth_char_plus__dict
	DB 2
	DM "1-"
	DW forth_one_minus - 2


forth_two_store__dict:
	DW forth_one_minus__dict
	DB 2
	DM "2!"
	DW forth_two_store - 2


forth_two_star__dict:
	DW forth_two_store__dict
	DB 2
	DM "2*"
	DW forth_two_star - 2


forth_cells__dict:
	DW forth_two_star__dict
	DB 5
	DM "CELLS"
	DW forth_cells - 2


forth_exit__dict:
	DW forth_cells__dict
	DB 4
	DM "EXIT"
	DW forth_exit - 2


forth_two_slash__dict:
	DW forth_exit__dict
	DB 2
	DM "2/"
	DW forth_two_slash - 2


forth_two_fetch__dict:
	DW forth_two_slash__dict
	DB 2
	DM "2@"
	DW forth_two_fetch - 2


forth_two_drop__dict:
	DW forth_two_fetch__dict
	DB 5
	DM "2DROP"
	DW forth_two_drop - 2


forth_two_dup__dict:
	DW forth_two_drop__dict
	DB 4
	DM "2DUP"
	DW forth_two_dup - 2


forth_two_over__dict:
	DW forth_two_dup__dict
	DB 5
	DM "2OVER"
	DW forth_two_over - 2


forth_to_r__dict:
	DW forth_two_over__dict
	DB 2
	DM ">R"
	DW forth_to_r - 2


forth_question_dup__dict:
	DW forth_to_r__dict
	DB 4
	DM "?DUP"
	DW forth_question_dup - 2


forth_abort__dict:
	DW forth_question_dup__dict
	DB 5
	DM "ABORT"
	DW forth_abort - 2


forth_abs__dict:
	DW forth_abort__dict
	DB 3
	DM "ABS"
	DW forth_abs - 2


forth_align__dict:
	DW forth_abs__dict
	DB 0x85
	DM "ALIGN"
	DW forth_align - 2


forth_aligned__dict:
	DW forth_align__dict
	DB 0x87
	DM "ALIGNED"
	DW forth_aligned - 2


forth_chars__dict:
	DW forth_aligned__dict
	DB 0x85
	DM "CHARS"
	DW forth_chars - 2


forth_allot__dict:
	DW forth_chars__dict
	DB 5
	DM "ALLOT"
	DW forth_allot - 2


forth_and__dict:
	DW forth_allot__dict
	DB 3
	DM "AND"
	DW forth_and - 2


forth_or__dict:
	DW forth_and__dict
	DB 2
	DM "OR"
	DW forth_or - 2


forth_xor__dict:
	DW forth_or__dict
	DB 3
	DM "XOR"
	DW forth_xor - 2


forth_bl__dict:
	DW forth_xor__dict
	DB 2
	DM "BL"
	DW forth_bl - 2


forth_c_store__dict:
	DW forth_bl__dict
	DB 2
	DM "C!"
	DW forth_c_store - 2


forth_c_comma__dict:
	DW forth_c_store__dict
	DB 2
	DM "C,"
	DW forth_c_comma - 2


forth_c_fetch__dict:
	DW forth_c_comma__dict
	DB 2
	DM "C@"
	DW forth_c_fetch - 2


forth_cell_plus__dict:
	DW forth_c_fetch__dict
	DB 5
	DM "CELL+"
	DW forth_cell_plus - 2


forth_count__dict:
	DW forth_cell_plus__dict
	DB 5
	DM "COUNT"
	DW forth_count - 2


forth_depth__dict:
	DW forth_count__dict
	DB 5
	DM "DEPTH"
	DW forth_depth - 2


forth_drop__dict:
	DW forth_depth__dict
	DB 4
	DM "DROP"
	DW forth_drop - 2


forth_dup__dict:
	DW forth_drop__dict
	DB 3
	DM "DUP"
	DW forth_dup - 2


forth_max__dict:
	DW forth_dup__dict
	DB 3
	DM "MAX"
	DW forth_max - 2


forth_cmove__dict:
	DW forth_max__dict
	DB 5
	DM "CMOVE"
	DW forth_cmove - 2


forth_cmove_up__dict:
	DW forth_cmove__dict
	DB 6
	DM "CMOVE>"
	DW forth_cmove_up - 2


forth_move__dict:
	DW forth_cmove_up__dict
	DB 4
	DM "MOVE"
	DW forth_move - 2


forth_negate__dict:
	DW forth_move__dict
	DB 6
	DM "NEGATE"
	DW forth_negate - 2


forth_question_do_raw__dict:
	DW forth_negate__dict
	DB 4
	DM "(?DO)"
	DW forth_question_do_raw - 2


forth_loop_raw__dict:
	DW forth_question_do_raw__dict
	DB 6
	DM "(LOOP)"
	DW forth_loop_raw - 2


forth_type__dict:
	DW forth_loop_raw__dict
	DB 4
	DM "TYPE"
	DW forth_type - 2


forth_s_quote_raw__dict:
	DW forth_type__dict
	DB 4
	DM '(S")'
	DW forth_s_quote_raw - 2


forth_over__dict:
	DW forth_s_quote_raw__dict
	DB 4
	DM "OVER"
	DW forth_over - 2


forth_reset_stacks__dict:
	DW forth_over__dict
	DB 12
	DM "RESET-STACKS"
	DW forth_reset_stacks - 2


forth_quit__dict:
	DW forth_reset_stacks__dict
	DB 4
	DM "QUIT"
	DW forth_quit - 2


forth_erase__dict:
	DW forth_quit__dict
	DB 5
	DM "ERASE"
	DW forth_erase - 2


forth_fill__dict:
	DW forth_erase__dict
	DB 4
	DM "FILL"
	DW forth_fill - 2


forth_fetch__dict:
	DW forth_fill__dict
	DB 1
	DM "@"
	DW forth_fetch - 2


forth_tick_s__dict:
	DW forth_fetch__dict
	DB 2
	DM "'S"
	DW forth_tick_s - 2


forth_rot__dict:
	DW forth_tick_s__dict
	DB 3
	DM "ROT"
	DW forth_rot - 2


forth_swap__dict:
	DW forth_rot__dict
	DB 4
	DM "SWAP"
	DW forth_swap - 2


forth_s_zero__dict:
	DW forth_swap__dict
	DB 2
	DM "S0"
	DW forth_s_zero - 2


in_buf__dict:
	DW forth_s_zero__dict
	DB 6
	DM "IN-BUF"
	DW in_buf - 2


in_size__dict:
	DW in_buf__dict
	DB 7
	DM "IN-SIZE"
	DW in_size - 2


forth_to_in__dict:
	DW in_size__dict
	DB 3
	DM ">IN"
	DW forth_to_in - 2


forth_minus_tick__dict:
	DW forth_to_in__dict
	DB 2
	DM "-'"
	DW forth_minus_tick - 2


forth_minus_rot__dict:
	DW forth_minus_tick__dict
	DB 4
	DM "-ROT"
	DW forth_minus_rot - 2


forth_less_than__dict:
	DW forth_minus_rot__dict
	DB 1
	DM "<"
	DW forth_less_than - 2


forth_greater_than__dict:
	DW forth_less_than__dict
	DB 1
	DM ">"
	DW forth_greater_than - 2


forth_nip__dict:
	DW forth_greater_than__dict
	DB 3
	DM "NIP"
	DW forth_nip - 2


forth_else_skip__dict:
	DW forth_nip__dict
	DB 6
	DM "(ELSE)"
	DW forth_else_skip - 2


forth_c_plus_store__dict:
	DW forth_else_skip__dict
	DB 3
	DM "C+!"
	DW forth_c_plus_store - 2


forth_c_literal__dict:
	DW forth_c_plus_store__dict
	DB 9
	DM "C-LITERAL"
	DW forth_c_literal - 2


forth_if_raw__dict:
	DW forth_c_literal__dict
	DB 4
	DM "(IF)"
	DW forth_if_raw - 2


forth_ms__dict:
	DW forth_if_raw__dict
	DB 2
	DM "MS"
	DW forth_ms - 2


forth_p_fetch__dict:
	DW forth_ms__dict
	DB 3
	DM "P@"
	DW forth_p_fetch - 2


forth_p_store__dict:
	DW forth_p_fetch__dict
	DB 3
	DM "P!"
	DW forth_p_store - 2


ula__dict:
	DW forth_p_store__dict
	DB 3
	DM "ULA"
	DW ula - 2


halt__dict:
	DW ula__dict
	DB 4
	DM "HALT"
	DW halt_ - 2


forth_d_plus_store__dict:
	DW halt__dict
	DB 4
	DM "D1+!"
	DW forth_d_plus_store - 2


forth_d_plus__dict:
	DW forth_d_plus_store__dict
	DB 2
	DM "D+"
	DW forth_d_plus - 2


forth_two_literal_raw__dict:
	DW forth_d_plus__dict
	DB 10
	DM "(2LITERAL)"
	DW forth_two_literal_raw - 2


forth_r_from__dict:
	DW forth_two_literal_raw__dict
	DB 2
	DM "R>"
	DW forth_r_from - 2


_di__dict:
	DW forth_r_from__dict
	DB 2
	DM "DI"
	DW _di - 2


_ei__dict:
	DW _di__dict
	DB 2
	DM "EI"
	DW _ei - 2


forth_at_xy__dict:
	DW _ei__dict
	DB 5
	DM "AT-XY"
	DW forth_at_xy - 2


keyq_len__dict:
	DW forth_at_xy__dict
	DB 8
	DM "KEYQ-LEN"
	DW keyq_len - 2


keyq__dict:
	DW keyq_len__dict
	DB 4
	DM "KEYQ"
	DW keyq - 2


keyq_s__dict:
	DW keyq__dict
	DB 6
	DM "KEYQ-S"
	DW keyq_s - 2


keyq_e__dict:
	DW keyq_s__dict
	DB 6
	DM "KEYQ-E"
	DW keyq_e - 2


ekey_question__dict:
	DW keyq_e__dict
	DB 5
	DM "EKEY?"
	DW ekey_question - 2


forth_zero_not_equals__dict:
	DW ekey_question__dict
	DB 3
	DM "0<>"
	DW forth_zero_not_equals - 2


forth_not_equals__dict:
	DW forth_zero_not_equals__dict
	DB 2
	DM "<>"
	DW forth_not_equals - 2


clamp__dict:
	DW forth_not_equals__dict
	DB 5
	DM "CLAMP"
	DW clamp - 2


forth_min__dict:
	DW clamp__dict
	DB 3
	DM "MIN"
	DW forth_min - 2


forth_invert__dict:
	DW forth_min__dict
	DB 6
	DM "INVERT"
	DW forth_invert - 2


ekey__dict:
	DW forth_invert__dict
	DB 4
	DM "EKEY"
	DW ekey - 2


until_raw__dict:
	DW ekey__dict
	DB 7
	DM "(UNTIL)"
	DW until_raw - 2


forth_equals__dict:
	DW until_raw__dict
	DB 1
	DM "="
	DW forth_equals - 2


less_than_or_equal__dict:
	DW forth_equals__dict
	DB 2
	DM "<="
	DW less_than_or_equal - 2


greater_than_or_equal__dict:
	DW less_than_or_equal__dict
	DB 2
	DM ">="
	DW greater_than_or_equal - 2

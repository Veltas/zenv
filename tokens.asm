; vim:set syntax=z80:

forth_literal_raw_tok: EQU ($-tokens)/2
	DW forth_literal_raw-2
forth_c_literal_tok: EQU ($-tokens)/2
	DW forth_c_literal-2
forth_zero_literal_tok: EQU ($-tokens)/2
	DW forth_zero_literal-2
forth_one_literal_tok: EQU ($-tokens)/2
	DW forth_one_literal-2
forth_dup_tok: EQU ($-tokens)/2
	DW forth_dup-2
forth_swap_tok: EQU ($-tokens)/2
	DW forth_swap-2
forth_rot_tok: EQU ($-tokens)/2
	DW forth_rot-2
forth_fetch_tok: EQU ($-tokens)/2
	DW forth_fetch-2
forth_store_tok: EQU ($-tokens)/2
	DW forth_store-2
forth_c_fetch_tok: EQU ($-tokens)/2
	DW forth_c_fetch-2
forth_c_store_tok: EQU ($-tokens)/2
	DW forth_c_store-2
forth_s_quote_raw_tok: EQU ($-tokens)/2
	DW forth_s_quote_raw-2
forth_dot_quote_raw_tok: EQU ($-tokens)/2
	DW 0 ; DW forth_dot_quote_raw-2
forth_exit_tok: EQU ($-tokens)/2
	DW forth_exit-2
forth_loop_raw_tok: EQU ($-tokens)/2
	DW forth_loop_raw-2
forth_one_plus_tok: EQU ($-tokens)/2
	DW forth_one_plus-2
forth_question_do_raw_tok: EQU ($-tokens)/2
	DW forth_question_do_raw-2
forth_plus_tok: EQU ($-tokens)/2
	DW forth_plus-2
forth_minus_tok: EQU ($-tokens)/2
	DW forth_minus-2
forth_two_slash_tok: EQU ($-tokens)/2
	DW forth_two_slash-2
forth_if_raw_tok: EQU ($-tokens)/2
	DW forth_if_raw-2
forth_else_skip_tok: EQU ($-tokens)/2
	DW forth_else_skip-2
forth_two_dup_tok: EQU ($-tokens)/2
	DW forth_two_dup-2
forth_nip_tok: EQU ($-tokens)/2
	DW forth_nip-2
forth_less_than_tok: EQU ($-tokens)/2
	DW forth_less_than-2
forth_greater_than_tok: EQU ($-tokens)/2
	DW forth_greater_than-2
forth_minus_rot_tok: EQU ($-tokens)/2
	DW forth_minus_rot-2
forth_drop_tok: EQU ($-tokens)/2
	DW forth_drop-2
forth_c_plus_store_tok: EQU ($-tokens)/2
	DW forth_c_plus_store-2
forth_r_from_tok: EQU ($-tokens)/2
	DW forth_r_from-2
forth_to_r_tok: EQU ($-tokens)/2
	DW forth_to_r-2
two_to_r_tok: EQU ($-tokens)/2
	DW two_to_r-2
forth_zero_equals_tok: EQU ($-tokens)/2
	DW forth_zero_equals-2
forth_zero_not_equals_tok: EQU ($-tokens)/2
	DW forth_zero_not_equals-2
forth_equals_tok: EQU ($-tokens)/2
	DW forth_equals-2
forth_not_equals_tok: EQU ($-tokens)/2
	DW forth_not_equals-2
forth_invert_tok: EQU ($-tokens)/2
	DW forth_invert-2
forth_two_star_tok: EQU ($-tokens)/2
	DW forth_two_star-2
until_raw_tok: EQU ($-tokens)/2
	DW until_raw-2
forth_and_tok: EQU ($-tokens)/2
	DW forth_and-2
forth_or_tok: EQU ($-tokens)/2
	DW forth_or-2
forth_xor_tok: EQU ($-tokens)/2
	DW forth_xor-2
lshift_tok: EQU ($-tokens)/2
	DW lshift-2

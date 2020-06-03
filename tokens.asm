; vim:set syntax=z80:

literal_raw_tok: EQU ($-tokens)/2
	DW literal_raw-2
c_literal_tok: EQU ($-tokens)/2
	DW c_literal-2
zero_literal_tok: EQU ($-tokens)/2
	DW zero_literal-2
one_literal_tok: EQU ($-tokens)/2
	DW one_literal-2
true_tok: EQU ($-tokens)/2
	DW true-2
dup_tok: EQU ($-tokens)/2
	DW dup-2
question_dup_tok: EQU ($-tokens)/2
	DW question_dup-2
swap_tok: EQU ($-tokens)/2
	DW swap-2
rot_tok: EQU ($-tokens)/2
	DW rot-2
over_tok: EQU ($-tokens)/2
	DW over-2
fetch_tok: EQU ($-tokens)/2
	DW fetch-2
store_tok: EQU ($-tokens)/2
	DW store-2
c_fetch_tok: EQU ($-tokens)/2
	DW c_fetch-2
c_store_tok: EQU ($-tokens)/2
	DW c_store-2
s_quote_raw_tok: EQU ($-tokens)/2
	DW s_quote_raw-2
dot_quote_raw_tok: EQU ($-tokens)/2
	DW dot_quote_raw-2
exit_tok: EQU ($-tokens)/2
	DW exit-2
loop_raw_tok: EQU ($-tokens)/2
	DW loop_raw-2
one_plus_tok: EQU ($-tokens)/2
	DW one_plus-2
one_minus_tok: EQU ($-tokens)/2
	DW one_minus-2
question_do_raw_tok: EQU ($-tokens)/2
	DW question_do_raw-2
plus_tok: EQU ($-tokens)/2
	DW plus-2
minus_tok: EQU ($-tokens)/2
	DW minus-2
two_slash_tok: EQU ($-tokens)/2
	DW two_slash-2
if_raw_tok: EQU ($-tokens)/2
	DW if_raw-2
of_raw_tok: EQU ($-tokens)/2
	DW of_raw-2
else_skip_tok: EQU ($-tokens)/2
	DW else_skip-2
two_dup_tok: EQU ($-tokens)/2
	DW two_dup-2
nip_tok: EQU ($-tokens)/2
	DW nip-2
tuck_tok: EQU ($-tokens)/2
	DW tuck-2
zero_less_tok: EQU ($-tokens)/2
	DW zero_less-2
less_than_tok: EQU ($-tokens)/2
	DW less_than-2
greater_than_tok: EQU ($-tokens)/2
	DW greater_than-2
minus_rot_tok: EQU ($-tokens)/2
	DW minus_rot-2
drop_tok: EQU ($-tokens)/2
	DW drop-2
plus_store_tok: EQU ($-tokens)/2
	DW plus_store-2
c_plus_store_tok: EQU ($-tokens)/2
	DW c_plus_store-2
r_from_tok: EQU ($-tokens)/2
	DW r_from-2
r_fetch_tok: EQU ($-tokens)/2
	DW r_fetch-2
to_r_tok: EQU ($-tokens)/2
	DW to_r-2
two_to_r_tok: EQU ($-tokens)/2
	DW two_to_r-2
zero_equals_tok: EQU ($-tokens)/2
	DW zero_equals-2
zero_not_equals_tok: EQU ($-tokens)/2
	DW zero_not_equals-2
equals_tok: EQU ($-tokens)/2
	DW equals-2
not_equals_tok: EQU ($-tokens)/2
	DW not_equals-2
invert_tok: EQU ($-tokens)/2
	DW invert-2
two_star_tok: EQU ($-tokens)/2
	DW two_star-2
until_raw_tok: EQU ($-tokens)/2
	DW until_raw-2
again_raw_tok: EQU ($-tokens)/2
	DW again_raw-2
and_tok: EQU ($-tokens)/2
	DW and-2
or_tok: EQU ($-tokens)/2
	DW or-2
xor_tok: EQU ($-tokens)/2
	DW xor-2
lshift_tok: EQU ($-tokens)/2
	DW lshift-2
rshift_tok: EQU ($-tokens)/2
	DW rshift-2
two_drop_tok: EQU ($-tokens)/2
	DW two_drop-2
repeat_raw_tok: EQU ($-tokens)/2
	DW repeat_raw-2

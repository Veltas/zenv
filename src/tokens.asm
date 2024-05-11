; vi:syntax=z80

; ZEnv - Forth for the ZX Spectrum
; Copyright 2021-2024 (C) - Christopher Leonard, MIT Licence
; https://github.com/veltas/zenv

; Tokens vector


	IF tokenised
	ALIGN 0x100
tokens:
exit_tok:
	DW exit
dup_tok:
	DW dup
question_dup_tok:
	DW question_dup
less_than_tok:
	DW less_than
greater_than_tok:
	DW greater_than
drop_tok:
	DW drop
two_drop_tok:
	DW two_drop
swap_tok:
	DW swap
rot_tok:
	DW rot
over_tok:
	DW over
nip_tok:
	DW nip
tuck_tok:
	DW tuck
plus_tok:
	DW plus
one_plus_tok:
	DW one_plus
one_minus_tok:
	DW one_minus
minus_tok:
	DW minus
raw_char_tok:
	DW raw_char
literal_raw_tok:
	DW literal_raw
if_raw_tok:
	DW if_raw
else_skip_tok:
	DW else_skip
store_tok:
	DW store
c_store_tok:
	DW c_store
fetch_tok:
	DW fetch
c_fetch_tok:
	DW c_fetch
two_fetch_tok:
	DW two_fetch
two_swap_tok:
	DW two_swap
to_r_tok:
	DW to_r
r_from_tok:
	DW r_from
r_fetch_tok:
	DW r_fetch
zero_literal_tok:
	DW zero_literal
one_literal_tok:
	DW one_literal
true_tok:
	DW true
to_in_tok:
	DW to_in
postpone_raw_tok:
	DW postpone_raw
zero_equals_tok:
	DW zero_equals
equals_tok:
	DW equals
not_equals_tok:
	DW not_equals
until_raw_tok:
	DW until_raw
again_raw_tok:
	DW again_raw
do_raw_tok:
	DW do_raw
question_do_raw_tok:
	DW question_do_raw
loop_raw_tok:
	DW loop_raw
compile_comma_tok:
	DW compile_comma
negate_tok:
	DW negate
_abs_tok:
	DW _abs
and_tok:
	DW and
or_tok:
	DW or
xor_tok:
	DW xor
invert_tok:
	DW invert
tick_in_tok:
	DW tick_in
in_size_tok:
	DW in_size
dot_quote_raw_tok:
	DW dot_quote_raw
abort_quote_raw_tok:
	DW abort_quote_raw
t_col_tok:
	DW t_col
t_row_tok:
	DW t_row
within_tok:
	DW within
cell_plus_tok:
	DW cell_plus
cells_tok:
	DW cells
	ENDIF

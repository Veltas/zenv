" ZEnv - Forth for the ZX Spectrum
" Copyright 2021-2025 (C) - Christopher Leonard, MIT Licence
" https://github.com/veltas/zenv

" Vim syntax file for Z80 assembly

" Copy to e.g. ~/.vim/syntax/ to enable

if exists("b:current_syntax")
  finish
endif

syntax case ignore

syn match z80Keywords "\<af'"
syn match z80Keywords "\$"
syn match z80Keywords "\$[[:digit:]]"
syn keyword z80Keywords savebin device define disp ent bss if elseif else end assert global local endlocal macro endm endif include org equ dw defw db defb ds dm defm ld push pop ex exx ldi ldir ldd lddr cpi cpir cpd cpdr add adc sub sbc and or xor cp inc dec daa cpl neg ccf scf nop halt di ei im rlca rla rrca rra rlc rl rrc rr sla sra srl rld rrd bit set res jp jr djnz call ret reti retn rst in ini inir ind indr out outi otir outd otdr nz z nc po pe p m a b c d e f af bc de ix iy i r hl h l sp ixh ixl iyh iyl

"syn match z80Identifiers '\<[_[:alpha:]][_[:alnum:]]*\>'

syn match z80Number "\<\d\+\>"
syn match z80Number "\<0[xX]\x\+\>"
syn match z80Number "\<0[bB][01]\+\>"

syn region z80String start=/\v"/ skip=/\v\\./ end=/\v"/
syn region z80String start=/\v(af)@2<!'/ skip=/\v\\./ end=/\v'/

syn keyword z80Todo contained TODO FIXME XXX NOTE
syn match z80Comments ';.*$' contains=z80Todo
syn match z80Macro '#.*$' contains=z80Todo

let b:current_syntax = "z80"

hi def link z80Todo Todo
hi def link z80Comments Comment
hi def link z80String Constant
hi def link z80Number Constant
hi def link z80Keywords Conditional
hi def link z80Macro Macro

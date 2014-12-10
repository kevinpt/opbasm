divert(-1)
changecom(;)
; m4 macros for enhanced Picoblaze assembly
; These are included as part of Opbasm and available automatically when processing
; files with .psm4 or .m4 extenstions or when the --m4 option is provided
;
; These can be used manually with any Picoblaze assembler by running the following:
;   m4 picoblaze.m4 [input source] > expanded_macros.gen.psm

; Copyright © 2014 Kevin Thibedeau
; (kevin 'period' thibedeau 'at' gmail 'punto' com)
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
; DEALINGS IN THE SOFTWARE.


;=============== LITERAL OPERATIONS ===============

;---------------------------------
; Extensions to eval
; Evaluate expression as a decimal number
define(`evald', `eval(const2m4($1))''d  `;' $1)

; Evaluate expression as an 8-bit hex number
define(`evalh', `eval((const2m4($1)) & 0xFF, 16, 2)  ; $1')

; Evaluate expression as a 12-bit hex number for addresses
define(`evala', `eval(($1) & 0xFFF, 16, 3)  ; $1')

; Evaluate expression as an 8-bit binary number
define(`evalb', `eval((const2m4($1)) & 0xFF, 2, 8)''b  `;' $1)

; Evaluate expression as a decimal number
define(`evalc', `eval(const2m4($1), $2, $3)')


; Only evaluate valid expressions, otherwise reproduce the original text in the
; first argument
define(`evalx', `ifelse(eval(regexp(`$1',`^[-+~0-9(]')>=0),1,ifelse($3,,ifelse($2,,`pushdef(`_evalx', `eval($1)')', `pushdef(`_evalx', `eval($1, $2)')'),`pushdef(`_evalx', `eval(($1 & 0x`'repeatstr(F,$3)), $2, $3)')'),`pushdef(`_evalx', $1)')'_evalx`'popdef(`_evalx'))

; Ex: constant cname,  evalh(20 + 6)      -->  constant cname,  1a
;     constant cname2, evald(20 * 4 - 1)  -->  constant cname2, 79'd
;     constant cname3, evalh(250 + 6)     -->  constant cname3, 01
;
;     evalx(some_name)  --> some_name
;     evalx(1+3)        --> 4


;=============== TYPE CONVERSION ===============

;---------------------------------
; Convert a list of values in Picoblaze hex format to decimal
; Arg1-Argn: Hex values to convert
; Ex: pbhex(01, 02, 03, 0a, ff)  ; Expands to 1, 2, 3, 10, 255
define(`pbhex', `ifelse(eval($#>1),1,eval(0x$1)`,' `$0(shift($@))',$1,,,`eval(0x$1)')')

;---------------------------------
; Convert a string to a list of decimal ASCII codes
; Arg1: String to convert
; Ex: asciiord(`My string')  ; Expands to 77, 121, 32, 115, 116, 114, 105, 110, 103
changequote(<!,!>) ; Change quotes so we can handle "`" and "'"

define(<!asciiord!>,<!changequote(<!,!>)<!!>_asciiord(<!$1!>)<!!>changequote`'dnl
!>)

define(<!_asciiord!>,<!ifelse(<!$1!>,,,<!_aconv(substr(<!$1!>,0,1))<!!>ifelse(len(<!$1!>),1,,<!,!>) $0(substr(<!$1!>,1))!>)!>)

define(<!_aconv!>,<!ifelse(<!$1!>,<! !>,32,<!$1!>,<!;!>,59,dnl
<!index(<!                                 !"#$%&'()*+,-./0123456789: <=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ !>,<!$1!>)!>)!>)

changequote

;---------------------------------
; Convert 16-bit words into bytes
; Arg1-Argn: Numbers to split into bytes
; words_le produces little-endian byte order, words_be is big-endian
; Ex: words_le(0xff01, 0xff02) ; Expands to 255, 1, 255, 2
define(`words_le', `ifelse(`$1',,,eval($#>1),1,`_split_le($1), $0(shift($@))',`_split_le($1)')')
define(`words_be', `ifelse(`$1',,,eval($#>1),1,`_split_be($1), $0(shift($@))',`_split_be($1)')')

define(`_split_le', `eval(($1) & 0xFF), eval((($1) & 0xFF00) >> 8)')
define(`_split_be', `eval((($1) & 0xFF00) >> 8), eval(($1) & 0xFF)')



;---------------------------------
; Convert Picoblaze literals into m4 syntax
; Arg1: String to convert
; Result is an integer in m4 syntax
; Handles "c" -> ascii ord.,  nn'd -> decimal,  nn -> hex,  nn'b -> bin
; Ex: pb2m4(10'd) expands to 10,  pb2m4("0") expands to 48
changequote(<!,!>)

define(<!pb2m4!>,<!changequote(<!,!>)<!!>ifelse(regexp(<!$1!>, <!^[0-9a-fA-F]+$!>),0,<!_conv_hex($1)!>,dnl
regexp(<!$1!>,<!^[0-9]+['!]d$!>),0,<!_conv_dec($1)!>, regexp(<!$1!>, <!^[01]+['!]b$!>),0,<!_conv_bin($1)!>,dnl
regexp(<!$1!>, <!^"."$!>),0,<!_conv_char(<!$1!>)!>,<!$1!>)<!!>changequote`'dnl
!>)

define(<!_conv_hex!>, <!eval(regexp(<!$1!>, <!^\([0-9a-fA-F]+\)$!>, <!0x\1!>))!>)
define(<!_conv_dec!>, <!regexp(<!$1!>, <!^\([0-9]+\)['!]d$!>, <!\1!>)!>)
define(<!_conv_bin!>, <!eval(regexp(<!$1!>, <!^\([01]+\)['!]b$!>, <!0b\1!>))!>)

define(<!_alt_ao!>, <!ifelse(<!$1!>, ,32,<!$1!>,<!;!>,59,dnl
<!index(<!                                 !"#$%&'()*+,-./0123456789: <=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ !>,<!$1!>)!>)!>)

define(<!_conv_char!>, <!_alt_ao(substr(<!$1!>,1,1))!>)

changequote

;=============== INTERNAL CONFIGURATION ===============

define(`_tempreg', `sE')

define(`use_tempreg', `pushdef(`_tempreg', $1)')


;=============== MISCELLANEOUS OPERATIONS ===============

;---------------------------------
; No-op macros
define(`nop', `load sF, sF  ; NOP')

;---------------------------------
; Swap registers
; Arg1: Register 1
; Arg2: Register 2
; Ex:  swap(s0, s1)
define(`swap', `xor $1, $2  ; Swap
xor $2, $1
xor $1, $2')

;---------------------------------
; Generate a random name for a label
; Arg1: Optional prefix to name
; Ex:   randlabel(PREFIX_)  --> PREFIX_?????
define(`randlabel', `esyscmd(`python -c "import sys; import random; import string; sys.stdout.write(\"$1\" + \"\".join([random.choice(string.ascii_letters) for _ in xrange(5)]))"')')

;---------------------------------
; Generate a unique name for a label
; Arg1: Optional prefix to name
; Ex:   uniqlabel(PREFIX_)  --> PREFIX_f0_0001
ifdef(`M4_FILE_NUM',,`define(`M4_FILE_NUM', 0)')
define(`_uniq_ix', 0)

define(`uniqlabel', `define(`_uniq_ix', incr(_uniq_ix))dnl
$1f`'eval(M4_FILE_NUM)_`'eval(_uniq_ix, 10, 4)')


;---------------------------------
; Reverse arguments
; Ex: reverse(1,2,3)  --> 3,2,1
define(`reverse', `ifelse(eval($# > 1), 1, `reverse(shift($@)), `$1'', ``$1'')')

;---------------------------------
; Define a series of contiguous port or scratchpad memory constants
; Arg1: Starting address for port or memory
; Arg2-Argn: Constant names
; Ex: iodefs(0, P_uart_out, P_uart_in, P_control)
;     Expands to:
;       constant P_uart_out, 00
;       constant P_uart_in, 01
;       constant P_control, 02
define(`iodefs', `constant $2, eval($1, 16, 2)'
`ifelse(eval($# > 2), 1, `$0(eval($1 + 1), shift(shift($@)))')')


;---------------------------------
; Load a register with a value and output to a port
; Arg1: Register to load with value
; Arg2: Value to load (constant or other register)
; Arg3: Port to output to
define(`load_out', `load $1, evalc($2, 16, 2)
output $1, $3')

;---------------------------------
; Load a register with a value and store to scratchpad
; Arg1: Register to load with value
; Arg2: Value to load (constant or other register)
; Arg3: Scratchpad address to output to

define(`load_st', `load $1, evalc($2, 16, 2)
store $1, $3')


;---------------------------------
; Define variables
; Arg1-ArgN: Series of variable alias expressions where an alias expression is:
;               <reg> is <alias> [:= value]
; The alias becomes an alternate name for the register. It is loaded with a value if the
; optional initializer is included. The value can be any constant expression or register.
; Ex: vars(s0 is counter := 0, s1 is sum, s2 is max := 20*3)
define(`vars', `ifelse(`$1',,,`_vardef(_vartokens(`$1'))
$0(shift($@))')')

define(`_vartokens', `regexp(`$1',`\([^ ]+\) +is +\([^ ]+\)\( *:= *\([^ ]+\)\)?',`\1, `\2', \4')')

define(`_vardef', `ifelse(`$1',,`errmsg(Invalid variable definition)')'`pushdef(`$2', $1)'dnl
`ifelse(`$3',,,`load $1, evalx($3, 16, 2) `;' Var `$2' := $3')')


;=============== DELAYS ===============

;---------------------------------
; Define system clock frequency
; ** Only invoke once. Must be executed before any delay macros **
; Arg1: Clock frequency in MHz
; Ex: use_clock(50) ; 50 MHz clock
define(`use_clock', `define(`_cfreq', $1)')

define(`_delay_initcheck', `_initcheck(`_cfreq', `Delays are `not' initialized. Use `use_clock()' before any delay operation')')


;---------------------------------
; Delay for a number of instruction cycles
; Arg1: Number of instructions to delay
; This delay generator constructs a minimal delay using a combination of
; delay trees and NOPs. The result uses fewer instructions that a plain chain
; of NOPs. Note that the delay trees use recursion on the stack to maintain delay state.
; There is a slight risk of overflowing the stack if this routine is invoked for long delays
; when already in a deeply nested call frame.
; The maximum delay is appx. 100e9 cycles (1000sec at 100MHz)
; Ex: delay_cycles(10) ; Delay for 10 instructions (20 clock cycles)
; Yo, dawg! I hard you like recursion so I put some recursion in your recursive loops
define(`delay_cycles', `ifelse(eval(const2m4($1) < 5),1,`repeat(`nop', const2m4($1))',`_delay_tree(floor_log2(eval(const2m4($1) - 1)))'
`$0(eval(const2m4($1) - 2**floor_log2(eval(const2m4($1) - 1)) - 1))')')

;define(`floor_log2', `ifelse(eval($1 <= 1),1,0,`eval($0(eval($1 >> 1)) + 1)')')
define(`floor_log', `ifelse(eval($1 < $2),1,0,`eval($0(eval($1 / $2), $2) + 1)')')
define(`floor_log2', `floor_log($1,2)')


; Generate a binary tree delay with recursive calls
; Arg1: Number of stages. Delayed instructions are 2**Arg1 + 1
define(`_delay_tree', `pushdef(`_dt', uniqlabel(DTREE_))'dnl
`call _dt`'_`'decr($1) `; Delay for' eval(2**$1 + 1) cycles
jump _dt`'_end
_delay_tree_gen(decr($1))dnl
_dt`'_0: return
_dt`'_end:'`popdef(`_dt')')

define(`_delay_tree_gen', `_dt`'_$1: call _dt`'_`'decr($1)'
`ifelse(eval($1 > 1),1,`$0(decr($1))')')


;---------------------------------
; Delay by milliseconds
; Arg1: Milliseconds to delay
; Arg2, Arg3: MSB, LSB of delay counter
; Arg4: Optional number of instructions to deduct from the delay (default is 0)
; This delay will be cycle accurate if the requested delay is an integer multiple
; of the clock period.
; At 100 MHz, the max delay is 214 ms. It increases with lower clock frequencies
; Ex:
;             use_clock(50) ; 50 MHz clock
;  delay_5ms: delay_ms(5, s4,s5, 2) ; Deduct 2 additional instructions to account for call and return
;             return
;  ...
;  call delay_5ms
define(`delay_ms', ``; Delay for' const2m4($1) ms at _cfreq MHz
_delay_us(eval(const2m4($1)*1000),$2,$3,`ifelse(`$4',,2,`eval(2 + $4)')')')


;---------------------------------
; Delay by microseconds
; Arg1: Microseconds to delay
; Arg2, Arg3: MSB, LSB of delay counter
; Arg4: Optional number of instructions to deduct from the delay (default is 0)
; This delay will be cycle accurate if the requested delay is an integer multiple
; of the clock period.
; Ex:
;              use_clock(50) ; 50 MHz clock
;  delay_40us: delay_us(40, s4,s5)
;              return
;  ...
;  call delay_40us
define(`delay_us', ``; Delay for' const2m4($1) us at _cfreq MHz
_delay_us(const2m4($1),$2,$3,`ifelse(`$4',,2,`eval(2 + $4)')')')

define(`_delay_us', `_delay_initcheck' `pushdef(`_dly', uniqlabel(DELAY_))'`dnl
ifelse(eval(_dval_us($1,$4) >= 2**16),1,`errmsg(`Delay value is too large')')dnl
load16($2,$3, _dval_us($1,$4))
_dly: `; Total loop delay:' eval(3 + _dnop_us($1,$4)) instructions
delay_cycles(_dnop_us($1,$4))
sub16($2,$3, 1)
jump nc, _dly
`;' Adjust delay with _dadj_us($1,$4) additional instruction cycles
delay_cycles(_dadj_us($1,$4))'`popdef(`_dly')')

; Calculate number of nops needed to fit delay count into 16-bits
; Arg1: Microseconds to delay
; Arg2: Additional instructions
define(`_dnop_us', `ifelse(eval(_dnop_calc($1,$2) < 0),1,0,_dnop_calc($1,$2))')
; Note: Scaled by 100 to retain fractional bits until the end. The +100 term is
; used to round up result to the next integer.
define(`_dnop_calc', `eval( (100 * $1 * _cfreq / (2 * (2**16 + $2)) - 300 + 100) / 100 )')

; Calculate microsecond delay value
; Arg1: Microseconds to delay
; Arg2: Additional instructions
define(`_dval_pre', `eval(($1 * _cfreq / 2 - $2) / (3 + _dnop_us($1,$2)) - 1)')

; Adjustment delays needed to reach requested delay
define(`_dadj_pre', `eval( ($1 * _cfreq - ((_dval_pre($1,$2)+1)*(3 + _dnop_us($1,$2)) + $2)*2) / 2  )')

; Final count value and adjustment delays
define(`_dval_us', `ifelse(eval(_dadj_pre($1,$2) < 0),1,`eval(_dval_pre($1,$2) - 1)',`_dval_pre($1,$2)')')
define(`_dadj_us', `eval( ($1 * _cfreq - ((_dval_us($1,$2)+1)*(3 + _dnop_us($1,$2)) + $2)*2) / 2  )')


;---------------------------------
; Variable delay by milliseconds
; Arg1: Maximum milliseconds to delay
; Arg2, Arg3: MSB, LSB of delay counter
; The var_count_ms() macro generates a 16-bit count value that is loaded
; into the counter registers before calling the delay function
; Ex:
;         use_clock(50) ; 50 MHz clock
;         define(MAXDELAY, 10) ; 10ms max delay
;         reg16(dly_count, s4,s5)
;  delay: var_delay_ms(MAXDELAY, dly_count)
;         return
;  ...
;  load16(dly_count, var_count_ms(1, MAXDELAY))
;  call delay ; Delay for 1 ms
;  ...
;  load16(dly_count, var_count_ms(8, MAXDELAY))
;  call delay ; Delay for 8 ms
define(`var_delay_ms', ``; Variable delay for max' const2m4($1) ms at _cfreq MHz
_var_delay_us(eval(const2m4($1)*1000),$2,$3)')

;---------------------------------
; Variable delay by microseconds
; Arg1: Maximum microseconds to delay
; Arg2, Arg3: MSB, LSB of delay counter
; The var_count_us() macro generates a 16-bit count value that is loaded
; into the counter registers before calling the delay function
; Ex:
;         use_clock(50) ; 50 MHz clock
;         define(MAXDELAY, 900) ; 900 us max delay
;         reg16(dly_count, s4,s5)
;  delay: var_delay_us(MAXDELAY, dly_count)
;         return
;  ...
;  load16(dly_count, var_count_us(100, MAXDELAY))
;  call delay ; Delay for 100 us
;  ...
;  load16(dly_count, var_count_us(800, MAXDELAY))
;  call delay ; Delay for 800 us
define(`var_delay_us', ``; Variable delay for max' const2m4($1) us at _cfreq MHz
_var_delay_us(const2m4($1),$2,$3)')

define(`_var_delay_us', `_delay_initcheck' `pushdef(`_dly', uniqlabel(VDELAY_))'`dnl
ifelse(eval(_dval_pre($1,0) >= 2**16),1,`errmsg(`Max delay value is too large')')dnl
_dly: `; Total loop delay:' eval(3 + _dnop_us($1,0)) instructions
delay_cycles(_dnop_us($1,0))
sub16($2,$3, 1)
jump nc, _dly'
`popdef(`_dly')')

;---------------------------------
; Generate 16-bit millisecond count for variable delay function
; Arg1: Milliseconds to delay
; Arg2: Max milliseconds for the delay loop (from definition using var_delay_ms())
define(`var_count_ms', `ifelse(eval(_dval_var(eval(const2m4($1)*1000),eval(const2m4($2)*1000)) < 0),1,dnl
`errmsg(`Delay is too small: const2m4($1) ms')',`_dval_var(eval(const2m4($1)*1000),eval(const2m4($2)*1000))')')

;---------------------------------
; Generate 16-bit microsecond count for variable delay function
; Arg1: Microseconds to delay
; Arg2: Max microseconds for the delay loop (from definition using var_delay_us())
define(`var_count_us', `ifelse(eval(_dval_var(const2m4($1),const2m4($2)) < 0),1,dnl
`errmsg(`Delay is too small: const2m4($1) us')',`_dval_var(const2m4($1),const2m4($2))')')

define(`_dval_var', `eval(($1 * _cfreq / 2 - 2) / (3 + _dnop_us($2,2)) - 1)')



;=============== CARRY FLAG OPERATIONS ===============

;---------------------------------
; Clear the carry flag
define(`clearcy', `and sF, sF  ; Clear carry')

;---------------------------------
; Set the carry flag
; Arg1: Optional temporary register to modify. Uses temp reg by default.
; Ex: setcy(sf)
define(`setcy', `ifelse(`$1',,`pushdef(`_cyreg', `_tempreg')', `pushdef(`_cyreg', `$1')')'dnl
`load _cyreg, 00  ; Set carry
compare _cyreg, 01'`popdef(`_cyreg')')

;=============== BITFIELD OPERATIONS ===============

;---------------------------------
; Set and clear bits in a register
; Arg1: Register to modify
; Arg2: Bit number (0-7) to set or clear
; Ex: setbit(s0, 2)
define(`setbit', `or $1, eval(2**($2), 16, 2)  ; Set bit $2')
define(`clearbit', `and $1,  eval((~(2**($2))) & 0xFF, 16, 2)  ; Clear bit $2')

;---------------------------------
; Define a mask with specific bits set
; Arg1-Argn: Bit numbers to set in mask (0-7)
; Ex: mask(0, 1, 2, 7)     ; Expands to 135 = 0x87
define(`mask', `ifelse($1,,0,`eval(2**($1) + $0(shift($@)))')')

;---------------------------------
; Alternate mask that can be used as a direct argument to a Picoblaze instruction
; Arg1: Bit numbers to set in mask (0-7) 
; Result is a mask in Picoblaze hex format
; Ex: test s0, maskh(3,4,5) ; Test if bits 3, 4, and 5 are clear
;     jump z, is_clear
define(`maskh', `eval(mask($@), 16, 2)')

;---------------------------------
; Set and clear mask bits in a register
; Arg1: Register to modify
; Arg2: Mask value
; Ex: setmask(s5, mask(0,1,2))
define(`setmask', `or $1, eval(const2m4($2), 16, 2)  ; Set mask')
define(`clearmask', `and $1, eval((~(const2m4($2))) & 0xFF, 16, 2)  ; Clear mask')

;---------------------------------
; Test if a bit is set or clear
; Arg1: Register to test
; Arg2: Bit number (0-7) to test
; Z is set if bit is clear, Z is clear if bit is set
; Ex:  testbit(s1, 3)
;      jump z, bit_cleared
define(`testbit', `test $1, eval(2**($2), 16, 2)  ; Test bit $2')



;=============== CONDITIONAL JUMP, CALL, and RETURN OPERATIONS ===============

; Jump if not equal
; Arg1: Label to jump to
define(`jne', `jump nz, $1  ; if not equal')

; Jump if equal
; Arg1: Label to jump to
define(`jeq', `jump z, $1  ; if equal')

; Jump if greater or equal
; Arg1: Label to jump to
define(`jge', `jump nc, $1  ; if greater or equal')

; Jump if less than
; Arg1: Label to jump to
define(`jlt', `jump c, $1  ; if less than')

;Ex: compare s0, s1
;    jeq(is_equal)   ; jumps if s0 == s1
;    jlt(less_than)  ; jumps if s0 < s1


; Call if not equal
; Arg1: Label to call
define(`callne', `call nz, $1  ; if not equal')

; Call if equal
; Arg1: Label to call
define(`calleq', `call z, $1  ; if equal')

; Call if greater or equal
; Arg1: Label to call
define(`callge', `call nc, $1  ; if greater or equal')

; Call if less than
; Arg1: Label to call
define(`calllt', `call c, $1  ; if less than')

; Ex: compare s3, 24
;     callne(not_equal)  ; call if s3 != 24
;     callge(greater)    ; call if s3 >= 24


; Return if not equal
define(`retne', `return nz  ; if not equal')

; Return if equal
define(`reteq', `return z  ; if equal')

; Return if greater or equal
define(`retge', `return nc  ; if greater or equal')

; Return if less than
define(`retlt', `return c  ; if less than')

;Ex: compare s0, s1
;    reteq   ; return if s0 == s1



;=============== CONDITIONAL IF-THEN-ELSE ===============


;---------------------------------
; Generic if macro
; Arg1: Boolean comparison expression
;       Must be of the form: "reg op reg|expression" where op is <, >=, ==, !=, or &
;       Signed comparison is invoked with "signed(comparison expr.)"
;       With signed comparison the right operand cannot be a named constant.
;       With the & operator a test instruction is used in place of compare. The true
;       clause is executed when the result is non-zero.
; Arg2: True clause
; Arg3: Optional else clause or next else-if comparison expression
; Arg4-Argn: Additional else-if clauses
;   This macro performs a comparison of the left and right operands and then inserts
;   the if* macro selected by the operation
; Ex: if(s0 < s1, `load s0, 01', `load s0, 02')
;     if(s0 != 0xa5, `load s0, 01')
;     if(signed(s0 < -10), `load s0, 01') ; Signed comparison with signed()
define(`if', `ifelse(eval($# > 3),1,`_if(_iftokens($1), `$2', `if(shift(shift($@)))')',dnl
`_if(_iftokens($1), `$2', `$3')')')

define(`_iftokens', `regexp(`$1', `\(\w+\) *\(s?[<>=!&]+\) *\(.+\)', `\1, \2, \3')')

define(`_if', `; If $1 $2 $3'
`ifelse(regexp($2,`^\(s<\|s>=\)$'),0,`compares($1, $3)',regexp($2,`^&'),0,`test $1, evalx($3, 16, 2)',dnl
regexp($2,`^\(>\|<=\)$'),0,`_comp_gt_or_le($1,$3)',dnl
regexp($2,`^\(s<=\|s>\)$'),0,`_comps_gt_or_le($1,$3)',dnl
`compare $1, evalx($3, 16, 2)')'
`ifelse($2,<,`iflt(`$4',`$5')', $2,>=,`ifge(`$4',`$5')',
$2,s<,`iflt(`$4',`$5')', $2,s>=,`ifge(`$4',`$5')',
$2,>,`ifelse(isnum(const2m4($3)),1,`ifge(`$4',`$5')',`iflt(`$4',`$5')')',
$2,<=,`ifelse(isnum(const2m4($3)),1,`iflt(`$4',`$5')',`ifge(`$4',`$5')')',
$2,s>,`ifelse(isnum(const2m4($3)),1,`ifge(`$4',`$5')',`iflt(`$4',`$5')')',
$2,s<=,`ifelse(isnum(const2m4($3)),1,`iflt(`$4',`$5')',`ifge(`$4',`$5')')',
$2,==,`ifeq(`$4',`$5')', $2,!=,`ifne(`$4',`$5')',
$2,&,`ifne(`$4',`$5')', `errmsg(`Invalid operation: $2')' )')'

;---------------------------------
; Convert boolean expression to use signed comparison
; Arg1: Expression to convert
; Ex: signed(s0 < 4)  ; Expands to "s0 s< 4"
define(`signed', `patsubst(`$1', `\([<>]=?\)', ` s\1')')

; Restore right curly brackets inside quoted strings that were protected before
; performing C-style syntax transformations
define(`_rcurly', `}')

; Restore right parens from signed() macros that were protected
define(`_rparen', `)')

; The preprocessor converts constant directives into const() macros so that
; the m4 code is aware of constants declards in picoblaze syntax
changequote(<!,!>)
define(<!const!>, <!changequote(<!,!>)<!!>pushdef(<!_cname_$1!>,<!$2!>)!><!constant $1, translit($2,!,')<!!>changequote`'dnl
!>)
changequote
define(`isconst', `ifdef(`_cname_$1',1,0)')
define(`const2m4', `ifelse(isconst($1),1,`pb2m4(_cname_$1)',`$1')')


; To get the <= and > Boolean comparisons we have to modify the arguments depending
; on the type of the right operand. If the right operand is a literal or a defined constant
; we increment the constant by 1. If the right operand is a register we swap arguments.
define(`_comp_gt_or_le',`ifelse(isconst($2),1,`compare $1, eval(pb2m4(_cname_$2)+1,16,2)',dnl
isnum($2),1,`compare $1, eval($2 + 1,16,2)',dnl
`compare $2, $1')')

define(`_comps_gt_or_le',`ifelse(isconst($2),1,`compares($1, eval(pb2m4(_cname_$2)+1))',dnl
isnum($2),1,`compares($1, eval($2 + 1))',dnl
`compares($2, $1)')')


;---------------------------------
; Low level if macros: ifeq, ifne, ifge, iflt
; Arg1: True clause
; Arg2: Optional else clause
;   These macros insert labels and jump instructions to implement the behavior of
;   an if-then or if-then-else statement testing for equality, inequality,
;   greater-or-equal, or less-than
; Ex: compare s0, s1
;     ifeq(`load s3, 20
;           output s3, MY_PORT',
;     ; else
;          `load s3, 30
;           output s3, MY_OTHER_PORT')

; If equal
define(`ifeq', `pushdef(`_neq', uniqlabel(NEQ_))'`pushdef(`_endif', uniqlabel(ENDIF_))'`jump nz, _neq
$1
ifelse(`$2',,,`jump _endif')
_neq:
$2
ifelse(`$2',,,`_endif:')'`popdef(`_neq')'`popdef(`_endif')')

; If not equal
define(`ifne', `pushdef(`_eq', uniqlabel(EQ_))'`pushdef(`_endif', uniqlabel(ENDIF_))'`jump z, _eq
$1
ifelse(`$2',,,`jump _endif')
_eq:
$2
ifelse(`$2',,,`_endif:')'`popdef(`_eq')'`popdef(`_endif')')

; If greater or equal
define(`ifge', `pushdef(`_lt', uniqlabel(LT_))'`pushdef(`_endif', uniqlabel(ENDIF_))'`jump c, _lt
$1
ifelse(`$2',,,`jump _endif')
_lt:
$2
ifelse(`$2',,,`_endif:')'`popdef(`_lt')'`popdef(`_endif')')

; If less than
define(`iflt', `pushdef(`_xge', uniqlabel(GE_))'`pushdef(`_endif', uniqlabel(ENDIF_))'`jump nc, _xge
$1
ifelse(`$2',,,`jump _endif')
_xge:
$2
ifelse(`$2',,,`_endif:')'`popdef(`_xge')'`popdef(`_endif')')


define(`errmsg', `errprint($1  __file__ line __line__)m4exit(1)')
define(`warnmsg', `errprint($1  __file__ line __line__)')




;=============== CONDITIONAL LOOPS ===============

;---------------------------------
; While loop
; Arg1: Boolean comparison expression
;       Must be of the form: "reg op reg|expression" where op is <, >=, ==, !=, or &
; Arg2: Code block for loop body
; Ex: load s0, 00
;     while(s0 < 10, `output s3, P_foo
;                     add s0, 01')
define(`while', `pushdef(`_clbl', uniqlabel(WHILE_))'`pushdef(`_elbl', uniqlabel(ENDLOOP_))'`_clbl:
if($1,`$2
jump _clbl')
_elbl:' `popdef(`_clbl')'`popdef(`_elbl')') 


;---------------------------------
; Do-while loop
; Arg1: Boolean comparison expression
;       Must be of the form: "reg op reg|expression" where op is <, >=, ==, !=, or &
; Arg2: Code block for loop body
; Ex: load s0, 15'd
;     dowhile(s0 != 10, `output s3, P_foo
;                        sub s0, 01')
define(`dowhile', `pushdef(`_clbl', uniqlabel(DOWHILE_))'`pushdef(`_elbl', uniqlabel(ENDLOOP_))'`_clbl: ; Do-while $1
$2
_dw(_iftokens($1))
_elbl:' `popdef(`_clbl')'`popdef(`_elbl')')

define(`_dw',dnl
`ifelse(regexp($2, `^\(s<\|s>=\)$'),0,`compares($1, $3)',regexp($2,`^&'),0,`test $1, evalx($3, 16, 2)',dnl
regexp($2,`^\(>\|<=\)$'),0,`_comp_gt_or_le($1,$3)',dnl
regexp($2,`^\(s<=\|s>\)$'),0,`_comps_gt_or_le($1,$3)',dnl
`compare $1, evalx($3, 16, 2)')'
`ifelse($2,<,`jump c, _clbl', $2,>=,`jump nc, _clbl',dnl
$2,s<,`jump c, _clbl', $2,s>=,`jump nc, _clbl',dnl
$2,>,`ifelse(isnum(const2m4($3)),1,`jge(_clbl)',`jlt(_clbl)')',dnl
$2,<=,`ifelse(isnum(const2m4($3)),1,`jlt(_clbl)',`jge(_clbl)')',dnl
$2,s>,`ifelse(isnum(const2m4($3)),1,`jge(_clbl)',`jlt(_clbl)')',dnl
$2,s<=,`ifelse(isnum(const2m4($3)),1,`jlt(_clbl)',`jge(_clbl)')',dnl
$2,==,`jump z, _clbl', $2,!=,`jump nz, _clbl',dnl
$2,&,`jump nz, _clbl', `errmsg(`Invalid operation: $2')')'
)

; Wrapper used to swap arguments when using C-style "do { } while()" syntax
define(`_dowhile2', `dowhile($2, `$1')')



;---------------------------------
; For loop
; Arg1: Initialization expression (passed to expr()) This can be empty
; Arg2: Boolean comparison expression
; Arg3: Update expression (passed to expr()) This can be empty
; Arg4: Code block for loop body
; Ex: for(s0 := 0, s0 < 5, s0 := s0 + 1, `output s0, 00')
; Note that the "continue" macro will behave as in C by jumping to the update code before
; restarting the loop
define(`for', `pushdef(`_slbl', uniqlabel(FOR_))'`pushdef(`_clbl', uniqlabel(NEXTFOR_))'dnl
`pushdef(`_elbl', uniqlabel(ENDLOOP_))'`ifelse(`$1',,,`expr($1)')
_slbl:
if($2,`$4
_clbl: ifelse(`$3',,,`expr($3)')
jump _slbl')
_elbl:' `popdef(`_clbl')'`popdef(`_elbl')') 


; Break statement to exit loops
define(`break', `jump _elbl')

; Continue statement to restart loop
define(`continue', `jump _clbl')

;=============== SHIFT AND ROTATE OPERATIONS ===============

;---------------------------------
; Repeat a string
; Arg1: Instruction or macro string to repeat
; Arg2: Numper of repetitions
define(`repeatstr', `ifelse($#,0, ``$0'', eval(const2m4($2)>0),1, `$1`'$0(`$1', decr(const2m4($2)))')')

;---------------------------------
; Repeat an instruction or macro
; Arg1: Instruction or macro string to repeat
; Arg2: Numper of repetitions
define(`repeat', `ifelse($#,0,``$0'', eval(const2m4($2)>0),1, `$1
$0(`$1', eval(const2m4($2) - 1))')')

;---------------------------------
; Repeated shifts
; Arg1: Register to shift
; Arg2: Number of shifts
; Ex: sl0(s2, 4)  ; Shift left by 4
define(`sl0', `ifelse($#,0, ``$0'', `repeat(`sl0 $1', eval(const2m4($2)))')')
define(`sl1', `ifelse($#,0, ``$0'', `repeat(`sl1 $1', eval(const2m4($2)))')')
define(`sla', `ifelse($#,0, ``$0'', `repeat(`sla $1', eval(const2m4($2)))')')
define(`slx', `ifelse($#,0, ``$0'', `repeat(`slx $1', eval(const2m4($2)))')')
define(`sr0', `ifelse($#,0, ``$0'', `repeat(`sr0 $1', eval(const2m4($2)))')')
define(`sr1', `ifelse($#,0, ``$0'', `repeat(`sr1 $1', eval(const2m4($2)))')')
define(`sra', `ifelse($#,0, ``$0'', `repeat(`sra $1', eval(const2m4($2)))')')
define(`srx', `ifelse($#,0, ``$0'', `repeat(`srx $1', eval(const2m4($2)))')')

; Repeated rotates
define(`rl', `ifelse($#,0, ``$0'', `repeat(`rl $1', eval(const2m4($2)))')')
define(`rr', `ifelse($#,0, ``$0'', `repeat(`rr $1', eval(const2m4($2)))')')


;=============== STACK OPERATIONS ===============

;---------------------------------
; Initialize and define stack pointer register.
; ** Only invoke once. Must be executed before any push() or pop() macros **
; Arg1: Stack pointer register
; Arg2: Scratchpad address for top of stack
; Ex: namereg sA, SP ; Reserve stack pointer register 
;     use_stack(SP, 0x3F) ; Start stack at end of 64-byte scratchpad
define(`use_stack', `load $1, eval(const2m4($2), 16, 2)' `define(`_stackptr', $1)')


define(`_initcheck', `ifdef(`$1',, `errmsg(`$2')')')

define(`_stack_initcheck', `_initcheck(`_stackptr', `Stack is `not' initialized. Use `use_stack()' before any operation')')



;---------------------------------
; Pseudo-stack operations using the scratchpad RAM
; The stack pointer grows from the end of the scratchpad to the start
; Arg1-Argn: Registers with values to push or pop
; Ex: use_stack(sa, 0x3F)
;     push(s0)
;     pop(s1)
;     push(s3, s4, s5)  ; Push and pop multiple registers at once
;     pop(s3, s4, s5)   ; Pop is performed in reverse order from push
define(`push', `_stack_initcheck' `ifelse(`$1',,,`store $1, (_stackptr)  ; Push
sub _stackptr, 01
$0(shift($@))')')


define(`pop', `_pop(reverse($@))')

define(`_pop', `_stack_initcheck' `ifelse(`$1',,,`add _stackptr, 01  ; Pop
fetch $1, (_stackptr)
$0(shift($@))')')


;---------------------------------
; Retrieve multiple contiguous values from the stack without modification
; Arg1-Argn: Registers to save values in
;            The first register corresponds to the highest address
; Ex: getstack(s3, s4, s5) ; Get stack offset SP+3, SP+2, and SP+1 into s3, s4, s5
define(`getstack', `_stack_initcheck' `add _stackptr, $#  ; Get stack registers
_gs($@)')

define(`_gs', `ifelse(`$1',,,`fetch $1, (_stackptr)
sub _stackptr, 01
$0(shift($@))')')

;---------------------------------
; Retrieve values from the stack without modification
; Arg1: Register to save value in
; Arg2: Offset from stack pointer (offset 1 is the first value) or a register
; Ex: getstackat(s3, 2)  ; Get the second value relative to the stack pointer
;     getstackat(s3, s0) ; Get stack value pointed at by s0
define(`getstackat', `_stack_initcheck' `add _stackptr, evalx($2, 16, 2)  ; Fetch stack offset $2
fetch $1, (_stackptr)
sub _stackptr, evalx($2, 16, 2)')


;---------------------------------
; Store multiple contiguous values on the stack without modification
; Arg1-Argn: Registers to store values from
;            The first register corresponds to the highest address
; Ex: putstack(s3, s4, s5) ; Put s3, s4, s5 into stack offset SP+3, SP+2, and SP+1
define(`putstack', `_stack_initcheck' `add _stackptr, $#  ; Put stack registers
_ps($@)')

define(`_ps', `ifelse(`$1',,,`store $1, (_stackptr)
sub _stackptr, 01
$0(shift($@))')')

;---------------------------------
; Store values to the stack without modification
; Arg1: Register with value to store
; Arg2: Offset from stack pointer (offset 1 is the first value) or a register
; Ex: putstackat(s3, 2)  ; Put the second value relative to the stack pointer
;     putstackat(s3, s0) ; Put stack value pointed at by s0
define(`putstackat', `_stack_initcheck' `add _stackptr, evalx($2, 16, 2)  ; Store stack offset $2
store $1, (_stackptr)
sub _stackptr, evalx($2, 16, 2)')


;---------------------------------
; Drop values stored on the stack
; Arg1: Number of values to drop from the stack
; Ex: dropstack(2)  ; Remove 2 values
define(`dropstack', `_stack_initcheck' `add _stackptr, eval($1, 16, 2)  ; Remove stack values')

;---------------------------------
; Drop values stored on the stack using a register
; Arg1: Number of values to drop from the stack
; Ex: dropstackreg(s1)  ; Remove number of values specified in s1 register
define(`dropstackreg', `_stack_initcheck' `add _stackptr, $1  ; Remove stack values')

;---------------------------------
; Allocate local space on the stack
; Arg1: Number of values to add to the stack
; Ex: addstack(2)  ; Add 2 values
define(`addstack', `_stack_initcheck' `sub _stackptr, eval($1, 16, 2)  ; Add local stack values')

;---------------------------------
; Allocate local space on the stack using a register
; Arg1: Number of values to add to the stack
; Ex: addstackreg(s1)  ; Add number of values from s1
define(`addstackreg', `_stack_initcheck' `sub _stackptr, $1  ; Add local stack values')


;=============== STRING AND TABLE OPERATIONS ===============

;---------------------------------
; Repeated string function call operation (useful for Picoblaze-3)
; Arg1: Subroutine to call for each character
; Arg2: Register used to hold characters (typically an argument to the subroutine)
; Arg3: String to split into characters
; Ex: callstring(write_char, s1, `My string')
;     Expands to:
;        load s1, "M"
;        call write_char
;        load s1, "y"
;        call write_char
;        ...
define(`callstring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
call $1'
`$0($1, $2, substr(`$3',1))')')

;---------------------------------
; Repeated string output operation
; Arg1: Output port in m4 integer format or a constant name
; Arg2: Register used to hold characters
; Arg3: String to split into characters
; Ex: constant UART_PORT, 0a
;     outputstring(UART_PORT, s1, `My string')
;     Expands to:
;        load s1, "M"
;        output s1, UART_PORT
;        load s1, "y"
;        output s1, UART_PORT
;        ...
;
;     outputstring(0x0a, s1, `My string') ; Without using a constant
define(`outputstring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
output $2, evalx($1, 16, 2)'
`$0($1, $2, substr(`$3',1))')')


;---------------------------------
; Store a string to scratchpad RAM
; Arg1: Adress of first byte
; Arg2: Temporary register for each character
; Arg3: String to store
define(`storestring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
store $2, eval($1, 16, 2)'
`$0(eval(($1) + 1), $2, substr(`$3',1))')')


;---------------------------------
; Store a string to scratchpad RAM
; Arg1: Pointer register to scratchpad address
; Arg2: Temporary register for each character
; Arg3: String to store
define(`storestringat', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
store $2, ($1)'
`ifelse(eval(len($3)>1),1,`add $1, 01')'
`$0($1, $2, substr(`$3',1))')')



;---------------------------------
; Repeated function call on a table of constants
; Arg1: Subroutine to call for each byte
; Arg2: Temporary register for each constant
; Arg3 - Argn: Decimal values representing table bytes
; Ex: calltable(my_subroutine, sf, pbhex(DE,AD,BE,EF)) ; Pass DE,AD,BE,EF to subroutine

define(`calltable', `pushdef(`_sname', $1)`'pushdef(`_treg', $2)'`_ct(shift(shift($@)))'`popdef(`_sname')`'popdef(`_treg')')

define(`_ct', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
call _sname'
`_ct(shift($@))'dnl
)')

;---------------------------------
; Output a table of constants
; Arg1: Output port in m4 integer format or a constant name
; Arg2: Temporary register for each constant
; Arg3 - Argn: Decimal values to output to port
; Ex: constant UART_PORT, 0a
;     outputtable(UART_PORT, sf, pbhex(DE,AD,BE,EF)) ; Output DE,AD,BE,EF to port

define(`outputtable', `pushdef(`_oreg', $1)`'pushdef(`_treg', $2)'`_ot(shift(shift($@)))'`popdef(`_oreg')`'popdef(`_treg')')

define(`_ot', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
output _treg, evalx(_oreg, 16, 2)'
`_ot(shift($@))'dnl
)')


;---------------------------------
; Store a table of constants in scratchpad RAM
; Arg1: Address of first byte
; Arg2: Temporary register for each constant
; Arg3 - Argn: Decimal values to load in scratchpad
; Ex: load s1, my_array
;     storetable(0x10, sf, pbhex(DE,AD,BE,EF)) ; Load DE,AD,BE,EF into memory
;     storetable(0x10, sf, 10, 11, 12)         ; Load decimals
define(`storetable', `pushdef(`_treg', $2)'`_st($1, shift(shift($@)))'`popdef(`_treg')')

define(`_st', `ifelse(`$2',,,`load _treg, eval($2, 16, 2)
store _treg, eval($1, 16, 2)'
`_st(eval(($1) + 1), shift(shift($@)))'dnl
)')


;---------------------------------
; Store a table of constants in scratchpad RAM
; Arg1: Pointer register to scratchpad address
; Arg2: Temporary register for each constant
; Arg3 - Argn: Decimal values to load in scratchpad
; Ex: load s1, my_array
;     storetableat(s1, sf, pbhex(DE,AD,BE,EF)) ; Load DE,AD,BE,EF into memory
;     storetableat(s1, sf, 10, 11, 12)         ; Load decimals
define(`storetableat', `pushdef(`_preg', $1)`'pushdef(`_treg', $2)'`_sta(shift(shift($@)))'`popdef(`_preg')`'popdef(`_treg')')

define(`_sta', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
store _treg, (_preg)'
`ifelse(eval($#>1),1,`add _preg, 01')'
`_sta(shift($@))'dnl
)')


;---------------------------------
; Generate an INST directive from a pair of decimal values
; Arg1: High 10-bits
; Arg2: Low byte
; Ex: instdata(pbhex(0a, 0b))  ; Expands to inst 00a0b
define(`instdata', `inst eval((($1) << 8) + (($2) & 0xFF), 16, 5)')

;---------------------------------
; Convert a list of data into a series of INST directives in little-endian byte order
; Arg1-Argn: Data to convert in decimal format
; Ex: insttable_le(pbhex(0a, 0b, 0c))
;     Expands to:  inst 00b0a
;                  inst 0000c
;
;     insttable_le(asciiord(`Pack strings into ROM'))
;
;       inst 06150
;       inst 06b63
;       inst 07320
;       ...
;       inst 0206f
;       inst 04f52
;       inst 0004d
define(`insttable_le', `ifelse(eval($#>1),1,`instdata($2, $1)
$0(shift(shift($@)))',$1,,,`instdata(00, $1)')')

;---------------------------------
; Convert a list of data into a series of INST directives in big-endian byte order
; Arg1-Argn: Data to convert in decimal format
; Ex: insttable_be(pbhex(0a, 0b, 0c))
;     Expands to:  inst 00a0b
;                  inst 00c00
define(`insttable_be', `ifelse(eval($#>1),1,`instdata($1, $2)
$0(shift(shift($@)))',$1,,,`instdata($1, 00)')')


;=============== ARITHMETIC OPERATIONS ===============

;---------------------------------
; 2s complement negation
; Arg1: Register to negate
; Result is in the same register
define(`negate', `xor $1, FF  ; Negate
add $1, 01')

;---------------------------------
; Logical not
; Arg1: Register to invert
; Result is in the same register
define(`not', `xor $1, FF  ; Not')

;---------------------------------
; Absolute value
; Arg1: Register to make positive
; Result is in the same register
define(`abs', `if($1 & 0x80, `negate($1)')')

;---------------------------------
; Sign extension
; Arg1: Register to extend sign into
; Arg2: Register to test for sign bit
define(`signex', `if($2 & 0x80, `load $1, FF `;' Sign extend', `load $1, 00')')

;---------------------------------
; Determine if argument is a number in m4 syntax
; Arg1: String to check
; Returns 1 for true 0 for false
define(`isnum', `ifelse(regexp($1, `^-?\(0[xXbB][0-9a-fA-F]+\|[0-9]+\)$'),0,1,0)')

;---------------------------------
; Signed compare
; Arg1: Register for left side of comparison
; Arg2: Register or constant for right side of comparison
;       Constant is a number in m4 syntax and cannot be a named constant
; Carry flag is set in accordance with signed relation
; Zero flag is indeterminate. Use normal compare instruction for == and !=
; Note: This calls the setcy() macro and depends on the temp reg
;define(`compares', `_compares_rr($1, $2)')
define(`compares', `ifelse(isconst($2),1,`_compares_rk($1, eval(pb2m4(_cname_$2)))',dnl
isnum($2),1,`_compares_rk($1, $2)',`_compares_rr($1, $2)')')


define(`_compares_rr', `xor $1, 80 ; Signed compare $1, $2
xor $2, 80
compare $1, $2
iflt(`xor $1, 80
xor $2, 80
setcy',`xor $1, 80
xor $2, 80')')

define(`_compares_rk', `pushdef(`_kx',`eval(($2 & 0xFF) ^ 0x80)')'`xor $1, 80 ; Signed compare $1, $2
compare $1, eval(_kx, 16, 2)
iflt(`xor $1, 80
setcy',`xor $1, 80')'`popdef(`_kx')')


;---------------------------------
; Multiply 8 x 8 subroutine
; Arg1: Subroutine name
; Arg2: Multiplicand
; Arg3: Multiplier
; Arg4, Arg5: Result MSB, LSB
; Arg6: Optional preamble code block. Also supresses return statement if present
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it.
; Ex: multiply8x8(mul8, s0, s1, s3, s2) ; (s3, s2) = s0 * s1
;     load s0, 04
;     load s1, 05
;     call mul8
define(`multiply8x8', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
            $1:  ; ($4, $5) = $2 * $3
            $6
            vars(`$2 is _cand', `$3 is _plier', `$4 is _msb := 0', `$5 is _lsb := 0', `_tempreg is _mask := 1')
$1_loop:    test _plier, _mask
            jump z, $1_no_add
            add _msb, _cand
$1_no_add:  sra _msb
            sra _lsb
            sl0 _mask
            jump nz, $1_loop ifelse(`$6',,`
            return 
            ; PRAGMA function end')')


;---------------------------------
; Signed Multiply 8 x 8 subroutine
; Same arguments as multiply8x8
define(`multiply8x8s', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
            $1:  ; ($4, $5) = $2 * $3 (signed)
            $6
            vars(`$2 is _cand', `$3 is _plier', `$4 is _msb := 0', `$5 is _lsb := 0', `_tempreg is _mask := 1')
$1_loop:    test _plier, _mask
            jump z, $1_no_add
            add _msb, _cand
$1_no_add:  srx _msb
            sra _lsb
            sl0 _mask
            jump nz, $1_loop
            test _plier, 80 ; Add correction for negative multiplier
            jump z, $1_no_correct
            sub  _msb, _cand
$1_no_correct: ifelse(`$6',,`
            return
            ; PRAGMA function end')')

;---------------------------------
; SignedxUnsigned Multiply 8 x 8 subroutine
; Same arguments as multiply8x8
define(`multiply8x8su', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
            $1:  ; ($4, $5) = $2 * $3 (signed)
            $6
            vars(`$2 is _cand', `$3 is _plier', `$4 is _msb := 0', `$5 is _lsb := 0', `_tempreg is _mask := 1')
$1_loop:    test _plier, _mask
            jump z, $1_no_add
            add _msb, _cand
$1_no_add:  srx _msb
            sra _lsb
            sl0 _mask
            jump nz, $1_loop
$1_no_correct: ifelse(`$6',,`
            return
            ; PRAGMA function end')')


;---------------------------------
; Divide 8 / 8 subroutine. Implements truncating division
; Arg1: Subroutine name
; Arg2: Dividend
; Arg3: Divisor
; Arg4: Quotient
; Arg5: Remainder
; Arg6: Optional preamble code block. Also supresses return statement if present
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it.
; Ex: divide8x8(div8, s0, s1, s2, s3)
;     load s0, 20'd
;     load s1, 4'd
;     call div8
define(`divide8x8', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
            $1: ; $4 = ($2 / $3)  remainder $5
            $6
            vars(`$2 is _dend', `$3 is _visor', `$4 is _quo := 0', `$5 is _rem := 0', `_tempreg is _mask := 0x80')
$1_loop:    test _dend, _mask
            sla _rem
            sl0 _quo
            compare _rem, _visor
            jump c, $1_no_sub
            sub _rem, _visor
            add _quo, 01
$1_no_sub:  sr0 _mask
            jump nz, $1_loop ifelse(`$6',,`
            return
            ; PRAGMA function end')')

;---------------------------------
; Signed Divide 8 x 8 subroutine
; Same arguments as divide8x8
define(`divide8x8s', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
            $1: ; $4 = ($2 / $3)  remainder $5
            $6
            vars(`$2 is _dend', `$3 is _visor', `$4 is _quo := 0', `$5 is _rem := 0', `_tempreg is _mask')
            ; Make dividend and divisor positive
            load _tempreg, _dend
            xor _tempreg, _visor
            and _tempreg, 80
            if(_dend & 0x80,`negate(_dend)
              or _tempreg, 01')
            if(_visor & 0x80, `negate(_visor)')
            ; Save the sign info
            push(_tempreg)
            load _tempreg, 80
$1_loop:    test _dend, _tempreg
            sla _rem
            sl0 _quo
            compare _rem, _visor
            jump c, $1_no_sub
            sub _rem, _visor
            add _quo, 01
$1_no_sub:  sr0 _tempreg
            jump nz, $1_loop
            pop(_tempreg)
            ; Fix signs
            if(_tempreg & 0x80, `negate(_quo)')
            if(_tempreg & 0x01, `negate(_rem)') ifelse(`$6',,`
            return
            ; PRAGMA function end')')


;---------------------------------
; Divide 16 / 8 subroutine. Implements truncating division
; Arg1:       Subroutine name
; Arg2, Arg3: Dividend MSB,LSB
; Arg4:       Divisor
; Arg5, Arg6: Quotient MSB, LSB
; Arg7:       Remainder
; Arg8:       Optional preamble code block. Also supresses return statement if present
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it.
; Ex: divide16x8(div16, s0,s1, s2, s3,s4, s5)
;     load s0, 20'd
;     load s1, 4'd
;     call div16
define(`divide16x8', `; PRAGMA function $1 [$2, $3, $4 return $5, $6, $7] begin
            $1: ; $5,$6 = ($2,$3 / $4)  remainder $7
            $8
            vars(`$2 is _dend_m', `$3 is _dend_l', `$4 is _visor', `$5 is _quo_m := 0', `$6 is _quo_l := 0',
                `$7 is _rem := 0', `_tempreg is _mask := 0x80')
$1_loop:    test _dend_m, _mask
            sla _rem
            sl0 _quo_l
            compare _rem, _visor
            jump c, $1_no_sub
            sub _rem, _visor
            add _quo_l, 01
$1_no_sub:  sr0 _mask
            jump nz, $1_loop

            load _mask, 80
$1_loop2:   test _dend_l, _mask
            sla _rem
            sl0 _quo_l
            sla _quo_m
            compare _rem, _visor
            jump c, $1_no_sub2
            sub _rem, _visor
            add _quo_l, 01
            addcy _quo_m, 00
$1_no_sub2: sr0 _mask
            jump nz, $1_loop2 ifelse(`$8',,`
            return
            ; PRAGMA function end')')

;---------------------------------
; Signed divide 16 / 8 subroutine. Implements truncating division
; Arg1:       Subroutine name
; Arg2, Arg3: Dividend MSB,LSB
; Arg4:       Divisor
; Arg5, Arg6: Quotient MSB, LSB
; Arg7:       Remainder
; Arg8:       Optional preamble code block. Also supresses return statement if present
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it.
; Ex: divide16x8(div16, s0,s1, s2, s3,s4, s5)
;     load s0, 20'd
;     load s1, 4'd
;     call div16
define(`divide16x8s', `; PRAGMA function $1 [$2, $3, $4 return $5, $6, $7] begin
            $1: ; $5,$6 = ($2,$3 / $4)  remainder $7
            $8
            vars(`$2 is _dend_m', `$3 is _dend_l', `$4 is _visor', `$5 is _quo_m := 0', `$6 is _quo_l := 0',
                `$7 is _rem := 0', `_tempreg is _mask')

            ; Make dividend and divisor positive
            load _tempreg, _dend_m
            xor _tempreg, _visor
            and _tempreg, 80
            if(_dend_m & 0x80,`negate16(_dend_m, _dend_l)
              or _tempreg, 01')
            if(_visor & 0x80, `negate(_visor)')
            ; Save the sign info
            push(_tempreg)
            load _mask, 80
$1_loop:    test _dend_m, _mask
            sla _rem
            sl0 _quo_l
            compare _rem, _visor
            jump c, $1_no_sub
            sub _rem, _visor
            add _quo_l, 01
$1_no_sub:  sr0 _mask
            jump nz, $1_loop

            load _mask, 80
$1_loop2:   test _dend_l, _mask
            sla _rem
            sl0 _quo_l
            sla _quo_m
            compare _rem, _visor
            jump c, $1_no_sub2
            sub _rem, _visor
            add _quo_l, 01
            addcy _quo_m, 00
$1_no_sub2: sr0 _mask
            jump nz, $1_loop2
            pop(_tempreg)
            ; Fix signs
            if(_tempreg & 0x80, `negate16(_quo_m, _quo_l)')
            if(_tempreg & 0x01, `negate(_rem)') ifelse(`$8',,`
            return
            ; PRAGMA function end')')


;---------------------------------
; Multiply 8 x constant subroutine with 16-bit result
; Arg1: Subroutine name
; Arg2: Multiplicand
; Arg3: Constant multiplier (can be wider than 8-bits)
; Arg4, Arg5: Result MSB, LSB
; Ex: multiply8xk(mul8k5, s0, 5, s5, s4)  ; (s5, s4) = s0 * 5
;     load s0, 7'd
;     call mul8k5
define(`multiply8xk', `; PRAGMA function $1 [$2 return $4, $5] begin
$1:  ; ($4, $5) = $2 * ($3)
load $4, 00
load $5, 00
_genmul8xk($2, eval($3,2), $4, $5)return
; PRAGMA function end')

define(`_genmul8xk', `ifelse($2,,,`ifelse(eval(substr($2,0,1) == 1),1,`add $4, $1
addcy $3, 00',`dnl')
ifelse(eval(len(`$2')>1),1,`sl0 $4
sla $3')
$0($1, substr(`$2', 1), $3, $4)')')

;---------------------------------
; Multiply 8 x constant subroutine with 8-bit result
; Arg1: Subroutine name
; Arg2: Multiplicand
; Arg3: Constant multiplier
; Arg4: Result byte
; Ex: multiply8xk_small(mul8k5, s0, 5, s4)  ; s4 = s0 * 5
;     load s0, 7'd
;     call mul8k5
define(`multiply8xk_small', `; PRAGMA function $1 [$2 return $4] begin
$1: ; $4 = $2 * ($3)
load $4, 00
_genmul8xk_small($2, eval($3,2), $4)return
; PRAGMA function end')

define(`_genmul8xk_small', `ifelse($2,,,`ifelse(eval(substr($2,0,1) == 1),1,`add $3, $1',`dnl')
ifelse(eval(len(`$2')>1),1,`sl0 $3  ; $2')
$0($1, substr(`$2', 1), $3)')')


;---------------------------------
; Divide 8 / constant subroutine with 8-bit result
; Arg1: Subroutine name
; Arg2: Dividend
; Arg3: Constant divisor (can be wider than 8-bits)
; Arg4: Result quotient
; Ex: divide8xk(div8k5, s0, 5, s4)  ; s4 = s0 / 5
;     load s0, 25'd
;     call div8k5
define(`divide8xk', `; PRAGMA function $1 [$2 return $4, $5] begin
$1:  ; $4 = $2 / ($3)
load $4, 00
load _tempreg, 00
_genmul8xk($2, eval(2**8 / ($3) + 1,2), $4, _tempreg) return
; PRAGMA function end')



;=============== EXPRESSIONS ===============

;---------------------------------
; Expression evaluators
; This is a family of macros that provide implementation of arithmetic expressions from
; compact input notation
; Arg1: Register assignment expression of the form:
;       sN := <val> op <val> [op <val>]*
;       val is one of:
;         register
;         literal expression (with no internal spaces)
;         sp[addr] scratchpad adddress
;         spi[reg] indirect scratchpad address in register
;       op is one of:
;         + - * /      add, subtract, multiply, divide
;         & | ^        and, or, xor
;         << >>        shift left, shift right (0-filled MSB)
;         =:           reverse assignment to register or scratchpad
;   ** Operations are evaluated left to right with *no precedence*
; Ex: expr(s0 := s1 + s2 - s3 >> 4 =: sp[M_value])
;       Arithmetic is performed on s0 and the result is stored in scratchpad at M_value
;       s0 <= s1, s0 <= s0 + s2, s0 <= s0 - s3, s0 <= s0 >> 4, sp[M_value] <= s0
;
;     expr(s1 := s4 + (28*4-1))
;       s1 <= s4, s1 <= s1 + 111   Constant expressions must have no spaces
;
; ### Summary of expression macros ###
;         target x operand   Supported operators
; expr    8x8                +, -, *, /, &, |, ^, <<, >>, =:
; exprs   8x8                +, -, *, /, &, |, ^, <<, >>, =:  (signed *, /, and >>)
; expr2   16x8 *             +, -, *, /, <<, >>, =:
; expr2s  16x8 *             +, -, *, /, <<, >>, =:           (signed for all except <<)
; expr16  16x16              +, -, &, |, ^, <<, >>, =:
; expr16s 16x16              +, -, &, |, ^, <<, >>, =:        (signed >>)
;
;   * The expr2 macros support 16-bit literals as operands of + and -
;
; For multiplication and division support you must initialize the internal functions with
; one of the following:
;
;          Multiply                           Divide
; expr     use_expr_mul                       use_expr_div
; exprs    use_expr_muls                      use_expr_divs
; expr2    use_expr_mul                       use_expr_div16
; expr2s   use_expr_muls and use_expr_mulsu   use_expr_div16s
;
; As an expedient you can invoke "use_expr_all" to include all of them and then eliminate any
; unused mul or div routines with the --remove-dead-code option to opbasm.
;
; These macros need to be called before any call to expr*() that uses multiplication or division.
; It is best to place them at the start of the program and jump over them to reach the startup code.
; The stack must be configured (use_stack(...)) before calling these macros because additional
; modified registers must be saved and restored.
;
; By default these macros configure the mul and div functions to use the s8,s9 or s7,s8, and s9
; registers for input and output. You can modify the register allocation by passing arguments
; to the use_* macros. The MSB of multiplication is ignored by subsequent operations. Division
; by 0 is not detected.
define(`expr', `pushdef(`_exstr', $1)'`_expr_start(u, patsubst($1, ` +', `,'))'`popdef(`_exstr')')
define(`exprs', `pushdef(`_exstr', $1)'`_expr_start(s, patsubst($1, ` +', `,'))'`popdef(`_exstr')')

define(`_expr_start', `pushdef(`_exreg', $2)'
`ifelse($3,:=,,`errmsg(Missing assignment operator in expression)')'dnl
``;' Expression:' _exstr
`ifelse($2,$4,,`load $2, evalx($4,16,2)')'
`ifelse($1,u,`_expr_ops(shift(shift(shift(shift($@)))))',`_exprs_ops(shift(shift(shift(shift($@)))))')'
`popdef(`_exreg')')

; Unsigned operations
define(`_expr_ops', `ifelse(`$1',,,`_expr_binary($1, evalx($2))
$0(shift(shift($@)))')')

define(`_expr_binary', `ifelse($1,>>,`sr0(_exreg, $2)', $1,*,`_expr_mul8(_exreg, evalx($2,16,2))',dnl
$1,/,`_expr_div8(_exreg, evalx($2,16,2))', `_expr_binary_common($1,$2)')')

define(`_expr_binary_common', `ifelse($1,+,`add _exreg, evalx($2,16,2)', $1,-,`sub _exreg, evalx($2,16,2)',
$1,&,`and _exreg, evalx($2,16,2)', $1,|,`or _exreg, evalx($2,16,2)', $1,^,`xor _exreg, evalx($2,16,2)',
$1,<<,`sl0(_exreg, $2)',
$1,=:,`ifelse(index($2, `sp['),0,`store _exreg, evalx(regexp($2, `sp\[\(.*\)\]',`\1'),16,2)',dnl
index($2, `spi['),0,`store _exreg, (evalx(regexp($2, `spi\[\(.*\)\]',`\1'),16,2))',dnl
`load $2, _exreg')',dnl
`errmsg(`Invalid operation: $1')'   )')

; Signed operations
define(`_exprs_ops', `ifelse(`$1',,,`_exprs_binary($1, evalx($2))
$0(shift(shift($@)))')')

define(`_exprs_binary', `ifelse($1,>>,`srx(_exreg, $2)', $1,*,`_expr_mul8s(_exreg, evalx($2,16,2))',dnl
$1,/,`_expr_div8s(_exreg, evalx($2,16,2))', `_expr_binary_common($1,$2)')')

;------------------------------------------------

define(`expr2', `pushdef(`_exstr', `$@')'`_expr2_start(u, patsubst(_encode16($@), ` +', `,'))'`popdef(`_exstr')')
define(`expr2s', `pushdef(`_exstr', `$@')'`_expr2_start(s, patsubst(_encode16($@), ` +', `,'))'`popdef(`_exstr')')

define(`_expr2_start', `_expect16($2)'`pushdef(`_exreg',`_rmsb($2),_rlsb($2)')'
`ifelse($3,:=,,`errmsg(Missing assignment operator in expression)')'dnl
``;' Expression 16x8:' _exstr
`ifelse($2,$4,,`ifelse(_is16($4),1,`load16(_exreg, _decode16($4))', isnum($4),1,`load16(_exreg, $4)',dnl
                      `ifelse($1,u,`load regupper(_exreg), 00
load reglower(_exreg), $4',`signex(_tempreg, $4)
                       load16(_exreg, _tempreg, $4)')'dnl
)')'
`ifelse($1,u,`_expr2_ops(shift(shift(shift(shift($@)))))',`_expr2s_ops(shift(shift(shift(shift($@)))))')'
`popdef(`_exreg')')

; Unsigned operations
define(`_expr2_ops', `ifelse(`$1',,,`_expr2_binary($1, evalx($2))
$0(shift(shift($@)))')')

define(`_expr2_binary', `ifelse($1,+,`ifelse(isnum($2),1,`add16(_exreg, $2)',dnl
`add reglower(_exreg), evalx($2,16,2)
addcy regupper(_exreg), 00')',dnl
$1,-,`ifelse(isnum($2),1,`sub16(_exreg, $2)',dnl
`sub reglower(_exreg), evalx($2,16,2)
subcy regupper(_exreg), 00')',dnl
$1,/,`_expr2_div8(_exreg, evalx($2,16,2))', $1,*,`_expr2_mul8(_exreg, evalx($2,16,2))',dnl
$1,>>,`sr0_16(_exreg, $2)', $1,<<,`sl0_16(_exreg, $2)',dnl
$1,=:,`ifelse(index($2, `sp['),0,`store16(_exreg, _decode16(regexp($2, `sp\[\(.*\)\]',`\1')))',dnl
index($2, `spi['),0,`store16(_exreg, _decode16(regexp($2, `spi\[\(.*\)\]',`\1')))',dnl
`load16(_decode16($2), _exreg)')',dnl
`errmsg(`Invalid operation: $1')'   )')

; Signed operations
define(`_expr2s_ops', `ifelse(`$1',,,`_expr2s_binary($1, evalx($2))
$0(shift(shift($@)))')')

define(`_expr2s_binary', `ifelse($1,+,`ifelse(isnum($2),1,`add16(_exreg, $2)',dnl
`signex(_tempreg, $2)
add reglower(_exreg), evalx($2,16,2)
addcy regupper(_exreg), _tempreg')',dnl
$1,-,`ifelse(isnum($2),1,`sub16(_exreg, $2)',dnl
`signex(_tempreg, $2)
sub reglower(_exreg), evalx($2,16,2)
subcy regupper(_exreg), 00')',dnl
$1,/,`_expr2_div8s(_exreg, evalx($2,16,2))', $1,*,`_expr2_mul8s(_exreg, evalx($2,16,2))',dnl
$1,>>,`srx_16(_exreg, $2)', $1,<<,`sl0_16(_exreg, $2)',dnl
$1,=:,`ifelse(index($2, `sp['),0,`store16(_exreg, _decode16(regexp($2, `sp\[\(.*\)\]',`\1')))',dnl
index($2, `spi['),0,`store16(_exreg, _decode16(regexp($2, `spi\[\(.*\)\]',`\1')))',dnl
`load16(_decode16($2), _exreg)')',dnl
`errmsg(`Invalid operation: $1')'   )')


;------------------------------------------------

define(`expr16', `pushdef(`_exstr', `$@')'`_expr16_start(u, patsubst(_encode16($@), ` +', `,'))'`popdef(`_exstr')')
define(`expr16s', `pushdef(`_exstr', `$@')'`_expr16_start(s, patsubst(_encode16($@), ` +', `,'))'`popdef(`_exstr')')

; Utility macros to manipulate 16-bit register pairs
define(`_encode16', `ifelse(eval($#>1),1,`$1!$0(shift($@))',`$1')')
define(`_decode16', `translit($1, !, `,')')
define(`_rmsb',`regupper(_decode16($1))')
;define(`__rmsb', `$1')
define(`_rlsb',`reglower(_decode16($1))')
;define(`__rlsb', `$2')
define(`_is16',`eval(index($1,!)>= 0)')
define(`_expect16', `ifelse(_is16($1),0,`errmsg(Expecting 16-bit register pair: $1)')')


define(`_expr16_start', `_expect16($2)'`pushdef(`_exreg',`_rmsb($2),_rlsb($2)')'
`ifelse($3,:=,,`errmsg(Missing assignment operator in expression)')'dnl
``;' Expression 16x16:' _exstr
`ifelse($2,$4,,`load16(_exreg, _decode16($4))')'
`ifelse($1,u,`_expr16_ops(shift(shift(shift(shift($@)))))',`_expr16s_ops(shift(shift(shift(shift($@)))))')'
`popdef(`_exreg')')

; Unsigned operations
define(`_expr16_ops', `ifelse(`$1',,,`_expr16_binary($1, $2)
$0(shift(shift($@)))')')

define(`_expr16_binary', `ifelse($1,>>,`sr0_16(_exreg, $2)',dnl
`_expr16_binary_common($1,$2)')')

define(`_expr16_binary_common', `ifelse($1,+,`add16(_exreg, _decode16($2))', $1,-,`sub16(_exreg, _decode16($2))',
$1,&,`and16(_exreg, _decode16($2))', $1,|,`or16(_exreg, _decode16($2))', $1,^,`xor16(_exreg, _decode16($2))',
$1,<<,`sl0_16(_exreg, _decode16($2))',
$1,=:,`ifelse(index($2, `sp['),0,`store16(_exreg, _decode16(regexp($2, `sp\[\(.*\)\]',`\1')))',dnl
index($2, `spi['),0,`store16(_exreg, _decode16(regexp($2, `spi\[\(.*\)\]',`\1')))',dnl
`load16(_decode16($2), _exreg)')',dnl
`errmsg(`Invalid operation: $1')'   )')

; Signed operations
define(`_expr16s_ops', `ifelse(`$1',,,`_expr16s_binary($1, $2)
$0(shift(shift($@)))')')

define(`_expr16s_binary', `ifelse($1,>>,`srx_16(_exreg, $2)',dnl
`_expr16_binary_common($1,$2)')')

;---------------------------------
; Configure all multiplication and divide subroutines
; Use this in conjunction with Opbasm's dead code removal to guarantee that
; only the arithmetic subroutines in use will be assembled.
define(`use_expr_all', `use_expr_mul
use_expr_muls
use_expr_mulsu
use_expr_div
use_expr_divs
use_expr_div16
use_expr_div16s')


;---------------------------------
; Configure unsigned multiplication for expressions
; All arguments are optional
; Arg1: Multiplicand (default is s8)
; Arg2: Multiplier   (default is s9)
; Arg3, Arg4: Internal result MSB, LSB (default is sa,sb) preserved on stack
; The result is copied to Arg1, Arg2
define(`use_expr_mul', `define(`_mul_init',1)'dnl
 `ifelse($#,0,`multiply8x8(`expr_mul8', s8, s9, sa, sb,`push(sa,sb)')
  define(`_mul8_msb',`s8') define(`_mul8_lsb',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`multiply8x8(`expr_mul8', $1, $2, $3, $4,`push($3,$4)')
  define(`_mul8_msb',`$1') define(`_mul8_lsb',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 multiply keeping only lower 8-bits of result
; Arg1: Multiplicand, Arg2: Multiplier
define(`_expr_mul8', `_initcheck(`_mul_init',`Unsigned multiply `not' initialized. Call `use_expr_mul()'')'dnl
`load _mul8_msb, $1
load _mul8_lsb, $2
call expr_mul8
load $1, _mul8_lsb
')

; 16x8 multiply keeping only lower 16-bits of result
; Arg1-Arg2: MSB, LSB Multiplicand, Arg3: Multiplier
define(`_expr2_mul8', `_initcheck(`_mul_init',`Unsigned multiply `not' initialized. Call `use_expr_mul()'')'dnl
`load _mul8_msb, $2
load _mul8_lsb, $3
call expr_mul8
load $2, _mul8_lsb
swap($1, _mul8_msb)
load _mul8_lsb, $3
call expr_mul8
add $1, _mul8_lsb
')


;---------------------------------
; Configure signed multiplication for expressions
; All arguments are optional
; Arg1: Multiplicand (default is s8)
; Arg2: Multiplier   (default is s9)
; Arg3, Arg4: Internal result MSB, LSB (default is sa,sb) preserved on stack
; The result is copied to Arg1, Arg2
define(`use_expr_muls', `define(`_muls_init',1)'dnl
 `ifelse($#,0,`multiply8x8s(`expr_mul8s', s8, s9, sa, sb,`push(sa,sb)')
  define(`_mul8s_msb',`s8') define(`_mul8s_lsb',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`multiply8x8s(`expr_mul8s', $1, $2, $3, $4,`push($3,$4)')
  define(`_mul8s_msb',`$1') define(`_mul8s_lsb',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 signed multiply keeping only lower 8-bits of result
; Arg1: Multiplicand, Arg2: Multiplier
define(`_expr_mul8s', `_initcheck(`_muls_init',`Signed multiply `not' initialized. Call `use_expr_muls()'')'dnl
`load _mul8s_msb, $1
load _mul8s_lsb, $2
call expr_mul8s
load $1, _mul8s_lsb
')

;---------------------------------
; Configure signedxunsigned (16x8) multiplication for expressions
; All arguments are optional
; Arg1: Multiplicand (default is s8)
; Arg2: Multiplier   (default is s9)
; Arg3, Arg4: Internal result MSB, LSB (default is sa,sb) preserved on stack
; The result is copied to Arg1, Arg2
define(`use_expr_mulsu', `define(`_mulsu_init',1)'dnl
 `ifelse($#,0,`multiply8x8su(`expr_mul8su', s8, s9, sa, sb,`push(sa,sb)')
  define(`_mul8su_msb',`s8') define(`_mul8su_lsb',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`multiply8x8su(`expr_mul8su', $1, $2, $3, $4,`push($3,$4)')
  define(`_mul8su_msb',`$1') define(`_mul8su_lsb',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')


; 16x8 signed multiply keeping only lower 16-bits of result
; Arg1-Arg2: MSB, LSB Multiplicand, Arg3: Multiplier
define(`_expr2_mul8s', `_initcheck(`_muls_init',`Unsigned multiply `not' initialized. Call `use_expr_muls()'')'dnl
`_initcheck(`_mulsu_init',`Signed x Unsigned multiply `not' initialized. Call `use_expr_mulsu()'')'dnl
`load _mul8su_msb, $3
load _mul8su_lsb, $2
call expr_mul8su
load $2, _mul8su_lsb
swap($1, _mul8s_msb)
load _mul8s_lsb, $3
call expr_mul8s
add $1, _mul8s_lsb
')


;---------------------------------
; Configure unsigned division for expressions
; All arguments are optional
; Arg1: Dividend (default is s8)
; Arg2: Divisor  (default is s9)
; Arg3, Arg4: Internal result Quotient, Remainder (default is sa,sb) preserved on stack
; The result is copied to Arg1, Arg2
define(`use_expr_div', `define(`_div_init',1)'dnl
 `ifelse($#,0,`divide8x8(`expr_div8', s8, s9, sa, sb,`push(sa,sb)')
  define(`_div8_quo',`s8') define(`_div8_rem',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`divide8x8(`expr_div8', $1, $2, $3, $4,`push($3,$4)')
  define(`_div8_quo',`$1') define(`_div8_rem',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 divide keeping only quotient
; Arg1: Dividend, Arg2: Divisor
define(`_expr_div8', `_initcheck(`_div_init',`Unsigned divide `not' initialized. Call `use_expr_div()'')'dnl
`load _div8_quo, $1
load _div8_rem, $2
call expr_div8
load $1, _div8_quo
')

;---------------------------------
; Configure signed division for expressions
; All arguments are optional
; Arg1: Dividend (default is s8)
; Arg2: Divisor  (default is s9)
; Arg3, Arg4: Internal result Quotient, Remainder (default is sa,sb) preserved on stack
; The result is copied to Arg1, Arg2
define(`use_expr_divs', `define(`_divs_init',1)'dnl
 `ifelse($#,0,`divide8x8s(`expr_div8s', s8, s9, sa, sb,`push(sa,sb)')
  define(`_div8s_quo',`s8') define(`_div8s_rem',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`divide8x8s(`expr_div8s', $1, $2, $3, $4,`push($3,$4)')
  define(`_div8s_quo',`$1') define(`_div8s_rem',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 divide keeping only quotient
; Arg1: Dividend, Arg2: Divisor
define(`_expr_div8s', `_initcheck(`_divs_init',`Signed divide `not' initialized. Call `use_expr_divs()'')'dnl
`load _div8s_quo, $1
load _div8s_rem, $2
call expr_div8s
load $1, _div8s_quo
')



;---------------------------------
; Configure unsigned 16x8 division for expressions
; All arguments are optional
; Arg1,Arg2: Dividend (default is s7,s8)
; Arg3:      Divisor  (default is s9)
; Arg4,Arg5, Arg6: Internal result Quotient, Remainder (default is sa,sb, sc) preserved on stack
; The result is copied to Arg1,Arg2, Arg3
define(`use_expr_div16', `define(`_div16_init',1)'dnl
 `ifelse($#,0,`divide16x8(`expr_div16', s7,s8, s9, sa,sb, sc, `push(sa,sb, sc)')
  define(`_div16_quo',`s7,s8') define(`_div16_rem',`s9')dnl
  load s7, sa
  load s8, sb
  load s9, sc
  pop(sa,sb, sc)
  return
  ; PRAGMA function end',`divide16x8(`expr_div16', $1,$2, $3, $4,$5, $6, `push($4,$5, $6)')
  define(`_div16_quo',`$1,$2') define(`_div16_rem',`$3')dnl
  load $1, $4
  load $2, $5
  load $3, $6
  pop($4,$5, $6)
  return
  ; PRAGMA function end')')

; 16x8 divide keeping only quotient
; Arg1,Arg2: Dividend, Arg3: Divisor
define(`_expr2_div8', `_initcheck(`_div16_init',`Unsigned 16x8 divide `not' initialized. Call `use_expr_div16()'')'dnl
`load16(_div16_quo, $1,$2)
load _div16_rem, $3
call expr_div16
load16($1,$2, _div16_quo)
')


;---------------------------------
; Configure signed 16x8 division for expressions
; All arguments are optional
; Arg1,Arg2: Dividend (default is s7,s8)
; Arg3:      Divisor  (default is s9)
; Arg4,Arg5, Arg6: Internal result Quotient, Remainder (default is sa,sb, sc) preserved on stack
; The result is copied to Arg1,Arg2, Arg3
define(`use_expr_div16s', `define(`_div16s_init',1)'dnl
 `ifelse($#,0,`divide16x8s(`expr_div16s', s7,s8, s9, sa,sb, sc, `push(sa,sb, sc)')
  define(`_div16s_quo',`s7,s8') define(`_div16s_rem',`s9')dnl
  load s7, sa
  load s8, sb
  load s9, sc
  pop(sa,sb, sc)
  return
  ; PRAGMA function end',`divide16x8s(`expr_div16s', $1,$2, $3, $4,$5, $6, `push($4,$5, $6)')
  define(`_div16s_quo',`$1,$2') define(`_div16s_rem',`$3')dnl
  load $1, $4
  load $2, $5
  load $3, $6
  pop($4,$5, $6)
  return
  ; PRAGMA function end')')

; 16x8 signed divide keeping only quotient
; Arg1,Arg2: Dividend, Arg3: Divisor
define(`_expr2_div8s', `_initcheck(`_div16s_init',`Signed 16x8 divide `not' initialized. Call `use_expr_div16s()'')'dnl
`load16(_div16s_quo, $1,$2)
load _div16s_rem, $3
call expr_div16s
load16($1,$2, _div16s_quo)
')


;=============== 16-bit ARITHMETIC AND LOGICAL OPERATIONS ===============

;---------------------------------
; Create a virtual 16-bit register
; The defined name can be used in place of the MSB, LSB pairs used in other 16-bit macros
; Arg1: Name of virtual register
; Arg2: MSB register
; Arg3: LSB register
; Ex: reg16(rx, s1, s0) ; rx = (s1, s0)
;     reg16(ry, s5, s4) ; ry = (s5, s4)
;     add16(rx, ry)     ; rx = rx + ry
;     add16(rx, s3, s2) ; rx = rx + (s3, s2)
define(`reg16', `ifelse($#,3,`define(`$1', `$2, $3')',`errmsg(`Wrong number of arguments to `reg16'. Is name quoted?')')')

;---------------------------------
; Create a constant for 16-bit memory and port addresses
; Named constants with "_H" and "_L" suffixes are created and usable for
; byte access.
; Arg1: Name of constant
; Arg2: MSB address
; Arg3: LSB address
; Ex: mem16(M_DATA, 0x05, 0x04) ; Allocate scratchpad 05, 04 for use as M_DATA
define(`mem16', `constant $1_H, evalx($2, 16, 2)
constant $1_L, evalx($3, 16, 2)'
`reg16($1, $1_H, $1_L)')

;---------------------------------
; Get the upper and lower registers from a reg16 definition
; Arg1, Arg2: MSB, LSB of 16-bit register
; Ex: reg16(rx, s5, s4)
;     load s1, regupper(rx) ; load upper byte from s5
;     load s1, reglower(rx) ; load lower byte from s4
define(`regupper', $1)
define(`reglower', $2)

; Split a 16-bit constant into upper and lower bytes
define(`constupper', `eval(((const2m4($1)) >> 8) & 0xFF)')
define(`constlower', `eval((const2m4($1)) & 0xFF)')

;---------------------------------
; 16-bit load
; Arg1, Arg2: MSB, LSB destination
; 3 arguments: load constant
;   Arg3: Decimal constant or expression
; 4 arguments: load register
;   Arg3, Arg4: MSB, LSB source
; Ex: load16(s1, s0, 2014)
;     load16(s1, s0, 200 * 11 + 5)
;     load16(s1, s0, s3, s2)
;     load16(rx, ry) ; Assuming rx and ry are reg16 definitions
define(`load16', `ifelse($#,4,`_load16($@)',`_load16k($@)')')

define(`_load16', `load $1, $3
load $2, $4')

define(`_load16k', `load $1, eval(constupper($3), 16, 2)  ; $3
load $2, eval(constlower($3), 16, 2)')

;---------------------------------
; 16-bit addition and subtraction
; Arg1, Arg2: MSB1, LSB1
; 3 arguments: add from constant
;   Arg3: Decimal constant or expression
; 4 arguments: add from register
;   Arg3, Arg4: MSB2, LSB2
; Result in Arg1, Arg2
; Ex: add16(s1,s0, s3,s2)
;     add16(rx, ry)
;     sub16(rx, 2000)
define(`add16', `ifelse($#,4,`_add16($@)',`_add16k($@)')')

define(`_add16', `add $2, $4
addcy $1, $3')

define(`_add16k', `add $2, eval(constlower($3), 16, 2)  ; $3
addcy $1, eval(constupper($3), 16, 2)')


define(`sub16', `ifelse($#,4,`_sub16($@)',`_sub16k($@)')')

define(`_sub16', `sub $2, $4
subcy $1, $3')

define(`_sub16k', `sub $2, eval(constlower($3), 16, 2)  ; $3
subcy $1, eval(constupper($3), 16, 2)')

;---------------------------------
; 16-bit 2s complement negation
; Arg1, Arg2: MSB, LSB to negate
; Result in Arg1, Arg2
; Ex: negate16(s1, s0)
define(`negate16', `xor $1, FF  ; Negate 16-bit
xor $2, FF
add $2, 01
addcy $1, 00')

;---------------------------------
; 16-bit logical not
; Arg1, Arg2: MSB, LSB to invert
; Result in Arg1, Arg2
; Ex: not16(s1, s0)
define(`not16', `xor $1, FF  ; Not 16-bit
xor $2, FF')

;---------------------------------
; 16-bit absolute value
; Arg1, Arg2: MSB, LSB to make positive
; Result is in Arg1, Arg2
; Ex: abs16(s1, s0)
define(`abs16', `if($1 & 0x80, `negate16($1, $2)')')


;---------------------------------
; 16-bit and
; Arg1, Arg2: MSB1, LSB1
; 3 arguments: and with constant
;   Arg3: Decimal constant or expression
; 4 arguments: and with register
;   Arg3, Arg4: MSB2, LSB2
; Result in Arg1, Arg2
define(`and16', `ifelse($#,4,`_and16($@)',`_and16k($@)')')

define(`_and16', `and $2, $4
and $1, $3')

define(`_and16k', `and $2, eval(constlower($3), 16, 2)  ; $3
and $1, eval(constupper($3), 16, 2)')

;---------------------------------
; 16-bit or
; Arg1, Arg2: MSB1, LSB1
; 3 arguments: or with constant
;   Arg3: Decimal constant or expression
; 4 arguments: or with register
;   Arg3, Arg4: MSB2, LSB2
; Result in Arg1, Arg2
define(`or16', `ifelse($#,4,`_or16($@)',`_or16k($@)')')

define(`_or16', `or $2, $4
or $1, $3')

define(`_or16k', `or $2, eval(constlower($3), 16, 2)  ; $3
or $1, eval(constupper($3), 16, 2)')

;---------------------------------
; 16-bit xor
; Arg1, Arg2: MSB1, LSB1
; 3 arguments: xor with constant
;   Arg3: Decimal constant or expression
; 4 arguments: xor with register
;   Arg3, Arg4: MSB2, LSB2
; Result in Arg1, Arg2
define(`xor16', `ifelse($#,4,`_xor16($@)',`_xor16k($@)')')

define(`_xor16', `xor $2, $4
xor $1, $3')

define(`_xor16k', `xor $2, eval(constlower($3), 16, 2)  ; $3
xor $1, eval(constupper($3), 16, 2)')


;---------------------------------
; 16-bit test
; Arg1, Arg2: MSB1, LSB1
; 3 arguments: test with constant
;   Arg3: Decimal constant or expression
; 4 arguments: test with register
;   Arg3, Arg4: MSB2, LSB2
; NOTE: On Picoblaze-3 only the Z flag is set properly
ifdef(`PB3', `define(`test16', `ifelse($#,4,`_test16pb3($@)',`_test16kpb3($@)')')',
`define(`test16', `ifelse($#,4,`_test16($@)',`_test16k($@)')')')

define(`_test16', `test $2, $4
testcy $1, $3')

define(`_test16k', `test $2, eval(constlower($3), 16, 2)  ; $3
testcy $1, eval(constupper($3), 16, 2)')


define(`_test16pb3', `pushdef(`_tnz', uniqlabel(NZ_))'`test $2, $4
jump nz, _tnz
test $1, $3
_tnz:'`popdef(`_tnz')')

define(`_test16kpb3', `pushdef(`_tnz', uniqlabel(NZ_))'`test $2, eval(constlower($3), 16, 2)  ; $3
jump nz, _tnz
test $1, eval(constupper($3), 16, 2)
_tnz:'`popdef(`_tnz')')


;---------------------------------
; 16-bit comparison
; Arg1, Arg2: MSB1, LSB1
; Arg3, Arg4: MSB2, LSB2
; Note: On Picoblaze-3 only the Z flag is correct
ifdef(`PB3', `define(`compare16', `if($1 == $3, `compare $2, $4')')',
`define(`compare16', `compare $2, $4
comparecy $1, $3')')




;---------------------------------
; 16-bit shifts

define(`_sl1_16', `sl1 $2
sla $1')

define(`_sl0_16', `sl0 $2
sla $1')

define(`_sla_16', `sla $2
sla $1')

define(`_slx_16', `slx $2
sla $1')


define(`_sr1_16', `sr1 $1
sra $2')

define(`_sr0_16', `sr0 $1
sra $2')

define(`_sra_16', `sra $1
sra $2')

define(`_srx_16', `srx $1
sra $2')

define(`sl0_16', `repeat(`_sl0_16($1, $2)', eval(const2m4($3)))')
define(`sl1_16', `repeat(`_sl1_16($1, $2)', eval(const2m4($3)))')
define(`sla_16', `repeat(`_sla_16($1, $2)', eval(const2m4($3)))')
define(`slx_16', `repeat(`_slx_16($1, $2)', eval(const2m4($3)))')
define(`sr0_16', `repeat(`_sr0_16($1, $2)', eval(const2m4($3)))')
define(`sr1_16', `repeat(`_sr1_16($1, $2)', eval(const2m4($3)))')
define(`sra_16', `repeat(`_sra_16($1, $2)', eval(const2m4($3)))')
define(`srx_16', `repeat(`_srx_16($1, $2)', eval(const2m4($3)))')


define(`_rl16', `pushdef(`_rnc',uniqlabel(RL16_))'`sl0 $2
sla $1
jump nc, _rnc
or $2, 01
_rnc:
'`popdef(`_rnc')')

define(`rl16', `repeat(`_rl16($1, $2)', eval(const2m4($3)))')

define(`_rr16', `pushdef(`_rnc',uniqlabel(RR16_))'`sr0 $1
sra $2
jump nc, _rnc
or $1, 80
_rnc:
'`popdef(`_rnc')')

define(`rr16', `repeat(`_rr16($1, $2)', eval(const2m4($3)))')


;=============== 16-bit I/O OPERATIONS ===============

;---------------------------------
; 16-bit fetch
; Arg1, Arg2: MSB, LSB of target
; 3 arguments: Fetch from indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of fetch16()
; 4 arguments: MSB, LSB addresses to fetch from
;   Arg3, Arg4: MSB, LSB of source
; Result in Arg1, Arg2
; Ex: constant M_ACCUM_L, 1a
;     constant M_ACCUM_H, 1b
;     reg16(M_ACCUM, M_ACCUM_H, M_ACCUM_L)
;     reg16(rx, s4, s3)
;     fetch16(rx, M_ACCUM)  ; Fetch direct from address
;     load s0, M_ACCUM_L
;     fetch16(rx, s0)       ; Fetch from indirect pointer
;     fetch16(rx, s0)       ; Fetch next word
define(`fetch16', `ifelse($#,4,`_fetch16($@)',`_fetch16i($@)')')

define(`_fetch16', `fetch $2, $4
fetch $1, $3')

define(`_fetch16i', `fetch $2, ($3)
add $3, 01
fetch $1, ($3)
add $3, 01')


;---------------------------------
; 16-bit store
; Arg1, Arg2: MSB, LSB of source
; 3 arguments: Store to indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of store16()
; 4 arguments: MSB, LSB addresses to store to
;   Arg3, Arg4: MSB, LSB of target (LSB sent first)
; Ex: load16(rx, 2014)
;     store16(rx, M_ACCUM)  ; Store direct to address
;     load s0, M_ACCUM_L
;     store16(rx, s0)       ; Store to indirect pointer
;     store16(rx, s0)       ; Store next word
define(`store16', `ifelse($#,4,`_store16($@)',`_store16i($@)')')

define(`_store16', `store $2, $4
store $1, $3')

define(`_store16i', `store $2, ($3)
add $3, 01
store $1, ($3)
add $3, 01')


;---------------------------------
; 16-bit input
; Arg1, Arg2: MSB, LSB of target
; 3 arguments: Input from indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of input16()
; 4 arguments: MSB, LSB port addresses to input from
;   Arg3, Arg4: MSB, LSB of source port
; Result in Arg1, Arg2
; Ex: constant P_ACCUM_L, 1a
;     constant P_ACCUM_H, 1b
;     reg16(P_ACCUM, P_ACCUM_H, P_ACCUM_L)
;     reg16(rx, s4, s3)
;     input16(rx, P_ACCUM)  ; Input direct from address
;     load s0, P_ACCUM_L
;     input16(rx, s0)       ; Input from indirect pointer
;     input16(rx, s0)       ; Input next word
define(`input16', `ifelse($#,4,`_input16($@)',`_input16i($@)')')

define(`_input16', `input $2, $4
input $1, $3')

define(`_input16i', `input $2, ($3)
add $3, 01
input $1, ($3)
add $3, 01')


;---------------------------------
; 16-bit output
; Arg1, Arg2: MSB, LSB of source
; 3 arguments: Output to indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of output16()
; 4 arguments: MSB, LSB port addresses to output to
;   Arg3, Arg4: MSB, LSB of target (LSB sent first)
; Ex: load16(rx, 2014)
;     output16(rx, P_ACCUM)  ; Output direct to port address
;     load s0, P_ACCUM_L
;     output16(rx, s0)       ; Output to indirect pointer
;     output16(rx, s0)       ; Output next word
define(`output16', `ifelse($#,4,`_output16($@)',`_output16i($@)')')

define(`_output16', `output $2, $4
output $1, $3')

define(`_output16i', `output $2, ($3)
add $3, 01
output $1, ($3)
add $3, 01')


;=============== UPPERCASE MACROS ===============

define(`EVALD', `evald($@)')
define(`EVALH', `evalh($@)')
define(`EVALA', `evala($@)')
define(`EVALB', `evalb($@)')
define(`EVALX', `evalx($@)')
define(`PBHEX', `pbhex($@)')
define(`ASCIIORD', `asciiord($@)')
define(`WORDS_LE', `words_le($@)')
define(`WORDS_BE', `words_be($@)')
define(`USE_TEMPREG', `use_tempreg($@)')
define(`NOP', `nop($@)')
define(`SWAP', `swap($@)')
define(`RANDLABEL', `randlabel($@)')
define(`UNIQLABEL', `uniqlabel($@)')
define(`REVERSE', `reverse($@)')
define(`IODEFS', `iodefs($@)')
define(`LOAD_OUT', `load_out($@)')
define(`LOAD_ST', `load_st($@)')
define(`VARS', `vars($@)')
define(`CLEARCY', `clearcy($@)')
define(`SETCY', `setcy($@)')
define(`SETBIT', `setbit($@)')
define(`CLEARBIT', `clearbit($@)')
define(`MASK', `mask($@)')
define(`MASKH', `maskh($@)')
define(`SETMASK', `setmask($@)')
define(`CLEARMASK', `clearmask($@)')
define(`TESTBIT', `testbit($@)')
define(`JNE', `jne($@)')
define(`JEQ', `jeq($@)')
define(`JGE', `jge($@)')
define(`JLT', `jlt($@)')
define(`CALLNE', `callne($@)')
define(`CALLEQ', `calleq($@)')
define(`CALLGE', `callge($@)')
define(`CALLLT', `calllt($@)')
define(`RETNE', `retne($@)')
define(`RETEQ', `reteq($@)')
define(`RETGE', `retge($@)')
define(`RETLT', `retlt($@)')
define(`IF', `if($@)')
define(`SIGNED', `signed($@)')
define(`IFEQ', `ifeq($@)')
define(`IFNE', `ifne($@)')
define(`IFGE', `ifge($@)')
define(`IFLT', `iflt($@)')
define(`ERRMSG', `errmsg($@)')
define(`WARNMSG', `warnmsg($@)')
define(`WHILE', `while($@)')
define(`DOWHILE', `dowhile($@)')
define(`REPEATSTR', `repeatstr($@)')
define(`REPEAT', `repeat($@)')
define(`SL0', `sl0($@)')
define(`SL1', `sl1($@)')
define(`SLA', `sla($@)')
define(`SLX', `slx($@)')
define(`SR0', `sr0($@)')
define(`SR1', `sr1($@)')
define(`SRA', `sra($@)')
define(`SRX', `srx($@)')
define(`RL', `rl($@)')
define(`RR', `rr($@)')
define(`USE_STACK', `use_stack($@)')
define(`PUSH', `push($@)')
define(`POP', `pop($@)')
define(`GETSTACK', `getstack($@)')
define(`GETSTACKAT', `getstackat($@)')
define(`PUTSTACK', `putstack($@)')
define(`PUTSTACKAT', `putstackat($@)')
define(`DROPSTACK', `dropstack($@)')
define(`DROPSTACKREG', `dropstackreg($@)')
define(`ADDSTACK', `addstack($@)')
define(`ADDSTACKREG', `addstackreg($@)')
define(`CALLSTRING', `callstring($@)')
define(`OUTPUTSTRING', `outputstring($@)')
define(`STORESTRING', `storestring($@)')
define(`STORESTRINGAT', `storestringat($@)')
define(`CALLTABLE', `calltable($@)')
define(`OUTPUTTABLE', `outputtable($@)')
define(`STORETABLE', `storetable($@)')
define(`STORETABLEAT', `storetableat($@)')
define(`INSTDATA', `instdata($@)')
define(`INSTTABLE_LE', `insttable_le($@)')
define(`INSTTABLE_BE', `insttable_be($@)')
define(`NEGATE', `negate($@)')
define(`NOT', `not($@)')
define(`ABS', `abs($@)')
define(`SIGNEX', `signex($@)')
define(`ISNUM', `isnum($@)')
define(`COMPARES', `compares($@)')
define(`MULTIPLY8X8', `multiply8x8($@)')
define(`MULTIPLY8X8S', `multiply8x8s($@)')
define(`MULTIPLY8X8SU', `multiply8x8su($@)')
define(`DIVIDE8X8', `divide8x8($@)')
define(`DIVIDE8X8S', `divide8x8s($@)')
define(`DIVIDE16X8', `divide16x8($@)')
define(`DIVIDE16X8S', `divide16x8s($@)')
define(`MULTIPLY8XK', `multiply8xk($@)')
define(`MULTIPLY8XK_SMALL', `multiply8xk_small($@)')
define(`DIVIDE8XK', `divide8xk($@)')
define(`EXPR', `expr($@)')
define(`EXPRS', `exprs($@)')
define(`EXPR2', `expr2($@)')
define(`EXPR2S', `expr2s($@)')
define(`EXPR16', `expr16($@)')
define(`EXPR16S', `expr16s($@)')
define(`USE_EXPR_ALL', `use_expr_all($@)')
define(`USE_EXPR_MUL', `use_expr_mul($@)')
define(`USE_EXPR_MULS', `use_expr_muls($@)')
define(`USE_EXPR_MULSU', `use_expr_mulsu($@)')
define(`USE_EXPR_DIV', `use_expr_div($@)')
define(`USE_EXPR_DIVS', `use_expr_divs($@)')
define(`USE_EXPR_DIV16', `use_expr_div16($@)')
define(`USE_EXPR_DIV16S', `use_expr_div16s($@)')
define(`REG16', `reg16($@)')
define(`MEM16', `mem16($@)')
define(`REGUPPER', `regupper($@)')
define(`REGLOWER', `reglower($@)')
define(`CONSTUPPER', `constupper($@)')
define(`CONSTLOWER', `constlower($@)')
define(`LOAD16', `load16($@)')
define(`ADD16', `add16($@)')
define(`SUB16', `sub16($@)')
define(`NEGATE16', `negate16($@)')
define(`NOT16', `not16($@)')
define(`ABS16', `abs16($@)')
define(`AND16', `and16($@)')
define(`OR16', `or16($@)')
define(`XOR16', `xor16($@)')
define(`SL0_16', `sl0_16($@)')
define(`SL1_16', `sl1_16($@)')
define(`SLA_16', `sla_16($@)')
define(`SLX_16', `slx_16($@)')
define(`SR0_16', `sr0_16($@)')
define(`SR1_16', `sr1_16($@)')
define(`SRA_16', `sra_16($@)')
define(`SRX_16', `srx_16($@)')
define(`RL16', `rl16($@)')
define(`RR16', `rr16($@)')
define(`FETCH16', `fetch16($@)')
define(`STORE16', `store16($@)')
define(`INPUT16', `input16($@)')
define(`OUTPUT16', `output16($@)')
divert(0)dnl

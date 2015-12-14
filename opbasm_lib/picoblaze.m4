divert(-1)
changecom(;)
; m4 macros for enhanced PicoBlaze assembly
; These are included as part of Opbasm and available automatically when processing
; files with .psm4 or .m4 extenstions or when the --m4 option is provided
;
; These can be used manually with any PicoBlaze assembler by running the following:
;   m4 PicoBlaze.m4 [input source] > expanded_macros.gen.psm

; Copyright © 2014, 2015 Kevin Thibedeau
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
;==================================================

;---------------------------------
;Extensions to eval
;Evaluate expression as a decimal number
;Args:
;  Arg1: Expression or constant name
;Result:
;  Expands to a PicoBlaze decimal literal with a comment listing the original expression
define(`evald', `eval(const2m4($1))''d  `;' $1)

; Evaluate expression as an 8-bit hex number
define(`evalh', `eval((const2m4($1)) & 0xFF, 16, 2)  ; $1')

; Evaluate expression as a 12-bit hex number for addresses
define(`evala', `eval(($1) & 0xFFF, 16, 3)  ; $1')

; Evaluate expression as an 8-bit binary number
define(`evalb', `eval((const2m4($1)) & 0xFF, 2, 8)''b  `;' $1)

; Evaluate expression with constant expansion
define(`evalc', `eval(const2m4($1), ifelse($2,,10,$2), ifelse($3,,0,$3))')


; Only evaluate valid expressions, otherwise reproduce the original text in the
; first argument
define(`evalx', `ifelse(eval(regexp(`$1',`^[-+~0-9(]')>=0),1,ifelse($3,,ifelse($2,,`pushdef(`_evalx', `eval($1)')', `pushdef(`_evalx', `eval($1, $2)')'),`pushdef(`_evalx', `eval(($1 & 0x`'repeatstr(F,$3)), $2, $3)')'),`pushdef(`_evalx', $1)')'_evalx`'popdef(`_evalx'))

; Example:
;   constant cname,  evalh(20 + 6)      -->  constant cname,  1a
;     constant cname2, evald(20 * 4 - 1)  -->  constant cname2, 79'd
;     constant cname3, evalh(250 + 6)     -->  constant cname3, 01
;
;     evalx(some_name)  --> some_name
;     evalx(1+3)        --> 4


;=============== TYPE CONVERSION ===============
;===============================================

;---------------------------------
; Convert a list of values in PicoBlaze hex format to decimal
; Args:
;   Arg1-Argn: Hex values to convert
; Example:
;   pbhex(01, 02, 03, 0a, ff)  ; Expands to 1, 2, 3, 10, 255
define(`pbhex', `ifelse(eval($#>1),1,eval(0x$1)`,' `$0(shift($@))',$1,,,`eval(0x$1)')')

;---------------------------------
; Convert a list of decimal values to PicoBlaze hex format
; Args:
;   Arg1-Argn: Decimal values to convert
; Example:
;   dec2pbhex(1, 2, 100, 200)  ; Expands to 01, 02, 64, C8
define(`dec2pbhex', `ifelse(eval($#>1),1,`eval($1 & 0xFF,16,2)'`,' `$0(shift($@))',$1,,,`eval($1 & 0xFF,16,2)')')


;---------------------------------
; Convert a string to a list of decimal ASCII codes
; Args:
;   Arg1: String to convert
: Example:
;   asciiord(`My string')  ; Expands to 77, 121, 32, 115, 116, 114, 105, 110, 103
changequote(<!,!>) ; Change quotes so we can handle "`" and "'"

; NOTE: We have to escape the string argument to avoid unwanted substitutions
define(<!asciiord!>,<!changequote(<!,!>)changecom(-~-)<!!>_asciiord(_esc_words($1))<!!>changecom(;)changequote`'dnl
!>)

; We will use "^" as an escape char so we need to replace literal "^"s with "^1"
define(<!_esc_caret!>, <!patsubst(<!$@!>,<!\^!>,<!^1!>)!>)

; Escape commas in a string with "^2". This eliminates further problems with a string being split into separate args
define(<!_esc_comma!>, <!patsubst(_esc_caret(<!$@!>), <!,!>,<!^2!>)!>)

; We have to insert dummy chars to prevent recognition of substrings that are valid macro names
; At this point we will have a string of regular chars followed by "~"s and escaped commas and carets
define(<!_esc_words!>, <!patsubst(patsubst(_esc_comma(<!$*!>),<![^^]!>,<!\&~!>),<!\^\([0-9]\)~!>,<!^\1!>)!>)


; Main recursive loop to convert each character. Because of the escaping we retrieve two chars at a time.
define(<!_asciiord!>,<!ifelse(<!$1!>,,,<!_xaconv(substr(<!$1!>,0,2))<!!>ifelse(len(<!$1!>),2,,<!,!>) $0(substr(<!$1!>,2))!>)!>)

; Convert "^1" and "^2" escapes to their ASCII code or call _aconv() for everything else
define(<!_xaconv!>,<!ifelse(<!$1!>,<!^1!>,94,<!$1!>,<!^2!>,44,_aconv(substr(<!$1!>,0,1)))!>)

; Convert an ASCII character to its numeric value
define(<!_aconv!>,<!ifelse(<!$1!>,<! !>,32,<!$1!>,<!;!>,59,dnl
<!index(<!                                 !"#$%&'()*+,-./0123456789: <=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ !>,<!$1!>)!>)!>)

changequote

;---------------------------------
; Convert a string to a list of decimal ASCII codes with or without a NUL terminator
; The following C escape codes are translated to their ASCII value:
;   * \\  \
;   * \n  NL\LF
;   * \r  CR
;   * \t  HT
;   * \b  BS
;   * \a  BEL
;   * \e  ESC
;   * \s  semicolon ;
; Args:
;   Arg1: String to convert
; Example:
;   estr(`My string\r\n')  ; Expands to 77, 121, 32, 115, 116, 114, 105, 110, 103, 13, 10
;   cstr(`My string\r\n')  ; Expands to 77, 121, 32, 115, 116, 114, 105, 110, 103, 13, 10, 0
define(`estr', `_esc(asciiord(`$1'))')
define(`cstr', `_esc(asciiord(`$1')), 0')

; Step through decimal values replacing escape codes as necessary
define(`_esc', `ifelse(eval($#>1),1, `ifelse(eval($1==92),1,`_echar($2)`'ifelse(eval($#>2),1,`, $0(shift(shift($@)))')',`$1, $0(shift($@))')', `$1')')

; Convert escaped character codes into their ASCII value
;                               \\                 \n                  \r                  \t                 \b                \a             \e                \s
define(`_echar', `ifelse(eval($1==92),1,92, eval($1==110),1,10, eval($1==114),1,13, eval($1==116),1,9, eval($1==98),1,8, eval($1==97),1,7, eval($1==101),1,27, eval($1==115),1,59,dnl
`errmsg(`Invalid escape code')')')


;---------------------------------
; Return the length of a string constant, a portable string or a packed string
; The argument is passed through the estr() macro to collapse escaped characters
; before counting them.
; Args:
;   Arg1: String to count length from. This is either a constant or the label to a string
;         defined with string() or packed_string()
; Example:
;   load s0, strlenc(`foobar\r\n')  ; Expands to 8
;
;   packed_string(xyzzy, `This is a string')
;   load s0, strlenc(xyzzy) ; Expands to 16
define(`strlenc', `ifdef(`_$1_LENGTH', _$1_LENGTH, `_strlenc(estr($1))')')
define(`_strlenc', $#)

;---------------------------------
; Add double quotes around a string
; This is allows the use of macros to generate strings where substitution within "" would
; normally be suppressed.
; Args:
;   Arg1: String to quote
; Example:
;   define(DATE_STAMP, `1 Jan 2015')
;   string mystr$, qstr(DATE_STAMP)  ; Expands to string mystr$, "1 Jan 2015"
define(`qstr', `"$1"')

;---------------------------------
; Convert 16-bit words into bytes
; words_le produces little-endian byte order, words_be is big-endian
; Args:
;   Arg1-Argn: Numbers to split into bytes
; Example:
;   words_le(0xff01, 0xff02) ; Expands to 255, 1, 255, 2
define(`words_le', `ifelse(`$1',,,eval($#>1),1,`_split_le($1), $0(shift($@))',`_split_le($1)')')
define(`words_be', `ifelse(`$1',,,eval($#>1),1,`_split_be($1), $0(shift($@))',`_split_be($1)')')

define(`_split_le', `eval(($1) & 0xFF), eval((($1) & 0xFF00) >> 8)')
define(`_split_be', `eval((($1) & 0xFF00) >> 8), eval(($1) & 0xFF)')



;---------------------------------
; Convert PicoBlaze literals into m4 syntax
; Handles "c" -> ascii ord.,  nn'd -> decimal,  nn -> hex,  nn'b -> bin
; Args:
;   Arg1: String to convert
; Result:
;   An integer in m4 syntax
; Example:
;   pb2m4(10'd) expands to 10,  pb2m4("0") expands to 48
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
;======================================================

define(`_tempreg', `sE')

define(`use_tempreg', `pushdef(`_tempreg', $1)')


;=============== MISCELLANEOUS OPERATIONS ===============
;========================================================

;---------------------------------
; No-op macros
define(`nop', `load _tempreg, _tempreg  ; NOP')

;---------------------------------
; Swap registers
; Args:
;   Arg1: Register 1
;   Arg2: Register 2
; Example:
;   swap(s0, s1)
define(`swap', `xor $1, $2  ; Swap
xor $2, $1
xor $1, $2')

;---------------------------------
; Generate a random name for a label
; Args:
;   Arg1: Optional prefix to name
; Example:
;   randlabel(PREFIX_)  --> PREFIX_?????
define(`randlabel', `esyscmd(`python -c "import sys; import random; import string; sys.stdout.write(\"$1\" + \"\".join([random.choice(string.ascii_letters) for _ in xrange(5)]))"')')

;---------------------------------
; Generate a unique name for a label
; Args:
;   Arg1: Optional prefix to name
; Example:
;   uniqlabel(PREFIX_)  --> PREFIX_f0_0001
ifdef(`M4_FILE_NUM',,`define(`M4_FILE_NUM', 0)')
define(`_uniq_ix', 0)

define(`uniqlabel', `define(`_uniq_ix', incr(_uniq_ix))dnl
$1f`'eval(M4_FILE_NUM)_`'eval(_uniq_ix, 10, 4)')


;---------------------------------
; Reverse arguments
; Example:
;   reverse(1,2,3)  --> 3,2,1
define(`reverse', `ifelse(eval($# > 1), 1, `reverse(shift($@)), `$1'', ``$1'')')

;---------------------------------
; Define a series of contiguous port or scratchpad memory constants
; Args:
;   Arg1: Starting address for port or memory
;   Arg2-Argn: Constant names
; Example:
;   iodefs(0, P_uart_out, P_uart_in, P_control)
;     Expands to:
;       constant P_uart_out, 00
;       constant P_uart_in, 01
;       constant P_control, 02
define(`iodefs', `const($2, eval($1, 16, 2))'
`ifelse(eval($# > 2), 1, `$0(eval($1 + 1), shift(shift($@)))')')


;---------------------------------
; Load a register with a value and output to a port
; Args:
;   Arg1: Value to load (constant or other register)
;   Arg2: Port to output to
;   Arg3: Optional Register to load with value, uses _tempreg is omitted
define(`load_out', `load ifelse($3,,`_tempreg',`$3'), evalx($1, 16, 2)
output ifelse($3,,`_tempreg',`$3'), ifelse(isnum(const2m4($2)),1,evalx(const2m4($2), 16, 2),`($2)') ')


;---------------------------------
; Load a register with a value and store to scratchpad
; Args:
;   Arg1: Value to load (constant or other register)
;   Arg2: Scratchpad address to output to (constant or a register)
;   Arg3: Optional Register to load with value, uses _tempreg is omitted
define(`load_store', `load ifelse($3,,`_tempreg',`$3'), evalx($1, 16, 2)
store ifelse($3,,`_tempreg',`$3'), ifelse(isnum(const2m4($2)),1,evalx(const2m4($2), 16, 2),`($2)') ')


;---------------------------------
; Define variables
; Args:
;   Arg1-ArgN: Series of variable alias expressions where an alias expression is:
;               <reg> is <alias> [:= value]
; The alias becomes an alternate name for the register. It is loaded with a value if the
; optional initializer is included. The value can be any constant expression or register.
; Example:
;   vars(s0 is counter := 0, s1 is sum, s2 is max := 20*3)
define(`vars', `pushdef(`_vars_ctx', `_vars_names($@)')' `_vars($@)')

define(`_vars', `ifelse(`$1',,,`_vardef(_vartokens(`$1'))
$0(shift($@))')')

define(`_vartokens', `regexp(`$1',`\([^ ]+\) +is +\([^ ]+\)\( *:= *\([^ ]+\)\)?',``\1', `\2', \4')')

define(`_vardef', `ifelse(`$1',,`errmsg(Invalid variable definition)')'`pushdef(`$2', `$1')'dnl
`ifelse(`$3',,,`load $1, evalx($3, 16, 2) `;' Var `$2' := $3')')


;---------------------------------
; Remove definitions from previous call to vars()
define(`popvars', `_popvars(_vars_ctx) popdef(`_vars_ctx')')
define(`_popvars', `ifelse(`$1',,,`popdef(`$1')$0(shift($@))')')


; Eliminate arguments without an "is" clause to create a valid argument list for vars()
define(`_vars_filter', `ifelse(`$1',,,regexp(`$1',` is '),-1,`$0(shift($@))', ``$1', $0(shift($@))')')

; Get register names from vars() arg list without optional "is" clauses
define(`_vars_regs', `ifelse(`$1',,,regexp(`$1',`\(\w+\)\s+is ',``\1''),,`$1`'ifelse(eval(`$# >= 2'),1,`,')`'$0(shift($@))',`regexp(`$1',`\(\w+\)\s+is ',``\1'')`'ifelse(eval(`$# >= 2'),1,`,')`'$0(shift($@))')')

; Get var names from vars() arg list
define(`_vars_names', `ifelse(`$1',,,regexp(`$1',`\w+\s+is\s+\(\w+\)',``\1''),,`$1`'ifelse(eval(`$# >= 2'),1,`,')`'$0(shift($@))',`regexp(`$1',`\w+\s+is\s+\(\w+\)',``\1'')`'ifelse(eval(`$# >= 2'),1,`,')`'$0(shift($@))')')


; Split a quoted string on commas while guarding aginst unwanted substitution of the result
changequote(<!,!>)
define(<!_split_comma!>,
<!changequote(<!,!>)dnl
`patsubst(<!<!$1!>!>,<!,\s*!>,<!',`!>)'dnl
changequote!>)
changequote

define(`_strip_quotes', $*)

; Split a string into a list of quoted strings without any substitution
define(`_split_args', `_strip_quotes(_split_comma(`$1'))')



;---------------------------------
; Count the number of args
; Args:
;   Arg1-ArgN: Argument list to count
; Example:
;   argc(a,b,c,d) expands to 4
define(`argc',`$#')


;=============== DELAYS ===============
;======================================

;---------------------------------
; Define system clock frequency
; ** Only invoke once. Must be executed before any delay macros **
; Args:
;   Arg1: Clock frequency in MHz
; Example:
;   use_clock(50) ; 50 MHz clock
define(`use_clock', `define(`_cfreq', $1)')

define(`_delay_clk_initcheck', `_initcheck(`_cfreq', `Delays are `not' initialized. Use `use_clock()' before any delay operation')')

;---------------------------------
; Define delay loop register
; ** Only invoke once. Must be executed before any delay macros **
; This register must be different from the registers used in the delay_us() and delay_ms() macros.
; Args:
;   Arg1: register to use for inner loop counter
; Example:
;   use_delay_reg(sA)
define(`use_delay_reg', `define(`_dreg', $1)')

define(`_delay_reg_initcheck', `_initcheck(`_dreg', `Delay register is `not' initialized. Call `use_delay_reg()' first')')


;---------------------------------
; Delay for a number of instruction cycles
; Args:
;   Arg1: Number of instructions to delay
;
; This can generate two types of delay loops. The default is a recursive
; delay implemented without any registers. For delays of 511 cycles or less
; a more efficient loop can be generated if a loop count register is defined
; first by calling the use_delay_reg() macro.
; Example:
;   delay_cycles(10) ; Delay for 10 instructions (20 clock cycles)
define(`delay_cycles', `ifelse(eval(ifdef(`_dreg',1,0) && const2m4($1) <= 511),1,`_alt_delay_cycles($1)',`_delay_cycles($1)')')

; This delay generator constructs a minimal delay using a combination of
; delay trees and NOPs. The result uses fewer instructions that a plain chain
; of NOPs. Note that the delay trees use recursion on the stack to maintain delay state.
; There is a slight risk of overflowing the stack if this routine is invoked for long delays
; when already in a deeply nested call frame.
; The maximum delay is appx. 100e9 cycles (1000sec at 100MHz)
; Yo, dawg! I hard you like recursion so I put some recursion in your recursive loops
define(`_delay_cycles', `ifelse(eval(const2m4($1) < 5),1,`repeat(`nop', const2m4($1))',`_delay_tree(floor_log2(eval(const2m4($1) - 1)))'
`$0(eval(const2m4($1) - 2**floor_log2(eval(const2m4($1) - 1)) - 1))')')

;---------------------------------
; Compute floor(log(n,b)) for Base-b
; Args:
;   Arg1: Number to compute floor-log on
;   Arg2: Logarithm base
; Example:
;   floor_log(20, 2)    expands to 4 (2**4 = 16, 2**5 = 32)
;   floor_log(1000, 10) expands to 3 (10**3 = 1000)
define(`floor_log', `ifelse(eval($1 < $2),1,0,`eval($0(eval($1 / $2), $2) + 1)')')

;---------------------------------
; Compute floor(log(n)) for Base-2
; Args:
;   Arg1: Number to compute floor-log on
define(`floor_log2', `floor_log($1,2)')


; Generate a binary tree delay with recursive calls
; Args:
;   Arg1: Number of stages. Delayed instructions are 2**Arg1 + 1
define(`_delay_tree', `pushdef(`_dt', uniqlabel(DTREE_))'dnl
`call _dt`'_`'decr($1) `; Delay for' eval(2**$1 + 1) cycles
jump _dt`'_end
_delay_tree_gen(decr($1))dnl
_dt`'_0: return
_dt`'_end:'`popdef(`_dt')')

define(`_delay_tree_gen', `_dt`'_$1: call _dt`'_`'decr($1)'
`ifelse(eval($1 > 1),1,`$0(decr($1))')')

; This delay generator constructs a more efficient counter-based loop when the use_delay_reg() macro
; has been called. The delay must be 511 cycles or less.
define(`_alt_delay_cycles', `_delay_reg_initcheck' `ifelse(eval(const2m4($1) < 4),1,`repeat(`nop', const2m4($1))',dnl
eval(const2m4($1) > 511),1,`errmsg(`Delay is too large (max 511)')',`pushdef(`_dl', uniqlabel(DLOOP_))'dnl
`load _dreg, evalh((const2m4($1) - 1) / 2)
_dl:
sub _dreg, 01
jump nz, _dl
repeat(`nop', eval(const2m4($1) - 1 - ((const2m4($1)-1)/2)*2))
')')

;---------------------------------
; Delay by milliseconds
; Args:
;   Arg1      : Milliseconds to delay
;   Arg2, Arg3: MSB, LSB of delay counter
;   Arg4      : Optional number of instructions to deduct from the delay (default is 0)
;
; This delay will be cycle accurate if the requested delay is an integer multiple
; of the clock period.
; At 100 MHz, the max delay is 214 ms. It increases with lower clock frequencies
; Example:
;              use_clock(50) ; 50 MHz clock
;   delay_5ms: delay_ms(5, s4,s5, 2) ; Deduct 2 additional instructions to account for call and return
;              return
;   ...
;   call delay_5ms
define(`delay_ms', ``; Delay for' const2m4($1) ms at _cfreq MHz
_delay_us(eval(const2m4($1)*1000),$2,$3,`ifelse(`$4',,2,`eval(2 + $4)')')')


;---------------------------------
; Delay by microseconds
; Args:
;   Arg1      : Microseconds to delay
;   Arg2, Arg3: MSB, LSB of delay counter
;   Arg4      : Optional number of instructions to deduct from the delay (default is 0)
;
; This delay will be cycle accurate if the requested delay is an integer multiple
; of the clock period.
; Example:
;               use_clock(50) ; 50 MHz clock
;   delay_40us: delay_us(40, s4,s5)
;               return
;   ...
;   call delay_40us
define(`delay_us', ``; Delay for' const2m4($1) us at _cfreq MHz
_delay_us(const2m4($1),$2,$3,`ifelse(`$4',,2,`eval(2 + $4)')')')

define(`_delay_us', `_delay_clk_initcheck' `ifelse($2,_dreg,`errmsg(`Register "$2" is in use `for' inner delay loop')',dnl
$3,_dreg,`errmsg(`Register "$3" is in use `for' inner delay loop')')' `pushdef(`_dly', uniqlabel(DELAY_))'`dnl
ifelse(eval(_dval_us($1,$4) >= 2**16),1,`errmsg(`Delay value is too large')')dnl
load16($2,$3, _dval_us($1,$4))
_dly: `; Total loop delay:' eval(3 + _dnop_us($1,$4)) instructions
delay_cycles(_dnop_us($1,$4))
sub16($2,$3, 1)
jump nc, _dly
`;' Adjust delay with _dadj_us($1,$4) additional instruction cycles
delay_cycles(_dadj_us($1,$4))'`popdef(`_dly')')

; Calculate number of nops needed to fit delay count into 16-bits
; Args:
;   Arg1: Microseconds to delay
;   Arg2: Additional instructions
define(`_dnop_us', `ifelse(eval(_dnop_calc($1,$2) < 0),1,0,_dnop_calc($1,$2))')
; Note: Scaled by 100 to retain fractional bits until the end. The +100 term is
; used to round up result to the next integer.
define(`_dnop_calc', `eval( (100 * $1 * _cfreq / (2 * (2**16 + $2)) - 300 + 100) / 100 )')

; Calculate microsecond delay value
; Args:
;   Arg1: Microseconds to delay
;   Arg2: Additional instructions
define(`_dval_pre', `eval(($1 * _cfreq / 2 - $2) / (3 + _dnop_us($1,$2)) - 1)')

; Adjustment delays needed to reach requested delay
define(`_dadj_pre', `eval( ($1 * _cfreq - ((_dval_pre($1,$2)+1)*(3 + _dnop_us($1,$2)) + $2)*2) / 2  )')

; Final count value and adjustment delays
define(`_dval_us', `ifelse(eval(_dadj_pre($1,$2) < 0),1,`eval(_dval_pre($1,$2) - 1)',`_dval_pre($1,$2)')')
define(`_dadj_us', `eval( ($1 * _cfreq - ((_dval_us($1,$2)+1)*(3 + _dnop_us($1,$2)) + $2)*2) / 2  )')


;---------------------------------
; Variable delay by milliseconds
; Args:
;   Arg1      : Maximum milliseconds to delay
;   Arg2, Arg3: MSB, LSB of delay counter
;
; The var_count_ms() macro generates a 16-bit count value that is loaded
; into the counter registers before calling the delay function
; Example:
;          use_clock(50) ; 50 MHz clock
;          define(MAXDELAY, 10) ; 10ms max delay
;          reg16(dly_count, s4,s5)
;   delay: var_delay_ms(MAXDELAY, dly_count)
;          return
;   ...
;   load16(dly_count, var_count_ms(1, MAXDELAY))
;   call delay ; Delay for 1 ms
;   ...
;   load16(dly_count, var_count_ms(8, MAXDELAY))
;   call delay ; Delay for 8 ms
define(`var_delay_ms', ``; Variable delay for max' const2m4($1) ms at _cfreq MHz
_var_delay_us(eval(const2m4($1)*1000),$2,$3)')

;---------------------------------
; Variable delay by microseconds
; Args:
;   Arg1      : Maximum microseconds to delay
;   Arg2, Arg3: MSB, LSB of delay counter
;
; The var_count_us() macro generates a 16-bit count value that is loaded
; into the counter registers before calling the delay function
; Example:
;          use_clock(50) ; 50 MHz clock
;          define(MAXDELAY, 900) ; 900 us max delay
;          reg16(dly_count, s4,s5)
;   delay: var_delay_us(MAXDELAY, dly_count)
;          return
;   ...
;   load16(dly_count, var_count_us(100, MAXDELAY))
;   call delay ; Delay for 100 us
;   ...
;   load16(dly_count, var_count_us(800, MAXDELAY))
;   call delay ; Delay for 800 us
define(`var_delay_us', ``; Variable delay for max' const2m4($1) us at _cfreq MHz
_var_delay_us(const2m4($1),$2,$3)')

define(`_var_delay_us', `_delay_clk_initcheck' `ifelse($2,_dreg,`errmsg(`Register "$2" is in use `for' inner delay loop')',dnl
$3,_dreg,`errmsg(`Register "$3" is in use `for' inner delay loop')')' `pushdef(`_dly', uniqlabel(VDELAY_))'`dnl
ifelse(eval(_dval_pre($1,0) >= 2**16),1,`errmsg(`Max delay value is too large')')dnl
_dly: `; Total loop delay:' eval(3 + _dnop_us($1,0)) instructions
delay_cycles(_dnop_us($1,0))
sub16($2,$3, 1)
jump nc, _dly'
`popdef(`_dly')')

;---------------------------------
; Generate 16-bit millisecond count for variable delay function
; Args:
;   Arg1: Milliseconds to delay
;   Arg2: Max milliseconds for the delay loop (from definition using var_delay_ms())
define(`var_count_ms', `ifelse(eval(_dval_var(eval(const2m4($1)*1000),eval(const2m4($2)*1000)) < 0),1,dnl
`errmsg(`Delay is too small: const2m4($1) ms')',`_dval_var(eval(const2m4($1)*1000),eval(const2m4($2)*1000))')')

;---------------------------------
; Generate 16-bit microsecond count for variable delay function
; Args:
;   Arg1: Microseconds to delay
;   Arg2: Max microseconds for the delay loop (from definition using var_delay_us())
define(`var_count_us', `ifelse(eval(_dval_var(const2m4($1),const2m4($2)) < 0),1,dnl
`errmsg(`Delay is too small: const2m4($1) us')',`_dval_var(const2m4($1),const2m4($2))')')

define(`_dval_var', `eval(($1 * _cfreq / 2 - 2) / (3 + _dnop_us($2,2)) - 1)')



;=============== CARRY FLAG OPERATIONS ===============
;=====================================================

;---------------------------------
; Clear the carry flag
define(`clearcy', `and _tempreg, _tempreg  ; Clear carry')

;---------------------------------
; Set the carry flag
; Args:
;   Arg1: Optional temporary register to modify. Uses temp reg by default.
; Example:
;   setcy(sf)
define(`setcy', `ifelse(`$1',,`pushdef(`_cyreg', `_tempreg')', `pushdef(`_cyreg', `$1')')'dnl
`ifdef(`PB3',`load _cyreg, 00  ; Set carry
compare _cyreg, 01',dnl
`hwbuild _cyreg ; Set carry')'
`popdef(`_cyreg')')


;=============== BITFIELD OPERATIONS ===============
;===================================================

;---------------------------------
; Set and clear bits in a register
; Args:
;   Arg1: Register to modify
;   Arg2: Bit number (0-7) to set or clear
; Example:
;   setbit(s0, 2)
define(`setbit', `or $1, eval(2**($2), 16, 2)  ; Set bit $2')
define(`clearbit', `and $1,  eval((~(2**($2))) & 0xFF, 16, 2)  ; Clear bit $2')

;---------------------------------
; Define a mask with specific bits set
; Args:
;   Arg1-Argn: Bit numbers to set in mask (0-7)
; Example:
;   mask(0, 1, 2, 7)     ; Expands to 135 = 0x87
define(`mask', `ifelse($1,,0,`eval(2**($1) + $0(shift($@)))')')

;---------------------------------
; Alternate mask that can be used as a direct argument to a PicoBlaze instruction
; Args:
;   Arg1: Bit numbers to set in mask (0-7) 
;
; Result is a mask in PicoBlaze hex format
; Example:
;   test s0, maskh(3,4,5) ; Test if bits 3, 4, and 5 are clear
;     jump z, is_clear
define(`maskh', `eval(mask($@), 16, 2)')

;---------------------------------
; Set and clear mask bits in a register
; Args:
;   Arg1: Register to modify
;   Arg2: Mask value
; Example:
;   setmask(s5, mask(0,1,2))
define(`setmask', `or $1, eval(const2m4($2), 16, 2)  ; Set mask')
define(`clearmask', `and $1, eval((~(const2m4($2))) & 0xFF, 16, 2)  ; Clear mask')

;---------------------------------
; Test if a bit is set or clear
; Args:
;   Arg1: Register to test
;   Arg2: Bit number (0-7) to test
; Z is set if bit is clear, Z is clear if bit is set
; Example:
;   testbit(s1, 3)
;      jump z, bit_cleared
define(`testbit', `test $1, eval(2**($2), 16, 2)  ; Test bit $2')



;=============== CONDITIONAL JUMP, CALL, and RETURN OPERATIONS ===============
;=============================================================================

; Jump if not equal
; Args:
;   Arg1: Label to jump to
define(`jne', `jump nz, $1  ; if not equal')

; Jump if equal
; Args:
;   Arg1: Label to jump to
define(`jeq', `jump z, $1  ; if equal')

; Jump if greater or equal
; Args:
;   Arg1: Label to jump to
define(`jge', `jump nc, $1  ; if greater or equal')

; Jump if less than
; Args:
;   Arg1: Label to jump to
define(`jlt', `jump c, $1  ; if less than')

; Example:
;   compare s0, s1
;    jeq(is_equal)   ; jumps if s0 == s1
;    jlt(less_than)  ; jumps if s0 < s1


; Call if not equal
; Args:
;   Arg1: Label to call
define(`callne', `call nz, $1  ; if not equal')

; Call if equal
; Args:
;   Arg1: Label to call
define(`calleq', `call z, $1  ; if equal')

; Call if greater or equal
; Args:
;   Arg1: Label to call
define(`callge', `call nc, $1  ; if greater or equal')

; Call if less than
; Args:
;   Arg1: Label to call
define(`calllt', `call c, $1  ; if less than')

; Example:
;   compare s3, 24
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

; Example:
;   compare s0, s1
;    reteq   ; return if s0 == s1



;=============== CONDITIONAL IF-THEN-ELSE ===============
;========================================================


;---------------------------------
; Generic if macro
; Args:
;   Arg1: Boolean comparison expression
;         Must be of the form: "reg op reg|expression" where op is <, >, <=, >=, ==, !=, or &
;         or of the form "reg" which is treated as "reg != 0".
;         Signed comparison is invoked with "signed(comparison expr.)"
;         With signed comparison the right operand cannot be a named constant.
;         With the & operator a test instruction is used in place of compare. The true
;         clause is executed when the result is non-zero.
;   Arg2: True clause
;   Arg3: Optional else clause or next else-if comparison expression
;   Arg4-Argn: Additional else-if clauses
;
;   This macro performs a comparison of the left and right operands and then inserts
;   the if* macro selected by the operation
; Example:
;   if(s0 < s1, `load s0, 01', `load s0, 02')
;   if(s0 != 0xa5, `load s0, 01')
;   if(signed(s0 < -10), `load s0, 01') ; Signed comparison with signed()
define(`if', `ifelse(eval($# > 3),1,`_if(_iftokens($1), `$2', `if(shift(shift($@)))')',dnl
`_if(_iftokens($1), `$2', `$3')')')

; Split comparison expression into tokens
define(`_iftokens', `ifelse(regexp(`$1', `\(\w+\) *\(s?[<>=!&]+\) *\([^ ]+\)'),-1,`$1, !=, 0',`regexp(`$1', `\(\w+\) *\(s?[<>=!&]+\) *\([^ ]+\)', `\1, \2, \3')')')

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
; Args:
;   Arg1: Expression to convert
; Example:
;   signed(s0 < 4)  ; Expands to "s0 s< 4"
define(`signed', `patsubst(`$1', `\([<>]=?\)', ` s\1')')

; Restore right curly brackets inside quoted strings that were protected before
; performing C-style syntax transformations
define(`_rcurly', `}')

; Restore right parens from signed() macros that were protected
define(`_rparen', `)')

; The preprocessor converts constant directives into const() macros so that
; the m4 code is aware of constants declared in PicoBlaze syntax
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
; Args:
;   Arg1: True clause
;   Arg2: Optional else clause
;
;   These macros insert labels and jump instructions to implement the behavior of
;   an if-then or if-then-else statement testing for equality, inequality,
;   greater-or-equal, or less-than
; Example:
;   compare s0, s1
;   ifeq(`load s3, 20
;         output s3, MY_PORT',
;   ; else
;        `load s3, 30
;         output s3, MY_OTHER_PORT')

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
;=================================================

;---------------------------------
; While loop
; Args:
;   Arg1: Boolean comparison expression
;         Must be of the form: "reg op reg|expression" where op is <, >=, ==, !=, or &
;   Arg2: Code block for loop body
; Example:
;   load s0, 00
;   while(s0 < 10, `output s3, P_foo
;                   add s0, 01')
define(`while', `pushdef(`_clbl', uniqlabel(WHILE_))'`pushdef(`_elbl', uniqlabel(ENDLOOP_))'`_clbl:
if($1,`$2
jump _clbl')
_elbl:' `popdef(`_clbl')'`popdef(`_elbl')') 


;---------------------------------
; Do-while loop
; Args:
;   Arg1: Boolean comparison expression
;         Must be of the form: "reg op reg|expression" where op is <, >=, ==, !=, or &
;   Arg2: Code block for loop body
; Example:
;   load s0, 15'd
;   dowhile(s0 != 10, `output s3, P_foo
;                      sub s0, 01')
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
; Args:
;   Arg1: Initialization expression (passed to expr()) This can be empty
;   Arg2: Boolean comparison expression
;   Arg3: Update expression (passed to expr()) This can be empty
;   Arg4: Code block for loop body
; Example:
;   for(s0 := 0, s0 < 5, s0 := s0 + 1, `output s0, 00')
; Note:
; The "continue" macro will behave as in C by jumping to the update code before
; restarting the loop.
define(`for', `pushdef(`_slbl', uniqlabel(FOR_))'`pushdef(`_clbl', uniqlabel(NEXTFOR_))'dnl
`pushdef(`_elbl', uniqlabel(ENDLOOP_))'`ifelse(`$1',,,`expr($1)')
_slbl:
if($2,`$4
_clbl: ifelse(`$3',,,`expr($3)')
jump _slbl')
_elbl:' `popdef(`_slbl')'`popdef(`_clbl')'`popdef(`_elbl')') 


; Break statement to exit loops
define(`break', `jump _elbl')

; Continue statement to restart loop
define(`continue', `jump _clbl')


;=============== FUNCTION CALLS ===============
;==============================================


;---------------------------------
; Procedure definition
; Args:
;   Arg1: Label for procedure
;   Arg2: Variable definitions (same format as passed to the vars() macro)
;   Arg3: Code block for proc body
; Example:
;   proc(add2, `s0 is result', `add result, 02')
;   ...
;   load s0, 42
;   call add2
define(`proc', `;PRAGMA function $1 [$2] begin
$1:
  vars(_vars_filter(_split_args(`$2')))
  $3
  return
;PRAGMA function $1 end
popvars')


; Pull arguments from the stack and load them into local registers
define(`_func_get_args', `add _stackptr, evalx(`$1 + $# - 1', 16, 2)  `;' Fetch stack offset eval($1 + $# - 1)
_fetch_args(shift($@))dnl
sub _stackptr, evalx(`$1 + 1', 16, 2)')

define(`_fetch_args', `ifelse(`$1',,,`fetch $1, (_stackptr)
ifelse(eval($# >= 2),1,`sub _stackptr, 01',`dnl')
$0(shift($@))')')

;---------------------------------
; Function definition
; This creates a function that receives its arguments on the stack.
; A macro is generated to prepare the stack arguments and call the function.
; The function will save registers automatically and load the stack arguments.
; The saved registers and call frame are cleaned up at the end.
; Do not use RETURN instructions in the code body. Instead call the
; leave_func() macro. Use the retvalue() macro to store return values
; on the stack.
; Args:
;   Arg1: Label for function
;   Arg2: Variable definitions (same format as passed to the vars() macro)
;   Arg3: Number of bytes returned on stack (0 for no return values)
;   Arg4: Code block for func body
; Example:
;   func(mul, `s0 is m1, s1 is m2, s2 is result', 0, `expr(result := m1 * m2)')
define(`func', `_func(`$1', `$2', ifelse(`$3',,0,$3), `$4')')

define(`_func', `_stack_initcheck' `define(`_funcname', $1)' `;PRAGMA function $1 [stack($2 : $3)] begin
$1:
  define(`_func_args', `_split_args(`$2')')dnl
  define(`_func_ret_num', `$3')dnl
  ifelse(eval($3 > argc(_func_args)),1,`define(`_func_frame_cleanup',0)'dnl
  `; Adjust for extra return bytes on stack
  addstack(eval($3 - argc(_func_args)))',dnl
  `define(`_func_frame_cleanup',`eval(argc(_func_args) - $3)')')

  ; Save registers
  push(_vars_regs(_func_args))
  vars(_vars_filter(_func_args))dnl

  ; Get arguments
  _func_get_args(argc(_func_args), _vars_regs(_func_args))
  define(`_frame_offset', 0)dnl

  $4

LEAVE_$1:
  ; Restore registers
  pop(_vars_regs(_func_args))
  ifelse(_func_frame_cleanup,0,,dnl
  `; Remove call frame
  dropstack(_func_frame_cleanup)')
  return
;PRAGMA function $1 end
popvars'
`define(`$1',`; Push arguments:
push('$`'@`)
call `$1'')')


;---------------------------------
; Return from func() and isr() macro code bodies
; Args:
;   Arg1: Optional condition code (Z, NZ, C, or NC)
; Example:
;   leave_func(Z)
define(`leave_func', `jump ifelse(`$1',,,`$1,') LEAVE_`'_funcname')

define(`leave_isr', `leave_func($1)')

;---------------------------------
; Place func return value onto the stack
; Only call this macro inside a func code body
; Args:
;   Arg1: Register with value to save
;   Arg2: Offset from end of return frame (starting at 1)
; Example:
;   retvalue(s0, 1) ; First return value to be popped off after return
;   retvalue(s1, 2) ; Second value to return from func
define(`retvalue', `ifelse(eval($2 > 0 && $2 <= _func_ret_num),1,dnl
  `putstackat($1, eval($2 + argc(_func_args) + _func_frame_cleanup + _frame_offset))',dnl
  `errmsg(`Invalid `func' return value offset: $2')')')

;---------------------------------
; ISR definition
; This creates an ISR that takes care of saving registers on the stack.
; Do not use RETURNI instructions in the code body. Instead call the
; leave_func() macro.
; By default the ISR returns with interrupts enabled. You can leave them
; disabled by passing "disable" as Arg3.
; Args:
;   Arg1: Address for ISR
;   Arg2: Variable definitions (same format as passed to the vars() macro)
;   Arg3: Optional return interrupt state "enable"|"disable"|empty
;   Arg4: Code block for ISR body
; Example:
;   isr(0x3FF, `s0, s1, s2', `load s0, 42
;                             output s0, ff')
define(`isr', `_stack_initcheck' `define(`_funcname', `__ISR')'dnl
`ifdef(`_isr_exists', `errmsg(`ISR can only be defined once')', `define(`_isr_exists', 1)')'dnl
`__ISR:
address evala($1)
jump __ISR
address __ISR
;PRAGMA function __ISR begin
  ; Save registers
  push(_vars_regs($2))
  vars(_vars_filter($2))

  $4

LEAVE___ISR:
  ; Restore registers
  pop(_vars_regs($2))

  returni ifelse(`$3',,`enable',`$3')
;PRAGMA function __ISR end
popvars')



;=============== SHIFT AND ROTATE OPERATIONS ===============
;===========================================================

;---------------------------------
; Repeat a string
; Args:
;   Arg1: Instruction or macro string to repeat
;   Arg2: Numper of repetitions
define(`repeatstr', `ifelse($#,0, ``$0'', eval(const2m4($2)>0),1, `$1`'$0(`$1', decr(const2m4($2)))')')

;---------------------------------
; Repeat an instruction or macro
; Args:
;   Arg1: Instruction or macro string to repeat
;   Arg2: Numper of repetitions
define(`repeat', `ifelse($#,0,``$0'', eval(const2m4($2)>0),1, `$1
$0(`$1', eval(const2m4($2) - 1))')')

;---------------------------------
; Repeated shifts
; Args:
;   Arg1: Register to shift
;   Arg2: Number of shifts
; Example:
;   sl0(s2, 4)  ; Shift left by 4
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
;================================================

;---------------------------------
; Initialize and define stack pointer register.
; ** Only invoke once. Must be executed before any push() or pop() macros **
; Args:
;   Arg1: Stack pointer register
;   Arg2: Scratchpad address for top of stack
; Example:
;   namereg sA, SP ; Reserve stack pointer register 
;   use_stack(SP, 0x3F) ; Start stack at end of 64-byte scratchpad
define(`use_stack', `load $1, eval(const2m4($2), 16, 2)' `define(`_stackptr', $1)')


define(`_initcheck', `ifdef(`$1',, `errmsg(`$2')')')

define(`_stack_initcheck', `_initcheck(`_stackptr', `Stack is `not' initialized. Use `use_stack()' before any operation')')


define(`_frame_offset', 0)

define(`_inc_frame', `define(`_frame_offset', eval(_frame_offset + $1))')
define(`_dec_frame', `define(`_frame_offset', eval(_frame_offset - $1))')


;---------------------------------
; Pseudo-stack operations using the scratchpad RAM
; The stack pointer grows from the end of the scratchpad to the start
; Args:
;   Arg1-Argn: Registers with values to push or pop
; Example:
;   use_stack(sa, 0x3F)
;   push(s0)
;   pop(s1)
;   push(s3, s4, s5)  ; Push and pop multiple registers at once
;   pop(s3, s4, s5)   ; Pop is performed in reverse order from push
define(`push', `_stack_initcheck' `ifelse(`$1',,,`store $1, (_stackptr)  ; Push
sub _stackptr, 01`'_inc_frame(1)
$0(shift($@))')')


define(`pop', `_pop(reverse($@))')

define(`_pop', `_stack_initcheck' `ifelse(`$1',,,`add _stackptr, 01  ; Pop
fetch $1, (_stackptr)`'_dec_frame(1)
$0(shift($@))')')


;---------------------------------
; Retrieve multiple contiguous values from the stack without modification
; Args:
;   Arg1-Argn: Registers to save values in
;              The first register corresponds to the highest address
; Example:
;   getstack(s3, s4, s5) ; Get stack offset SP+3, SP+2, and SP+1 into s3, s4, s5
define(`getstack', `_stack_initcheck' `add _stackptr, $#  ; Get stack registers
_gs($@)')

define(`_gs', `ifelse(`$1',,,`fetch $1, (_stackptr)
sub _stackptr, 01
$0(shift($@))')')

;---------------------------------
; Retrieve values from the stack without modification
; Args:
;   Arg1: Register to save value in
;   Arg2: Offset from stack pointer (offset 1 is the first value) or a register
; Example:
;   getstackat(s3, 2)  ; Get the second value relative to the stack pointer
;   getstackat(s3, s0) ; Get stack value pointed at by s0
define(`getstackat', `_stack_initcheck' `add _stackptr, evalx($2, 16, 2)  ; Fetch stack offset $2
fetch $1, (_stackptr)
sub _stackptr, evalx($2, 16, 2)')


;---------------------------------
; Store multiple contiguous values on the stack without modification
; Args:
;   Arg1-Argn: Registers to store values from
;              The first register corresponds to the highest address
; Example:
;   putstack(s3, s4, s5) ; Put s3, s4, s5 into stack offset SP+3, SP+2, and SP+1
define(`putstack', `_stack_initcheck' `add _stackptr, $#  ; Put stack registers
_ps($@)')

define(`_ps', `ifelse(`$1',,,`store $1, (_stackptr)
sub _stackptr, 01
$0(shift($@))')')

;---------------------------------
; Store values to the stack without modification
; Args:
;   Arg1: Register with value to store
;   Arg2: Offset from stack pointer (offset 1 is the first value) or a register
; Example:
;   putstackat(s3, 2)  ; Put the second value relative to the stack pointer
;   putstackat(s3, s0) ; Put stack value pointed at by s0
define(`putstackat', `_stack_initcheck' `add _stackptr, evalx($2, 16, 2)  ; Store stack offset $2
store $1, (_stackptr)
sub _stackptr, evalx($2, 16, 2)')


;---------------------------------
;Drop values stored on the stack
;Args:
;  Arg1: Number of values to drop from the stack or a register
;
;Example:
;  dropstack(2)  ; Remove 2 values
;  dropstack(s1) ; Remove number of values specified in s1 register
define(`dropstack', `_stack_initcheck' `ifelse(isnum($1),1,`_dec_frame($1)')'dnl
`add _stackptr, evalx($1, 16, 2)  ; Remove stack values')

;---------------------------------
;Allocate local space on the stack
; Args:
;   Arg1: Number of values to add to the stack or a register
;
;Example:
;  addstack(2)  ; Add 2 values
;  addstack(s1) ; Add number of values from s1
define(`addstack', `_stack_initcheck' `ifelse(isnum($1),1,`_inc_frame($1)')'dnl
`sub _stackptr, evalx($1, 16, 2)  ; Add local stack values')


;=============== STRING AND TABLE OPERATIONS ===============
;===========================================================

;---------------------------------
; Repeated string function call operation (useful for PicoBlaze-3)
; Args:
;   Arg1: Subroutine to call for each character
;   Arg2: Register used to hold characters (typically an argument to the subroutine)
;   Arg3: String to split into characters
; Example:
;   callstring(write_char, s1, `My string')
;   Expands to:
;      load s1, "M"
;      call write_char
;      load s1, "y"
;      call write_char
;      ...
define(`callstring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
call $1'
`$0($1, $2, substr(`$3',1))')')

;---------------------------------
; Repeated string output operation
; Args:
;   Arg1: Output port in m4 integer format or a constant name
;   Arg2: Register used to hold characters
;   Arg3: String to split into characters
; Example:
;   constant UART_PORT, 0a
;   outputstring(UART_PORT, s1, `My string')
;   Expands to:
;      load s1, "M"
;      output s1, UART_PORT
;      load s1, "y"
;      output s1, UART_PORT
;      ...
;
;   outputstring(0x0a, s1, `My string') ; Without using a constant
define(`outputstring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
output $2, evalx($1, 16, 2)'
`$0($1, $2, substr(`$3',1))')')


;---------------------------------
; Store a string to scratchpad RAM
; Args:
;   Arg1: Adress of first byte
;   Arg2: Temporary register for each character
;   Arg3: String to store
define(`storestring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
store $2, eval($1, 16, 2)'
`$0(eval(($1) + 1), $2, substr(`$3',1))')')


;---------------------------------
; Store a string to scratchpad RAM
; Args:
;   Arg1: Pointer register to scratchpad address
;   Arg2: Temporary register for each character
;   Arg3: String to store
define(`storestringat', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
store $2, ($1)'
`ifelse(eval(len($3)>1),1,`add $1, 01')'
`$0($1, $2, substr(`$3',1))')')



;---------------------------------
; Repeated function call on a table of constants
; Args:
;   Arg1: Subroutine to call for each byte
;   Arg2: Temporary register for each constant
;   Arg3 - Argn: Decimal values representing table bytes
; Example:
;   calltable(my_subroutine, sf, pbhex(DE,AD,BE,EF)) ; Pass DE,AD,BE,EF to subroutine
define(`calltable', `pushdef(`_sname', $1)`'pushdef(`_treg', $2)'`_ct(shift(shift($@)))'`popdef(`_sname')`'popdef(`_treg')')

define(`_ct', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
call _sname'
`_ct(shift($@))'dnl
)')

;---------------------------------
; Output a table of constants
; Args:
;   Arg1: Output port in m4 integer format or a constant name
;   Arg2: Temporary register for each constant
;   Arg3 - Argn: Decimal values to output to port
; Example:
;   constant UART_PORT, 0a
;   outputtable(UART_PORT, sf, pbhex(DE,AD,BE,EF)) ; Output DE,AD,BE,EF to port
define(`outputtable', `pushdef(`_oreg', $1)`'pushdef(`_treg', $2)'`_ot(shift(shift($@)))'`popdef(`_oreg')`'popdef(`_treg')')

define(`_ot', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
output _treg, evalx(_oreg, 16, 2)'
`_ot(shift($@))'dnl
)')


;---------------------------------
; Store a table of constants in scratchpad RAM
; Args:
;   Arg1: Address of first byte
;   Arg2: Temporary register for each constant
;   Arg3 - Argn: Decimal values to load in scratchpad
; Example:
;   load s1, my_array
;   storetable(0x10, sf, pbhex(DE,AD,BE,EF)) ; Load DE,AD,BE,EF into memory
;   storetable(0x10, sf, 10, 11, 12)         ; Load decimals
define(`storetable', `pushdef(`_treg', $2)'`_st($1, shift(shift($@)))'`popdef(`_treg')')

define(`_st', `ifelse(`$2',,,`load _treg, eval($2, 16, 2)
store _treg, eval($1, 16, 2)'
`_st(eval(($1) + 1), shift(shift($@)))'dnl
)')


;---------------------------------
; Store a table of constants in scratchpad RAM
; Args:
;   Arg1: Pointer register to scratchpad address
;   Arg2: Temporary register for each constant
;   Arg3 - Argn: Decimal values to load in scratchpad
; Example:
;   load s1, my_array
;   storetableat(s1, sf, pbhex(DE,AD,BE,EF)) ; Load DE,AD,BE,EF into memory
;   storetableat(s1, sf, 10, 11, 12)         ; Load decimals
define(`storetableat', `pushdef(`_preg', $1)`'pushdef(`_treg', $2)'`_sta(shift(shift($@)))'`popdef(`_preg')`'popdef(`_treg')')

define(`_sta', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
store _treg, (_preg)'
`ifelse(eval($#>1),1,`add _preg, 01')'
`_sta(shift($@))'dnl
)')


;---------------------------------
; Generate an INST directive from a pair of decimal values
; Args:
;   Arg1: High 10-bits
;   Arg2: Low byte
; Example:
;   instdata(pbhex(0a, 0b))  ; Expands to inst 00a0b
define(`instdata', `inst eval((($1) << 8) + (($2) & 0xFF), 16, 5)')

;---------------------------------
; Convert a list of data into a series of INST directives in little-endian byte order
; Args:
;   Arg1-Argn: Data to convert in decimal format
; Example:
;   insttable_le(pbhex(0a, 0b, 0c))
;   Expands to:  inst 00b0a
;                inst 0000c
;
;   insttable_le(asciiord(`Pack strings into ROM'))
;
;     inst 06150
;     inst 06b63
;     inst 07320
;     ...
;     inst 0206f
;     inst 04f52
;     inst 0004d
define(`insttable_le', `ifelse(eval($#>1),1,`instdata($2, $1)
$0(shift(shift($@)))',$1,,,`instdata(00, $1)')')

;---------------------------------
; Convert a list of data into a series of INST directives in big-endian byte order
; Args:
;   Arg1-Argn: Data to convert in decimal format
; Example:
;   insttable_be(pbhex(0a, 0b, 0c))
;   Expands to:  inst 00a0b
;                inst 00c00
define(`insttable_be', `ifelse(eval($#>1),1,`instdata($1, $2)
$0(shift(shift($@)))',$1,,,`instdata($1, 00)')')


;---------------------------------
; Configure string handling for either PB3 or PB6
; This will set up a string output function for either platform
; On PB6, the string() macro calls into a generic handler routine that
; scans for a NUL terminator.
; Args:
;   Arg1: Register to store each character
;   Arg2: Register for MSB of address to string (Only used on PB6)
;   Arg3: Register for LSB of address to string (Only used on PB6)
;   Arg4: Label of user provided function to process each character
;   Arg5: Optional name of macro to generate strings (default is "string")
; Example:
;   use_strings(s0, s5,s4, write_char)
;     ; write_char is called for each character stored in s0 using the "string()" macro
;   use_strings(s1, s5, s4, write_char_to_console, console_str)
;     ; write_char_to_console is called for each character stored in s1 using the "console_str()" macro
;
; This generates a new macro with the name used for Arg5.
; You must take steps to ensure that the normal execution path skips the
; instructions generated by this macro.
; It takes the following arguments:
; Args:
;   Arg1: Label used to identify string
;   Arg2: String value. This can contain all of the escape chars supported by estr/cstr
; Example:
;   jump main
;       string(my_string, `Hello, World!\r\n')
;   main:
;   ...
;       call my_string ; Output the string
define(`use_strings', `_use_strings($1, $2, $3, $4, ifelse($5,,`string',`$5'))')

define(`_use_strings', `define(`_string_char', $1)'`define(`_saddr_msb', $2)'`define(`_saddr_lsb', $3)'dnl
`define(`_string_char_handler', $4)'dnl
`ifdef(`PB3',,`; PB6 common string handler routine
__`'$5`'_handler: call@ (_saddr_msb, _saddr_lsb) ; Read next char
compare _string_char, 00 ; Check if NUL
return z
call _string_char_handler ; Handle the char
add16(_saddr_msb, _saddr_lsb, 1) ; Increment address
jump __`'$5`'_handler')'
`define($5, `ifelse('$`'#`,0, ``$5'',
  `define(`_'''$`'1```_LENGTH',strlenc(''$`'2``))'dnl
  `; "''$`'2``"'
  `ifdef(`PB3', ''$`'1```: calltable($4, $1, estr('''$`'2```))dnl
  return',dnl
  `table '''$`'1```#, [dec2pbhex(cstr('''$`'2```))]
  '''$`'1```: loadaddr($2, $3, _'''$`'1```_STR)
  jump __`'$5`'_handler
  _'''$`'1```_STR: load&return $1, '''$`'1```#')'
  )')'
)



;---------------------------------
; Configure packed string handling. This uses string data packed into
; INST statements stored in big-endian order. You must provide a routine to read
; pairs of characters from a dual-ported memory.
; The packed_string() macro calls into a generic handler routine that
; scans for a NUL terminator on data read from an external ROM.
; Args:
;   Arg1: Register to store even characters
;   Arg2: Register to store odd characters
;   Arg3, Arg4: Registers for MSB, LSB of address to string
;   Arg5: Label of user provided function to process each character (Only needs to handle the even char register)
;   Arg6: Label of user provided function to read pairs of characters from memory
;   Arg7: Optional name of macro to generate strings (default is "packed_string")
; Example:
;   use_packed_strings(s0, s1, s5,s4, write_char, read_next_chars)
;   read_next_chars is called until a NUL is found
;   write_char is called for each character stored in s0
;
; This generates a new macro with the name used for Arg7.
; You must take steps to ensure that the normal execution path skips the
; instructions generated by this macro.
; It takes the following arguments:
; Args:
;   Arg1: Label used to identify string
;   Arg2: String value. This can contain all of the escape chars supported by estr/cstr
; Example:
;   jump main
;       packed_string(my_string, `Hello, World!\r\n')
;   main:
;   ...
;       call my_string ; Output the string
define(`use_packed_strings', `_use_packed_strings($1, $2, $3, $4, $5, $6, ifelse($7,,`packed_string',`$7'))')

define(`_use_packed_strings', `define(`_pstring_char1', $1)'`define(`_pstring_char2', $2)'`define(`_psaddr_msb', $3)'`define(`_psaddr_lsb', $4)'dnl
`; Common packed string handler routine
__`'$7`'_handler: call $6 `;' Read next two chars into _pstring_char1, _pstring_char2
compare _pstring_char1, 00 ; Check if NUL
return z
call $5 ; Handle the char
load _pstring_char1, _pstring_char2 ; Copy second char over first
compare _pstring_char1, 00 ; Check if NUL
return z
call $5 ; Handle the char
add16(_psaddr_msb, _psaddr_lsb, 1) ; Increment address
jump __`'$7`'_handler'
`define($7, `ifelse('$`'#`,0, ``$7'',
  `define(`_'''$`'1```_LENGTH',strlenc(''$`'2``))'dnl
  `; "''$`'2``"'
  `''$`'1``: loadaddr($3, $4, _''$`'1``_STR)
  jump __`'$7`'_handler
  _''$`'1``_STR: insttable_be(cstr(''$`'2``))'
  )')'
)


;---------------------------------
; ANSI escape codes for generating color text
; The resulting string contains backslash escapes that must be processed
; by cstr() or estr().
; Args:
;   Arg1: Optional argument of "bold" will activate bold/bright text
; Example:
;   ansi_red`foobar'ansi_reset       ; red text
;   ansi_red(bold)`foobar'ansi_reset ; bold/bright red text
define(`ansi_black',   `\e[30'`ifelse($1,bold,`\s1')'`m')
define(`ansi_red',     `\e[31'`ifelse($1,bold,`\s1')'`m')
define(`ansi_green',   `\e[32'`ifelse($1,bold,`\s1')'`m')
define(`ansi_yellow',  `\e[33'`ifelse($1,bold,`\s1')'`m')
define(`ansi_blue',    `\e[34'`ifelse($1,bold,`\s1')'`m')
define(`ansi_magenta', `\e[35'`ifelse($1,bold,`\s1')'`m')
define(`ansi_cyan',    `\e[36'`ifelse($1,bold,`\s1')'`m')
define(`ansi_white',   `\e[37'`ifelse($1,bold,`\s1')'`m')
define(`ansi_reset',   `\e[0m')

;---------------------------------
; Wrap a string in ANSI color codes
; The resulting string contains backslash escapes that must be processed
; by cstr() or estr().
; Args:
;   Arg1: String to modify
;   Arg2: ANSI color name (black, red, green, yellow, blue, magenta, cyan, and white)
;   Arg3: Optional argument "bold" will select bold text
; Example:
;   colorize(`foobar', blue) is equivalent to ansi_blue`foobar'ansi_reset
;   colorize(`foobar', red, bold) is equivalent to ansi_red(bold)`foobar'ansi_reset
define(`colorize', `ifelse($3,bold,`ansi_$2(bold)',ansi_$2)$1`'ansi_reset')

;=============== ARITHMETIC OPERATIONS ===============
;=====================================================

;---------------------------------
;2s complement negation
;Args:
;  Arg1: Register to negate
;Result:
;  Result is in the same register
define(`negate', `xor $1, FF  ; Negate
add $1, 01')

;---------------------------------
;Logical not
;Args:
;  Arg1: Register to invert
;Result:
;  Result is in the same register
define(`not', `xor $1, FF  ; Not')

;---------------------------------
; Absolute value
; Args:
;   Arg1: Register to make positive
; Result is in the same register
define(`abs', `if($1 & 0x80, `negate($1)')')

;---------------------------------
; Sign extension
; Args:
;   Arg1: Register to extend sign into
;   Arg2: Register to test for sign bit
define(`signex', `if($2 & 0x80, `load $1, FF `;' Sign extend', `load $1, 00')')

;---------------------------------
; Determine if argument is a number in m4 syntax
; Note:
;   There must be no leading or trailing whitespace
; Args:
;   Arg1: String to check
; Returns:
;   1 for true 0 for false
define(`isnum', `ifelse(regexp($1, `^-?\(0[xXbB][0-9a-fA-F]+\|[0-9]+\)$'),0,1,0)')

;---------------------------------
; Signed compare
; Args:
;   Arg1: Register for left side of comparison
;   Arg2: Register or constant for right side of comparison
;         Constant is a number in m4 syntax and cannot be a named constant
;
; Carry flag is set in accordance with signed relation
; Zero flag is indeterminate. Use normal compare instruction for == and !=
; Note:
;   This calls the setcy() macro and depends on the temp reg
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
; Args:
;   Arg1: Subroutine name
;   Arg2: Multiplicand
;   Arg3: Multiplier
;   Arg4, Arg5: Result MSB, LSB
;   Arg6: Optional preamble code block. Also supresses return statement if present
;
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it.
; Example:
;   use_multiply8x8(mul8, s0, s1, s3, s2) ; (s3, s2) = s0 * s1
;   load s0, 04
;   load s1, 05
;   call mul8
define(`use_multiply8x8', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
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
            ; PRAGMA function end
            ')popvars')


;---------------------------------
; Signed Multiply 8 x 8 subroutine
; Same arguments as use_multiply8x8
define(`use_multiply8x8s', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
            $1:  ; ($4, $5) = $2 * $3 (signed)
            $6
            vars(`$2 is _cand', `$3 is _plier', `$4 is _msb := 0', `$5 is _lsb := 0', `_tempreg is _mask := 1')
$1_loop:    test _plier, _mask
            jump z, $1_no_add
            add _msb, _cand
$1_no_add:  sra _msb
            sra _lsb
            sl0 _mask
            jump nz, $1_loop
            test _plier, 80 ; Add correction for negative multiplier
            jump z, $1_no_correct1
            sub  _msb, _cand
$1_no_correct1:            
            test _cand, 80  ; Add correction for negative multiplicand
            jump z, $1_no_correct2
            sub _msb, _plier
$1_no_correct2: ifelse(`$6',,`
            return
            ; PRAGMA function end
            ')popvars')
            
;---------------------------------
; SignedxUnsigned Multiply 8 x 8 subroutine
; Same arguments as use_multiply8x8
define(`use_multiply8x8su', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
            $1:  ; ($4, $5) = $2 * $3 (signed)
            $6
            vars(`$2 is _cand', `$3 is _plier', `$4 is _msb := 0', `$5 is _lsb := 0', `_tempreg is _mask := 1')
$1_loop:    test _plier, _mask
            jump z, $1_no_add
            add _msb, _cand
$1_no_add:  sra _msb
            sra _lsb
            sl0 _mask
            jump nz, $1_loop
            test _cand, 80 ; Add correction for negative multiplicand
            jump z, $1_no_correct
            sub _msb, _plier
$1_no_correct: ifelse(`$6',,`
            return
            ; PRAGMA function end
            ')popvars')


;---------------------------------
; Divide 8 / 8 subroutine. Implements truncating division
; Args:
;   Arg1: Subroutine name
;   Arg2: Dividend
;   Arg3: Divisor
;   Arg4: Quotient
;   Arg5: Remainder
;   Arg6: Optional preamble code block. Also supresses return statement if present
;
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it.
; Example:
;   use_divide8x8(div8, s0, s1, s2, s3)
;   load s0, 20'd
;   load s1, 4'd
;   call div8
define(`use_divide8x8', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
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
            ; PRAGMA function end
            ')popvars')

;---------------------------------
; Signed Divide 8 x 8 subroutine
; Same arguments as use_divide8x8
define(`use_divide8x8s', `; PRAGMA function $1 [$2, $3 return $4, $5] begin
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
            ; PRAGMA function end
            ')popvars')


;---------------------------------
; Divide 16 / 8 subroutine. Implements truncating division
; Args:
;   Arg1:       Subroutine name
;   Arg2, Arg3: Dividend MSB,LSB
;   Arg4:       Divisor
;   Arg5, Arg6: Quotient MSB, LSB
;   Arg7:       Remainder
;   Arg8:       Optional preamble code block. Also supresses return statement if present
;
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it. The MSB of the dividend is destroyed
; Example:
;   use_divide16x8(div16, s0,s1, s2, s3,s4, s5)
;   load s0, 20'd
;   load s1, 4'd
;   call div16
define(`use_divide16x8', `; PRAGMA function $1 [$2, $3, $4 return $5, $6, $7] begin
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
            load _dend_m, 00    ; Using _dend_m as temporary upper byte of remainder
$1_loop2:   test _dend_l, _mask
            sla _rem
            sla _dend_m
            sl0 _quo_l
            sla _quo_m
            compare _dend_m, 00
            jump nz, $1_do_sub2
            compare _rem, _visor
            jump c, $1_no_sub2
$1_do_sub2:
            sub _rem, _visor
            subcy _dend_m, 00
            add _quo_l, 01      ; Dont need addcy since LSB is guaranteed to be 0
$1_no_sub2: sr0 _mask
            jump nz, $1_loop2 ifelse(`$8',,`
            return
            ; PRAGMA function end
            ')popvars')

;---------------------------------
; Signed divide 16 / 8 subroutine. Implements truncating division
; Args:
;   Arg1:       Subroutine name
;   Arg2, Arg3: Dividend MSB,LSB
;   Arg4:       Divisor
;   Arg5, Arg6: Quotient MSB, LSB
;   Arg7:       Remainder
;   Arg8:       Optional preamble code block. Also supresses return statement if present
;
; The temp register is overwritten. It is sE by default. Call use_tempreg(reg_nam)
; before invoking this macro to change it. The MSB of the dividend is destroyed
; Example:
;   use_divide16x8s(div16, s0,s1, s2, s3,s4, s5)
;   load s0, 20'd
;   load s1, 4'd
;   call div16
define(`use_divide16x8s', `; PRAGMA function $1 [$2, $3, $4 return $5, $6, $7] begin
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
            load _dend_m, 00    ; Using _dend_m as temporary upper byte of remainder
$1_loop2:   test _dend_l, _mask
            sla _rem
            sla _dend_m
            sl0 _quo_l
            sla _quo_m
            compare _dend_m, 00
            jump nz, $1_do_sub2
            compare _rem, _visor
            jump c, $1_no_sub2
$1_do_sub2:            
            sub _rem, _visor
            subcy _dend_m, 00
            add _quo_l, 01    ; Dont need addcy since LSB is guaranteed to be 0
$1_no_sub2: sr0 _mask
            jump nz, $1_loop2
            pop(_tempreg)
            ; Fix signs
            if(_tempreg & 0x80, `negate16(_quo_m, _quo_l)')
            if(_tempreg & 0x01, `negate(_rem)') ifelse(`$8',,`
            return
            ; PRAGMA function end
            ')popvars')


;---------------------------------
; Multiply 8 x constant subroutine with 16-bit result
; Args:
;   Arg1: Subroutine name
;   Arg2: Multiplicand
;   Arg3: Constant multiplier (can be wider than 8-bits)
;   Arg4, Arg5: Result MSB, LSB
; Example:
;   use_multiply8xk(mul8k5, s0, 5, s5, s4)  ; (s5, s4) = s0 * 5
;   load s0, 7'd
;   call mul8k5
define(`use_multiply8xk', `; PRAGMA function $1 [$2 return $4, $5] begin
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
; Args:
;   Arg1: Subroutine name
;   Arg2: Multiplicand
;   Arg3: Constant multiplier
;   Arg4: Result byte
; Example:
;   use_multiply8xk_small(mul8k5, s0, 5, s4)  ; s4 = s0 * 5
;   load s0, 7'd
;   call mul8k5
define(`use_multiply8xk_small', `; PRAGMA function $1 [$2 return $4] begin
$1: ; $4 = $2 * ($3)
load $4, 00
_genmul8xk_small($2, eval($3,2), $4)return
; PRAGMA function end')

define(`_genmul8xk_small', `ifelse($2,,,`ifelse(eval(substr($2,0,1) == 1),1,`add $3, $1',`dnl')
ifelse(eval(len(`$2')>1),1,`sl0 $3  ; $2')
$0($1, substr(`$2', 1), $3)')')


;---------------------------------
; Divide 8 / constant subroutine with 8-bit result
; Args:
;   Arg1: Subroutine name
;   Arg2: Dividend
;   Arg3: Constant divisor (can be wider than 8-bits)
;   Arg4: Result quotient
; Example:
;   use_divide8xk(div8k5, s0, 5, s4)  ; s4 = s0 / 5
;   load s0, 25'd
;   call div8k5
define(`use_divide8xk', `; PRAGMA function $1 [$2 return $4, $5] begin
$1:  ; $4 = $2 / ($3)
load $4, 00
load _tempreg, 00
_genmul8xk($2, eval(2**8 / ($3) + 1,2), $4, _tempreg) return
; PRAGMA function end')



;=============== EXPRESSIONS ===============
;===========================================

;---------------------------------
; Expression evaluators
; This is a family of macros that provide implementation of arithmetic expressions from
; compact input notation
; Args:
;   Arg1: Register assignment expression of the form:
;         sN := <val> op <val> [op <val>]*
;         val is one of:
;           register
;           literal expression (with no internal spaces)
;           sp[addr] scratchpad adddress
;           spi[reg] indirect scratchpad address in register
;         op is one of:
;           + - * /      add, subtract, multiply, divide
;           & | ^        and, or, xor
;           << >>        shift left, shift right (0-filled MSB)
;           =:           reverse assignment to register or scratchpad
;
;   ** Operations are evaluated left to right with *no precedence*
; Example:
;   expr(s0 := s1 + s2 - s3 >> 4 =: sp[M_value])
;     Arithmetic is performed on s0 and the result is stored in scratchpad at M_value
;     s0 <= s1, s0 <= s0 + s2, s0 <= s0 - s3, s0 <= s0 >> 4, sp[M_value] <= s0
;
;   expr(s1 := s4 + (28*4-1))
;     s1 <= s4, s1 <= s1 + 111   Constant expressions must have no spaces

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

define(`_expr_start', `pushdef(`_exreg', $2)'dnl
`ifelse($3,:=,,`errmsg(Missing assignment operator in expression)')'dnl
``;' Expression:' _exstr
`ifelse($2,$4,,`load $2, evalx($4,16,2)')'
`ifelse($1,u,`_expr_ops(shift(shift(shift(shift($@)))))',`_exprs_ops(shift(shift(shift(shift($@)))))')'dnl
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
; Same arguments as expr except that the target
; is a 16-bit register pair and the first term can be
; a 16-bit pair or a 16-bit literal
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
define(`_expr2_ops', `ifelse(`$1',,,`
_expr2_binary($1, evalx($2)) dnl
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
define(`_expr2s_ops', `ifelse(`$1',,,`
_expr2s_binary($1, evalx($2)) dnl
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
; Same arguments as expr except that all terms
; are 16-bit register pairs or 16-bit literals
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
; Args:
;   Arg1: Multiplicand (default is s8)
;   Arg2: Multiplier   (default is s9)
;   Arg3, Arg4: Internal result MSB, LSB (default is sa,sb) preserved on stack
; Returns:
;   The result is copied to Arg1, Arg2
define(`use_expr_mul', `define(`_mul_init',1)'dnl
 `ifelse($#,0,`use_multiply8x8(`expr_mul8', s8, s9, sa, sb,`push(sa,sb)')
  define(`_mul8_msb',`s8') define(`_mul8_lsb',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`use_multiply8x8(`expr_mul8', $1, $2, $3, $4,`push($3,$4)')
  define(`_mul8_msb',`$1') define(`_mul8_lsb',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 multiply keeping only lower 8-bits of result
; Args:
;   Arg1: Multiplicand, Arg2: Multiplier
define(`_expr_mul8', `_initcheck(`_mul_init',`Unsigned multiply `not' initialized. Call `use_expr_mul()'')'dnl
`load _mul8_msb, $1
load _mul8_lsb, $2
call expr_mul8
load $1, _mul8_lsb
')

; 16x8 multiply keeping only lower 16-bits of result
; Args:
;   Arg1-Arg2: MSB, LSB Multiplicand, Arg3: Multiplier
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
; Args:
;   Arg1: Multiplicand (default is s8)
;   Arg2: Multiplier   (default is s9)
;   Arg3, Arg4: Internal result MSB, LSB (default is sa,sb) preserved on stack
; Returns:
;   The result is copied to Arg1, Arg2
define(`use_expr_muls', `define(`_muls_init',1)'dnl
 `ifelse($#,0,`use_multiply8x8s(`expr_mul8s', s8, s9, sa, sb,`push(sa,sb)')
  define(`_mul8s_msb',`s8') define(`_mul8s_lsb',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`use_multiply8x8s(`expr_mul8s', $1, $2, $3, $4,`push($3,$4)')
  define(`_mul8s_msb',`$1') define(`_mul8s_lsb',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 signed multiply keeping only lower 8-bits of result
; Args:
;   Arg1: Multiplicand, Arg2: Multiplier
define(`_expr_mul8s', `_initcheck(`_muls_init',`Signed multiply `not' initialized. Call `use_expr_muls()'')'dnl
`load _mul8s_msb, $1
load _mul8s_lsb, $2
call expr_mul8s
load $1, _mul8s_lsb
')

;---------------------------------
; Configure signedxunsigned (16x8) multiplication for expressions
; All arguments are optional
; Args:
;   Arg1: Multiplicand (default is s8)
;   Arg2: Multiplier   (default is s9)
;   Arg3, Arg4: Internal result MSB, LSB (default is sa,sb) preserved on stack
; Returns:
;   The result is copied to Arg1, Arg2
define(`use_expr_mulsu', `define(`_mulsu_init',1)'dnl
 `ifelse($#,0,`use_multiply8x8su(`expr_mul8su', s8, s9, sa, sb,`push(sa,sb)')
  define(`_mul8su_msb',`s8') define(`_mul8su_lsb',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`use_multiply8x8su(`expr_mul8su', $1, $2, $3, $4,`push($3,$4)')
  define(`_mul8su_msb',`$1') define(`_mul8su_lsb',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')


; 16x8 signed multiply keeping only lower 16-bits of result
; Args:
;   Arg1-Arg2: MSB, LSB Multiplicand, Arg3: Multiplier
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
; Args:
;   Arg1: Dividend (default is s8)
;   Arg2: Divisor  (default is s9)
;   Arg3, Arg4: Internal result Quotient, Remainder (default is sa,sb) preserved on stack
; Returns:
;   The result is copied to Arg1, Arg2
define(`use_expr_div', `define(`_div_init',1)'dnl
 `ifelse($#,0,`use_divide8x8(`expr_div8', s8, s9, sa, sb,`push(sa,sb)')
  define(`_div8_quo',`s8') define(`_div8_rem',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`use_divide8x8(`expr_div8', $1, $2, $3, $4,`push($3,$4)')
  define(`_div8_quo',`$1') define(`_div8_rem',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 divide keeping only quotient
; Args:
;   Arg1: Dividend, Arg2: Divisor
define(`_expr_div8', `_initcheck(`_div_init',`Unsigned divide `not' initialized. Call `use_expr_div()'')'dnl
`load _div8_quo, $1
load _div8_rem, $2
call expr_div8
load $1, _div8_quo
')

;---------------------------------
; Configure signed division for expressions
; All arguments are optional
; Args:
;   Arg1: Dividend (default is s8)
;   Arg2: Divisor  (default is s9)
;   Arg3, Arg4: Internal result Quotient, Remainder (default is sa,sb) preserved on stack
; Returns:
;   The result is copied to Arg1, Arg2
define(`use_expr_divs', `define(`_divs_init',1)'dnl
 `ifelse($#,0,`use_divide8x8s(`expr_div8s', s8, s9, sa, sb,`push(sa,sb)')
  define(`_div8s_quo',`s8') define(`_div8s_rem',`s9')dnl
  load s8, sa
  load s9, sb
  pop(sa,sb)
  return
  ; PRAGMA function end',`use_divide8x8s(`expr_div8s', $1, $2, $3, $4,`push($3,$4)')
  define(`_div8s_quo',`$1') define(`_div8s_rem',`$2')dnl
  load $1, $3
  load $2, $4
  pop($3,$4)
  return
  ; PRAGMA function end')')

; 8x8 divide keeping only quotient
; Args:
;   Arg1: Dividend, Arg2: Divisor
define(`_expr_div8s', `_initcheck(`_divs_init',`Signed divide `not' initialized. Call `use_expr_divs()'')'dnl
`load _div8s_quo, $1
load _div8s_rem, $2
call expr_div8s
load $1, _div8s_quo
')



;---------------------------------
; Configure unsigned 16x8 division for expressions
; All arguments are optional
; Args:
;   Arg1,Arg2: Dividend (default is s7,s8)
;   Arg3:      Divisor  (default is s9)
;   Arg4,Arg5, Arg6: Internal result Quotient, Remainder (default is sa,sb, sc) preserved on stack
;
; The result is copied to Arg1,Arg2, Arg3
define(`use_expr_div16', `define(`_div16_init',1)'dnl
 `ifelse($#,0,`use_divide16x8(`expr_div16', s7,s8, s9, sa,sb, sc, `push(sa,sb, sc)')
  define(`_div16_quo',`s7,s8') define(`_div16_rem',`s9')dnl
  load s7, sa
  load s8, sb
  load s9, sc
  pop(sa,sb, sc)
  return
  ; PRAGMA function end',`use_divide16x8(`expr_div16', $1,$2, $3, $4,$5, $6, `push($4,$5, $6)')
  define(`_div16_quo',`$1,$2') define(`_div16_rem',`$3')dnl
  load $1, $4
  load $2, $5
  load $3, $6
  pop($4,$5, $6)
  return
  ; PRAGMA function end')')

; 16x8 divide keeping only quotient
; Args:
;   Arg1,Arg2: Dividend, Arg3: Divisor
define(`_expr2_div8', `_initcheck(`_div16_init',`Unsigned 16x8 divide `not' initialized. Call `use_expr_div16()'')'dnl
`load16(_div16_quo, $1,$2)
load _div16_rem, $3
call expr_div16
load16($1,$2, _div16_quo)
')


;---------------------------------
; Configure signed 16x8 division for expressions
; All arguments are optional
; Args:
;   Arg1,Arg2: Dividend (default is s7,s8)
;   Arg3:      Divisor  (default is s9)
;   Arg4,Arg5, Arg6: Internal result Quotient, Remainder (default is sa,sb, sc) preserved on stack
;
; The result is copied to Arg1,Arg2, Arg3
define(`use_expr_div16s', `define(`_div16s_init',1)'dnl
 `ifelse($#,0,`use_divide16x8s(`expr_div16s', s7,s8, s9, sa,sb, sc, `push(sa,sb, sc)')
  define(`_div16s_quo',`s7,s8') define(`_div16s_rem',`s9')dnl
  load s7, sa
  load s8, sb
  load s9, sc
  pop(sa,sb, sc)
  return
  ; PRAGMA function end',`use_divide16x8s(`expr_div16s', $1,$2, $3, $4,$5, $6, `push($4,$5, $6)')
  define(`_div16s_quo',`$1,$2') define(`_div16s_rem',`$3')dnl
  load $1, $4
  load $2, $5
  load $3, $6
  pop($4,$5, $6)
  return
  ; PRAGMA function end')')

; 16x8 signed divide keeping only quotient
; Args:
;   Arg1,Arg2: Dividend, Arg3: Divisor
define(`_expr2_div8s', `_initcheck(`_div16s_init',`Signed 16x8 divide `not' initialized. Call `use_expr_div16s()'')'dnl
`load16(_div16s_quo, $1,$2)
load _div16s_rem, $3
call expr_div16s
load16($1,$2, _div16s_quo)
')


;=============== 16-bit ARITHMETIC AND LOGICAL OPERATIONS ===============
;========================================================================

;---------------------------------
; Create a virtual 16-bit register
; The defined name can be used in place of the MSB, LSB pairs used in other 16-bit macros
; Args:
;   Arg1: Name of virtual register
;   Arg2: MSB register
;   Arg3: LSB register
; Example:
;   reg16(rx, s1, s0) ; rx = (s1, s0)
;   reg16(ry, s5, s4) ; ry = (s5, s4)
;   add16(rx, ry)     ; rx = rx + ry
;   add16(rx, s3, s2) ; rx = rx + (s3, s2)
define(`reg16', `ifelse($#,3,`define(`$1', `$2, $3')',`errmsg(`Wrong number of arguments to `reg16'. Is name quoted?')')')

;---------------------------------
; Create a constant for 16-bit memory and port addresses
; Named constants with "_H" and "_L" suffixes are created and usable for
; byte access.
; Args:
;   Arg1: Name of constant
;   Arg2: MSB address
;   Arg3: LSB address
; Example:
;   mem16(M_DATA, 0x05, 0x04) ; Allocate scratchpad 05, 04 for use as M_DATA
define(`mem16', `const($1_H, evalc($2, 16, 2))
const($1_L, evalc($3, 16, 2))'
`reg16($1, $1_H, $1_L)')

;---------------------------------
; Get the upper and lower registers from a reg16 definition
; Args:
;   Arg1, Arg2: MSB, LSB of 16-bit register
; Example:
;   reg16(rx, s5, s4)
;   load s1, regupper(rx) ; load upper byte from s5
;   load s1, reglower(rx) ; load lower byte from s4
define(`regupper', $1)
define(`reglower', $2)

; Split a 16-bit constant into upper and lower bytes
define(`constupper', `eval(((const2m4($1)) >> 8) & 0xFF)')
define(`constlower', `eval((const2m4($1)) & 0xFF)')

;---------------------------------
; 16-bit load
; Args:
;   Arg1, Arg2: MSB, LSB destination
; 3 arguments: load constant
;   Arg3: Decimal constant or expression
; 4 arguments: load register
;   Arg3, Arg4: MSB, LSB source
; Example:
;   load16(s1, s0, 2014)
;   load16(s1, s0, 200 * 11 + 5)
;   load16(s1, s0, s3, s2)
;   load16(rx, ry) ; Assuming rx and ry are reg16 definitions
define(`load16', `ifelse($#,4,`_load16($@)',`_load16k($@)')')

define(`_load16', `load $1, $3
load $2, $4')

define(`_load16k', `load $1, eval(constupper($3), 16, 2)  ; $3
load $2, eval(constlower($3), 16, 2)')

;---------------------------------
; Load a 16-bit address from a label
; Args:
;   Arg1, Arg2: MSB, LSB destination
;   Arg3: address label
; Example:
;   my_func: return
;   loadaddr(s1,s0, my_func)
changequote(<!,!>)
define(<!loadaddr!>, <!changequote(<!,!>)<!!>load $1, $3'upper
load $2, $3'lower<!!>changequote!>)
changequote

;---------------------------------
; 16-bit addition and subtraction
; Args:
;   Arg1, Arg2: MSB1, LSB1
; 3 arguments: add from constant
;   Arg3: Decimal constant or expression
; 4 arguments: add from register
;   Arg3, Arg4: MSB2, LSB2
; Result in Arg1, Arg2
; Example:
;   add16(s1,s0, s3,s2)
;   add16(rx, ry)
;   sub16(rx, 2000)
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
; Args:
;   Arg1, Arg2: MSB, LSB to negate
; Result in Arg1, Arg2
; Example:
;   negate16(s1, s0)
define(`negate16', `xor $1, FF  ; Negate 16-bit
xor $2, FF
add $2, 01
addcy $1, 00')

;---------------------------------
; 16-bit logical not
; Args:
;   Arg1, Arg2: MSB, LSB to invert
; Result in Arg1, Arg2
; Example:
;   not16(s1, s0)
define(`not16', `xor $1, FF  ; Not 16-bit
xor $2, FF')

;---------------------------------
; 16-bit absolute value
; Args:
;   Arg1, Arg2: MSB, LSB to make positive
; Result is in Arg1, Arg2
; Example:
;   abs16(s1, s0)
define(`abs16', `if($1 & 0x80, `negate16($1, $2)')')


;---------------------------------
; 16-bit and
; Args:
;   Arg1, Arg2: MSB1, LSB1
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
; Args:
;   Arg1, Arg2: MSB1, LSB1
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
; Args:
;   Arg1, Arg2: MSB1, LSB1
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
; Args:
;   Arg1, Arg2: MSB1, LSB1
; 3 arguments: test with constant
;   Arg3: Decimal constant or expression
; 4 arguments: test with register
;   Arg3, Arg4: MSB2, LSB2
; Note:
;   On PicoBlaze-3 only the Z flag is set properly
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
; Args:
;   Arg1, Arg2: MSB1, LSB1
;   Arg3, Arg4: MSB2, LSB2
; Note:
;   On PicoBlaze-3 only the Z flag is correct
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

define(`sla_16', `repeat(`_sla_16($1, $2)', eval(const2m4($3)))')
define(`sra_16', `repeat(`_sra_16($1, $2)', eval(const2m4($3)))')


define(`sl0_16', `ifelse(eval(const2m4($3) > 8),1,`load $1, $2
load $2, 00
repeat(`sl0 $1', eval(const2m4($3) - 8))',dnl
`repeat(`_sl0_16($1, $2)', eval(const2m4($3)))')')

define(`sl1_16', `ifelse(eval(const2m4($3) > 8),1,`load $1, $2
load $2, FF
repeat(`sl1 $1', eval(const2m4($3) - 8))',dnl
`repeat(`_sl1_16($1, $2)', eval(const2m4($3)))')')

define(`slx_16', `ifelse(eval(const2m4($3) > 8),1,`load $1, $2
if($2 & 0x01, `load $2, FF', `load $2, 00')
repeat(`_slx_16($1, $2)', eval(const2m4($3) - 8))',dnl
`repeat(`_slx_16($1, $2)', eval(const2m4($3)))')')


define(`sr0_16', `ifelse(eval(const2m4($3) > 8),1,`load $2, $1
load $1, 00
repeat(`sr0 $2', eval(const2m4($3) - 8))',dnl
`repeat(`_sr0_16($1, $2)', eval(const2m4($3)))')')

define(`sr1_16', `ifelse(eval(const2m4($3) > 8),1,`load $2, $1
load $1, FF
repeat(`sr1 $2', eval(const2m4($3) - 8))',dnl
`repeat(`_sr1_16($1, $2)', eval(const2m4($3)))')')

define(`srx_16', `ifelse(eval(const2m4($3) > 8),1,`load $2, $1
if($1 & 0x80, `load $1, FF', `load $1, 00')
repeat(`_srx_16($1, $2)', eval(const2m4($3) - 8))',dnl
`repeat(`_srx_16($1, $2)', eval(const2m4($3)))')')


define(`_rl16', `pushdef(`_rnc',uniqlabel(RL16_))'`sl0 $2
sla $1
jump nc, _rnc
or $2, 01
_rnc:
'`popdef(`_rnc')')

define(`rl16', `ifelse(eval(const2m4($3) > 8),1,`swap($1,$2)
repeat(`_rl16($1, $2)', eval(const2m4($3) - 8))',dnl
`repeat(`_rl16($1, $2)', eval(const2m4($3)))')')


define(`_rr16', `pushdef(`_rnc',uniqlabel(RR16_))'`sr0 $1
sra $2
jump nc, _rnc
or $1, 80
_rnc:
'`popdef(`_rnc')')

define(`rr16', `ifelse(eval(const2m4($3) > 8),1,`swap($1,$2)
repeat(`_rr16($1, $2)', eval(const2m4($3) - 8))',dnl
`repeat(`_rr16($1, $2)', eval(const2m4($3)))')')



;=============== 16-bit I/O OPERATIONS ===============
;=====================================================

;---------------------------------
; 16-bit fetch
; Args:
;   Arg1, Arg2: MSB, LSB of target
; 3 arguments: Fetch from indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of fetch16()
; 4 arguments: MSB, LSB addresses to fetch from
;   Arg3, Arg4: MSB, LSB of source
; Result in Arg1, Arg2
; Example:
;   constant M_ACCUM_L, 1a
;   constant M_ACCUM_H, 1b
;   reg16(M_ACCUM, M_ACCUM_H, M_ACCUM_L)
;   reg16(rx, s4, s3)
;   fetch16(rx, M_ACCUM)  ; Fetch direct from address
;   load s0, M_ACCUM_L
;   fetch16(rx, s0)       ; Fetch from indirect pointer
;   fetch16(rx, s0)       ; Fetch next word
define(`fetch16', `ifelse($#,4,`_fetch16($@)',`_fetch16i($@)')')

define(`_fetch16', `fetch $2, $4
fetch $1, $3')

define(`_fetch16i', `fetch $2, ($3)
add $3, 01
fetch $1, ($3)
add $3, 01')


;---------------------------------
; 16-bit store
; Args:
;   Arg1, Arg2: MSB, LSB of source
; 3 arguments: Store to indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of store16()
; 4 arguments: MSB, LSB addresses to store to
;   Arg3, Arg4: MSB, LSB of target (LSB sent first)
; Example:
;   load16(rx, 2014)
;   store16(rx, M_ACCUM)  ; Store direct to address
;   load s0, M_ACCUM_L
;   store16(rx, s0)       ; Store to indirect pointer
;   store16(rx, s0)       ; Store next word
define(`store16', `ifelse($#,4,`_store16($@)',`_store16i($@)')')

define(`_store16', `store $2, $4
store $1, $3')

define(`_store16i', `store $2, ($3)
add $3, 01
store $1, ($3)
add $3, 01')


;---------------------------------
; 16-bit input
; Args:
;   Arg1, Arg2: MSB, LSB of target
; 3 arguments: Input from indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of input16()
; 4 arguments: MSB, LSB port addresses to input from
;   Arg3, Arg4: MSB, LSB of source port
; Result in Arg1, Arg2
; Example:
;   constant P_ACCUM_L, 1a
;   constant P_ACCUM_H, 1b
;   reg16(P_ACCUM, P_ACCUM_H, P_ACCUM_L)
;   reg16(rx, s4, s3)
;   input16(rx, P_ACCUM)  ; Input direct from address
;   load s0, P_ACCUM_L
;   input16(rx, s0)       ; Input from indirect pointer
;   input16(rx, s0)       ; Input next word
define(`input16', `ifelse($#,4,`_input16($@)',`_input16i($@)')')

define(`_input16', `input $2, $4
input $1, $3')

define(`_input16i', `input $2, ($3)
add $3, 01
input $1, ($3)
add $3, 01')


;---------------------------------
; 16-bit output
; Args:
;   Arg1, Arg2: MSB, LSB of source
; 3 arguments: Output to indirect register
;   Arg3: Register with pointer to low byte
;         Incremented twice to permit sequential use of output16()
; 4 arguments: MSB, LSB port addresses to output to
;   Arg3, Arg4: MSB, LSB of target (LSB sent first)
; Example:
;   load16(rx, 2014)
;   output16(rx, P_ACCUM)  ; Output direct to port address
;   load s0, P_ACCUM_L
;   output16(rx, s0)       ; Output to indirect pointer
;   output16(rx, s0)       ; Output next word
define(`output16', `ifelse($#,4,`_output16($@)',`_output16i($@)')')

define(`_output16', `output $2, $4
output $1, $3')

define(`_output16i', `output $2, ($3)
add $3, 01
output $1, ($3)
add $3, 01')



;=============== RANDOM NUMBER GENERATORS ===============
;========================================================

;---------------------------------
; 8-bit pseudo-random generator
; Based on George Marsaglia's xorshift algorithm. Generates a full cycle of 255 values.
; Expands to 11 instructions.
; Args:
;   Arg1: label to use for random function
;   Arg2: Random state variable (Initialize this with a non-zero seed)
;
; The common temp register is destructively modified.
; Example:
;   namereg s8, RS
;   use_random8(random, RS)
;   ...
;   load RS, 5A   ; Seed the PRNG (Use an external entropy source like an ADC in real life)
;   call random
define(`use_random8', `; PRAGMA function $1 [$2 return $2] begin
  $1:    ; 8-bit PRNG with state in $2
  ; Shift left 1
  load _tempreg, $2
  sl0 _tempreg
  xor $2, _tempreg
  ; Shift right 1
  load _tempreg, $2
  sr0 _tempreg
  xor $2, _tempreg
  ; Shift left 2
  load _tempreg, $2
  sl0 _tempreg
  sl0 _tempreg
  xor $2, _tempreg
  return
  ;PRAGMA function end')

;---------------------------------
; 16-bit pseudo-random generator
; Based on George Marsaglia's xorshift algorithm. Generates a full cycle of 65535 values.
; Expands to 23 instructions.
; Args:
;   Arg1:       label to use for random function
;   Arg2, Arg3: MSB, LSB of 16-bit random state variable (Initialize this with a non-zero seed)
;   Arg4, Arg5: MSB, LSB of a 16-bit temp register
; Example:
;   reg16(RS, s0,s1)
;   use_random16(random, RS, sA, sB)
;   ...
;   load16(RS, 0x1234)   ; Seed the PRNG (Use an external entropy source like an ADC in real life)
;   call random
define(`use_random16', `; PRAGMA function $1 [$2, $3 return $2, $3] begin
  $1:    ; 16-bit PRNG with state in $2,$3
  ; Shift left 1
  load16($4, $5, $2, $3)
  sl0_16($4, $5, 1)       
  xor16($2, $3, $4, $5)
  ; Shift right 1
  load16($4, $5, $2, $3)
  sr0_16($4, $5, 1)
  xor16($2, $3, $4, $5)
  ; Shift left 14 (copy lower to upper byte and shift left 14-8=6)
  load $4, $3
  load $5, 00
  sl0($4, 6)
  xor16($2, $3, $4, $5)
  return
  ; PRAGMA function end')


;---------------------------------
; Generate a 16-bit checksum constant from a string using the BSD algorithm.
; This does not dynamically compute a hash from variable data. It can be used to seed a PRNG
; from a build time string like a timestamp.
; Args:
;   Arg1: String to compute hash over
; Example:
;   strhash(`Hello world')  ; Expands to 27566
;
;   reg16(RS, s0, s1)
;   load16(RS, strhash(DATE_STAMP TIME_STAMP)) ; Seed the PRNG with the build time
;   call random16
define(`strhash', `_strhash(asciiord($1))')

define(`_strhash', `ifelse(`$1',,0,`pushdef(`_SH_CS',$0(shift($@)))'`eval(((_SH_CS >> 1) + ((_SH_CS & 0x01) << 15) + $1) & 0xFFFF)')'`popdef(`_SH_CS')')



;=============== SCRATCHPAD ARRAY OPERATIONS ===============
;===========================================================

;---------------------------------
;Generate a function to copy an array in scratchpad memory
;Args:
;  Arg1: Name of function to generate
;  Arg2: Register for first function argument,
;        the scratchpad address of the source array
;  Arg3: Register for destination address
;  Arg4: Register for number of bytes to copy
;Returns:
;  The generated function has no return value. All registers are
;  preserved on the stack.
;Example:
;  use_memcopy(memcopy, s0, s1, s2)
;  load s0, 20 ; Source address
;  load s1, 30 ; Dest address
;  load s2, 05 ; Copy 5 bytes
;  call memcopy
define(`use_memcopy', `; PRAGMA function $1 [$2, $3, $4] begin
            $1:  ; Copy count ($4) bytes from src ($2) to dest ($3) in scratchpad
              vars(`$2 is _src', `$3 is _dest', `$4 is _count')
              push(_src, _dest, _count)
              add _count, _src
              dowhile(`_src < _count', `
                fetch _tempreg, (_src)
                store _tempreg, (_dest)
                add _src, 01
                add _dest, 01')
              pop(_src, _dest, _count)
              return
              ; PRAGMA function end
              popvars')

;---------------------------------
;Generate a function to set an array in scratchpad memory
;Args:
;  Arg1: Name of function to generate
;  Arg2: Register for first function argument,
;        the scratchpad address of the destination array
;  Arg3: Register for number of bytes to copy
;  Arg4: Register for value to set array bytes to
;Returns:
;  The generated function has no return value. All registers are
;  preserved on the stack.
;Example:
;  use_memset(memset, s0, s1, s2)
;  load s0, 20 ; Destination address
;  load s1, 05 ; Copy 5 bytes
;  load s2, 00 ; Set all bytes to 0
;  call memset
define(`use_memset', `; PRAGMA function $1 [$2, $3, $4] begin
            $1:  ; Write count ($3) bytes of value ($4) to dest ($2) in scratchpad
              vars(`$2 is _dest', `$3 is _count', `$4 is _value')
              push(_count, _dest)
              add _count, _dest
              dowhile(`_dest < _count', `
                store _value, (_dest)
                add _dest, 01')
              pop(_count, _dest)
              return            
              ; PRAGMA function end
              popvars')

;---------------------------------
;Generate a function to write an array of bytes to an output port
;Args:
;  Arg1: Name of function to generate
;  Arg2: Register for first function argument,
;        the scratchpad address of the source array
;  Arg3: Register for number of bytes to write
;  Arg4: Port address to write. This is a fixed value that can't
;        be changed at runtime
;Returns:
;  The generated function has no return value. All registers are
;  preserved on the stack.
;Example:
;  constant ConsolePort, FE
;  use_memwrite(memwrite, s0, s1, ConsolePort)
;  load s0, 20 ; Source address
;  load s1, 05 ; Write 5 bytes
;  call memwrite
define(`use_memwrite', `; PRAGMA function $1 [$2, $3] begin
            $1: ; Write an array in memory to port $4
              vars(`$2 is _src', `$3 is _count')
              push(_src, _count)
              add _count, _src
              dowhile(`_src < _count', `
                fetch _tempreg, (_src)
                output _tempreg, evalh($4)
                add _src, 01')
              pop(_src, _count)
              return
              ; PRAGMA function end
              popvars')


;---------------------------------
;Generate a function to write an array of BCD coded bytes to an output port
;Args:
;  Arg1: Name of function to generate
;  Arg2: Register for first function argument,
;        the scratchpad address of the source BCD data
;  Arg3: Register for number of bytes to write
;  Arg4: Port address to write. This is a fixed value that can't
;        be changed at runtime
;Returns:
;  The generated function has no return value. All registers are
;  preserved on the stack.
;Example:
;  constant ConsolePort, FE
;  use_bcdwrite(bcdwrite, s0, s1, ConsolePort)
;  load s0, 20 ; Source address
;  load s1, 05 ; Write 5 bytes
;  call bcdwrite
define(`use_bcdwrite', `; PRAGMA function $1 [$2, $3] begin
            $1: ; Write BCD array in memory as ASCII digits to port $4
              vars(`$2 is _src', `$3 is _count')
              push(_src, _count)
              add _count, _src
              sub _count, 01
              dowhile(`_src < _count', `
                fetch _tempreg, (_src)
                if(`_tempreg != 0', `break')
                add _src, 01')
              dowhile(`_src <= _count', `
                fetch _tempreg, (_src)
                add _tempreg, "0"
                output _tempreg, evalh($4)
                add _src, 01')
              pop(_src, _count)
              return
              ; PRAGMA function end
              popvars')


;---------------------------------
;Generate a function to write an array to an output port as ASCII hex
;Args:
;  Arg1: Name of function to generate
;  Arg2: Register for first function argument,
;        the scratchpad address of the source data
;  Arg3: Register for number of bytes to write
;  Arg4: Port address to write. This is a fixed value that can't
;        be changed at runtime
;Returns:
;  The generated function has no return value. All registers are
;  preserved on the stack.
;Example:
;  constant ConsolePort, FE
;  use_hexwrite(hexwrite, s0, s1, ConsolePort)
;  load s0, 20 ; Source address
;  load s1, 05 ; Write 5 bytes
;  call hexwrite
define(`use_hexwrite', `; PRAGMA function $1 [$2, $3] begin
            $1: ; Write array in memory as ASCII hex digits to port $4
              vars(`$2 is _src', `$3 is _count')
              push(_src, _count)
              add _count, _src
              dowhile(`_src < _count', `
                ; Upper nibble
                fetch _tempreg, (_src)
                sr0(_tempreg, 4)
                if(`_tempreg >= 10',`add _tempreg, 07')
                add _tempreg, "0"
                output _tempreg, evalh($4)
                ; Lower nibble
                fetch _tempreg, (_src)
                and _tempreg, 0F
                if(`_tempreg >= 10',`add _tempreg, 07')
                add _tempreg, "0"
                output _tempreg, evalh($4)
                add _src, 01')
              pop(_src, _count)
              return
              ; PRAGMA function end
              popvars')


;---------------------------------
;Generate a function to convert an integer to unpacked BCD coded
;bytes stored in a scratchpad buffer.
;The number to convert is passed on the stack as one or more bytes
;with the MSB pushed last. The buffer size is fixed after generating
;the function. You must ensure it is large enough to contain the
;largest integer you expect to convert. Each buffer byte corresponds to
;one decimal digit. The converted BCD number is right justified within
;the buffer with leading 0's for padding. The least significant digit
;is always at the end of the buffer.
;Args:
;  Arg1: Name of function to generate
;  Arg2: Number of bytes for scratchpad buffer (fixed constant)
;  Arg3: First argument of generated function.
;        Register with destination scratchpad address
;  Arg4: Register containing number of bytes of data to convert
;        on the stack
;  Arg5: Internal temp register
;  Arg6: Internal temp register
;  Arg7: Internal temp register
;  Arg8: Internal temp register
;Returns:
;  The generated function has no return value. All registers are
;  preserved on the stack.
;Example:
;  use_int2bcd(int2bcd, 5, s0, s1, s2, s3, s4, s5) ; Support up to 5 decimal digits
;  load s0, 20 ; Dest address
;  load s1, 02 ; Convert 16-bit number (2-bytes)
;  load16(s4,s3, 31337)
;  push(s3,s4) ; Put number to convert on stack; LSB then MSB
;  call int2bcd
define(`use_int2bcd', `
; PRAGMA function $1 [$3, $4] begin
$1:  ; Convert a number on the stack into BCD stored in a scratchpad buffer of $2 bytes
  vars(`$3 is _dest', `$4 is _bytes', `$5 is _curbyte', `$6 is _bitcount', `$7 is _bytecount', `$8 is _digit')
  push(_curbyte, _bitcount, _bytecount, _digit)
  ; Zero scratch array
  load _digit, 00
  load _tempreg, _dest
  add _tempreg, evalh($2)
  dowhile(`_tempreg != _dest',
    `sub _tempreg, 01
    store _digit, (_tempreg)')

  dropstack(4) ; Get SP back to number we are converting
  
  load _bytecount, _bytes
  while(`_bytecount > 0', `
    pop(_curbyte) ; Get next byte to convert (in MSB to LSB order on stack)
    
    load _bitcount, 01   ; Init one-hot bit counter
    $1_bitloop:
      load _tempreg, _dest
      add _tempreg, evalh($2 - 1)
      ; Load lower digit from array
      fetch _digit, (_tempreg)
      
      sl0 _curbyte ; Get next MSb from binary byte
      sla _digit   ; Multiply by 2 and shift in MSb
      
      dowhile(`_tempreg != _dest', `
        if(`_digit > 9',
          `sub _digit, 0A ; Correct for overflow in digit
          
          ; Store digit
          store _digit, (_tempreg)
          sub _tempreg, 01
          fetch _digit, (_tempreg) ; Get next digit
          sl1 _digit               ; Multiply by 2 and carry overflow into next digit',
         `; No carry to next digit
          ; Store digit
          store _digit, (_tempreg)
          sub _tempreg, 01
          fetch _digit, (_tempreg) ; Get next digit
          sl0 _digit               ; Multiply by 2')

      ')
      
      store _digit, (_tempreg) ; Store last digit
      
      sl0 _bitcount
      jump nz, $1_bitloop
    sub _bytecount, 01
  ')

  addstack(_bytes)  ; Restore SP so we can pop saved registers
  addstack(4)
  pop(_curbyte, _bitcount, _bytecount, _digit)
  dropstack(_bytes) ; Clean original stack argument
  return
  ; PRAGMA function end
  popvars')


;---------------------------------
;Generate a function to convert an ASCII number to BCD.
;Invalid characters are converted to "0" digits.
;Args:
;  Arg1: Name of function to generate
;  Arg2: Register for first function argument,
;        the scratchpad address of the ASCII data
;  Arg3: Register for length of the data
;Returns:
;  The generated function has no return value. All registers are
;  preserved on the stack.
;Example:
;  use_ascii2bcd(ascii2bcd, s0, s1)
;  load s0, 20 ; Array address
;  load s1, 05 ; Length
;  call ascii2bcd
;  ; The array now contains all BCD digits
define(`use_ascii2bcd', `
; PRAGMA function $1 [$2, $3] begin
$1:  ; Convert an ASCII number stored in a scratchpad buffer into BCD
  vars(`$2 is _buf', `$3 is _buflen')
  push(_buf, _buflen)
  add _buflen, _buf
  dowhile(`_buf != _buflen',
    `fetch _tempreg, (_buf)
    sub _tempreg, 30 ; Convert ASCII numbers to BCD
    if(`_tempreg > 9',
      `; Wasnt originally a valid digit
      load _tempreg, 00')
    store _tempreg, (_buf)
    add _buf, 01')
  pop(_buf, _buflen)
  return
  popvars
  ; PRAGMA function end')

;---------------------------------
;Generate a function to convert an unpacked BCD coded bytes to a
;variable size integer. The BCD input is stored in a buffer in
;scratchpad memory. The buffer address and length are passed as arguments.
;The converted result is overwritten to the leftmost portion of the buffer.
;The length register returns with the number of bytes in the result.
;This function handles conversion to any size integer as the result is
;guaranteed to be smaller than the initial buffer.
;Internal temp registers must be allocated for use by the macro. They must
;not include the _tempreg register.
;Args:
;  Arg1: Name of function to generate
;  Arg2: First argument of generated function.
;        Register with scratchpad BCD address
;  Arg3: Register with number of bytes for scratchpad buffer
;  Arg4: Internal temp register
;  Arg5: Internal temp register
;  Arg6: Internal temp register
;  Arg7: Internal temp register
;  Arg8: Internal temp register
;Returns:
;  The converted integer value is located at the start of the buffer with the LSB
;  first, opposite to the order of the BCD digits.
;  The number of bytes in the converted integer is returned in the second argument
;  to the function (Arg3 of this generator macro). All other registers are preserved
;  on the stack.
;Example:
;  use_bcd2int(bcd2int, s0, s1, s2,s3,s4,s5,s6)
;  load s0, 20 ; BCD buffer
;  load s1, 03 ; 3 BCD digits long
;  call bcd2int
;  ; s1 contains the number of converted bytes
;  ; scratchpad 20 (and possibly 21) contain the binary result
define(`use_bcd2int', `
; PRAGMA function $1 [$2, $3] begin
$1:  ; Convert a BCD number stored in a scratchpad buffer into a binary number
  vars(`$2 is _buf', `$3 is _buflen', `$4 is _numend', `$5 is _curbyte', `$6 is _curdigit',
      `$7 is _digit', `$8 is _carry')

  compare _buflen, 01
  return z ; No conversion to do

  push(_numend, _curbyte, _curdigit, _digit, _carry)

  load _curdigit, _buf
  add _curdigit, 01     ; Skip first digit
  ; Convert buflen into the end address
  add _buflen, _buf
  load _numend, _buf

  ; Iterate over each digit from MSD to LSD
  ; We multiply the current binary value by 10 and then add the
  ; next BCD digit.
  dowhile(`_curdigit != _buflen',
    `; Multiply byte array by 10
    load _carry, 00
    for(`_curbyte := _buf', `_curbyte <= _numend', `_curbyte := _curbyte + 1',
      `fetch _tempreg, (_curbyte)
      if(`_tempreg < 26',
        `; Product fits within a byte
        ; N*10 --> (N * 4 + N) * 2 --> (N << 2 + N) << 1
        expr(_digit := _tempreg << 2 + _tempreg << 1)
        add _digit, _carry
        load _carry, 00
        store _digit, (_curbyte)',
       `; Product spans two bytes
        ; 16-bit mul by 10
        push(_carry)  ; Save current carry
        expr2(_carry, _digit := _tempreg << 2 + _tempreg << 1)
        pop(_tempreg) ; Retrieve previous carry
        add _digit, _tempreg
        store _digit, (_curbyte)

        if(`_curbyte == _numend',
          `add _numend, 01 ; Extend byte array
          load _tempreg, 00
          store _tempreg, (_numend)')
      ')
    ')

    ; Add next digit to byte array
    load _curbyte, _buf
    fetch _tempreg, (_curbyte)
    fetch _digit, (_curdigit)
    add _tempreg, _digit
    store _tempreg, (_curbyte)
    jump nc, $1_addition_done

    ; Propagate carry through remaining bytes
    dowhile(`_curbyte <= _numend',
      `if(`_curbyte == _numend',
        `add _numend, 01 ; Extend byte array
        load _tempreg, 00
        store _tempreg, (_numend)')
      add _curbyte, 01 ; Advance to next byte
      fetch _tempreg, (_curbyte)
      add _tempreg, 01 ; Add carry from previous byte
      store _tempreg, (_curbyte)
      jump nc, $1_addition_done')

$1_addition_done:

    add _curdigit, 01')


  ; Return number of bytes in result
  load _buflen, _numend
  sub _buflen, _buf
  add _buflen, 01
  pop(_numend, _curbyte, _curdigit, _digit, _carry)

  return
  popvars
  ; PRAGMA function end')


;=============== UPPERCASE MACROS ===============
;================================================


define(`EVALD', `evald($@)')
define(`EVALH', `evalh($@)')
define(`EVALA', `evala($@)')
define(`EVALB', `evalb($@)')
define(`EVALC', `evalc($@)')
define(`EVALX', `evalx($@)')
define(`PBHEX', `pbhex($@)')
define(`ASCIIORD', `asciiord($@)')
define(`ESTR', `estr($@)')
define(`CSTR', `cstr($@)')
define(`QSTR', `qstr($@)')
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
define(`LOAD_STORE', `load_store($@)')
define(`VARS', `vars($@)')
define(`USE_CLOCK', `use_clock($@)')
define(`DELAY_CYCLES', `delay_cycles($@)')
define(`FLOOR_LOG', `floor_log($@)')
define(`FLOOR_LOG2', `floor_log2($@)')
define(`DELAY_MS', `delay_ms($@)')
define(`DELAY_US', `delay_us($@)')
define(`VAR_DELAY_MS', `var_delay_ms($@)')
define(`VAR_DELAY_US', `var_delay_us($@)')
define(`VAR_COUNT_MS', `var_count_ms($@)')
define(`VAR_COUNT_US', `var_count_us($@)')
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
define(`ISCONST', `isconst($@)')
define(`CONST2M4', `const2m4($@)')
define(`IFEQ', `ifeq($@)')
define(`IFNE', `ifne($@)')
define(`IFGE', `ifge($@)')
define(`IFLT', `iflt($@)')
define(`ERRMSG', `errmsg($@)')
define(`WARNMSG', `warnmsg($@)')
define(`WHILE', `while($@)')
define(`DOWHILE', `dowhile($@)')
define(`FOR', `for($@)')
define(`BREAK', `break($@)')
define(`CONTINUE', `continue($@)')
define(`REPEATSTR', `repeatstr($@)')
define(`REPEAT', `repeat($@)')
define(`SL0', `ifelse($#,0, ``$0'',`sl0($@)')')
define(`SL1', `ifelse($#,0, ``$0'',`sl1($@)')')
define(`SLA', `ifelse($#,0, ``$0'',`sla($@)')')
define(`SLX', `ifelse($#,0, ``$0'',`slx($@)')')
define(`SR0', `ifelse($#,0, ``$0'',`sr0($@)')')
define(`SR1', `ifelse($#,0, ``$0'',`sr1($@)')')
define(`SRA', `ifelse($#,0, ``$0'',`sra($@)')')
define(`SRX', `ifelse($#,0, ``$0'',`srx($@)')')
define(`RL', `ifelse($#,0, ``$0'',`rl($@)')')
define(`RR', `ifelse($#,0, ``$0'',`rr($@)')')
define(`USE_STACK', `use_stack($@)')
define(`PUSH', `push($@)')
define(`POP', `pop($@)')
define(`GETSTACK', `getstack($@)')
define(`GETSTACKAT', `getstackat($@)')
define(`PUTSTACK', `putstack($@)')
define(`PUTSTACKAT', `putstackat($@)')
define(`DROPSTACK', `dropstack($@)')
define(`ADDSTACK', `addstack($@)')
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
define(`USE_STRINGS', `use_strings($@)')
define(`USE_PACKED_STRINGS', `use_packed_strings($@)')
define(`STRING', `ifelse($#,0, ``$0'',`string($@)')')
define(`PACKED_STRING', `packed_string($@)')
define(`NEGATE', `negate($@)')
define(`NOT', `not($@)')
define(`ABS', `abs($@)')
define(`SIGNEX', `signex($@)')
define(`ISNUM', `isnum($@)')
define(`COMPARES', `compares($@)')
define(`USE_MULTIPLY8X8', `use_multiply8x8($@)')
define(`USE_MULTIPLY8X8S', `use_multiply8x8s($@)')
define(`USE_MULTIPLY8X8SU', `use_multiply8x8su($@)')
define(`USE_DIVIDE8X8', `use_divide8x8($@)')
define(`USE_DIVIDE8X8S', `use_divide8x8s($@)')
define(`USE_DIVIDE16X8', `use_divide16x8($@)')
define(`USE_DIVIDE16X8S', `use_divide16x8s($@)')
define(`USE_MULTIPLY8XK', `use_multiply8xk($@)')
define(`USE_MULTIPLY8XK_SMALL', `use_multiply8xk_small($@)')
define(`USE_DIVIDE8XK', `use_divide8xk($@)')
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
define(`LOADADDR', `loadaddr($@)')
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

; New macros for v1.3

define(`ANSI_BLACK', `ansi_black($@)')
define(`ANSI_BLUE', `ansi_blue($@)')
define(`ANSI_CYAN', `ansi_cyan($@)')
define(`ANSI_GREEN', `ansi_green($@)')
define(`ANSI_MAGENTA', `ansi_magenta($@)')
define(`ANSI_RED', `ansi_red($@)')
define(`ANSI_RESET', `ansi_reset($@)')
define(`ANSI_WHITE', `ansi_white($@)')
define(`ANSI_YELLOW', `ansi_yellow($@)')
define(`ARGC', `argc($@)')
define(`COLORIZE', `colorize($@)')
define(`DEC2PBHEX', `dec2pbhex($@)')
define(`FUNC', `func($@)')
define(`ISR', `isr($@)')
define(`LEAVE_FUNC', `leave_func($@)')
define(`LEAVE_ISR', `leave_isr($@)')
define(`POPVARS', `popvars($@)')
define(`PROC', `proc($@)')
define(`RETVALUE', `retvalue($@)')
define(`STRHASH', `strhash($@)')
define(`STRLENC', `strlenc($@)')
define(`USE_ASCII2BCD', `use_ascii2bcd($@)')
define(`USE_BCD2INT', `use_bcd2int($@)')
define(`USE_BCDWRITE', `use_bcdwrite($@)')
define(`USE_DELAY_REG', `use_delay_reg($@)')
define(`USE_HEXWRITE', `use_hexwrite($@)')
define(`USE_INT2BCD', `use_int2bcd($@)')
define(`USE_MEMCOPY', `use_memcopy($@)')
define(`USE_MEMSET', `use_memset($@)')
define(`USE_MEMWRITE', `use_memwrite($@)')
define(`USE_RANDOM16', `use_random16($@)')
define(`USE_RANDOM8', `use_random8($@)')

divert(0)dnl

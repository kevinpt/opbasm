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


;---------------------------------
; No-op macros
define(`nop', `load sF, sF  ; NOP')
define(`NOP', `load sF, sF  ; NOP')

;=============== LITERAL OPERATIONS ===============

;---------------------------------
; Extensions to eval
; Evaluate expression as a decimal number
define(`evald', `eval($1)''d  `;' $1)

; Evaluate expression as an 8-bit hex number
define(`evalh', `eval(($1) & 0xFF, 16, 2)  ; $1')

; Evaluate expression as an 8-bit binary number
define(`evalb', `eval(($1) & 0xFF, 2, 8)''b  `;' $1)

; Ex: constant cname,  evalh(20 + 6)      -->  constant cname,  1a
;     constant cname2, evald(20 * 4 - 1)  -->  constant cname2, 79'd
;     constant cname3, evalh(250 + 6)     -->  constant cname3, 01


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
define(`asciiord', `esyscmd(`python -c "import sys; sys.stdout.write(\", \".join(str(ord(c)) for c in \"$1\"))"')') 

;---------------------------------
; Express a decimal in Picoblaze hex (without any comment)
; Arg1: Number to convert
define(`dec', `eval(($1) & 0xFF, 16, 2)')

;---------------------------------
; Convert a number 0-9 to its ASCII code in Picoblaze hex format
; Arg1: Digit to convert
; Ex: asciinum(0)  ; Expands to 30
define(`asciinum', `eval(($1) + 0x30, 16, 2)')


;=============== CARRY FLAG OPERATIONS ===============

;---------------------------------
; Clear the carry flag
define(`clearcy', `and sF, sF  ; Clear carry')

;---------------------------------
; Set the carry flag
; Arg1: Temporary register to modify
; Ex: setcy(sf)
define(`setcy', `load $1, 00  ; Set carry
compare $1, 01')

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
define(`setmask', `or $1, eval($2, 16, 2)  ; Set mask')
define(`clearmask', `and $1, eval((~($2)) & 0xFF, 16, 2)  ; Clear mask')

;---------------------------------
; Test if a bit is set or clear
; Arg1: Register to test
; Arg2: Bit number (0-7) to test
; Z is set if bit is clear, Z is clear if bit is set
; Ex:  testbit(s1, 3)
;      jump z, bit_cleared
define(`testbit', `test $1, eval(2**($2), 16, 2)  ; Test bit $2')


;=============== CONDITIONAL JUMP AND CALL OPERATIONS ===============

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


;=============== SHIFT AND ROTATE OPERATIONS ===============

;---------------------------------
; Repeat an instruction or macro
; Arg1: Instruction or macro string to repeat
; Arg2: Numper of repetitions
define(`repeat', `ifelse($#,0, ``$0'', eval($2>0),1, `$1
$0($1, decr($2))')')

;---------------------------------
; Repeated shifts
; Arg1: Register to shift
; Arg2: Number of shifts
; Ex: sl0(s2, 4)  ; Shift left by 4
define(`sl0', `ifelse($#,0, ``$0'', `repeat(`sl0 $1', eval($2))')')
define(`sl1', `ifelse($#,0, ``$0'', `repeat(`sl1 $1', eval($2))')')
define(`sla', `ifelse($#,0, ``$0'', `repeat(`sla $1', eval($2))')')
define(`slx', `ifelse($#,0, ``$0'', `repeat(`slx $1', eval($2))')')
define(`sr0', `ifelse($#,0, ``$0'', `repeat(`sr0 $1', eval($2))')')
define(`sr1', `ifelse($#,0, ``$0'', `repeat(`sr1 $1', eval($2))')')
define(`sra', `ifelse($#,0, ``$0'', `repeat(`sra $1', eval($2))')')
define(`srx', `ifelse($#,0, ``$0'', `repeat(`srx $1', eval($2))')')

; Repeated rotates
define(`rl', `ifelse($#,0, ``$0'', `repeat(`rl $1', eval($2))')')
define(`rr', `ifelse($#,0, ``$0'', `repeat(`rr $1', eval($2))')')


;=============== STACK OPERATIONS ===============

;---------------------------------
; Initialize and define stack pointer register.
; ** Only invoke once. Must be executed before any push() or pop() macros **
; Arg1: Stack pointer register
; Arg2: Scratchpad address for top of stack
; Ex: namereg sA, SP ; Reserve stack pointer register 
;     stackinit(SP, 0x3F) ; Start stack at end of 64-byte scratchpad
define(`stackinit', `load $1, eval($2, 16, 2)' `define(`_stackptr', $1)')

;---------------------------------
; Pseudo-stack operations using the scratchpad RAM
; The stack pointer grows from the end of the scratchpad to the start
; Arg1: Register with value to push or pop
; Ex: stackinit(sa, 0x3F)
;     push(s0)
;     push(s1)
;     pop(s1)
define(`push', `store $1, (_stackptr)  ; Push
sub _stackptr, 01')

define(`pop', `add _stackptr, 01  ; Pop
fetch $1, (_stackptr)')

;---------------------------------
; Retrieve values from the stack without modification
; Arg1: Register to save value in
; Arg2: Offset from stack pointer (offset 1 is the first value)
; Ex: getstack(s3, 2) ; Get the second value relative to the stack pointer
define(`getstack', `add _stackptr, eval($2)  ; Fetch stack offset $2
fetch $1, (_stackptr)
sub _stackptr, eval($2)')

;---------------------------------
; Retrieve multiple contiguous values from the stack without modification
; Arg1-Argn: Registers to save values in
;            The first register corresponds to the highest address
; Ex: getstackregs(s3, s4, s5) ; Get stack offset SP+3, SP+2, and SP+1 into s3, s4, s5
define(`getstackregs', `add _stackptr, $#  ; Get stack registers
_gsr($@)')

define(`_gsr', `ifelse(`$1',,,`fetch $1, (_stackptr)
sub _stackptr, 01
$0(shift($@))')')

;---------------------------------
; Drop values stored on the stack
; Arg1: Number of values to drop from the stack
; Ex: dropstack(2)  ; Remove 2 values
define(`dropstack', `add _stackptr, eval($1, 16, 2)  ; Remove stack values')

;---------------------------------
; Drop values stored on the stack using a register
; Arg1: Number of values to drop from the stack
; Ex: dropstackreg(s1)  ; Remove number of values specified in s1 register
define(`dropstackreg', `add _stackptr, $1  ; Remove stack values')


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
; Arg1: Output port in Picoblaze hex format
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
define(`outputstring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
output $2, $1'
`$0($1, $2, substr(`$3',1))')')

;---------------------------------
; Store a string to scratchpad RAM
; Arg1: Pointer register to scratchpad address
; Arg2: Temporary register for each character
; Arg3: String to store
define(`storestring', `ifelse($3,,,`load $2, "substr(`$3', 0,1)"
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
; Arg1: Output port in Picoblaze hex format
; Arg2: Temporary register for each constant
; Arg3 - Argn: Decimal values to output to port
; Ex: constant UART_PORT, 0a
;     outputtable(UART_PORT, sf, pbhex(DE,AD,BE,EF)) ; Output DE,AD,BE,EF to port

define(`outputtable', `pushdef(`_oreg', $1)`'pushdef(`_treg', $2)'`_ot(shift(shift($@)))'`popdef(`_oreg')`'popdef(`_treg')')

define(`_ot', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
output _treg, _oreg'
`_ot(shift($@))'dnl
)')


;---------------------------------
; Store a table of constants in scratchpad RAM
; Arg1: Pointer register to scratchpad address
; Arg2: Temporary register for each constant
; Arg3 - Argn: Decimal values to load in scratchpad
; Ex: load s1, my_array
;     storetable(s1, sf, pbhex(DE,AD,BE,EF)) ; Load DE,AD,BE,EF into memory
;     storetable(s1, sf, 10, 11, 12)         ; Load decimals
define(`storetable', `pushdef(`_preg', $1)`'pushdef(`_treg', $2)'`_st(shift(shift($@)))'`popdef(`_preg')`'popdef(`_treg')')

define(`_st', `ifelse(`$1',,,`load _treg, eval($1, 16, 2)
store _treg, (_preg)'
`ifelse(eval($#>1),1,`add _preg, 01')'
`_st(shift($@))'dnl
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
; Ex: insttable_le(pbhex(0b, 0b, 0c))
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
; Ex: insttable_be(pbhex(0b, 0b, 0c))
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
; Multiply 8 x 8 subroutine
; Arg1: Subroutine name
; Arg2: Multiplicand
; Arg3: Multiplier
; Arg4, Arg5: Result MSB, LSB
; Arg6: Bit mask temp register
; Ex: multiply8x8(mul8, s0, s1, s3, s2, sf) ; (s3, s2) = s0 * s1
;     load s0, 04
;     load s1, 05
;     call mul8
define(`multiply8x8', `$1:  ; ($4, $5) = $2 * $3
            load $6, 01
            load $4, 00
            load $5, 00
$1_loop:    test $3, $6
            jump z, $1_no_add
            add $4, $2
$1_no_add:  sra $4
            sra $5
            sl0 $6
            jump nz, $1_loop
            return')

;---------------------------------
; Divide 8 / 8 subroutine
; Arg1: Subroutine name
; Arg2: Dividend
; Arg3: Divisor
; Arg4: Quotient
; Arg5: Remainder
; Arg6: Bit mask temp register
; Ex: divide8x8(div8, s0, s1, s2, s3, sf)
;     load s0, 20'd
;     load s1, 4'd
;     call div8
define(`divide8x8', `$1: ; $4 = ($2 / $3)  remainder $5
            load $5, 00
            load $6, 80
$1_loop:    test $2, $6
            sla $5
            sl0 $4
            compare $5, $3
            jump c, $1_no_sub
            sub $5, $3
            add $4, 01
$1_no_sub:  sr0 $6
            jump nz, $1_loop
            return')

;---------------------------------
; Multiply 8 x constant subroutine with 16-bit result
; Arg1: Subroutine name
; Arg2: Multiplicand
; Arg3: Constant multiplier (can be wider than 8-bits)
; Arg4, Arg5: Result MSB, LSB
; Ex: multiply8xk(mul8k5, s0, 5, s5, s4)  ; (s5, s4) = s0 * 5
;     load s0, 7'd
;     call mul8k5
define(`multiply8xk', `$1:  ; ($4, $5) = $2 * ($3)
load $4, 00
load $5, 00
_genmul8xk($2, eval($3,2), $4, $5)return')

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
define(`multiply8xk_small', `$1: ; $4 = $2 * ($3)
load $4, 00
_genmul8xk_small($2, eval($3,2), $4)return')

define(`_genmul8xk_small', `ifelse($2,,,`ifelse(eval(substr($2,0,1) == 1),1,`add $3, $1',`dnl')
ifelse(eval(len(`$2')>1),1,`sl0 $3  ; $2')
$0($1, substr(`$2', 1), $3)')')


;---------------------------------
; Divide 8 / constant subroutine with 8-bit result
; Arg1: Subroutine name
; Arg2: Dividend
; Arg3: Constant divisor (can be wider than 8-bits)
; Arg4: Result quotient
; Arg5: Temporary register
; Ex: divide8xk(div8k5, s0, 5, s4, sf)  ; s4 = s0 / 5
;     load s0, 25'd
;     call div8k5
define(`divide8xk', `$1:  ; ($4, $5) = $2 * ($3)
load $4, 00
load $5, 00
_genmul8xk($2, eval(2**8 / ($3) + 1,2), $4, $5)return')



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
define(`reg16', `define(`$1', `$2, $3')')

;---------------------------------
; Get the upper and lower registers from a reg16 definition
; Arg1, Arg2: MSB, LSB of 16-bit register
; Ex: reg16(rx, s5, s4)
;     load s1, regupper(rx) ; load upper byte from s5
;     load s1, reglower(rx) ; load lower byte from s4
define(`regupper', $1)
define(`reglower', $2)

; Split a 16-bit constant into upper and lower bytes
define(`constupper', `eval((($1) >> 8) & 0xFF)')
define(`constlower', `eval(($1) & 0xFF)')

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
; Ex: not(s1, s0)
define(`not16', `xor $1, FF  ; Not 16-bit
xor $2, FF')

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
;   Arg3: Decimal constant xor expression
; 4 arguments: xor with register
;   Arg3, Arg4: MSB2, LSB2
; Result in Arg1, Arg2
define(`xor16', `ifelse($#,4,`_xor16($@)',`_xor16k($@)')')

define(`_xor16', `xor $2, $4
xor $1, $3')

define(`_xor16k', `xor $2, eval(constlower($3), 16, 2)  ; $3
xor $1, eval(constupper($3), 16, 2)')


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

define(`sl0_16', `repeat(`_sl0_16($1, $2)', eval($3))')
define(`sl1_16', `repeat(`_sl1_16($1, $2)', eval($3))')
define(`sla_16', `repeat(`_sla_16($1, $2)', eval($3))')
define(`slx_16', `repeat(`_slx_16($1, $2)', eval($3))')
define(`sr0_16', `repeat(`_sr0_16($1, $2)', eval($3))')
define(`sr1_16', `repeat(`_sr1_16($1, $2)', eval($3))')
define(`sra_16', `repeat(`_sra_16($1, $2)', eval($3))')
define(`srx_16', `repeat(`_srx_16($1, $2)', eval($3))')


divert(0)dnl

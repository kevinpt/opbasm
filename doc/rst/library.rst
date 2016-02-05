
Opbasm PicoBlaze macro library reference
========================================

.. pb:macro:: abs(Arg1)

  Absolute value.
  
  :param Arg1: Register to make positive
  
  :returns: Result is in the same register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, evalh(-10)
     abs(s0)             ; Result is 10
  

.. pb:macro:: abs16(Arg1, Arg2)

  16-bit absolute value.
  
  :param Arg1: MSB register to make positive
  :param Arg2: LSB register to make positive
  
  Result is in Arg1, Arg2
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1,s0, -1000)
     abs16(s1, s0)      ; s1, s0 contain +1000
     load16(s1,s0, 1000)
     abs16(s1, s0)      ; s1, s0 contain +1000
  

.. pb:macro:: add16(Arg1, Arg2, Arg3, [Arg4])

  16-bit addition.
  
  :param Arg1: MSB1 register
  :param Arg2: LSB1 register
  :param Arg3: Decimal constant or expression or MSB2 when Arg4 is present
  :param Arg4: Optional register for LSB2
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a constant literal. When four arguments are passed,
  Arg3 and Arg4 are a 16-bit pair to add to the result.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     add16(s1,s0, 0x1234) ; s1,s0 += 0x1234
     add16(s1,s0, s3,s2)  ; s1,s0 += s3,s2
  

.. pb:macro:: addstack(Arg1)

  Allocate local space on the stack.
  
  :param Arg1: Number of values to add to the stack or a register
  
  You can use the added space for a local variable storage on the stack. This avoids
  the need to have static stratchpad memory allocations. Use :pb:macro:`dropstack` to
  restore the stack pointer.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     addstack(2)  ; Add 2 values
     addstack(s1) ; Add number of values from s1
  

.. pb:macro:: and16(Arg1, Arg2, Arg3, [Arg4])

  16-bit logical AND.
  
  :param Arg1: MSB1
  :param Arg2: LSB1
  :param Arg3: Decimal constant or expression or MSB2 when Arg4 is present
  :param Arg4: Optional register for LSB2
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a constant literal. When four arguments are passed,
  Arg3 and Arg4 are a 16-bit pair to AND to the result.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1,s0, 0x787F)
     and16(s1,s0, 0x0FC0)  ; Result is 0x0840
     and16(s1,s0, s3,s2)   ; (s1,s0) AND (s3,s2)
  

.. pb:macro:: ansi_black([Arg1])

  ANSI escape codes for generating color text.
  
  The resulting string contains backslash escapes that must be processed
  by :pb:macro:`cstr` or :pb:macro:`estr`.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     ansi_red`foobar'ansi_reset       ; red text
     ansi_red(bold)`foobar'ansi_reset ; bold/bright red text
  

.. pb:macro:: ansi_blue([Arg1])

  ANSI escape codes for generating blue text.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: ansi_cyan([Arg1])

  ANSI escape codes for generating cyan text.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: ansi_green([Arg1])

  ANSI escape codes for generating green text.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: ansi_magenta([Arg1])

  ANSI escape codes for generating magenta text.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: ansi_red([Arg1])

  ANSI escape codes for generating red text.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: ansi_reset()

  ANSI escape code for clearing pervious colors.
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: ansi_white([Arg1])

  ANSI escape codes for generating white text.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: ansi_yellow([Arg1])

  ANSI escape codes for generating yellow text.
  
  :param Arg1: Optional argument of "bold" will activate bold/bright text
  
  .. seealso:: :pb:macro:`ansi_black` for more information.
  

.. pb:macro:: argc(Arg1-ArgN)

  Count the number of args.
  
  :param Arg1-ArgN: Argument list to count
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     argc(a,b,c,d) ; Expands to 4
  

.. pb:macro:: asciiord(Arg1)

  Convert a string to a list of decimal ASCII codes.
  
  :param Arg1: String to convert
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     asciiord(`My string')  ; Expands to 77, 121, 32, 115, 116, 114, 105, 110, 103
  
  .. note:: You must use m4 quotes on the string argument to avoid unwanted substitutions.
  

.. pb:macro:: break()

  Break statement to exit from for(), while(), and dowhile() loops.

.. pb:macro:: calleq(Arg1)

  Call if equal.
  
  :param Arg1: Label to call
  

.. pb:macro:: callge(Arg1)

  Call if greater or equal.
  
  :param Arg1: Label to call
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     compare s3, 24
     callge(greater)    ; call if s3 >= 24
  

.. pb:macro:: calllt(Arg1)

  Call if less than.
  
  :param Arg1: Label to call
  

.. pb:macro:: callne(Arg1)

  Call if not equal.
  
  :param Arg1: Label to call
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     compare s3, 24
     callne(not_equal)  ; call if s3 != 24
  

.. pb:macro:: callstring(Arg1, Arg2, Arg3)

  Repeated string function call operation (useful for PicoBlaze-3).
  
  :param Arg1: Subroutine to call for each character
  :param Arg2: Register used to hold characters (typically an argument to the subroutine)
  :param Arg3: String to split into characters
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     callstring(write_char, s1, `My string')
     Expands to:
        load s1, "M"
        call write_char
        load s1, "y"
        call write_char
        ...
  

.. pb:macro:: calltable(Arg1, Arg2, Arg3-ArgN)

  Repeated function call on a table of constants.
  
  :param Arg1: Subroutine to call for each byte
  :param Arg2: Temporary register for each constant
  :param Arg3-ArgN: Decimal values representing table bytes
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     calltable(my_subroutine, sF, pbhex(DE,AD,BE,EF)) ; Pass DE,AD,BE,EF in repeated calls to my_subroutine
  

.. pb:macro:: clearcy([Arg1])

  Clear the carry flag.
  
  :param Arg1: Optional temp register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     clearcy
     clearcy(s0)
  

.. pb:macro:: clearmask(Arg1, Arg2)

  Clear mask bits in a register.
  
  :param Arg1: Register to modify
  :param Arg2: Mask value
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     clearmask(s5, mask(0,1,2))
     clearmask(s4, 0xF1)
  

.. pb:macro:: colorize(Arg1, Arg2, [Arg3])

  Wrap a string in ANSI color codes.
  
  The resulting string contains backslash escapes that must be processed
  by :pb:macro:`cstr` or :pb:macro:`estr`.
  
  :param Arg1: String to modify
  :param Arg2: ANSI color name (black, red, green, yellow, blue, magenta, cyan, and white)
  :param Arg3: Optional argument "bold" will select bold text
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     colorize(`foobar', blue)      ; Equivalent to ansi_blue`foobar'ansi_reset
     colorize(`foobar', red, bold) ; Equivalent to ansi_red(bold)`foobar'ansi_reset
  

.. pb:macro:: compares(Arg1, Arg2)

  Signed compare.
  
  :param Arg1: Register for left side of comparison
  :param Arg2: Register or constant for right side of comparison.
               Constant is a number in m4 syntax and cannot be a named constant
  
  
  Carry flag is set in accordance with signed relation.
  
  Zero flag is indeterminate. Use normal compare instruction for == and !=
  
  .. note:: This calls the setcy() macro and depends on the tempreg
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, evalh(-20)
     load s1, 42
     compares(s0, s1)     ; C flag is set because -20 < 0x42
  

.. pb:macro:: constlower(Arg1)

  Split a 16-bit constant and return its lower byte.
  
  :param Arg1: Constant to split
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constlower(0x1234) ; Expands to 0x34
  

.. pb:macro:: constupper(Arg1)

  Split a 16-bit constant and return its upper byte.
  
  :param Arg1: Constant to split
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constupper(0x1234) ; Expands to 0x12
  

.. pb:macro:: continue()

  Continue statement to restart a for(), while(), or dowhile() loop.

.. pb:macro:: cstr(Arg1)

  Convert a string to a list of decimal ASCII codes with a NUL terminator.
  
  :param Arg1: String to convert
  
  .. seealso:: :pb:macro:`estr` for more information.
  

.. pb:macro:: dec2pbhex(Arg1-ArgN)

  Convert a list of decimal values to PicoBlaze hex format.
  
  :param Arg1-ArgN: Decimal values to convert
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     dec2pbhex(1, 2, 100, 200)  ; Expands to 01, 02, 64, C8
  

.. pb:macro:: delay_cycles(Arg1)

  Delay for a number of instruction cycles.
  
  :param Arg1: Number of instructions to delay
  
  
  This can generate two types of delay loops. The default is a recursive
  delay implemented without any registers. For delays of 511 cycles or less
  a more efficient loop can be generated if a loop count register is defined
  first by calling the use_delay_reg() macro.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     delay_cycles(10) ; Delay for 10 instructions (20 clock cycles)
  

.. pb:macro:: delay_ms(Arg1, Arg2, Arg3, [Arg4])

  Delay by milliseconds.
  
  :param Arg1: Milliseconds to delay
  :param Arg2: MSB register of delay counter
  :param Arg3: LSB register of delay counter
  :param Arg4: Optional number of instructions to deduct from the delay (default is 0)
  
  
  This delay will be cycle accurate if the requested delay is an integer multiple
  of the clock period.
  At 100 MHz, the max delay is 214 ms. It increases with lower clock frequencies.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_clock(50) ; 50 MHz clock
  
    delay_5ms: delay_ms(5, s4,s5, 2) ; Deduct 2 additional instructions to account for call and return
               return
    ...
    call delay_5ms

.. pb:macro:: delay_us(Arg1, Arg2, Arg3, [Arg4])

  Delay by microseconds.
  
  :param Arg1: Microseconds to delay
  :param Arg2: MSB register of delay counter
  :param Arg3: LSB register of delay counter
  :param Arg4: Optional number of instructions to deduct from the delay (default is 0)
  
  
  This delay will be cycle accurate if the requested delay is an integer multiple
  of the clock period.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_clock(50) ; 50 MHz clock
  
    delay_40us: delay_us(40, s4,s5)
                return
    ...
    call delay_40us

.. pb:macro:: dowhile(Arg1, Arg2)

  Do-while loop.
  
  :param Arg1: Boolean comparison expression
  :param Arg2: Code block for loop body
  
  The Boolean expression must be of the form: ``reg op reg|expression`` where op is <, >=, ==, !=, or &
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, 15'd
     dowhile(s0 != 10, `output s3, P_foo
                        sub s0, 01')
  

.. pb:macro:: dropstack(Arg1)

  Drop values stored on the stack.
  
  :param Arg1: Number of values to drop from the stack or a register
  
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     dropstack(2)  ; Remove 2 values
     dropstack(s1) ; Remove number of values specified in s1 register
  

.. pb:macro:: errmsg(Arg1)

  Print an error message and terminate m4.
  
  :param Arg1: message string
  
  This prints a message with the file and line number where the macro was invoked for debugging.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     errmsg(`Bad arguments to foo macro')
  

.. pb:macro:: estr(Arg1)

  Convert a string to a list of decimal ASCII codes without a NUL terminator.
  
  
  :param Arg1: String to convert
  
  The following C escape codes are translated to their ASCII value:
  
  * \\\\  \\
  * \\n  NL\\LF
  * \\r  CR
  * \\t  HT
  * \\b  BS
  * \\a  BEL
  * \\e  ESC
  * \\s  semicolon ;
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     estr(`My string\r\n')  ; Expands to 77, 121, 32, 115, 116, 114, 105, 110, 103, 13, 10
     cstr(`My string\r\n')  ; Expands to 77, 121, 32, 115, 116, 114, 105, 110, 103, 13, 10, 0
  

.. pb:macro:: evala(Arg1)

  Evaluate m4 expression as a 12-bit PicoBlaze address.
  
  :param Arg1: Expression
  
  :returns: Expands to a PicoBlaze address literal with a comment listing the original expression
  

.. pb:macro:: evalb(Arg1)

  Evaluate m4 expression as a PicoBlaze binary number.
  
  :param Arg1: Expression or constant name
  
  :returns: Expands to a PicoBlaze binary literal with a comment listing the original expression
  

.. pb:macro:: evalc(Arg1, [Arg2, Arg3])

  Evaluate m4 expression with expansion of constants.
  
  :param Arg1: Expression or constant name
  :param Arg2: Optional numeric base to convert to (default is 10)
  :param Arg3: Optional minimum number of digits in result (default is 0)
  
  :returns: Expands to a literal for the expression or constant
  

.. pb:macro:: evald(Arg1)

  Evaluate m4 expression as a PicoBlaze decimal number.
  
  :param Arg1: Expression or constant name
  
  :returns: Expands to a PicoBlaze decimal literal with a comment listing the original expression
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant cname, evald(20 * 4 - 1)  -->  constant cname, 79'd
  

.. pb:macro:: evalh(Arg1)

  Evaluate m4 expression as an 8-bit PicoBlaze hex number.
  
  :param Arg1: Expression or constant name
  
  :returns: Expands to a PicoBlaze hex literal with a comment listing the original expression
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant cname,  evalh(20 + 6)      -->  constant cname,  1a
     constant cname2, evalh(250 + 6)     -->  constant cname2, 01
  

.. pb:macro:: evalx(Arg1, [Arg2, Arg3])

  Only evaluate valid expressions, otherwise reproduce the original text in the
  first argument.
  
  :param Arg1: Expression or string literal
  :param Arg2: Optional numeric base to convert to (default is 10)
  :param Arg3: Optional minimum number of digits in result (default is 0)
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     evalx(some_name)  --> some_name
     evalx(1+3)        --> 4
  

.. pb:macro:: expr(Arg1)

  Expression evaluators
  This is a family of macros that provide implementation of arithmetic expressions from
  compact input notation
  
  :param Arg1: Register assignment expression of the form:
               sN := <val> op <val> [op <val>]*
  
  
  val is one of:
  
  * register
  * literal expression (with no internal spaces)
  * sp[addr] scratchpad adddress
  * spi[reg] indirect scratchpad address in register
  
  op is one of:
  
    `+ - * /`
      add, subtract, multiply, divide
  
    `& | ^`
      and, or, xor
  
    `<< >>`
      shift left, shift right (0-filled MSB)
  
    `=:`
      reverse assignment to register or scratchpad
  
  .. note:: Operations are evaluated left to right with **no precedence**
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     expr(s0 := s1 + s2 - s3 >> 4 =: sp[M_value])
       Arithmetic is performed on s0 and the result is stored in scratchpad at M_value
       s0 <= s1, s0 <= s0 + s2, s0 <= s0 - s3, s0 <= s0 >> 4, sp[M_value] <= s0
     
     expr(s1 := s4 + (28*4-1))
       s1 <= s4, s1 <= s1 + 111   Constant expressions must have no spaces
     
  
  Summary of expression macros:
  
  ======= ================= =================================== ==========================
  Macro   Target x operand  Supported operators                 Notes
  ======= ================= =================================== ==========================
  expr    8x8               ``+, -, *, /, &, |, ^, <<, >>, =:``
  exprs   8x8               ``+, -, *, /, &, |, ^, <<, >>, =:`` (signed \*, /, and >>)
  expr2   16x8 (see note)   ``+, -, *, /, <<, >>, =:``
  expr2s  16x8 (see note)   ``+, -, *, /, <<, >>, =:``          (signed for all except <<)
  expr16  16x16             ``+, -, &, |, ^, <<, >>, =:``
  expr16s 16x16             ``+, -, &, |, ^, <<, >>, =:``       (signed >>)
  ======= ================= =================================== ==========================
  
  .. note:: The expr2 macros support 16-bit literals as operands of + and -
  
  For multiplication and division support you must initialize the internal functions with
  one of the following:
  
  ======   ================================   ===============
  Macro    Multiply                           Divide
  ======   ================================   ===============
  expr     use_expr_mul                       use_expr_div
  exprs    use_expr_muls                      use_expr_divs
  expr2    use_expr_mul                       use_expr_div16
  expr2s   use_expr_muls and use_expr_mulsu   use_expr_div16s
  ======   ================================   ===============
  
  As an expedient you can invoke :pb:macro:`use_expr_all` to include all of them and then
  eliminate any unused mul or div routines with the ``--remove-dead-code`` option to opbasm.
  
  These macros need to be called before any call to expr*() that uses multiplication or division.
  It is best to place them at the start of the program and jump over them to reach the startup code.
  The stack must be configured with :pb:macro:`use_stack` before calling these macros because additional
  modified registers must be saved and restored.
  
  By default these macros configure the mul and div functions to use the s8,s9 or s7,s8, and s9
  registers for input and output. You can modify the register allocation by passing arguments
  to the use_* macros. The MSB of multiplication is ignored by subsequent operations. Division
  by 0 is not detected.

.. pb:macro:: expr16(Arg1)

  16x16 unsigned expression evaluator.
  All terms are 16-bit register pairs or 16-bit literals.
  
  :param Arg1: Register assignment expression of the form:
               sN := <val> op <val> [op <val>]*
  
  .. seealso:: :pb:macro:`expr` for more information.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reg16(rx, s4,s5)
     reg16(ry, s6,s7)
     load16(ry, 400)
     expr2(rx := 2000 + ry * 3)  ; (2000 + 400) * 3 => 12600
  

.. pb:macro:: expr16s(Arg1)

  16x16 signed expression evaluator.
  All terms are 16-bit register pairs or 16-bit literals.
  
  :param Arg1: Register assignment expression of the form:
               sN := <val> op <val> [op <val>]*
  
  .. seealso:: :pb:macro:`expr` for more information.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reg16(rx, s4,s5)
     reg16(ry, s6,s7)
     load16(ry, -400)
     expr2(rx := 2000 + ry * -3)  ; (2000 + -400) * -3 => -4800
  

.. pb:macro:: expr2(Arg1)

  16x8 unsigned expression evaluator.
  The target register is a 16-bit pair and the first term can be a 16-bit pair
  or a 16-bit literal.
  
  :param Arg1: Register assignment expression of the form:
               sN := <val> op <val> [op <val>]*
  
  .. seealso:: :pb:macro:`expr` for more information.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reg16(rx, s4,s5)
     reg16(ry, s6,s7)
     load16(ry, 400)
     expr2(rx := ry + 200 * 3)  ; (400 + 200) * 3 => 1800
  

.. pb:macro:: expr2s(Arg1)

  16x8 signed expression evaluator.
  The target register is a 16-bit pair and the first term can be a 16-bit pair
  or a 16-bit literal.
  
  :param Arg1: Register assignment expression of the form:
               sN := <val> op <val> [op <val>]*
  
  .. seealso:: :pb:macro:`expr` for more information.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reg16(rx, s4,s5)
     reg16(ry, s6,s7)
     load16(ry, -400)
     expr2(rx := ry + 200 * -3)  ; (-400 + 200) * -3 => 600
  

.. pb:macro:: exprs(Arg1)

  Signed 8-bit expression evaluator.
  
  :param Arg1: Register assignment expression of the form:
               sN := <val> op <val> [op <val>]*
  
  .. seealso:: :pb:macro:`expr` for more information.
  

.. pb:macro:: fetch16(Arg1, Arg2, Arg3, [Arg4])

  16-bit fetch.
  
  :param Arg1: MSB register of target
  :param Arg2: LSB register of target
  :param Arg3: Register pointing to low byte or MSB of source address when Arg4 is present
  :param Arg4: Optional register for LSB of source address
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a register pointing to the low byte to fetch.
  It is incremented twice to permit sequential use of fetch16().
  When four arguments are passed, Arg3 and Arg4 are a 16-bit pair of address constants to fetch from.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant M_ACCUM_L, 1a
     constant M_ACCUM_H, 1b
     reg16(M_ACCUM, M_ACCUM_H, M_ACCUM_L)
     reg16(rx, s4, s3)
     fetch16(rx, M_ACCUM)  ; Fetch direct from address
     load s0, M_ACCUM_L
     fetch16(rx, s0)       ; Fetch from indirect pointer
     fetch16(rx, s0)       ; Fetch next word
  

.. pb:macro:: floor_log(Arg1, Arg2)

  Compute floor(log(n,b)) for Base-b.
  
  :param Arg1: Number to compute floor-log on
  :param Arg2: Logarithm base
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     floor_log(20, 2)    ; Expands to 4 (2**4 = 16, 2**5 = 32)
     floor_log(1000, 10) ; Expands to 3 (10**3 = 1000)
  

.. pb:macro:: floor_log2(Arg1)

  Compute floor(log(n)) for Base-2.
  
  :param Arg1: Number to compute floor-log on
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     floor_log2(64) ; Expands to 6
  

.. pb:macro:: for(Arg1, Arg2, Arg3, Arg4)

  For loop.
  
  :param Arg1: Initialization expression (passed to expr()). This can be empty.
  :param Arg2: Boolean comparison expression
  :param Arg3: Update expression (passed to expr()). This can be empty.
  :param Arg4: Code block for loop body
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     for(s0 := 0, s0 < 5, s0 := s0 + 1, `output s0, 00')
  
  Note:
  The :pb:macro:`continue` macro will behave as in C by jumping to the update code before
  restarting the loop.

.. pb:macro:: func(Arg1, Arg2, Arg3, Arg4)

  Function definition.
  
  This creates a function that receives its arguments on the stack.
  A macro is generated to prepare the stack arguments and call the function.
  The function will save registers automatically and load the stack arguments.
  The saved registers and call frame are cleaned up at the end.
  Do not use RETURN instructions in the code body. Instead call the
  leave_func() macro. Use the retvalue() macro to store return values
  on the stack.
  
  :param Arg1: Label for function
  :param Arg2: Variable definitions (same format as passed to the vars() macro)
  :param Arg3: Number of bytes returned on stack (0 for no return values)
  :param Arg4: Code block for func body
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     func(mul, `s0 is m1, s1 is m2, s2 is result', 0, `expr(result := m1 * m2)')
  

.. pb:macro:: getstack(Arg1-ArgN)

  Retrieve multiple contiguous values from the stack without changing the stack pointer.
  
  :param Arg1-ArgN: Registers to save values in
                    The first register corresponds to the highest address
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     getstack(s3, s4, s5) ; Get stack offset SP+3, SP+2, and SP+1 into s3, s4, s5
  

.. pb:macro:: getstackat(Arg1, Arg2)

  Retrieve values from the stack without changing the stack pointer.
  
  :param Arg1: Register to save value in
  :param Arg2: Offset from stack pointer (offset 1 is the first value) or a register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     getstackat(s3, 2)  ; Get the second value relative to the stack pointer
     getstackat(s3, s0) ; Get stack value pointed at by s0
  

.. pb:macro:: if(Arg1, Arg2, [Arg3], Arg4-ArgN)

  Generic if macro.
  
  :param Arg1: Boolean comparison expression.
  :param Arg2: True clause
  :param Arg3: Optional else clause or next else-if comparison expression
  :param Arg4-ArgN: Additional else-if clauses
  
  
  The Boolean expression must be of the form: ``reg op reg|expression`` where op is <, >, <=, >=, ==, !=, or &
  or of the form ``reg`` which is treated as ``reg != 0``.
  
  Signed comparison is invoked with ``signed(comparison expr.)``
  With signed comparison the right operand cannot be a named constant.
  
  With the & operator, a test instruction is used in place of compare. The true
  clause is executed when the result is non-zero.
  
  This macro performs a comparison of the left and right operands and then inserts
  the if* macro selected by the operation.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     if(s0 < s1, `load s0, 01', `load s0, 02')
     if(s0 != 0xa5, `load s0, 01')
     if(signed(s0 < -10), `load s0, 01') ; Signed comparison with signed()
  

.. pb:macro:: ifeq(Arg1, [Arg2])

  Low level if-equals
  
  :param Arg1: True clause
  :param Arg2: Optional else clause
  
  These macros insert labels and jump instructions to implement the behavior of
  an if-then or if-then-else statement testing for equality, inequality,
  greater-or-equal, or less-than.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     compare s0, s1
     ifeq(`load s3, 20
           output s3, MY_PORT',
     ; else
          `load s3, 30
           output s3, MY_OTHER_PORT')
  

.. pb:macro:: ifge(Arg1, [Arg2])

  Low level if-greater-or-equal.
  
  :param Arg1: True clause
  :param Arg2: Optional else clause
  
  .. seealso:: :pb:macro:`ifeq` for more information.
  

.. pb:macro:: iflt(Arg1, [Arg2])

  Low level if-less-than.
  
  :param Arg1: True clause
  :param Arg2: Optional else clause
  
  .. seealso:: :pb:macro:`ifeq` for more information.
  

.. pb:macro:: ifne(Arg1, [Arg2])

  Low level if-not-equal.
  
  :param Arg1: True clause
  :param Arg2: Optional else clause
  
  .. seealso:: :pb:macro:`ifeq` for more information.
  

.. pb:macro:: input16(Arg1, Arg2, Arg3, [Arg4])

  16-bit input.
  
  :param Arg1: MSB register of target
  :param Arg2: LSB register of target
  :param Arg3: Register pointing to low byte or MSB of source address when Arg4 is present
  :param Arg4: Optional register for LSB of source address
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a register pointing to the low port byte to read from.
  It is incremented twice to permit sequential use of input16().
  When four arguments are passed, Arg3 and Arg4 are a 16-bit pair of port constants to input from.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant P_ACCUM_L, 1a
     constant P_ACCUM_H, 1b
     reg16(P_ACCUM, P_ACCUM_H, P_ACCUM_L)
     reg16(rx, s4, s3)
     input16(rx, P_ACCUM)  ; Input direct from address
     load s0, P_ACCUM_L
     input16(rx, s0)       ; Input from indirect pointer
     input16(rx, s0)       ; Input next word
  

.. pb:macro:: instdata(Arg1, Arg2)

  Generate an INST directive from a pair of decimal values.
  
  :param Arg1: High 10-bits
  :param Arg2: Low byte
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     instdata(pbhex(0A, 0B))  ; Expands to inst 00A0B
  

.. pb:macro:: insttable_be(Arg1-ArgN)

  Convert a list of data into a series of INST directives in big-endian byte order.
  
  :param Arg1-ArgN: Data to convert in decimal format
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     insttable_be(pbhex(0a, 0b, 0c))
     Expands to:  inst 00a0b
                  inst 00c00
  

.. pb:macro:: insttable_le(Arg1-ArgN)

  Convert a list of data into a series of INST directives in little-endian byte order.
  
  :param Arg1-ArgN: Data to convert in decimal format
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     insttable_le(pbhex(0a, 0b, 0c))
     Expands to:  inst 00b0a
                  inst 0000c
     
     insttable_le(asciiord(`Pack strings into ROM'))
     
       inst 06150
       inst 06b63
       inst 07320
       ...
       inst 0206f
       inst 04f52
       inst 0004d
  

.. pb:macro:: iodefs(Arg1, Arg2-ArgN)

  Define a series of contiguous port or scratchpad memory constants.
  
  :param Arg1: Starting address for port or memory
  :param Arg2-ArgN: Constant names
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     iodefs(0, P_uart_out, P_uart_in, P_control)
     
     Expands to:
       constant P_uart_out, 00
       constant P_uart_in, 01
       constant P_control, 02
  

.. pb:macro:: isnum(Arg1)

  Determine if argument is a number in m4 syntax.
  
  .. note:: There must be no leading or trailing whitespace.
  
  
  :param Arg1: String to check
  
  :returns: 1 for true 0 for false
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     isnum(foo)  ; Expands to 0
     isnum(0x42) ; Expands to 1
  

.. pb:macro:: isr(Arg1, Arg2, [Arg3], Arg4)

  ISR definition.
  
  This creates an ISR that takes care of saving registers on the stack.
  Do not use RETURNI instructions in the code body. Instead call the
  leave_func() macro.
  By default the ISR returns with interrupts enabled. You can leave them
  disabled by passing "disable" as Arg3.
  
  :param Arg1: Address for ISR
  :param Arg2: Variable definitions (same format as passed to the vars() macro)
  :param Arg3: Optional return interrupt state "enable" | "disable" | empty
  :param Arg4: Code block for ISR body
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     isr(0x3FF, `s0, s1, s2', `load s0, 42
                               output s0, ff')
  

.. pb:macro:: jeq(Arg1)

  Jump if equal.
  
  :param Arg1: Label to jump to
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     compare s0, s1
     jeq(is_equal)   ; jumps if s0 == s1
  

.. pb:macro:: jge(Arg1)

  Jump if greater or equal.
  
  :param Arg1: Label to jump to
  

.. pb:macro:: jlt(Arg1)

  Jump if less than.
  
  :param Arg1: Label to jump to
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     compare s0, s1
     jlt(less_than)  ; jumps if s0 < s1
  

.. pb:macro:: jne(Arg1)

  Jump if not equal.
  
  :param Arg1: Label to jump to
  

.. pb:macro:: leave_func([Arg1])

  Return from func() macro code bodies.
  
  :param Arg1: Optional condition code (Z, NZ, C, or NC)
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     leave_func
     leave_func(Z)
  

.. pb:macro:: leave_isr([Arg1])

  Return from isr() macro code bodies.
  
  :param Arg1: Optional condition code (Z, NZ, C, or NC)
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     leave_isr(Z)
  

.. pb:macro:: load16(Arg1, Arg2, Arg3, [Arg4])

  16-bit load.
  
  :param Arg1: MSB destination register
  :param Arg2: LSB destination register
  :param Arg3: Decimal constant or expression or source MSB when Arg4 is present
  :param Arg4: Optional register for source LSB
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a constant literal. When four arguments are passed,
  Arg3 and Arg4 are a 16-bit register pair copied to the destination.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1, s0, 2014)         ; Load 16-bit literal
     load16(s1, s0, 200 * 11 + 5) ; Load 16-bit constant expresson
     load16(s1, s0, s3, s2)       ; Load 16-bits from a register pair
     load16(rx, ry)               ; Assuming rx and ry are reg16 definitions
  

.. pb:macro:: load_out(Arg1, Arg2, [Arg3])

  Load a register with a value and output to a port.
  
  :param Arg1: Value to load (constant or other register)
  :param Arg2: Port to output to
  :param Arg3: Optional register to load with value, uses _tempreg if omitted
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load_out(0x5A, 0x10)     ; Write 0x5A to port 0x10 using _tempreg
     load_out(0x5A, 0x10, sA) ; Write 0x5A to port 0x10 using sA
  

.. pb:macro:: load_store(Arg1, Arg2, [Arg3])

  Load a register with a value and store to scratchpad.
  
  :param Arg1: Value to load (constant or other register)
  :param Arg2: Scratchpad address to output to (constant or a register)
  :param Arg3: Optional Register to load with value, uses _tempreg if omitted
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load_store(0x42, 0x20)     ; Store 0x42 at scratchpad address 0x20 using _tempreg
     load_store(0x42, 0x20, sA) ; Store 0x42 at scratchpad address 0x20 using sA
  

.. pb:macro:: loadaddr(Arg1, Arg2, Arg3)

  Load a 16-bit address from a label.
  
  :param Arg1: MSB destination register
  :param Arg2: LSB destination register
  :param Arg3: Address label
  
  :returns: Result in Arg1, Arg2
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     my_func: return
     loadaddr(s1,s0, my_func)
  

.. pb:macro:: mask(Arg1-ArgN)

  Define a mask with specific bits set.
  
  :param Arg1-ArgN: Bit numbers to set in mask (0-7)
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     mask(0, 1, 2, 7)     ; Expands to 135 = 0x87
  

.. pb:macro:: maskh(Arg1)

  Alternate mask that can be used as a direct argument to a PicoBlaze instruction.
  
  :param Arg1: Bit numbers to set in mask (0-7)
  
  
  :returns: A mask in PicoBlaze hex format
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     test s0, maskh(3,4,5) ; Test if bits 3, 4, and 5 are clear
     jump z, is_clear
  

.. pb:macro:: mem16(Arg1, Arg2, Arg3)

  Create a constant for 16-bit memory and port addresses.
  This is similar to :pb:macro:`reg16` but it also generates
  named constants with "_H" and "_L" suffixes for
  byte access.
  
  :param Arg1: Name of constant
  :param Arg2: MSB address
  :param Arg3: LSB address
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     mem16(M_DATA, 0x05, 0x04) ; Allocate scratchpad 05, 04 for use as M_DATA
     load s0, M_DATA_L  ; load address 0x04
     load s1, M_DATA_H  ; load address 0x05
  

.. pb:macro:: negate(Arg1)

  2s complement negation.
  
  :param Arg1: Register to negate
  
  :returns: Result is in the same register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, 10'd
     negate(s0)     ; Result is -10
  

.. pb:macro:: negate16(Arg1, Arg2)

  16-bit 2s-complement negation.
  
  :param Arg1: MSB register to negate
  :param Arg2: LSB register to negate
  
  Result in Arg1, Arg2
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1,s0, -2600)
     negate16(s1, s0)     ; s1,s0 pair now contain +2600
  

.. pb:macro:: nop()

  No-op macro.
  Delay by one instruction without affecting processor state.

.. pb:macro:: not(Arg1)

  Logical not.
  
  :param Arg1: Register to invert
  
  :returns: Result is in the same register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, 5A
     not(s0)     ; Result is 0xA5
  

.. pb:macro:: not16(Arg1, Arg2)

  16-bit logical not.
  
  :param Arg1: MSB register to invert
  :param Arg2: LSB register to invert
  
  Result in Arg1, Arg2
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1,s0, 0x8F5A)
     not16(s1, s0) ; s1,s0 pair now contain 0x70A5
  

.. pb:macro:: or16(Arg1, Arg2, Arg3, [Arg4])

  16-bit logical OR.
  
  :param Arg1: MSB1
  :param Arg2: LSB1
  :param Arg3: Decimal constant or expression or MSB2 when Arg4 is present
  :param Arg4: Optional register for LSB2
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a constant literal. When four arguments are passed,
  Arg3 and Arg4 are a 16-bit pair to OR to the result.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1,s0, 0x10F0)
     or16(s1,s0, 0x008F)   ; Result is 0x10FF
     or16(s1,s0, s3,s2)    ; (s1,s0) OR (s3,s2)
  

.. pb:macro:: output16(Arg1, Arg2, Arg3, [Arg4])

  16-bit output.
  
  :param Arg1: MSB register of source
  :param Arg2: LSB register of source
  :param Arg3: Register pointing to low byte or MSB of output address when Arg4 is present
  :param Arg4: Optional register for LSB of output address
  
  When three arguments are passed, Arg3 is a register pointing to the low byte to output to.
  It is incremented twice to permit sequential use of output16().
  When four arguments are passed, Arg3 and Arg4 are a 16-bit pair of port address constants to output to.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(rx, 2014)
     output16(rx, P_ACCUM)  ; Output direct to port address
     load s0, P_ACCUM_L
     output16(rx, s0)       ; Output to indirect pointer
     output16(rx, s0)       ; Output next word
  

.. pb:macro:: outputstring(Arg1, Arg2, Arg3)

  Repeated string output operation.
  
  :param Arg1: Output port in m4 integer format or a constant name
  :param Arg2: Register used to hold characters
  :param Arg3: String to split into characters
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant UART_PORT, 0a
     outputstring(UART_PORT, s1, `My string')
     Expands to:
        load s1, "M"
        output s1, UART_PORT
        load s1, "y"
        output s1, UART_PORT
        ...
     
     outputstring(0x0A, s1, `My string') ; Without using a constant
  

.. pb:macro:: outputtable(Arg1, Arg2, Arg3-ArgN)

  Output a table of constants.
  
  :param Arg1: Output port in m4 integer format or a constant name
  :param Arg2: Temporary register for each constant
  :param Arg3-ArgN: Decimal values to output to port
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant UART_PORT, 0a
     outputtable(UART_PORT, sF, pbhex(DE,AD,BE,EF)) ; Output DE,AD,BE,EF to port
  

.. pb:macro:: pb2m4(Arg1)

  Convert PicoBlaze literals into m4 syntax. Performs the following conversions:
  
  * Character literal to ASCII ordinal: ``"c" -> 99``
  * Decimal to integer: ``20'd -> 20``
  * Hex to integer: ``5A -> 90``
  * Binary to integer: ``1100'b -> 12``
  
  
  :param Arg1: String to convert
  
  :returns: An integer in m4 syntax.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     pb2m4(10'd) expands to 10,  pb2m4("0") expands to 48
  

.. pb:macro:: pbhex(Arg1-ArgN)

  Convert a list of values in PicoBlaze hex format to decimal.
  
  :param Arg1-ArgN: Hex values to convert
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     pbhex(01, 02, 03, 0a, ff)  ; Expands to 1, 2, 3, 10, 255
  

.. pb:macro:: pop(Arg1-ArgN)

  Pop register values from a simulated stack in scratchpad RAM.
  
  Arguments should be passed in the same order as push() to restore register contents.
  
  :param Arg1-ArgN: Registers with values to pop
  
  .. seealso:: :pb:macro:`push` for more information.
  

.. pb:macro:: popvars()

  Remove definitions from previous call to vars().

.. pb:macro:: proc(Arg1, Arg2, Arg3)

  Procedure definition.
  
  :param Arg1: Label for procedure
  :param Arg2: Variable definitions (same format as passed to the vars() macro)
  :param Arg3: Code block for proc body
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     proc(add2, `s0 is result', `add result, 02')
     ...
     load s0, 42
     call add2
  

.. pb:macro:: push(Arg1-ArgN)

  Push register values onto a simulated stack in scratchpad RAM.
  
  The stack pointer grows from the end of the scratchpad to the start.
  
  :param Arg1-ArgN: Registers with values to push
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_stack(sa, 0x3F)
     push(s0)
     pop(s1)
     push(s3, s4, s5)  ; Push and pop multiple registers at once
     pop(s3, s4, s5)   ; Pop is performed in reverse order from push
  

.. pb:macro:: putstack(Arg1-ArgN)

  Store multiple contiguous values on the stack without changing the stack pointer.
  
  :param Arg1-ArgN: Registers to store values from
                    The first register corresponds to the highest address
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     putstack(s3, s4, s5) ; Put s3, s4, s5 into stack offset SP+3, SP+2, and SP+1
  

.. pb:macro:: putstackat(Arg1, Arg2)

  Store values to the stack without changing the stack pointer.
  
  :param Arg1: Register with value to store
  :param Arg2: Offset from stack pointer (offset 1 is the first value) or a register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     putstackat(s3, 2)  ; Put the second value relative to the stack pointer
     putstackat(s3, s0) ; Put stack value pointed at by s0
  

.. pb:macro:: qstr(Arg1)

  Add double quotes around a string.
  This is allows the use of macros to generate strings where substitution within "" would
  normally be suppressed.
  
  :param Arg1: String to quote
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     define(DATE_STAMP, `1 Jan 2015')
     string mystr$, qstr(DATE_STAMP)  ; Expands to string mystr$, "1 Jan 2015"
  

.. pb:macro:: randlabel([Arg1])

  Generate a random name for a label.
  
  :param Arg1: Optional prefix to name
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     randlabel(PREFIX_) ; Expands to "PREFIX_?????"
  

.. pb:macro:: reg16(Arg1, Arg2, Arg3)

  Create a virtual 16-bit register.
  The defined name can be used in place of the MSB, LSB pairs used in other 16-bit macros.
  
  :param Arg1: Name of virtual register
  :param Arg2: MSB register
  :param Arg3: LSB register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reg16(rx, s1, s0) ; rx = (s1, s0)
     reg16(ry, s5, s4) ; ry = (s5, s4)
     add16(rx, ry)     ; rx = rx + ry
     add16(rx, s3, s2) ; rx = rx + (s3, s2)
  

.. pb:macro:: reglower(Arg1, Arg2)

  Get the lower register from a :pb:macro:`reg16` or :pb:macro:`mem16` definition.
  
  :param Arg1: MSB of 16-bit register pair
  :param Arg2: LSB of 16-bit register pair
  
  .. seealso:: :pb:macro:`regupper` for more information.
  

.. pb:macro:: regupper(Arg1, Arg2)

  Get the upper register from a :pb:macro:`reg16` or :pb:macro:`mem16` definition.
  
  :param Arg1: MSB of 16-bit register pair
  :param Arg2: LSB of 16-bit register pair
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reg16(rx, s5, s4)
     load s1, regupper(rx) ; load upper byte from s5 (rx expands to "s5,s4")
     load s1, reglower(rx) ; load lower byte from s4
  

.. pb:macro:: repeat(Arg1, Arg2)

  Repeat an instruction or macro.
  
  :param Arg1: Instruction or macro string to repeat
  :param Arg2: Numper of repetitions
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     repeat(`output s0, 00', 3)
  

.. pb:macro:: repeatstr(Arg1, Arg2)

  Repeat a string.
  
  :param Arg1: Instruction or macro string to repeat
  :param Arg2: Numper of repetitions
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     repeat(`A', 3) ; Expands to "AAA"
  

.. pb:macro:: reteq()

  Return if equal.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     compare s0, s1
     reteq   ; return if s0 == s1
  

.. pb:macro:: retge()

  Return if greater or equal.

.. pb:macro:: retlt()

  Return if less than.

.. pb:macro:: retne()

  Return if not equal.

.. pb:macro:: retvalue(Arg1, Arg2)

  Place func return value onto the stack.
  
  Only call this macro inside a func code body
  
  :param Arg1: Register with value to save
  :param Arg2: Offset from end of return frame (starting at 1)
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     retvalue(s0, 1) ; First return value to be popped off after return
     retvalue(s1, 2) ; Second value to return from func
  

.. pb:macro:: reverse(Arg1-ArgN)

  Reverse arguments.
  
  :param Arg1-ArgN: List of arguments to reverse
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reverse(1,2,3) ; Expands to 3,2,1
  

.. pb:macro:: rl(Arg1, Arg2)

  Multi-bit rotate left.
  
  :param Arg1: Register to rotate
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     rl(s2, 4)  ; Rotate left by 4
  

.. pb:macro:: rl16(Arg1, Arg2, Arg3)

  16-bit rotate left.
  
  :param Arg1: MSB register to rotate
  :param Arg2: LSB register to rotate
  :param Arg3: Number of bits to rotate (0-16)
  

.. pb:macro:: rr(Arg1, Arg2)

  Multi-bit rotate right.
  
  :param Arg1: Register to rotate
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     rr(s2, 4)  ; Rotate right by 4
  

.. pb:macro:: rr16(Arg1, Arg2, Arg3)

  16-bit rotate right.
  
  :param Arg1: MSB register to rotate
  :param Arg2: LSB register to rotate
  :param Arg3: Number of bits to rotate (0-16)
  

.. pb:macro:: setbit(Arg1, Arg2)

  Set and clear bits in a register.
  
  :param Arg1: Register to modify
  :param Arg2: Bit number (0-7) to set or clear
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     setbit(s0, 2)
  

.. pb:macro:: setcy([Arg1])

  Set the carry flag.
  
  :param Arg1: Optional temporary register to modify. Uses temp register by default.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     setcy
     setcy(sF)
  

.. pb:macro:: setmask(Arg1, Arg2)

  Set mask bits in a register.
  
  :param Arg1: Register to modify
  :param Arg2: Mask value
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     setmask(s5, mask(0,1,2))
     setmask(s4, 0xF1)
  

.. pb:macro:: signed(Arg1)

  Convert a Boolean expression to use signed comparison.
  
  :param Arg1: Expression to convert
  
  
  This is used in the expression passed to the if(), for(), while(), and dowhile()
  macros to invoke signed comparison.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     signed(s0 < 4)  ; Expands to "s0 s< 4"
  

.. pb:macro:: signex(Arg1, Arg2)

  Sign extension.
  
  This extends the sign from an 8-bit number into a new register producing a 16-bit result.
  The argument order allows you to use this with virtual registers created with :pb:macro:`reg16`.
  
  :param Arg1: Register to extend sign into (MSB)
  :param Arg2: Register to test for sign bit (LSB)
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, 81     ; Sign bit is set
     signex(s1, s0)  ; s1 now contains 0xFF
     
     reg16(rx, s4,s5)
     load reglower(rx), 82
     signex(rx)
  

.. pb:macro:: sl0(Arg1, Arg2)

  Multi-bit shift left with '0' insert.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     sl0(s2, 4)  ; Shift left by 4
  

.. pb:macro:: sl0_16(Arg1, Arg2, Arg3)

  16-bit shift left, inserting '0'.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: sl1(Arg1, Arg2)

  Multi-bit shift left with '1' insert.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     sl1(s2, 4)  ; Shift left by 4
  

.. pb:macro:: sl1_16(Arg1, Arg2, Arg3)

  16-bit shift left, inserting '1'.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: sla(Arg1, Arg2)

  Multi-bit shift left with C insert.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     sla(s2, 4)  ; Shift left by 4
  

.. pb:macro:: sla_16(Arg1, Arg2, Arg3)

  16-bit arithmetic shift left, inserting C flag.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: slx(Arg1, Arg2)

  Multi-bit shift left with LSB sign extension.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     slx(s2, 4)  ; Shift left by 4
  

.. pb:macro:: slx_16(Arg1, Arg2, Arg3)

  16-bit sign extending shift left, duplicating LSB.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: sr0(Arg1, Arg2)

  Multi-bit shift right with '0' insert.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     sr0(s2, 4)  ; Shift right by 4
  

.. pb:macro:: sr0_16(Arg1, Arg2, Arg3)

  16-bit shift right, inserting '0'.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: sr1(Arg1, Arg2)

  Multi-bit shift right with '1' insert.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     sr1(s2, 4)  ; Shift right by 4
  

.. pb:macro:: sr1_16(Arg1, Arg2, Arg3)

  16-bit shift right, inserting '1'.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: sra(Arg1, Arg2)

  Multi-bit shift right with C insert.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     sra(s2, 4)  ; Shift right by 4
  

.. pb:macro:: sra_16(Arg1, Arg2, Arg3)

  16-bit arithmetic shift right, inserting C flag.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: srx(Arg1, Arg2)

  Multi-bit shift right with MSB sign extension.
  
  :param Arg1: Register to shift
  :param Arg2: Number of shifts
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     srx(s2, 4)  ; Shift right by 4
  

.. pb:macro:: srx_16(Arg1, Arg2, Arg3)

  16-bit sign extending shift right, duplicating MSB.
  
  :param Arg1: MSB register to shift
  :param Arg2: LSB register to shift
  :param Arg3: Number of bits to shift (0-16)
  

.. pb:macro:: store16(Arg1, Arg2, Arg3, [Arg4])

  16-bit store.
  
  :param Arg1: MSB register of source
  :param Arg2: LSB register of source
  :param Arg3: Register pointing to low byte or MSB of target address when Arg4 is present
  :param Arg4: Optional register for LSB of target address
  
  When three arguments are passed, Arg3 is a register pointing to the low byte to store to.
  It is incremented twice to permit sequential use of store16().
  When four arguments are passed, Arg3 and Arg4 are a 16-bit pair of address constants to store to.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(rx, 2014)
     store16(rx, M_ACCUM)  ; Store direct to address
     load s0, M_ACCUM_L
     store16(rx, s0)       ; Store to indirect pointer
     store16(rx, s0)       ; Store next word
  

.. pb:macro:: storestring(Arg1, Arg2, [Arg3])

  Store a string to scratchpad RAM.
  
  :param Arg1: Address of first byte
  :param Arg2: String to store
  :param Arg3: Optional register to load each character, uses _tempreg if omitted
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     storestring(0x10, `Hello')
  

.. pb:macro:: storestringat(Arg1, Arg2, [Arg3])

  Store a string to scratchpad RAM using a register address.
  
  :param Arg1: Pointer register to scratchpad address
  :param Arg2: String to store
  :param Arg3: Optional register to load each character, uses _tempreg if omitted
  
  The pointer register will finish with the address of the last character in the string.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, M_BUFFER
     storestringat(s0, `Hello')
  

.. pb:macro:: storetable(Arg1, Arg2, Arg3-ArgN)

  Store a table of constants in scratchpad RAM.
  
  :param Arg1: Address of first byte
  :param Arg2: Temporary register for each constant
  :param Arg3-ArgN: Decimal values to load in scratchpad
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s1, my_array
     storetable(0x10, sF, pbhex(DE,AD,BE,EF)) ; Load DE,AD,BE,EF into memory
     storetable(0x10, sF, 10, 11, 12)         ; Load decimals
  

.. pb:macro:: storetableat(Arg1, Arg2, Arg3-ArgN)

  Store a table of constants in scratchpad RAM.
  
  :param Arg1: Pointer register to scratchpad address
  :param Arg2: Temporary register for each constant
  :param Arg3-ArgN: Decimal values to load in scratchpad
  
  The pointer register will finish with the address of the last byte in the table.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s1, my_array
     storetableat(s1, sF, pbhex(DE,AD,BE,EF)) ; Load DE,AD,BE,EF into memory
     storetableat(s1, sF, 10, 11, 12)         ; Load decimals
  

.. pb:macro:: strhash(Arg1)

  Generate a 16-bit checksum constant from a string using the BSD algorithm.
  This does not dynamically compute a hash from variable data. It can be used to seed a PRNG
  from a build time string like a timestamp.
  
  :param Arg1: String to compute hash over
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     strhash(`Hello world')  ; Expands to 27566
     
     reg16(RS, s0, s1)
     load16(RS, strhash(DATE_STAMP TIME_STAMP)) ; Seed the PRNG with the build time
     call random16
  

.. pb:macro:: strlenc(Arg1)

  Return the length of a string constant, a portable string or a packed string.
  The argument is passed through the :pb:macro:`estr` macro to collapse escaped characters
  before counting them.
  
  :param Arg1: String to count length from. This is either a constant or the label to a string
               defined with string() or packed_string()
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, strlenc(`foobar\r\n')  ; Expands to 8
     
     packed_string(xyzzy, `This is a string')
     load s0, strlenc(xyzzy) ; Expands to 16
  

.. pb:macro:: sub16(Arg1, Arg2, Arg3, [Arg4])

  16-bit subtraction.
  
  :param Arg1: MSB1 register
  :param Arg2: LSB1 register
  :param Arg3: Decimal constant or expression or MSB2 when Arg4 is present
  :param Arg4: Optional register for LSB2
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a constant literal. When four arguments are passed,
  Arg3 and Arg4 are a 16-bit pair to add to the result.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     sub16(s1,s0, 0x1234) ; s1,s0 -= 0x1234
     sub16(s1,s0, s3,s2)  ; s1,s0 -= s3,s2
     sub16(rx, ry)        ; rx    -= ry
  

.. pb:macro:: swap(Arg1, Arg2)

  Swap registers.
  
  :param Arg1: Register 1
  :param Arg2: Register 2
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     swap(s0, s1) ; s1 -> s0 and s0 -> s1
  

.. pb:macro:: test16(Arg1, Arg2, Arg3, [Arg4])

  16-bit test.
  
  :param Arg1: MSB1
  :param Arg2: LSB1
  :param Arg3: Decimal constant or expression or MSB2 when Arg4 is present
  :param Arg4: Optional register for LSB2
  
  When three arguments are passed, Arg3 is a constant literal. When four arguments are passed,
  Arg3 and Arg4 are a 16-bit pair to use with the TEST comparison.
  .. note::
  
     On PicoBlaze-3, only the Z flag is set properly. On PicoBlaze-6 the C flag contains the XOR
     (odd parity) of all bits.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1,s0, 0x10F0)
     test16(s1,s0, 0x0080)  ; Z flag is clear
     test16(s1,s0, 0x0800)  ; Z flag is set
  

.. pb:macro:: testbit(Arg1, Arg2)

  Test if a bit is set or clear.
  
  :param Arg1: Register to test
  :param Arg2: Bit number (0-7) to test
  
  Z is set if bit is clear, Z is clear if bit is set
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     testbit(s1, 3)
     jump z, bit_cleared
  

.. pb:macro:: uniqlabel([Arg1])

  Generate a unique name for a label.
  The labels will be unique to each included file.
  
  :param Arg1: Optional prefix to name
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     uniqlabel(PREFIX_) ; Expands to "PREFIX_f0_0001"
  

.. pb:macro:: use_ascii2bcd(Arg1, Arg2, Arg3)

  Generate a function to convert an ASCII number to BCD.
  Invalid characters are converted to "0" digits.
  
  :param Arg1: Name of function to generate
  :param Arg2: Register for first function argument,
               the scratchpad address of the ASCII data
  :param Arg3: Register for length of the data
  
  :returns: The generated function has no return value. All registers are
            preserved on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_ascii2bcd(ascii2bcd, s0, s1)
     load s0, 20 ; Array address
     load s1, 05 ; Length
     call ascii2bcd
     ; The array now contains all BCD digits
  

.. pb:macro:: use_bcd2int(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)

  Generate a function to convert an unpacked BCD coded bytes to a
  variable size integer.
  
  The BCD input is stored in a buffer in
  scratchpad memory. The buffer address and length are passed as arguments.
  The converted result is overwritten to the leftmost portion of the buffer.
  The length register returns with the number of bytes in the result.
  This function handles conversion to any size integer as the result is
  guaranteed to be smaller than the initial buffer.
  Internal temp registers must be allocated for use by the macro. They must
  not include the _tempreg register.
  
  :param Arg1: Name of function to generate
  :param Arg2: First argument of generated function.
               Register with scratchpad BCD address
  :param Arg3: Register with number of bytes for scratchpad buffer
  :param Arg4: Internal temp register
  :param Arg5: Internal temp register
  :param Arg6: Internal temp register
  :param Arg7: Internal temp register
  :param Arg8: Internal temp register
  
  :returns: The converted integer value is located at the start of the buffer with the LSB
            first, opposite to the order of the BCD digits.
            The number of bytes in the converted integer is returned in the second argument
            to the function (Arg3 of this generator macro). All other registers are preserved
            on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_bcd2int(bcd2int, s0, s1, s2,s3,s4,s5,s6)
     load s0, 20 ; BCD buffer
     load s1, 03 ; 3 BCD digits long
     call bcd2int
     ; s1 contains the number of converted bytes
     ; scratchpad 20 (and possibly 21) contain the binary result
  

.. pb:macro:: use_bcdwrite(Arg1, Arg2, Arg3, Arg4)

  Generate a function to write an array of BCD coded bytes to an output port.
  
  :param Arg1: Name of function to generate
  :param Arg2: Register for first function argument,
               the scratchpad address of the source BCD data
  :param Arg3: Register for number of bytes to write
  :param Arg4: Port address to write. This is a fixed value that can't
               be changed at runtime
  
  :returns: The generated function has no return value. All registers are
            preserved on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant ConsolePort, FE
     use_bcdwrite(bcdwrite, s0, s1, ConsolePort)
     load s0, 20 ; Source address
     load s1, 05 ; Write 5 bytes
     call bcdwrite
  

.. pb:macro:: use_clock(Arg1)

  Define system clock frequency.
  
  **Only invoke once. Must be executed before any delay macros.**
  
  :param Arg1: Clock frequency in MHz. Limited to integer values.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_clock(50) ; 50 MHz clock
  

.. pb:macro:: use_delay_reg(Arg1)

  Define delay loop register.
  
  **Only invoke once. Must be executed before any delay macros.**
  This register must be different from the registers used in the delay_us() and delay_ms() macros.
  
  :param Arg1: Register to use for inner loop counter
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_delay_reg(sA)
  

.. pb:macro:: use_divide16x8(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, [Arg8])

  Divide 16 / 8 subroutine. Implements truncating division.
  
  :param Arg1: Subroutine name
  :param Arg2: Dividend MSB
  :param Arg3: Dividend LSB
  :param Arg4: Divisor
  :param Arg5: Quotient MSB
  :param Arg6: Quotient LSB
  :param Arg7: Remainder
  :param Arg8: Optional preamble code block. Also supresses return statement if present.
  
  
  The temp register is overwritten. It is sE by default. Call use_tempreg(reg_name)
  before invoking this macro to change it. The MSB of the dividend is destroyed.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_divide16x8(div16, s0,s1, s2, s3,s4, s5) ; (s3,s4) (rem s5) <= (s0,s1) / s2
     load16(s0,s1, 400)
     load s2, 5'd
     call div16
  

.. pb:macro:: use_divide16x8s(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, [Arg8])

  Signed Divide 16 / 8 subroutine. Implements truncating division.
  
  :param Arg1: Subroutine name
  :param Arg2: Dividend MSB (signed)
  :param Arg3: Dividend LSB
  :param Arg4: Divisor      (signed)
  :param Arg5: Quotient MSB (signed)
  :param Arg6: Quotient LSB
  :param Arg7: Remainder
  :param Arg8: Optional preamble code block. Also supresses return statement if present.
  
  
  The temp register is overwritten. It is sE by default. Call use_tempreg(reg_name)
  before invoking this macro to change it. The MSB of the dividend is destroyed.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_divide16x8s(div16, s0,s1, s2, s3,s4, s5) ; (s3,s4) (rem s5) <= (s0,s1) / s2
     load16(s0,s1, -400)
     load s2, 5'd
     call div16
  

.. pb:macro:: use_divide8x8(Arg1, Arg2, Arg3, Arg4, Arg5, [Arg6])

  Divide 8 / 8 subroutine. Implements truncating division.
  
  :param Arg1: Subroutine name
  :param Arg2: Dividend
  :param Arg3: Divisor
  :param Arg4: Quotient
  :param Arg5: Remainder
  :param Arg6: Optional preamble code block. Also supresses return statement if present.
  
  
  The temp register is overwritten. It is sE by default. Call use_tempreg(reg_name)
  before invoking this macro to change it.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_divide8x8(div8, s0, s1, s2, s3) ; s2 (rem s3) <= s0 / s1
     load s0, 20'd
     load s1, 4'd
     call div8
  

.. pb:macro:: use_divide8x8s(Arg1, Arg2, Arg3, Arg4, Arg5, [Arg6])

  Signed Divide 8 / 8 subroutine. Implements truncating division.
  
  :param Arg1: Subroutine name
  :param Arg2: Dividend (signed)
  :param Arg3: Divisor (signed)
  :param Arg4: Quotient
  :param Arg5: Remainder
  :param Arg6: Optional preamble code block. Also supresses return statement if present.
  
  Same arguments as unsigned 8 / 8 divide.
  .. seealso:: :pb:macro:`use_divide8x8` for more information.
  

.. pb:macro:: use_divide8xk(Arg1, Arg2, Arg3, Arg4)

  Divide 8 / constant subroutine with 8-bit result.
  
  :param Arg1: Subroutine name
  :param Arg2: Dividend
  :param Arg3: Constant divisor (can be wider than 8-bits)
  :param Arg4: Result quotient
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_divide8xk(div8k5, s0, 5, s4)  ; s4 = s0 / 5
     load s0, 30'd
     call div8k5    ; 30 / 5 => 6
  

.. pb:macro:: use_expr_all()

  Configure all multiplication and divide subroutines.
  Use this in conjunction with Opbasm's dead code removal to guarantee that
  only the arithmetic subroutines in use will be assembled.
  This is equivalent to calling the following macros with no arguments:
  
  * :pb:macro:`use_expr_mul`
  * :pb:macro:`use_expr_muls`
  * :pb:macro:`use_expr_mulsu`
  * :pb:macro:`use_expr_div`
  * :pb:macro:`use_expr_divs`
  * :pb:macro:`use_expr_div16`
  * :pb:macro:`use_expr_div16s`

.. pb:macro:: use_expr_div([Arg1, Arg2, Arg3, Arg4])

  Configure unsigned division for expressions.
  All arguments are optional.
  
  :param Arg1: Optional Dividend (default is s8)
  :param Arg2: Optional Divisor  (default is s9)
  :param Arg3: Optional Internal result Quotient (default is sA) preserved on stack
  :param Arg4: Optional Internal result Remainder (default is sB) preserved on stack
  
  :returns: The result is copied to Arg1, Arg2
  

.. pb:macro:: use_expr_div16([Arg1, Arg2, Arg3, Arg4, Arg5, Arg6])

  Configure unsigned 16x8 division for expressions. This creates a new function "expr_div16"
  used by the expr2() macro.
  
  :param Arg1: Optional MSB of Dividend (default is s7)
  :param Arg2: Optional LSB of Dividend (default is s8)
  :param Arg3: Optional Divisor  (default is s9)
  :param Arg4: Optional MSB of Quotient (default is sA)
  :param Arg5: Optional LSB of Quotient (default is sB)
  :param Arg6: Optional Remainder (default is sC)
  
  :returns: The generated function places the quotient in Arg1,Arg2 and the remainder in Arg3.
  
  Arg4-6 act as temp registers and are restored
  from the stack.

.. pb:macro:: use_expr_div16s([Arg1, Arg2, Arg3, Arg4, Arg5, Arg6])

  Configure signed 16x8 division for expressions. This creates a new function "expr_div16s"
  used by the expr2s() macro.
  
  :param Arg1: Optional MSB of Dividend (default is s7)
  :param Arg2: Optional LSB of Dividend (default is s8)
  :param Arg3: Optional Divisor  (default is s9)
  :param Arg4: Optional MSB of Quotient (default is sA)
  :param Arg5: Optional LSB of Quotient (default is sB)
  :param Arg6: Optional Remainder (default is sC)
  
  :returns: The generated function places the quotient in Arg1,Arg2 and the remainder in Arg3.
  
  Arg4-6 act as temp registers and are restored
  from the stack.

.. pb:macro:: use_expr_divs([Arg1, Arg2, Arg3, Arg4])

  Configure signed division for expressions.
  All arguments are optional.
  
  :param Arg1: Optional Dividend (default is s8)
  :param Arg2: Optional Divisor  (default is s9)
  :param Arg3: Optional Internal result Quotient (default is sA) preserved on stack
  :param Arg4: Optional Internal result Remainder (default is sB) preserved on stack
  
  :returns: The result is copied to Arg1, Arg2
  

.. pb:macro:: use_expr_mul([Arg1, Arg2, Arg3, Arg4])

  Configure unsigned multiplication for expressions.
  All arguments are optional.
  
  :param Arg1: Optional Multiplicand (default is s8)
  :param Arg2: Optional Multiplier   (default is s9)
  :param Arg3: Optional Internal result MSB (default is sA) preserved on stack
  :param Arg4: Optional Internal result LSB (default is sB) preserved on stack
  
  :returns: The result is copied to Arg1, Arg2
  

.. pb:macro:: use_expr_muls([Arg1, Arg2, Arg3, Arg4])

  Configure signed multiplication for expressions.
  All arguments are optional.
  
  :param Arg1: Optional Multiplicand (default is s8)
  :param Arg2: Optional Multiplier   (default is s9)
  :param Arg3: Optional Internal result MSB (default is sA) preserved on stack
  :param Arg4: Optional Internal result LSB (default is sB) preserved on stack
  
  :returns: The result is copied to Arg1, Arg2
  

.. pb:macro:: use_expr_mulsu([Arg1, Arg2, Arg3, Arg4])

  Configure signedxunsigned (16x8) multiplication for expressions.
  All arguments are optional.
  
  :param Arg1: Optional Multiplicand (default is s8)
  :param Arg2: Optional Multiplier   (default is s9)
  :param Arg3: Optional Internal result MSB (default is sA) preserved on stack
  :param Arg4: Optional Internal result LSB (default is sB) preserved on stack
  
  :returns: The result is copied to Arg1, Arg2
  

.. pb:macro:: use_hexwrite(Arg1, Arg2, Arg3, Arg4)

  Generate a function to write an array to an output port as ASCII hex.
  
  :param Arg1: Name of function to generate
  :param Arg2: Register for first function argument,
               the scratchpad address of the source data
  :param Arg3: Register for number of bytes to write
  :param Arg4: Port address to write. This is a fixed value that can't
               be changed at runtime
  
  :returns: The generated function has no return value. All registers are
            preserved on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant ConsolePort, FE
     use_hexwrite(hexwrite, s0, s1, ConsolePort)
     load s0, 20 ; Source address
     load s1, 05 ; Write 5 bytes
     call hexwrite
  

.. pb:macro:: use_int2bcd(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)

  Generate a function to convert an integer to unpacked BCD coded
  bytes stored in a scratchpad buffer.
  
  The number to convert is passed on the stack as one or more bytes
  with the MSB pushed last. The buffer size is fixed after generating
  the function. You must ensure it is large enough to contain the
  largest integer you expect to convert. Each buffer byte corresponds to
  one decimal digit. The converted BCD number is right justified within
  the buffer with leading 0's for padding. The least significant digit
  is always at the end of the buffer.
  
  :param Arg1: Name of function to generate
  :param Arg2: Number of bytes for scratchpad buffer (fixed constant)
  :param Arg3: First argument of generated function.
               Register with destination scratchpad address
  :param Arg4: Register containing number of bytes of data to convert
               on the stack
  :param Arg5: Internal temp register
  :param Arg6: Internal temp register
  :param Arg7: Internal temp register
  :param Arg8: Internal temp register
  
  :returns: The generated function has no return value. All registers are
            preserved on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_int2bcd(int2bcd, 5, s0, s1, s2, s3, s4, s5) ; Support up to 5 decimal digits
     load s0, 20 ; Dest address
     load s1, 02 ; Convert 16-bit number (2-bytes)
     load16(s4,s3, 31337)
     push(s3,s4) ; Put number to convert on stack; LSB then MSB
     call int2bcd
  

.. pb:macro:: use_memcopy(Arg1, Arg2, Arg3, Arg4)

  Generate a function to copy an array in scratchpad memory.
  
  :param Arg1: Name of function to generate
  :param Arg2: Register for first function argument,
               the scratchpad address of the source array
  :param Arg3: Register for destination address
  :param Arg4: Register for number of bytes to copy
  
  :returns: The generated function has no return value. All registers are
            preserved on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_memcopy(memcopy, s0, s1, s2)
     load s0, 20 ; Source address
     load s1, 30 ; Dest address
     load s2, 05 ; Copy 5 bytes
     call memcopy
  

.. pb:macro:: use_memset(Arg1, Arg2, Arg3, Arg4)

  Generate a function to set an array in scratchpad memory.
  
  :param Arg1: Name of function to generate
  :param Arg2: Register for first function argument,
               the scratchpad address of the destination array
  :param Arg3: Register for number of bytes to copy
  :param Arg4: Register for value to set array bytes to
  
  :returns: The generated function has no return value. All registers are
            preserved on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_memset(memset, s0, s1, s2)
     load s0, 20 ; Destination address
     load s1, 05 ; Copy 5 bytes
     load s2, 00 ; Set all bytes to 0
     call memset
  

.. pb:macro:: use_memwrite(Arg1, Arg2, Arg3, Arg4)

  Generate a function to write an array of bytes to an output port.
  
  :param Arg1: Name of function to generate
  :param Arg2: Register for first function argument,
               the scratchpad address of the source array
  :param Arg3: Register for number of bytes to write
  :param Arg4: Port address to write. This is a fixed value that can't
               be changed at runtime
  
  :returns: The generated function has no return value. All registers are
            preserved on the stack.
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     constant ConsolePort, FE
     use_memwrite(memwrite, s0, s1, ConsolePort)
     load s0, 20 ; Source address
     load s1, 05 ; Write 5 bytes
     call memwrite
  

.. pb:macro:: use_multiply8x8(Arg1, Arg2, Arg3, Arg4, Arg5, [Arg6])

  Multiply 8 x 8 subroutine.
  
  :param Arg1: Subroutine name
  :param Arg2: Multiplicand
  :param Arg3: Multiplier
  :param Arg4: Result MSB
  :param Arg5: Result LSB
  :param Arg6: Optional preamble code block. Also supresses return statement if present.
  
  
  The temp register is overwritten. It is sE by default. Call use_tempreg(reg_name)
  before invoking this macro to change it. The optional code block can be used to insert
  initialization code at the beginning of the generated subroutine. The final ``return`` is
  omitted so that you can also append finalization code. You must provide an explicit ``return``
  when the preamble block is in use.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_multiply8x8(mul8, s0, s1, s3, s2) ; (s3, s2) = s0 * s1
     load s0, 04
     load s1, 05
     call mul8
  

.. pb:macro:: use_multiply8x8s(Arg1, Arg2, Arg3, Arg4, Arg5, [Arg6])

  Signed Multiply 8 x 8 subroutine.
  
  :param Arg1: Subroutine name
  :param Arg2: Multiplicand (signed)
  :param Arg3: Multiplier (signed)
  :param Arg4: Result MSB
  :param Arg5: Result LSB
  :param Arg6: Optional preamble code block. Also supresses return statement if present.
  
  Same arguments as unsigned multiply. Both arguments are treated as signed numbers.
  .. seealso:: :pb:macro:`use_multiply8x8` for more information.
  

.. pb:macro:: use_multiply8x8su(Arg1, Arg2, Arg3, Arg4, Arg5, [Arg6])

  SignedxUnsigned Multiply 8 x 8 subroutine.
  
  :param Arg1: Subroutine name
  :param Arg2: Multiplicand (signed)
  :param Arg3: Multiplier (unsigned)
  :param Arg4: Result MSB
  :param Arg5: Result LSB
  :param Arg6: Optional preamble code block. Also supresses return statement if present.
  
  Same arguments as unsigned multiply. Only the multiplicand is treated as signed.
  .. seealso:: :pb:macro:`use_multiply8x8` for more information.
  

.. pb:macro:: use_multiply8xk(Arg1, Arg2, Arg3, Arg4, Arg5)

  Multiply 8 x constant subroutine with 16-bit result.
  
  :param Arg1: Subroutine name
  :param Arg2: Multiplicand
  :param Arg3: Constant multiplier (can be wider than 8-bits)
  :param Arg4: Result MSB
  :param Arg5: Result LSB
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_multiply8xk(mul8k5, s0, 5, s5, s4)  ; (s5, s4) = s0 * 5
     load s0, 7'd
     call mul8k5   ; 7 * 5 => 35
  

.. pb:macro:: use_multiply8xk_small(Arg1, Arg2, Arg3, Arg4)

  Multiply 8 x constant subroutine with 8-bit result.
  
  :param Arg1: Subroutine name
  :param Arg2: Multiplicand
  :param Arg3: Constant multiplier
  :param Arg4: Result byte
  
  It is your responsibility to ensure that the result doesn't overflow the
  size of a byte.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_multiply8xk_small(mul8k5, s0, 5, s4)  ; s4 = s0 * 5
     load s0, 7'd
     call mul8k5  ; 7 * 5 => 35
  

.. pb:macro:: use_packed_strings(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, [Arg7])

  Configure packed string handling.
  
  This uses string data packed into INST statements stored in big-endian order.
  You must provide a routine to read pairs of characters from a dual-ported memory.
  The packed_string() macro calls into a generic handler routine that
  scans for a NUL terminator on data read from an external ROM.
  
  :param Arg1: Register to store even characters
  :param Arg2: Register to store odd characters
  :param Arg3: Register for MSB of address to string
  :param Arg4: Register for LSB of address to string
  :param Arg5: Label of user provided function to process each character (Only needs to handle the even char register)
  :param Arg6: Label of user provided function to read pairs of characters from memory
  :param Arg7: Optional name of macro to generate strings (default is "packed_string")
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_packed_strings(s0, s1, s5,s4, write_char, read_next_chars)
     ; read_next_chars is called until a NUL is found
     ; write_char is called for each character stored in s0
     
  
  This generates a new macro with the name used for Arg7.
  You must take steps to ensure that the normal execution path skips the
  instructions generated by this macro.
  It takes the following arguments:
  
  :param Arg1: Label used to identify string
  :param Arg2: String value. This can contain all of the escape chars supported by estr/cstr
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     jump main
         packed_string(my_string, `Hello, World!\r\n')
     main:
     ...
         call my_string ; Output the string
  

.. pb:macro:: use_random16(Arg1, Arg2, Arg3, Arg4, Arg5)

  16-bit pseudo-random number generator.
  Based on George Marsaglia's xorshift algorithm. This generates a full cycle of 65535 values.
  Expands to 23 instructions.
  
  :param Arg1: Label to use for random function
  :param Arg2: MSB of 16-bit random state variable (Initialize this with a non-zero seed)
  :param Arg3: LSB of 16-bit random state variable (Initialize this with a non-zero seed)
  :param Arg4: MSB of a 16-bit internal temp register
  :param Arg5: LSB of a 16-bit internal temp register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     reg16(RS, s0,s1)
     use_random16(random, RS, sA, sB)
     ...
     load16(RS, 0x1234)   ; Seed the PRNG (Use an external entropy source like an ADC in real life)
     call random
  

.. pb:macro:: use_random8(Arg1, Arg2)

  8-bit pseudo-random number generator.
  Based on George Marsaglia's xorshift algorithm. Generates a full cycle of 255 values.
  Expands to 11 instructions.
  
  :param Arg1: label to use for random function
  :param Arg2: Random state variable (Initialize this with a non-zero seed)
  
  
  The common temp register is destructively modified.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     namereg s8, RS
     use_random8(random, RS)
     ...
     load RS, 5A   ; Seed the PRNG (Use an external entropy source like an ADC in real life)
     call random
  

.. pb:macro:: use_stack(Arg1, Arg2)

  Initialize and define stack pointer register.
  
  **Only invoke once. Must be executed before any push() or pop() macros**
  
  :param Arg1: Stack pointer register
  :param Arg2: Scratchpad address for top of stack
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     namereg sA, SP      ; Reserve stack pointer register
     use_stack(SP, 0x3F) ; Start stack at end of 64-byte scratchpad
  

.. pb:macro:: use_strings(Arg1, Arg2, Arg3, Arg4, [Arg5])

  Configure string handling for either PB3 or PB6.
  
  This will set up a string output function for either platform
  On PB6, the string() macro calls into a generic handler routine that
  scans for a NUL terminator.
  
  :param Arg1: Register to store each character
  :param Arg2: Register for MSB of address to string (Only used on PB6)
  :param Arg3: Register for LSB of address to string (Only used on PB6)
  :param Arg4: Label of user provided function to process each character
  :param Arg5: Optional name of macro to generate strings (default is "string")
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_strings(s0, s5,s4, write_char)
       ; write_char is called for each character stored in s0 using the "string()" macro
     use_strings(s1, s5, s4, write_char_to_console, console_str)
       ; write_char_to_console is called for each character stored in s1 using the "console_str()" macro
     
  
  This generates a new macro with the name used for Arg5.
  You must take steps to ensure that the normal execution path skips the
  instructions generated by this macro.
  It takes the following arguments:
  
  :param Arg1: Label used to identify string
  :param Arg2: String value. This can contain all of the escape chars supported by estr/cstr
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     jump main
         string(my_string, `Hello, World!\r\n')
     main:
     ...
         call my_string ; Output the string
  

.. pb:macro:: use_tempreg(Arg1)

  Define a common temporary register used by other macros.
  This allows you to change the temp register from its default of ``sE``.
  
  :param Arg1: Register to use for temp register
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_tempreg(sA)
     load _tempreg, 01 ; Puts 0x01 into sA
  

.. pb:macro:: var_count_ms(Arg1, Arg2)

  Generate 16-bit millisecond count for variable delay macro var_delay_ms().
  
  :param Arg1: Milliseconds to delay
  :param Arg2: Max milliseconds for the delay loop (from definition using var_delay_ms())
  

.. pb:macro:: var_count_us(Arg1, Arg2)

  Generate 16-bit microsecond count for variable delay macro var_delay_us().
  
  :param Arg1: Microseconds to delay
  :param Arg2: Max microseconds for the delay loop (from definition using var_delay_us())
  

.. pb:macro:: var_delay_ms(Arg1, Arg2, Arg3)

  Variable delay by milliseconds
  
  :param Arg1: Maximum milliseconds to delay
  :param Arg2: MSB of delay counter
  :param Arg3: LSB of delay counter
  
  
  The var_count_ms() macro generates a 16-bit count value that is loaded
  into the counter registers before calling the delay function.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_clock(50) ; 50 MHz clock
     define(MAXDELAY, 10) ; 10ms max delay
     reg16(dly_count, s4,s5)
  
    delay: var_delay_ms(MAXDELAY, dly_count)
           return
    ...
    load16(dly_count, var_count_ms(1, MAXDELAY))
    call delay ; Delay for 1 ms
    ...
    load16(dly_count, var_count_ms(8, MAXDELAY))
    call delay ; Delay for 8 ms

.. pb:macro:: var_delay_us(Arg1, Arg2, Arg3)

  Variable delay by microseconds.
  
  :param Arg1: Maximum microseconds to delay
  :param Arg2: MSB of delay counter
  :param Arg3: LSB of delay counter
  
  
  The var_count_us() macro generates a 16-bit count value that is loaded
  into the counter registers before calling the delay function.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     use_clock(50) ; 50 MHz clock
     define(MAXDELAY, 900) ; 900 us max delay
     reg16(dly_count, s4,s5)
  
    delay: var_delay_us(MAXDELAY, dly_count)
           return
    ...
    load16(dly_count, var_count_us(100, MAXDELAY))
    call delay ; Delay for 100 us
    ...
    load16(dly_count, var_count_us(800, MAXDELAY))
    call delay ; Delay for 800 us

.. pb:macro:: vars(Arg1-ArgN)

  Define variables.
  
  :param Arg1-ArgN: Series of variable alias expressions where an alias expression is:
                    <reg> is <alias> [:= value]
  
  The alias becomes an alternate name for the register. It is loaded with a value if the
  optional initializer is included. The value can be any constant expression or register.
  Use the popvars() macro to remove the variable definitions.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     vars(s0 is counter := 0, s1 is sum, s2 is max := 20*3)
     ; s0 is loaded with 0
     ; s2 is loaded with 60
     ; Three macros "counter", "sum", and "max" will expand into their registers
  

.. pb:macro:: warnmsg(Arg1)

  Print a warning message and continue.
  
  :param Arg1: message string
  
  This prints a message with the file and line number where the macro was invoked for debugging.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     warnmsg(`Bad arguments to foo macro')
  

.. pb:macro:: while(Arg1, Arg2)

  While loop.
  
  :param Arg1: Boolean comparison expression
  :param Arg2: Code block for loop body
  
  The Boolean expression must be of the form: ``reg op reg|expression`` where op is <, >=, ==, !=, or &
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load s0, 00
     while(s0 < 10, `output s3, P_foo
                     add s0, 01')
  

.. pb:macro:: words_be(Arg1-ArgN)

  Convert 16-bit words into bytes with big-endian order.
  
  :param Arg1-ArgN: Numbers to split into bytes
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     words_be(0xff01, 0xff02) ; Expands to 1, 255, 2, 255
  

.. pb:macro:: words_le(Arg1-ArgN)

  Convert 16-bit words into bytes with little-endian order.
  
  
  :param Arg1-ArgN: Numbers to split into bytes
  
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     words_le(0xff01, 0xff02) ; Expands to 255, 1, 255, 2
  

.. pb:macro:: xor16(Arg1, Arg2, Arg3, [Arg4])

  16-bit logical XOR.
  
  :param Arg1: MSB1
  :param Arg2: LSB1
  :param Arg3: Decimal constant or expression or MSB2 when Arg4 is present
  :param Arg4: Optional register for LSB2
  
  :returns: Result in Arg1, Arg2
  
  When three arguments are passed, Arg3 is a constant literal. When four arguments are passed,
  Arg3 and Arg4 are a 16-bit pair to XOR to the result.
  
  .. rubric:: Example:
  
  .. code-block:: picoblaze
  
     load16(s1,s0, 0x5A01)
     xor16(s1,s0, 0xF0FF)   ; Result is 0xAAFE
     xor16(s1,s0, s3,s2)    ; (s1,s0) XOR (s3,s2)
  


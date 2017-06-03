================================
PicoBlaze architecture reference
================================


This is an overview of the architecture and assembly language used for the PicoBlaze-6 and PicoBlaze-3 microcontrollers.



Instructions
------------

The following instruction mnemonics are used in the PicoBlaze processors:

.. rubric:: PicoBlaze-3 and PicoBlaze-6

=========== =========== =========== =========== ===========
`add`_      `addcy`_    `and`_      `call`_     `compare`_
`disable`_  `enable`_   `fetch`_    `input`_    `jump`_
`load`_     `or`_       `output`_   `return`_   `returni`_
`rl`_       `rr`_       `sl0`_      `sl1`_      `sla`_
`slx`_      `sr0`_      `sr1`_      `sra`_      `srx`_
`store`_    `sub`_      `subcy`_    `test`_     `xor`_
=========== =========== =========== =========== ===========

.. _inst-pb6:

.. rubric:: PicoBlaze-6 only

=========== ============ =========== =========== ==============
`call@`_    `comparecy`_ `hwbuild`_  `jump@`_    `load&return`_
`outputk`_  `regbank`_   `star`_     `testcy`_
=========== ============ =========== =========== ==============

.. rubric:: Common directives

=============== =========== =========== =========== ===========
`address`_      `constant`_ `namereg`_  `include`_  `inst`_
`default_jump`_
=============== =========== =========== =========== ===========

The Xilinx PicoBlaze-3 assembler only supports the ``address``, ``constant``, and ``namereg`` directives.

.. rubric:: PicoBlaze-6 directives

========== ===========
`string`_  `table`_
========== ===========

The assembler is case insensitive for commands. Uppercase or mixed case mnemonics can also be used. Labels and renamed registers are case sensitive.


Instruction formats
-------------------

PicoBlaze assembly follows the usual convention of having a single instruction or directive per line.
An optional label creates a named reference that can be referred to by other instructions. An optional single line comment can appear at the end prefixed by a ";" character.

  Basic syntax
    .. image:: ../images/statement.svg
    
Blank lines are ignored. Labels and comments can be used without an instruction.

Instructions generally use the following formats with a few minor exceptions:

  Register
    .. image:: ../images/register.svg

  Immediate
    .. image:: ../images/immediate.svg
  
  Indirect scratchpad / I/O port
    .. image:: ../images/indirect_sp.svg
    
  Indirect jump / call
    .. image:: ../images/indirect_jump.svg

  Conditional
    .. image:: ../images/conditional.svg

  Outputk
    .. image:: ../images/outputk.svg
  
The fields have the following meanings:

================== ================================================================================
``<instruction>``  Instruction mnemonic or directive
``<register>``     One of the 16 currently active registers (s0-sF)
``<literal>``      Constant literal (hex [nn], decimal [nnn'd], binary [nnnnnnnn'b], or char ["c"])
================== ================================================================================

Literal syntax
--------------

The language supports a number of different formats for representing literal values. These are constant numeric and character values that are used as operands to many instructions.

  Hexadecimal

    A hex byte is the default format for literal numbers. It is represented by two hex digits with no additional
    syntactic marker. The :ref:`inst-address` directive takes a special 12-bit hex literal as an address.
    
  Decimal
  
    Decimal byte values are written in the form ``nnn'd``.
    
  Binary
  
    Binary byte values are written in the form ``nnnnnnnn'b``.
    
  Characters
  
    Printable ASCII character literals are written in the form ``"c"``.
    
You can invert the value of a literal by prefixing it with "~".

  .. code-block:: picoblaze
  
    load s0,  50
    load s1, ~50  ; Same as 0xAF
    
    ; This is useful for inverting mask constants:
    constant BIT_FIELD_MASK, 31
    or  s0,  BIT_FIELD_MASK     ; Set bits in the mask
    and s0, ~BIT_FIELD_MASK     ; Clear bits in the mask

There are predefined character constants for the following ASCII control characters:

==== ==== ==== ====
NUL  BEL  BS   HT
LF   VT   CR   ESC
DEL  DCS  ST
==== ==== ==== ====

.. code-block:: picoblaze

  load s0, CR
  output s0, COM_PORT
  load s0, LF
  output s0, COM_PORT

There are additional special constants containing the date and time the code was assembled:

* timestamp_hours
* timestamp_minutes
* timestamp_seconds

* datestamp_year (two digit year from 00 - 99)
* datestamp_month
* datestamp_day

You can access environment variables by prefixing their name with "%". They must evaluate to a valid PicoBlaze literal.

.. code-block:: picoblaze

  ; You could have a build script generating different images
  ; for multiple PicoBlaze instances. Their behavior could
  ; be controlled by an environment variable "PROC_NUM".

  load s0, %PROC_NUM
  compare s0, 00
  jump    Z,  proc_0   ; Branch to special code for processor 0
  ; Remaining processors here

.. note::

  The Opbasm command line option ``--define=NAME[=VALUE]`` can be used in conjunction with the m4 preprocesor as another way to :ref:`alter code generation at build time <m4-conditional-code>`.

Address spaces
--------------

The PicoBlaze has a simple architecture that operates on information stored in the following address spaces:

  Registers
    A set of 16 8-bit registers. They default to the registers named s0-sF but can be renamed with the :ref:`inst-namereg` directive.

    The PicoBlaze-3 has a single bank of registers. PicoBlaze-6 has a second bank of 16 (bank B) that can be swapped in or out using the :ref:`inst-regbank` instruction. The default register names are case-insensitive so "SA", "sa", "Sa", and "sA" are all valid.
    
    All register values, flags, and the call stack are initialized to 0 on initial FPGA power up but the registers retain their values on subsequent resets. Your code should not assume registers are cleared after a reset.

  Instruction memory
    Instruction words are stored in an isolated memory. Limited to 1K on PicoBlaze-3. Selectable between 1K, 2K, or 4K on PicoBlaze-6. This memory is implemented outside the PicoBlaze core component and attached to the instruction memory port. Because the PicoBlaze is a Harvard architecture micro, this memory is not directly accessible from within your program. However, a dual ported memory can be implemented to access data stored in instruction memory or to modify instructions through the I/O port interface.
    
  Scratchpad memory
    A small pool of RAM used as a local memory. This is 64 bytes on the PicoBlaze-3 and
    configurable for 64, 128, or 256 bytes on PicoBlaze-6. This memory is accessed with the `fetch`_ and `store`_ instructions.
    
  I/O ports
    A set of 256 input and 256 output ports are used to interact with external hardware. These ports are
    accessed with the `input`_ and `output`_ instructions. Additional logic must be provided to decode these ports.
    
  Outputk ports
    The PicoBlaze-6 has a special set of 16 output ports intended for directly driving constant values with minimal overhead.
    This is accessed with the `outputk`_ instruction.
    
  Call stack
    A hardware call stack is maintained to track return addresses for subroutines and the interrupt handler. You can have a call depth of up to 31 subroutines on PicoBlaze-3 and 30 subroutines on PicoBlaze-6. On PB3 the stack will silently roll over if you exceed the limit. On PB6 it will detect the overflow and generate an internal reset, restarting the program from address 0x000.

Flags
-----

The PicoBlaze processor has two internal status flags that represent metadata from ALU operations. They are used to evaluate the result of an operation and execute conditional code. These are the the zero "Z" flag and the carry "C" flag.

The Z flag is set when the result of an operation is zero and cleared otherwise. The C flag is set when an arithmetic carry or borrow (for subtraction) is generated. As a special case, the C flag is set by the :ref:`inst-test` and :ref:`inst-testcy` instructions to reflect the odd parity of their result. The :ref:`inst-hwbuild` instruction always sets the C flag unconditionally.

The most common application of the flags is to execute conditional code after a :ref:`inst-compare` instruction. The table shown for ``compare`` indicates how to interpret the flags for various Boolean comparison operations. Not all comparisons are possible with a single instruction because PicoBlaze can only test one flag at a time.

The `return`_ and `jump`_ have a conditional form where the first operand is a flag value of C, NC, Z, or NZ. When it is C or Z whese instructions conditionally return or jump when the respective flag is set. for NC and NZ, the branch is taken when the flag is clear.


Interrupts
----------

The PicoBlaze has a single ``interrupt`` input. When interrupts are enabled and this pin transitions high, the normal execution is suspended and the processor jumps to a special interrupt handler routine. This behavior lets your code respond to external events in the FPGA fabric without having to explicitly poll for changes in state. Handling multiple interrupts requires external logic to multiplex them onto the single input provided by the PicoBlaze.

The PB3 has a fixed interrupt vector address of 0x3FF which is the last instruction in the 1K instruction memory space. You must insert a `jump`_ instruction here that branches to your ISR code. The PB6 defaults to 0x3FF but you can change the default vector address with the ``interrupt_vector`` generic on the ``KCPSM6`` component. This gives you the option to place the vector at the start of your ISR without the need for an extra ``jump``.

The ``interrupt`` signal should stay high for at least two clock cycles to guarantee it is seen by the processor. An optional ``interrupt_ack`` output signal is provided to interface with external interrupt generating logic. It notifies you when the processor has started processing the interrupt, at which point it is safe to release the ``interrupt`` input signal.

Interrupts are controlled with the `enable`_ and `disable`_ instructions. they take a special dummy operand of "interrupt". A special `returni`_ instruction is used to exit from the ISR and resume normal execution. It takes an operand of "enable" or"disable" to set the new state of the interrupt flag after the return. You cannot use the normal `return`_ instruction to accomplish this since the original interrupt saved the flag and register bank state and only ``returni`` can restore it so that your normal code is not affected by the change of execution state.
  

Directives
----------

The following are the assembler directives used by Opbasm. These are special keywords used in place of machine instructions. They do not produce any code directly but are instead used to alter the behavior of the assembler.

.. _inst-address:

address
~~~~~~~

The ``address`` directive is used to change the address where the next instruction will be assembled to.

===================================== ================= ========================================
Format                                Example           Result
===================================== ================= ========================================
``address <address>``                 address 3FF       The instruction offset is moved to 0x3FF
``address <label>``                   address MyISR     The instruction offset is addr. of MyISR
===================================== ================= ========================================

This allows you to place code at the interrupt vector location or implement complex memory layouts such as bank switched pages. The second veriant with a label as an operand is a non-standard Opbasm extension. It is most useful for implementing an ISR with a `jump`_ for the entry point.

.. _inst-constant:

constant
~~~~~~~~

Define a named constant. The name cannot be any 1-3 character string that is a valid hex number. Constants can be used in place of any literal operand.

===================================== ======================== ========================================
Format                                Example                  Result
===================================== ======================== ========================================
``constant <name>, <literal>``        constant foo, 5A         The name "foo" is substituted with 0x5A
*(decimal literals)*                  constant bar, 20'd       bar is 0x14
*(binary literals)*                   constant baz, 01000010'b baz is 0x42
*(char literals)*                     constant bat, "0"        bat is 0x30
===================================== ======================== ========================================

.. _inst-default_jump:

default_jump
~~~~~~~~~~~~

By default, unused memory is filled with zeros. On PB3 this equates to "LOAD s0, 00". On PB6 it is "LOAD s0, s0" (a NOP). This can be problematic in high reliability code that needs to recover from accidentally falling into these unused areas of memory. To protect against this the ``default_jump`` directive lets you fill unused memory with a jump to a specified label or address.

===================================== ======================== ========================================
Format                                Example                  Result
===================================== ======================== ========================================
``default_jump <label>``              default_jump handle_err  Unused memory is filled with JUMP
                                                               instructions to handle_err
*(address target)*                    default_jump 001         Fill with JUMP to address 0x001
===================================== ======================== ========================================

.. _inst-include:

include
~~~~~~~

The ``include`` directive lets you incorporate the contents of other source files into your final program. It inserts the contents of the specified file at the location of the directive.

===================================== ======================== ========================================
Format                                Example                  Result
===================================== ======================== ========================================
``include "<file>"``                  include "lib.psm"        The contents of lib.psm are added to the
                                                               program
===================================== ======================== ========================================


.. _inst-inst:

inst
~~~~

The ``inst`` directive is used to manually construct an instruction from its hex encoding. This is primarily of value for encoding non-instruction data into the instruction memory. Note that the instruction size is 18-bits so the fifth nibble of the hex value is truncated down to 2-bits.

===================================== ======================== ========================================
Format                                Example                  Result
===================================== ======================== ========================================
``inst <hex literal>``                inst 3AABB               Place 0x3AABB at the current point in
                                                               memory
===================================== ======================== ========================================


.. _inst-namereg:

namereg
~~~~~~~

The ``namereg`` directive lets you rename a register mnemonic. This allows you to create more descriptive register names rather the default s0-sF. Once namereg has been applied, the affected register can only be referenced by its new name. Another call to ``namereg`` is needed to change its name again. The new name is case sensitive unlike the default where both "sn" "Sn" are accepted.

The new name takes effect after this directive. Previous references to the old register name remain valid. You can rename the register again at any point.

===================================== ======================== ========================================
Format                                Example                  Result
===================================== ======================== ========================================
``namereg <cur. name>, <new name>``   namereg sF, TEMP         rename sF to "TEMP"
===================================== ======================== ========================================

As an alternative, the m4 macro processor used with Opbasm has a :ref:`define() <m4-define>` macro that can be used to create alternate register names without replacing the original. 


.. _inst-string:

string
~~~~~~

Available on PicoBlaze-6 only. This directive creates a text string. It must be used in conjunction with the `load&return`_ or `outputk`_ instructions. When a string name is an argument to these instructions they are expanded into multiple instructions with each character as their literal operand.

===================================== ============================ ========================================
Format                                Example                      Result
===================================== ============================ ========================================
``string <name>$, "<string text>"``   string hello$, "Hello world" Create string named hello$
===================================== ============================ ========================================

The Opbasm macro package has :ref:`additional methods <Portable string and table operations>` for generating strings that are portable across PB3 and PB6.

There are three predefined strings you can reference in your PB6 code:

=============== ===============================
timestamp$      Build timestamp in H:M:S format
datestamp$      Build datestamp in M D Y format
Opbasm_version$ Opbasm assembler version
=============== ===============================

.. _inst-table:

table
~~~~~

Available on PicoBlaze-6 only. This directive creates a data array. It must be used in conjunction with the `load&return`_ or `outputk`_ instructions. When a table name is an argument to these instructions they are expanded into multiple instructions with each byte as their literal operand.

======================================= ==================================== ================================
Format                                  Example                              Result
======================================= ==================================== ================================
``table <name>#, [<hex values>]``       table hex#, [01, 02, AA]             Table named hex#
``table <name>#, [<decimal values>]'d`` table dec#, [50, 100, 200]'d         Table named dec#
``table <name>#, [<bin values>]'b``     table bin#, [11110000, 00100110]'b   Table named bin#
======================================= ==================================== ================================


Register assignment instructions
--------------------------------

PicoBlaze has a minimal set of instructions for moving data into and between registers.

.. _inst-load:

load
~~~~

The ``load`` instruction copies the value of its second argument into the register of the first argument.
This is the only way to directly set a register to an arbitrary constant value.

===================================== ====================== =================================
Format                                Example                Result
===================================== ====================== =================================
``load <dest. register>, <literal>``  load s0, 5A            s0 ⇐ 0x5A
*(loading other literal formats)*     load s0, 42'd          s0 ⇐ 42 (decimal)
*(loading address fragments)*         load sA, label'lower   sA ⇐ low byte of "label" address
``load <dest. register>, <register>`` load s0, s2            s0 ⇐ s2
===================================== ====================== =================================

.. _inst-star:

star
~~~~

The ``star`` instruction is specific to the PicoBlaze-6. It is used to transfer register values between
register banks A and B. When executed, a register in the active bank is copied into a register in the inactive bank.
There is no way to transfer in the other direction without switching banks using `regbank`_.

===================================== ================= ======================================
Format                                Example           Result
===================================== ================= ======================================
``star <dest. register>, <register>`` star s0, s3       (inactive bank) s0 ⇐ (active bank) s3
===================================== ================= ======================================

.. note::

   ``star`` has special behavior for its first operand. Because the `namereg`_ directive can obscure the actual register locations, you can *only* use the default register names s0-sF for the first operand. This is the only case where the ``namereg`` directive is ignored. The second register operand follows the usual register naming behavior.


ALU instructions
----------------

The following set of instructions perform arithmetic and logical operations on registers.

.. _inst-add:

add
~~~

Add two 8-bit values.

======================================= ==================== ===============================
Format                                  Example              Result
======================================= ==================== ===============================
``add <dest. register>, <literal>``     add s0, 5A           s0 ⇐ s0 + 0x5A
*(adding other literal formats)*        add s0, "0"          s0 ⇐ s0 + 0x30
``add <dest. register>, <register>``    add s0, s2           s0 ⇐ s0 + s2
======================================= ==================== ===============================

The C flag is set if an overflow occurs. i.e. the arithmetic result is greater than 255. The Z flag is set when
the result is zero.

.. _inst-addcy:

addcy
~~~~~

Add two 8-bit values and the carry flag. This is used to extend addition to support values larger than 8-bits.

====================================== ==================== =================================
Format                                 Example              Result
====================================== ==================== =================================
``addcy <dest. register>, <literal>``  addcy s0, 5A         s0 ⇐ s0 + 0x5A + C
*(adding other literal formats)*       addcy s0, 42'd       s0 ⇐ s0 + 42 (decimal) + C
``addcy <dest. register>, <register>`` addcy s0, s2         s0 ⇐ s0 + s2 + C
====================================== ==================== =================================

The C flag is set if an overflow occurs. i.e. the arithmetic result is greater than 255. On PicoBlaze-6 The Z flag is set when
the result is zero and the previous Z flag was set. On PicoBlaze-3 the Z flag disregards the previous state of Z.

.. code-block:: picoblaze

  add   s0, 01
  addcy s1, 00  ; Add carry bit from s0 into s1
  
  jump  Z, foo  ; PB6: Jump when s1,s0 == 0x0000;  PB3: Jump when s1 == 0x00

.. _inst-sub:

sub
~~~

Subtract two 8-bit values.

===================================== ==================== =================================
Format                                Example              Result
===================================== ==================== =================================
``sub <dest. register>, <literal>``   sub s0, 5A           s0 ⇐ s0 - 0x5A
*(subtracting other literal formats)* sub s0, "0"          s0 ⇐ s0 - 0x30
``sub <dest. register>, <register>``  sub s0, s2           s0 ⇐ s0 - s2
===================================== ==================== =================================

The C flag is set if an underflow occurs. i.e. the arithmetic result is less than 0. The Z flag is set when
the result is zero.

.. _inst-subcy:

subcy
~~~~~

Subtract two 8-bit values and the carry flag. This is used to extend subtraction to support values larger than 8-bits.

====================================== ==================== =================================
Format                                 Example              Result
====================================== ==================== =================================
``subcy <dest. register>, <literal>``  subcy s0, 5A         s0 ⇐ s0 - 0x5A - C
*(subtracting other literal formats)*  subcy s0, 42'd       s0 ⇐ s0 - 42 (decimal) - C
``subcy <dest. register>, <register>`` subcy s0, s2         s0 ⇐ s0 - s2 - C
====================================== ==================== =================================

The C flag is set if an underflow occurs. i.e. the arithmetic result is less than 0. On PicoBlaze-6 the Z flag is set when
the result is zero and the previous Z flag was set. On PicoBlaze-3 the Z flag disregards the previous state of Z.

.. code-block:: picoblaze

  sub   s0, 01
  subcy s1, 00  ; Subtract with borrow from s0
  
  jump  Z, foo  ; PB6: Jump when s1,s0 == 0x0000;  PB3: Jump when s1 == 0x00

.. _inst-and:

and
~~~

Perform the logical bitwise AND of two 8-bit values.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``and <dest. register>, <literal>``   and s0, 5A             s0 ⇐ s0 and 0x5A
*(anding other literal formats)*      and s0, 1011000'b      s0 ⇐ s0 and 0xB0
``and <dest. register>, <register>``  and s0, s2             s0 ⇐ s0 and s2
===================================== ====================== ================================

The C flag is always cleared. The Z flag is set when the result is zero.

.. _inst-or:

or
~~

Perform the logical bitwise OR of two 8-bit values.

===================================== ==================== =================================
Format                                Example              Result
===================================== ==================== =================================
``or <dest. register>, <literal>``    or s0, 5A            s0 ⇐ s0 or 0x5A
*(oring other literal formats)*       or s0, 1011000'b     s0 ⇐ s0 or 0xB0
``or <dest. register>, <register>``   or s0, s2            s0 ⇐ s0 or s2
===================================== ==================== =================================

The C flag is always cleared. The Z flag is set when the result is zero.

.. _inst-xor:

xor
~~~

Perform the logical bitwise XOR of two 8-bit values.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``xor <dest. register>, <literal>``   xor s0, 5A             s0 ⇐ s0 xor 0x5A
*(xoring other literal formats)*      xor s0, 1011000'b      s0 ⇐ s0 xor 0xB0
``xor <dest. register>, <register>``  xor s0, s2             s0 ⇐ s0 xor s2
===================================== ====================== ================================

The C flag is always cleared. The Z flag is set when the result is zero.


Comparson instructions
----------------------

The comparison instructions are used to compare values without modifying registers. They are only used to set and clear flags that will be inspected by subsequent instructions.

.. _inst-compare:

compare
~~~~~~~

Compare two 8-bit values. This is the same as subtraction without modifying the first operand.

===================================== ====================== ===============================
Format                                Example                Result
===================================== ====================== ===============================
``compare <register>, <literal>``     compare s0, 5A         temp ⇐ s0 - 0x5A
*(comparing other literal formats)*   compare s0, "0"        temp ⇐ s0 - 0x30
``compare <register>, <register>``    compare s0, s2         temp ⇐ s0 - s2
===================================== ====================== ===============================

The C flag is set if an underflow occurs. i.e. the arithmetic result is less than 0. The Z flag is set when
the result is zero.

The flags can be interpreted as follows:

==== ==== =====================================
Z    C    Meaning
==== ==== =====================================
1    \-   = operands are equal
0    \-   ≠ operands are not equal
0    0    > first is greater than second
\-   0    ≥ first is greater or equal to second
\-   1    < first is less than second
1    1    ≤ first is less or equal to second
==== ==== =====================================

Note that you cannot determine > "greater than" or ≤ "less than or equal" without inspecting both flags. It is best to structure your code to avoid these comparisons. If both operands are registers you can always swap them and use the complementary comparison operator. If the second operand is a literal you can implement ">" or "≤" by incrementing the literal and using "≥" or "<" instead. This will fail, however, when the literal is 0xFF. For this case you have to rearrange your logic to avoid these comparisons.

  .. code-block:: picoblaze

    ; Loop from 0 to 0xFE with < operation
            load s0, 00
    loop_a: <do something>
            add s0, 01       ; Increment counter
            compare s0, FF
            jump C, loop_a   ; jump if s0 < 0xFF
            
    ; Loop from 0 to 0xFF with simulated ≤ operation
            load s0, 00
    loop_b: <do something>
            add s0, 01       ; Increment counter
            compare s0, 00
            jump NZ, loop_b  ; Jump if s0 ≠ 0x00 (Same as s0 ≤ 0xFF)
            
    ; Alternatively, since overflow from 0xFF to 0x00 sets the C flag
    ; you can skip the compare instruction:
    
    ; Loop from 0 to 0xFF with simulated ≤ operation
            load s0, 00
    loop_c: <do something>
            add s0, 01       ; Increment counter
            jump NC, loop_c  ; Jump if s0 didn't overflow (Same as s0 ≤ 0xFF)
  

.. _inst-comparecy:

comparecy
~~~~~~~~~

Compare two 8-bit values with carry. Available on PicoBlaze-6 only. This is the same as subtraction with carry without modifying the first operand. It extends comparison to support values larger than 8-bits.

===================================== ====================== ===============================
Format                                Example                Result
===================================== ====================== ===============================
``comparecy <register>, <literal>``   comparecy s0, 5A       temp ⇐ s0 - 0x5A - C
*(comparing other literal formats)*   comparecy s0, 12'd     temp ⇐ s0 - 12 (decimal) - C
``comparecy <register>, <register>``  comparecy s0, s2       temp ⇐ s0 - s2 - C
===================================== ====================== ===============================

The C flag is set if an underflow occurs. i.e. the arithmetic result is less than 0. The Z flag is set when
the result is zero and the previous Z flag was set.

.. _inst-test:

test
~~~~

Perform the logical bitwise AND of two 8-bit values without modifying the first operand.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``test <dest. register>, <literal>``  test s0, 5A            temp ⇐ s0 and 0x5A
*(other literal formats)*             test s0, 1011000'b     temp ⇐ s0 and 0xB0
``test <dest. register>, <register>`` test s0, s2            temp ⇐ s0 and s2
===================================== ====================== ================================

The C flag is set to the odd parity of the bits in the result (set for an odd number of '1' bits). The Z flag is set when the result is zero.

This instruction is primarily used to test if certain bits in a register are set by non-destructively ANDing it with a constant mask.

.. code-block:: picoblaze

  constant my_mask, 02
  load s0, 23
  test s0, my_mask
  jump NZ, bit_1_is_set
  


.. _inst-testcy:

testcy
~~~~~~

Perform the logical bitwise AND of two 8-bit values without modifying the first operand. Available on PicoBlaze-6 only. This extends the `test`_ instruction by combining the previous Z and C flag values into the result.

======================================= ====================== ================================
Format                                  Example                Result
======================================= ====================== ================================
``testcy <dest. register>, <literal>``  testcy s0, 5A          temp ⇐ s0 and 0x5A
*(other literal formats)*               testcy s0, 1011000'b   temp ⇐ s0 and 0xB0
``testcy <dest. register>, <register>`` testcy s0, s2          temp ⇐ s0 and s2
======================================= ====================== ================================

The C flag is set to the odd parity of the bits in the result and the previous C flag (set for an odd number of '1' bits). The Z flag is set when the result is zero and the previous Z flag was set.


Shift/rotate instructions
-------------------------

Owing to space constraints, the PicoBlaze does not have a barrel shifter. This means that shifts and rotations can only be performed one bit at a time. Multiple shifts and rotates must be performed with repeated instructions. The Opbasm m4 package :ref:`has macros <Shift and rotate by multiple bits>` to do this for you.

.. _inst-rl:

rl
~~

Rotate left 1-bit.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``rl <register>``                     rl s0                  s0 ⇐ s0[6:0] & s0[7]
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.

.. _inst-rr:

rr
~~

Rotate right 1-bit.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``rr <register>``                     rr s0                  s0 ⇐ s0[0] & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.

.. _inst-sl0:

sl0
~~~

Shift left 1-bit inserting '0'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sl0 <register>``                    sl0 s0                 s0 ⇐ s0[6:0] & '0'
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.

.. _inst-sl1:

sl1
~~~

Shift left 1-bit inserting '1'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sl1 <register>``                    sl1 s0                 s0 ⇐ s0[6:0] & '1'
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is always cleared.

.. _inst-sla:

sla
~~~

Shift left 1-bit, inserting previous C flag. This is used to extend shifts over multiple bytes.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sla <register>``                    sla s0                 s0 ⇐ s0[6:0] & C
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.

.. code-block:: picoblaze

  sl0 s0 ; MSB -> C
  sla s1 ; Shift former MSB from s0 into LSB of s1


.. _inst-slx:

slx
~~~

Shift arithmetic left 1-bit. This performs "sign-extension" of the LSB.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``slx <register>``                    slx s0                 s0 ⇐ s0[6:0] & s0[0]
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.

.. _inst-sr0:

sr0
~~~

Shift right 1-bit inserting '0'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sr0 <register>``                    sr0 s0                 s0 ⇐ '0' & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.

.. _inst-sr1:

sr1
~~~

Shift right 1-bit inserting '1'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sr1 <register>``                    sr1 s0                 s0 ⇐ '1' & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.

.. _inst-sra:

sra
~~~

Shift right 1-bit, inserting previous C flag. This is used to extend shifts over multiple bytes.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sra <register>``                    sra s0                 s0 ⇐ C & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.

.. code-block:: picoblaze

  sr0 s1 ; LSB -> C
  sra s0 ; Shift former LSB from s1 into MSB of s0


.. _inst-srx:

srx
~~~

Shift arithmetic right 1-bit. This performs sign-extension of the MSB.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``srx <register>``                    srx s0                 s0 ⇐ s0[7] & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.

.. code-block:: picoblaze

  load s0, F0 ; -16
  sra  s0     ; s0 == 0xF8  == -8


Branching instructions
----------------------

The following instructions are used to change the flow of execution.


.. _inst-call:

call
~~~~

Execute a subroutine.

===================================== ====================== =============================================
Format                                Example                Result
===================================== ====================== =============================================
``call <address>``                    call my_label          PC is saved and jump to my_label address
*(hardcoded address)*                 call 12A               PC is saved and jump to address 0x12A
``call <flag code>, <address>``       call Z, my_label       Conditional call to my_label if Z flag is set
===================================== ====================== =============================================

This is similar to `jump`_ but the program counter (PC) is saved on the hardware stack. This allows execution to resume at the next instruction when the subroutine completes with a `return`_ instruction.

The address of the subroutine is typically provided through a label but a hardcoded 12-bit address can also be used. A conditional call can be implemented by using one of the flag codes (Z, NZ, C, NC) as the first operand. The conditional call is only made if the flag matches the state in the instruction. Otherwise execution continues with the next instruction.

.. _inst-call-at:

call@
~~~~~

Execute a subroutine at a variable address. The target address is determined at runtime. Available on PicoBlaze-6 only.

=========================================== ====================== =============================================
Format                                      Example                Result
=========================================== ====================== =============================================
``call@ (<high register>, <low register>)`` call@ (s0, s1)         PC is saved and jump to address in s0, s1
=========================================== ====================== =============================================

This is a variation of an unconditional `call`_ that takes the address from a pair of registers. This is used to compute the target dynamically. Addresses are typically generated by initializing with the ``'upper`` and ``'lower`` modifiers on a label and then adding an offset:

.. code-block:: picoblaze

  my_func:
    <Special subroutine with multiple entry points>
    return

  load  s0, my_func'upper ; Load upper nibble
  load  s1, my_func'lower ; Load lower byte
  add   s1, sA            ; Add offset
  addcy s0, 00
  call@ (s0, s1)          ; Branch into my_func offset by sA number of instruction words

.. _inst-jump:

jump
~~~~

Jump to an address.

===================================== ====================== =============================================
Format                                Example                Result
===================================== ====================== =============================================
``jump <address>``                    jump my_label          Jump to my_label address
*(hardcoded address)*                 jump 12A               Jump to address 0x12A
``jump <flag code>, <address>``       jump Z, my_label       Conditional jump to my_label if Z flag is set
===================================== ====================== =============================================

The target address is typically provided through a label but a hardcoded address can also be used. A conditional jump can be implemented by using one of the flag codes (Z, NZ, C, NC) as the first operand. The conditional jump is only made if the flag matches the state in the instruction. Otherwise execution continues with the next instruction.

.. _inst-jump-at:

jump@
~~~~~

Jump to a variable address. The target address is determined at runtime. Available on PicoBlaze-6 only.

=========================================== ====================== =============================================
Format                                      Example                Result
=========================================== ====================== =============================================
``jump@ (<high register>, <low register>)`` jump@ (s0, s1)         Jump to address in s0, s1
=========================================== ====================== =============================================

This is a variation of an unconditional `jump`_ that takes the address from a pair of registers. This is used to compute the target dynamically. Addresses are typically generated by initializing with the ``'upper`` and ``'lower`` modifiers on a label and then adding an offset.

.. _inst-return:

return
~~~~~~

Return from a subroutine.

===================================== ====================== =====================================================
Format                                Example                Result
===================================== ====================== =====================================================
``return``                            return                 Resume execution following the last call instruction.
``return <flag code>``                return NZ              Return if the Z flag is clear
===================================== ====================== =====================================================

A conditional return can be implemented by using one of the flag codes (Z, NZ, C, NC) as the first operand. The conditional return is only made if the flag matches the state in the instruction. Otherwise execution continues with the next instruction.

.. _inst-returni:

returni
~~~~~~~

Return from an interrupt handler.

===================================== ====================== =====================================================
Format                                Example                Result
===================================== ====================== =====================================================
``returni <enable|disable>``          returni enable         Resume normal execution with interrupts enabled.
===================================== ====================== =====================================================

This performs an unconditional return from an interrupt handler. Interrupts are enabled or disabled based upon the required argument. The saved flags and register bank (PB6) are restored.


Memory access instructions
--------------------------

.. _inst-fetch:

fetch
~~~~~

Fetch a byte from scratchpad RAM into a register.

======================================== ====================== ================================
Format                                   Example                Result
======================================== ====================== ================================
``fetch <dest. register>, <address>``    fetch s0, 01           s0 ⇐ scratchpad[01]
``fetch <dest. register>, (<register>)`` fetch s0, (sA)         s0 ⇐ scratchpad[sA]
======================================== ====================== ================================

.. _inst-store:

store
~~~~~

Store a byte from a register into scratchpad RAM.

========================================= ====================== ================================
Format                                    Example                Result
========================================= ====================== ================================
``store <source register>, <address>``    store s0, 01           scratchpad[01] ⇐ s0
``store <source register>, (<register>)`` store s0, (sA)         scratchpad[sA] ⇐ s0
========================================= ====================== ================================

.. _inst-input:

input
~~~~~

Read a byte from an input port into a register.

======================================== ====================== ==============================
Format                                   Example                Result
======================================== ====================== ==============================
``input <dest. register>, <address>``    input s0, 01           s0 ⇐ in_port[01]
``input <dest. register>, (<register>)`` input s0, (sA)         s0 ⇐ in_port[sA]
======================================== ====================== ==============================

.. _inst-output:

output
~~~~~~

Write a byte from a register to an output port.

========================================== ====================== ==============================
Format                                     Example                Result
========================================== ====================== ==============================
``output <source register>, <address>``    output s0, 01           out_port[01] ⇐ s0
``output <source register>, (<register>)`` output s0, (sA)         out_port[sA] ⇐ s0
========================================== ====================== ==============================

.. _inst-outputk:

outputk
~~~~~~~

Write to a constant optimized output port. Available on PicoBlaze-6 only.

========================================== ======================== ==============================
Format                                     Example                  Result
========================================== ======================== ==============================
``outputk <literal>, <address>``           outputk 5A, B            out_port[B] ⇐ 0x5A
========================================== ======================== ==============================

This writes a constant value to the dedicated outputk ports. This avoids the `load`_, `output`_ instruction pair required to write a constant to a normal output port. Useful if you want to write static data as fast as possible. There are only 16 outputk ports 0-F.

When the literal is a string or table name this instruction is expanded into multiple copies for each character or byte.

Miscellaneous instructions
--------------------------

.. _inst-disable:

disable
~~~~~~~

Disable interrupts.

==================================== ====================== ==================================================
Format                               Example                Result
==================================== ====================== ==================================================
``disable interrupt``                disable interrupt      Interrupts are no longer handled.
==================================== ====================== ==================================================

.. _inst-enable:

enable
~~~~~~

Enable interrupts.

==================================== ====================== ==================================================
Format                               Example                Result
==================================== ====================== ==================================================
``enable interrupt``                 enable interrupt       Interrupts are handled.
==================================== ====================== ==================================================

.. _inst-hwbuild:

hwbuild
~~~~~~~

Generate the hardware build version number. Available on PicoBlaze-6 only.

==================================== ====================== ==================================================
Format                               Example                Result
==================================== ====================== ==================================================
``hwbuild <register>``               hwbuild s0             s0 ⇐ hardware build number
==================================== ====================== ==================================================

The C flag is always set to '1'. The Z flag is set when the result is zero. This is the only instruction that can set C by itself.

The build number is specified in the KCPSM6 "hwbuild" generic when it is instantiated.

.. _inst-load_return:

load&return
~~~~~~~~~~~

Load a register and return. Available on PicoBlaze-6 only.

===================================== ====================== ==================================================
Format                                Example                Result
===================================== ====================== ==================================================
``load&return <register>, <literal>`` load&return s0, "A"    s0 ⇐ "A" and return to caller.
===================================== ====================== ==================================================

This is used to do a constant load and return in a single instruction. It is primarily used to load tables of data and strings stored in program memory accessed with a `call@`_ instruction.

.. code-block:: picoblaze

  message:
    load&return s0, "H"
    load&return s0, "e"
    load&return s0, "l"
    load&return s0, "l"
    load&return s0, "o"
    load&return s0, NUL

    ...
    
    load sA, message'upper
    load sB, message'lower
  loop:  
    call@ (sA, sB) ; Load s0 with next byte
    compare s0, NUL
    jump Z, done
    <Do something with s0>
    add   sB, 01
    addcy sA, 00
    jump loop
  done:

You ordinarily wouldn't implement tables by manually writing each ``load&return`` instruction. The assembler has built in support for generating sequences of ``load&return`` instructions when the literal operand is the name of a defined string or table.

.. code-block:: picoblaze

  table my_data#, [1,2,3,8,100,200]'d
  
  my_table:
    load&return s0, my_data# ; Expands into 6 load&return instructions
    
  string my_string$, "Hello world"

  message:
    load&return s0, my_string$ ; Expands into 11 load&return instructions

  ...
  <call@ code to access my_table and message>

.. _inst-regbank:

regbank
~~~~~~~

Switch between register banks. Available on PicoBlaze-6 only.

===================================== ====================== ==================================================
Format                                Example                Result
===================================== ====================== ==================================================
``regbank <A|B>``                     regbank B              Switch to second bank of registers
===================================== ====================== ==================================================



Missing instructions
--------------------

The PicoBlaze processors have a minimal instruction set owing to their compact implementation. Some notable instructions commonly seen in other processor architectures are missing. However, it is possible to replicate their behavior with alternate instructions.

nop
~~~

The NOP instruction represents a no-operation that has no effect on the processor state and only adds a processing delay to following code. To get the effect of a NOP you can assign a register to itself:

 .. code-block:: picoblaze
 
   load s0, s0 ; No change to registers or flags == NOP
   
The m4 macro package includes a :pb:macro:`nop` macro.
   
not
~~~

There is no bitwise negation instruction in PicoBlaze. Instead you can use the XOR operator's behavior as a controlled inverter to get the same result: 

 .. code-block:: picoblaze
 
   xor s0, FF ; XOR with all 1's flips the bits as with NOT

The m4 macro package includes a :pb:macro:`not` macro.

negate
~~~~~~

When working with signed values it is useful to negate them. In 2's complement form, negation is the same as inversion and add 1 so we combine the NOT operation with an increment to create a numeric negate:

 .. code-block:: picoblaze
 
   xor s0, FF
   add s0, 01 ; s0 = -s0

The m4 macro package includes a :pb:macro:`negate` macro.


=====================================
PicoBlaze assembly language reference
=====================================


This is an overview of the assembly language used for the PicoBlaze-6 and PicoBlaze-3 microcontrollers.

If you've never written assembly code before it may be confusing where to start. A typical PicoBlaze assembly program will follow a pattern like that shown below. You should follow this general plan when starting out with PicoBlaze programming.

.. code-block:: picoblaze

  ;==============================
  ;== Preamble

  <Directives to set up constants, rename registers, and include other files>
  <Initialization code executed once at startup>
  
  ; Skip over subroutines so they don't execute at startup.
  ; They can also be placed after the main program but this
  ; style is necessary when using some of the Opbasm macros.
  jump main
  
  ;==============================
  ;== Subroutines
  
  my_function:
    <Function body>
    return
    
  my_other_function:
    <Function body>
    return
  
  ; Interrupt handler (optional)
  my_ISR:
    <Save registers in scratchpad RAM or switch register banks>
    <ISR body>
    <Restore registers or switch register banks back>
    returni enable ; Restore interrupts after returning
  
  ;=============================
  ;== Main application code
  main:
    ...
    ; Prepare arguments passed through registers
    load s1, 42'd
    load s2, "0"
    call my_function
    ; Handle possible return value in a register or placed in scratchpad RAM
    ...
    ; There is no OS to return to so the main program typically loops over itself
    jump main

  ;=============================
  ;== Special code
  
  ; Guard to avoid falling into the ISR code.
  ; You could also try to recover or restart.
  default_jump fatal_error
  fatal_error: jump fatal_error
  
  ; Jump into the ISR from the default interrupt vector
  ; at the end of 1K address space.
  address 3FF
  jump my_ISR
    

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

.. rubric:: PicoBlaze-6 directives

========== ===========
`string`_  `table`_
========== ===========

The assembler is case insensitive. Uppercase or mixed case mnemonics can also be used.


Instruction formats
-------------------

PicoBlaze assembly follows the usual convention of having a single instruction or directive per line.
An optional label creates a named reference that can be referred to by other instructions. An optional single line comment can appear at the end prefixed by a ";" character.

  Basic syntax
    ``[label:] [<instruction> <operands>*] [; comment]``
  
Blank lines are ignored. Labels and comments can be used without an instruction.

Instructions generally use the following formats with a few minor exceptions:

  Register
    ``<instruction> <register> [, <register>]``

  Immediate
    ``<instruction> <register>, <literal>``
  
  Indirect scratchpad / I/O port
    ``<instruction> <register>, (<register>)``
    
  Indirect jump / call
    ``<instruction> (<register>, <register>)``

  Conditional
    ``<instruction> <flag state>``

  Outputk
    ``OUTPUTK <literal>, <literal>``
  
The fields have the following meanings:

================== ================================================================================
``<flag state>``   One of the condition codes for zero and carry (Z, NZ, C, or NC)
``<instruction>``  Instruction mnemonic or directive
``<register>``     One of the 16 currently active registers (s0-sF)
``<literal>``      Constant literal (hex [nn], decimal [nnn'd], binary [nnnnnnnn'b], or char ["c"])
================== ================================================================================

Literal syntax
--------------

Address spaces
--------------

The PicoBlaze has a simple architecture that operates on information stored in the following address spaces:

  Registers
    A set of 16 8-bit registers. They default to the registers named s0-sF but can be renamed with the ``namereg`` directive.
    The PicoBlaze-3 has a single bank of registers. PicoBlaze-6 has a second bank of 16 (bank B) that can be swapped in or out.

  Instruction memory
    Instruction words are stored in an isolated memory. Limited to 1K on PicoBlaze-3. Selectable between 1K, 2K, or 4K on PicoBlaze-6.
    This is not directly accessible from within the PicoBlaze program but a dual ported memory can be implemented to access data
    stored in instruction memory or to modify instructions through the I/O port interface.
    
  Scratchpad memory
    A small pool of RAM used as a local memory. This is 64 bytes on the PicoBlaze-3 and
    configurable for 64, 128, or 256 bytes on PicoBlaze-6. This memory is accessed with the `fetch`_ and `store`_ instructions.
    
  I/O ports
    A set of 256 input and 256 output ports are used to interact with external hardware. This memory is
    accessed with the `input`_ and `output`_ instructions.
    
  Outputk ports
    The PicoBlaze-6 has a special set of 16 output ports intended for directly driving constant values with minimal overhead.
    This is accessed with the `outputk`_ instruction.
    
  Call stack
    A hardware call stack is maintained to track return addresses.

Flags
-----  

Initialization
--------------

Interrupts
----------
  

Directives
----------

address
~~~~~~~

The address directive is used to change the address where the next instruction will be assembled to.

===================================== ================= ========================================
Format                                Example           Result
===================================== ================= ========================================
``address <address>``                 address 3FF       The instruction offset is moved to 0x3FF
===================================== ================= ========================================

This is useful for placing code at the interrupt vector location or implementing complex memory layouts such as bank switched pages.


constant
~~~~~~~~

Define a named constant. The name cannot be any 1-3 character string that is a valid hex number.

===================================== ======================== ========================================
Format                                Example                  Result
===================================== ======================== ========================================
``constant <name>, <literal>``        constant foo, 5A         The name "foo" is substituted with 0x5A
*(decimal literals)*                  constant bar, 20'd       bar is 0x14
*(binary literals)*                   constant baz, 01000010'b baz is 0x42
*(char literals)*                     conatant bat, "0"        bat is 0x30
===================================== ======================== ========================================


default_jump
~~~~~~~~~~~~

include
~~~~~~~

inst
~~~~

namereg
~~~~~~~

string
~~~~~~

table
~~~~~


Register assignment instructions
--------------------------------

PicoBlaze has a minimal set of instructions for moving data into and between registers.

load
~~~~

The ``load`` instruction copies the value of its second argument into the register of the first argument.
This is the only to set a register to an arbitrary constant value.

===================================== ====================== =================================
Format                                Example                Result
===================================== ====================== =================================
``load <dest. register>, <literal>``  load s0, 5A            s0 ⇐ 0x5A
*(loading other literal formats)*     load s0, 42'd          s0 ⇐ 42 (decimal)
*(loading address fragments)*         load sA, label'lower   sA ⇐ low byte of "label" address
``load <dest. register>, <register>`` load s0, s2            s0 ⇐ s2
===================================== ====================== =================================

star
~~~~

The ``star`` instruction is specific to the PicoBlaze-6. It is used to transfer register values between
register banks A and B. A register in the active bank is copied into a register in the inactive bank.
There is no way to transfer in the other direction without switching banks using ``regbank``.

===================================== ================= ======================================
Format                                Example           Result
===================================== ================= ======================================
``load <dest. register>, <register>`` star s0, s3       (inactive bank) s0 ⇐ (active bank) s3
===================================== ================= ======================================

``star`` has special behavior for its first operand. Because the ``namereg`` directive can obscure
the actual register locations, you can only use the default register names s0-sF for the first operand.
This is the only case where the ``namereg`` directive is ignored. The second register operand follows
the usual register naming behavior.


ALU instructions
----------------

The following set of instructions perform arithmetic and logical operations on registers.

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

Note that you cannot determine > "greater than" or ≤ "less than or equal" without inspecting both flags.


comparecy
~~~~~~~~~

Compare two 8-bit values with carry. It is only available on PicoBlaze-6. This is the same as subtraction with carry without modifying the first operand. It extends comparison to support values larger than 8-bits.

===================================== ====================== ===============================
Format                                Example                Result
===================================== ====================== ===============================
``comparecy <register>, <literal>``   comparecy s0, 5A       temp ⇐ s0 - 0x5A - C
*(comparing other literal formats)*   comparecy s0, 12'd     temp ⇐ s0 - 12 (decimal) - C
``comparecy <register>, <register>``  comparecy s0, s2       temp ⇐ s0 - s2 - C
===================================== ====================== ===============================

The C flag is set if an underflow occurs. i.e. the arithmetic result is less than 0. The Z flag is set when
the result is zero and the previous Z flag was set.


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



testcy
~~~~~~

Perform the logical bitwise AND of two 8-bit values without modifying the first operand. It is only available on PicoBlaze-6. This extends the `test`_ instruction by combining the previous Z and C flag values into the result.

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

Owing to space constraints, the PicoBlaze does not have a barrel shifter. This means that shifts and rotations can only be performed one bit at a time. Multiple shifts and rotates must be performed with repeated instructions.

rl
~~

Rotate left 1-bit.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``rl <register>``                     rl s0                  s0 ⇐ s0[6:0] & s0[7]
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.

rr
~~

Rotate right 1-bit.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``rr <register>``                     rr s0                  s0 ⇐ s0[0] & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.

sl0
~~~

Shift left 1-bit inserting '0'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sl0 <register>``                    sl0 s0                 s0 ⇐ s0[6:0] & '0'
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.


sl1
~~~

Shift left 1-bit inserting '1'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sl1 <register>``                    sl1 s0                 s0 ⇐ s0[6:0] & '1'
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is always cleared.


sla
~~~

Shift left 1-bit, inserting previous C flag. This is used to extend shifts over multiple bytes.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sla <register>``                    sla s0                 s0 ⇐ s0[6:0] & C
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.


slx
~~~

Shift arithmetic left 1-bit. This performs "sign-extension" of the LSB.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``slx <register>``                    slx s0                 s0 ⇐ s0[6:0] & s0[0]
===================================== ====================== ================================

The C flag gets the old MSB from the register. The Z flag is set if the result is zero.


sr0
~~~

Shift right 1-bit inserting '0'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sr0 <register>``                    sr0 s0                 s0 ⇐ '0' & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.


sr1
~~~

Shift right 1-bit inserting '1'.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sr1 <register>``                    sr1 s0                 s0 ⇐ '1' & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.


sra
~~~

Shift right 1-bit, inserting previous C flag. This is used to extend shifts over multiple bytes.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``sra <register>``                    sra s0                 s0 ⇐ C & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.


srx
~~~

Shift arithmetic right 1-bit. This performs sign-extension of the MSB.

===================================== ====================== ================================
Format                                Example                Result
===================================== ====================== ================================
``srx <register>``                    srx s0                 s0 ⇐ s0[7] & s0[7:1]
===================================== ====================== ================================

The C flag gets the old LSB from the register. The Z flag is set if the result is zero.


Branching instructions
----------------------

The following instructions are used to change the flow of execution.

call
~~~~

Execute a subroutine.

===================================== ====================== =============================================
Format                                Example                Result
===================================== ====================== =============================================
``call <address>``                    call my_label          PC is saved and jump to my_label address
*(hardcoded address)*                 call 12A               PC is saved and jump to address 0x12A
``call <flag code> <address>``        call Z, my_label       Conditional call to my_label if Z flag is set
===================================== ====================== =============================================

This is similar to `jump`_ but the program counter (PC) is saved on the hardware stack. This allows execution to resume at the next instruction when the subroutine completes with a `return`_ instruction.

The address of the subroutine is typically provided through a label but a hardcoded address can also be used. A conditional call can be implemented by using one of the flag codes (Z, NZ, C, NC) as the first operand. The conditional call is only made if the flag matches the state in the instruction. Otherwise execution continues with the next instruction.


call@
~~~~~

Execute a subroutine at a variable address. The target address is determined at runtime. It is only available on PicoBlaze-6.

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

jump
~~~~

Jump to an address.

===================================== ====================== =============================================
Format                                Example                Result
===================================== ====================== =============================================
``jump <address>``                    jump my_label          Jump to my_label address
*(hardcoded address)*                 jump 12A               Jump to address 0x12A
``jump <flag code> <address>``        jump Z, my_label       Conditional jump to my_label if Z flag is set
===================================== ====================== =============================================

The target address is typically provided through a label but a hardcoded address can also be used. A conditional jump can be implemented by using one of the flag codes (Z, NZ, C, NC) as the first operand. The conditional jump is only made if the flag matches the state in the instruction. Otherwise execution continues with the next instruction.


jump@
~~~~~

Jump to a variable address. The target address is determined at runtime. It is only available on PicoBlaze-6.

=========================================== ====================== =============================================
Format                                      Example                Result
=========================================== ====================== =============================================
``jump@ (<high register>, <low register>)`` jump@ (s0, s1)         Jjump to address in s0, s1
=========================================== ====================== =============================================

This is a variation of an unconditional `jump`_ that takes the address from a pair of registers. This is used to compute the target dynamically. Addresses are typically generated by initializing with the ``'upper`` and ``'lower`` modifiers on a label and then adding an offset.


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

returni
~~~~~~~

Return from an interrupt handler.

===================================== ====================== =====================================================
Format                                Example                Result
===================================== ====================== =====================================================
``returni <enable|disable>``          returni enable         Resume normal execution with interrupts enabled.
===================================== ====================== =====================================================

This performs an unconditional return from an interrupt handler. Interrupts are enabled or disabled based upon the required argument.


Memory access instructions
--------------------------

fetch
~~~~~

Fetch a byte from scratchpad RAM into a register.

======================================== ====================== ================================
Format                                   Example                Result
======================================== ====================== ================================
``fetch <dest. register>, <address>``    fetch s0, 01           s0 ⇐ scratchpad[01]
``fetch <dest. register>, (<register>)`` fetch s0, (sA)         s0 ⇐ scratchpad[sA]
======================================== ====================== ================================


store
~~~~~

Store a byte from a register into scratchpad RAM.

========================================= ====================== ================================
Format                                    Example                Result
========================================= ====================== ================================
``store <source register>, <address>``    store s0, 01           scratchpad[01] ⇐ s0
``store <source register>, (<register>)`` store s0, (sA)         scratchpad[sA] ⇐ s0
========================================= ====================== ================================

input
~~~~~

Read a byte from an input port into a register.

======================================== ====================== ==============================
Format                                   Example                Result
======================================== ====================== ==============================
``input <dest. register>, <address>``    input s0, 01           s0 ⇐ in_port[01]
``input <dest. register>, (<register>)`` input s0, (sA)         s0 ⇐ in_port[sA]
======================================== ====================== ==============================


output
~~~~~~

Write a byte from a register to an output port.

========================================== ====================== ==============================
Format                                     Example                Result
========================================== ====================== ==============================
``output <source register>, <address>``    output s0, 01           out_port[01] ⇐ s0
``output <source register>, (<register>)`` output s0, (sA)         out_port[sA] ⇐ s0
========================================== ====================== ==============================

outputk
~~~~~~~

Write to a constant optimized output port.

========================================== ======================== ==============================
Format                                     Example                  Result
========================================== ======================== ==============================
``outputk <literal>, <address>``           outputk 5A, B            out_port[B] ⇐ 0x5A
========================================== ======================== ==============================

This writes a constant value to the dedicated outputk ports. This avoids the `load`_, `output`_ pair required to write a constant to a normal output port. There are only 16 outputk ports 0-F.

Miscellaneous instructions
--------------------------

disable
~~~~~~~

Disable interrupts.

==================================== ====================== ==================================================
Format                               Example                Result
==================================== ====================== ==================================================
``disable interrupt``                disable interrupt      Interrupts are no longer handled.
==================================== ====================== ==================================================


enable
~~~~~~

Enable interrupts.

==================================== ====================== ==================================================
Format                               Example                Result
==================================== ====================== ==================================================
``enable interrupt``                 enable interrupt       Interrupts are handled.
==================================== ====================== ==================================================


hwbuild
~~~~~~~

Generate the hardware build version number. This is a PicoBlaze-6 only instruction.

==================================== ====================== ==================================================
Format                               Example                Result
==================================== ====================== ==================================================
``hwbuild <register>``               hwbuild s0             s0 ⇐ hardware build number
==================================== ====================== ==================================================

The C flag is always set to '1'. The Z flag is set when the result is zero. This is the only instruction that can set C by itself.

The build number is specified in the KCPSM6 "hwbuild" generic when it is instantiated.

load&return
~~~~~~~~~~~

Load a register and return. This is a PicoBlaze-6 only instruction.

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

You ordinarily wouldn't implement tables by manually writing each load&return instruction. The assembler has built in support for generating sequences of load&return instructions when the literal operand is the name of a defined string or table

.. code-block:: picoblaze

  table my_data#, [1,2,3,8,100,200]'d
  
  my_table:
    load&return s0, my_data# ; Expands into 6 load&return instructions
    
  string my_string$, "Hello world"

  message:
    load&return s0, my_string$ ; Expands into 11 load&return instructions

  ...
  <call@ code to access my_table and message>

regbank
~~~~~~~

Switch between register banks. Only for PicoBlaze-6.

===================================== ====================== ==================================================
Format                                Example                Result
===================================== ====================== ==================================================
``regbank <A|B>``                     regbank B              Switch to second bank of registers
===================================== ====================== ==================================================



Missing instructions
--------------------

nop
~~~

not
~~~

negate
~~~~~~

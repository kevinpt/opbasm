===========================
PicoBlaze assembly tutorial
===========================


If you've never written assembly code before it may be confusing to determine where to start. The following tutorial will provide guidance on how to develop a PicoBlaze program with explanations of ways to implement the most common idioms. If you aren't familiar with the PicoBlaze architecture you should review the official Xilinx documentation or browse through the :doc:`architecture and language reference <language>` to learn the basics of what facilities the PicoBlaze has and what the assembly syntax is like.

A first look
------------

A typical PicoBlaze assembly program will follow a pattern like that shown below. You should follow this general plan when starting out with PicoBlaze programming.

.. code-block:: picoblaze

  ;==============================
  ;== PREAMBLE

  <Directives to set up constants, rename registers, and include other files>
  <Initialization code executed once at startup>
  
  ; Skip over subroutines so they don't execute at startup.
  ; They can also be placed after the main program but this
  ; style is necessary when using some of the Opbasm macros.
  jump main
  
  ;==============================
  ;== SUBROUTINES
  
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
    <Restore registers>
    returni enable ; Restore interrupts after returning
  
  ;=============================
  ;== MAIN APPLICATION CODE
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
  ;== SPECIAL CODE
  
  ; Guard to avoid falling into the ISR code.
  ; All unused memory jumps into this loop.
  ; You could also try to recover or restart.
  default_jump fatal_error
  fatal_error: jump fatal_error  ; Infinite loop
  
  ; Jump into the ISR from the default interrupt vector
  ; at the end of 1K address space.
  address 3FF
  jump my_ISR


Program storage
---------------

The PicoBlaze is a Harvard architecture machine with an instruction memory separate from data storage and the I/O address space. Your program is typically stored in a Xilinx Block RAM (BRAM) that has predefined initial values to make it act like a ROM. The PicoBlaze has no built in mechanisms to write to this memory but it is possible to do so with additional logic for more advanced applications. It is also possible to store non-instruction data in the in left over free space of a Block RAM which can be accessed through a second address port connected to the PicoBlaze I/O.

Each instruction is a fixed 18-bit word that naturally fills a Xilinx BRAM utilizing all of its extra parity bits. The PicoBlaze-3 supports up to 1k instruction words and the PB6 can use up to 4k. It is possible to put smaller programs into the other type of memory available on Xilinx parts, distributed RAM, which is synthesized from the programmable logic LUTs configured for a special SRAM mode. Another option is to use the dual-ported capability to split a BRAM between two PicoBlaze devices.

The assembler converts your program into a list of instruction words that are used to initialize a BRAM. This becomes part of your FPGA project in one of two ways. The traditional way is to use an HDL template file that instantiates the proper BRAM for your device. The template BRAM has the the program words filled in to create a synthesizable ROM that is instantiated in your FPGA project. The alternative is to the Opbasm :ref:`synthesizable ROM template <generic_rom>` which currently only works with the Xilinx ISE toolset and not Vivado. Either way, the synthesizer will translate the template into a memory with initial instruction values assigned on power up so that it behaves as a ROM.

Assembler syntax
----------------

PicoBlaze assembly consists of a series of lines that contain machine instructions and assembler directives that are used to control the generated program data. It follows this basic structure:

.. image:: ../images/statement.svg

There are three parts, all of which are optional. The instruction portion is the main part of a statement. It has a named mnemonic possibly folowed by some operands. You can have an optional label at the beginning and an optional comment at the end. Blank lines are ignored. It is possible for a label or comment to be on a line by itself if you wish. The label functions as a reference that you can use as a target for branching and calling subroutines.


.. code-block:: picoblaze

  ; This is a comment
  my_label:
  another_label:         ; This is a label and comment
  add s0, s1             ; An instruction and comment
  last_label: add s2, s3 ; Label, instruction, and comment

Assigning variables
-------------------

The most fundamental action you can take in a program is to assign a value to a storage location. PB3 and PB6 have two areas for storing data internally: registers, and scratchpad memory. There are 16 8-bit registers which are all fully general purpose. PB6 has a second bank of 16 registers that can be exchanged with the first set for special purposes. The scratchpad is a 64 byte RAM on PB3 expandable to 128 or 256 bytes on PB6.

Values that need to be accessed frequently will typically be kept in a register. Values that need to be saved for long periods of time may be better kept in scratchpad to avoid monopolizing registers. All PicoBlaze instructions can work directly with registers but scratchpad memory is only accessible through two dedicated access instructions :ref:`inst-fetch` and :ref:`inst-store`. Data stored in scratchpad takes more code to process and consumes more time and program memory as a result.

The most basic instruction for assigning a value to a register is :ref:`inst-load`. It takes a destination register as the first argument and either another register or a constant literal as the second.

.. code-block:: picoblaze

  load s0, 5A  ; Load s0 with 0x5A (90 decimal)
  load s1, s0  ; Load s1 with value of s0

Using ``fetch`` and ``store`` we can save variables in scratchpad RAM:

.. code-block:: picoblaze

  constant M_COUNTER, 0F  ; Scratchpad address 0x0F used for our variable
  load s0, 00             ; Initialize counter to 0
  store s0, M_COUNTER     ; Save initial value
  
  ; Increment variable in scratchpad
  fetch s4, M_COUNTER
  add s4, 01
  store s4, M_COUNTER
  ; Scratchpad[15] is now 1
  
Using a constant for scratchpad variable addresses makes it easy to modify their location in the future. You should avoid hardcoding numeric addresses directly into ``fetch`` and ``store`` instructions.

The ``load`` and ``store`` instructions have an indirect variant where the second operand is a register containing a scratchpad address rather than a fixed literal value. This register acts as a pointer variable to a piece of memory. Because PicoBlaze doesn't have any relative indexed addressing modes you have to directly modify this register to access different parts of the scratchpad. This allows you to store and retrieve arrays of data:

.. code-block:: picoblaze

  const M_ARRAY, 0F         ; Allocate array from 0x0F to 0x1F
  const M_ARRAY_END, 1F
  
  load sA, M_ARRAY          ; Init pointer to start of array
  loop:  
    fetch  s9, (sA)         ; Indirect access through sA
    output s9, COM_PORT
    add    sA, 01           ; Advance to next byte
    compare sA, M_ARRAY_END
    jump   NZ, loop
  

Register allocation
~~~~~~~~~~~~~~~~~~~

Unlike compiled programming languages, it is left up to you to determine how registers are used in your program. It is useful to come up with a regular scheme for using the registers for specific purposes to reduce confusion and improve maintainability. It becomes difficult to manage registers if you randomly assign them in various parts of your program.

There are five common classes of data that registers can be used for:

  1. Arguments to subroutines
  2. Return values from subroutines
  3. Local variables (preserved on a stack)
  4. Temporary values (never preserved)
  5. Special purpose values (globals)

By default all registers are general purpose and can be used interchangeably. The PicoBlaze assembly syntax includes a :ref:`inst-namereg` directive that can rename a register. This can give more meaningful names to commonly used registers. It is also useful to protect registers you've reserved for a special purpose from being accidentally overwritten by other code.

One possible register usage convention:

======== ======= ======================================
Register Renamed Purpose
======== ======= ======================================
s0               Subroutine return value
s1               Argument 1
s2               Argument 2
s3               Argument 3
s4               Argument 4
s5               Local 1
s6               Local 2
s7               Local 3
s8               Local 4
s9               Local 5
sA               Temporary 1
sB               Temporary 2
sC               Temporary 3
sD               Temporary 4
sE               Temporary 5
sF       SP      :ref:`Stack pointer <stack-variables>`
======== ======= ======================================

Computing in assembly
---------------------

You can't accomplish much by just assigning values to registers and RAM. To get useful work done in assembly you have to use the class of instructions associated with the Arithmetic Logic Unit (ALU) of the processor. This is a part of the PicoBlaze that performs arithmetic, logical, and shift operations on registers.

In addition to performing operations on register values, the ALU maintains two state flags that represent additional information about the result. These are the Z and C flags for zero and carry. The Z flag is fairly simple. It is almost always set to 1 when the result of an ALU operation is zero. It is cleared to 0 when the result is non-zero. The C flag represents a carry from addition or a borrow from subtraction. In a few instructions it is used to hold the result of a parity calculation representing the number of 1 bits in a number.

The flags can be examined after an operation to execute conditional code that branches to different parts of your program. In this way, arithmetic is used to control the order of execution as well as actual numeric computation.


Arithmetic operations
~~~~~~~~~~~~~~~~~~~~~

================= ===============================
Instruction       Description
================= ===============================
:ref:`inst-add`   Add two values
:ref:`inst-sub`   Subtract two values
:ref:`inst-addcy` Add two values with carry
:ref:`inst-subcy` Subtract two values with borrow
================= ===============================

The ``add`` and ``sub`` instructions perform addition and subtraction respectively on a pair of 8-bit operands. The first operand is always a register and it is used as the final destination of the result. The second operand can be another register or a constant value.

The ``addcy`` and ``subcy`` instructions are used to extend the addition and subtraction operations for numbers larger than 8-bits. While the PicoBlaze is always limited to working on 8-bit values in a single instruction, larger numbers can be represented by groups of 8-bit registers processed in pieces.

.. code-block:: picoblaze

  ; 8-bit addition
  add   s5, sA  ; 8-bit addition
  ; Result in s5
  
  ; 16-bit addition
  add   s5, sA  ; Least significant byte first
  addcy s6, sB  ; Extend carry into most significant byte
  ; Result in s6,s5

  ; 24-bit subtraction
  sub   s5, sA  ; Least significant byte first
  subcy s6, sB  ; Extend carry (borrow) into next byte
  subcy s7, sC  ; Extend carry (borrow) into most significant byte
  ; Result in s7,s6,s5

For multi-byte addition, the carry flag is set when the previous addition overflows beyond an 8-bit result. This overflow can never be more than 1 since the largest 8-bit sum is: ``255 + 255 = 510 = 0x1FE``. The overflow carries into the next most significant addition by the use of ``addcy``.

For multi-byte subtraction, the carry flag functions as a "borrow" bit. When it is set, the previous subtraction is considered to have borrowed from the current pair of bytes and so an additional -1 is taken from the result by ``subcy``.

.. note::

  In PicoBlaze architectures prior to PB6, the ``addcy`` and ``subcy`` instructions don't set the Z flag in the logically expected way. Instead of setting Z only when the entire multi-byte result is zero. They only consider the last 8-bits of the result. The Z flag could be set even if a previous byte was non-zero. Because of this the Z flag cannot be used to check for a zero result on PB3 after performing multi-byte addition or subtraction.

Logical operations
~~~~~~~~~~~~~~~~~~

=============== ========================
Instruction     Description
=============== ========================
:ref:`inst-and` Bitwise AND of two bytes
:ref:`inst-or`  Bitwise OR of two bytes
:ref:`inst-xor` Bitwise XOR of two bytes
=============== ========================

At times it can be useful to perform operations on values using binary logic gates. These instructions are conceptually equivalent to a set of 8 parallel AND, OR, or XOR gates operating on the corresponding bits of the two operands simultaneously. The fourth fundamental logic gate, the NOT, does not have a dedicated instruction but it can be performed by the XOR operation with a constant second operand of 0xFF. These have a variety of uses but among the most common is the ability to set and clear selected bits within a register when needed.

.. code-block:: picoblaze

  and s5, s6         ; s5 = s5 AND s6
  and s5, 7F         ; Clear upper bit of s5
  and s5, ~80        ; Clear upper bit of s5 (Using inverted bitmask)
  or  s5, 80         ; Set upper bit of s5
  or  s5, 10000000'b ; Set upper bit of s5 (binary mask)
  xor s5, s6         ; s5 = s5 XOR s6
  xor s5, FF         ; s5 = NOT s5

Bit shifting operations
~~~~~~~~~~~~~~~~~~~~~~~

=============== ===================
Instruction     Description
=============== ===================
:ref:`inst-sl0` Shift left
:ref:`inst-sl1` Shift left
:ref:`inst-sla` Shift left
:ref:`inst-slx` Shift left
:ref:`inst-sr0` Shift right
:ref:`inst-sr1` Shift right
:ref:`inst-sra` Shift right
:ref:`inst-srx` Shift right
:ref:`inst-rl`  Rotate left
:ref:`inst-rr`  Rotate right
=============== ===================

The third set of ALU operations is used to shift and rotate the position of bits in a register. 

Control structures
------------------

If you are used to programming in high level languages the biggest change when using assembly is that there are no built in control structures. You have to implement them all yourself implicitly in assembly code. This can create some tedium in writing assembly and can make it hard to follow along when reading code. The Opbasm macro package has a system to let you write :ref:`control structures in a high-level style syntax <c-style-if-then>`. However, it is still useful to know the basics of how this is done as explained in the following.

If-then-else
~~~~~~~~~~~~

An if-then-else statement consists of a three parts. An expression to evaluate, a block of code to execute when the expression is true and an optional block for a false expression. A basic if-then-else is of the following form:

.. code-block:: c

  if(RX_DATA == 42) {
    TX_DATA = 'E';
  } else {
    TX_DATA = 'N';
  }

In PicoBlaze assembly the expression is evaluated with instructions that will set or clear the C and Z flags. Subsequent conditional :ref:`inst-jump` and :ref:`inst-call` instructions will examine these flags to determine what to execute next. This allows us to follow the different execution paths of the if-then-else construct.

The main instruction for evaluating expressions is :ref:`inst-compare`. It subtracts its second argument from the first and changes the C and Z flags based on the result. Note that it only changes the flags. The subtraction result is thrown away and does not affect the registers. 

After a ``compare`` instruction the flags can be interpreted as follows:

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

This gives us enough tools to replicate the pseudocode above:

.. code-block:: picoblaze
  :emphasize-lines: 3

         input   s5, RX_DATA  ; Load a local register to work with
         compare s5, 42'd     ; Subtract 42 from s5 and update C and Z flags
         jump    Z, equal     ; If s5 == 42 the Z flag is set
  ; Not equal (false block)
         load   sE, "N"
         jump   end_if
  equal: ; (true block)
         load   sE, "E"
  end_if:
         output sE, TX_DATA

Here the ``jump Z, equal`` instruction branches to the "equal" label when the Z flag is set. Otherwise the next instruction is executed.

When you have no else condition, the true block can be placed immediately after the expression evaluation code:

.. code-block:: picoblaze
  :emphasize-lines: 7

  ; if(RX_DATA < 42) {
  ;   TX_DATA = 'L';
  ; }

         input   s5, RX_DATA
         compare s5, 42'd      ; Subtract 42 from s5 and update C and Z flags
         jump    NC, gte       ; If s5 < 42 the C flag is set. It is clear when s5 ≥ 42
  ; Less than (true block)
         load   sE, "L"
         output sE, TX_DATA
  gte: ; (false)

In this case we want to branch past the true block when the expression is false so we use "NC" instead of "C" to check for ``RX_DATA < 42``.

It isn't always necessary to use the ``compare`` instruction to evaluate an expression. If an instruction you already need to use changes the flags in a useful way then you can check them directly without a ``compare``.

Consider you are incementing a register and want to detect when it overflows past 0xFF. In this case the result is zero so you could compare for equality with 0x00 but the :ref:`inst-add` instruction also sets the C flag on overflow so you could also just branch directly after the increment.

.. code-block:: picoblaze

  add     s5, 01       ; Increment
  compare s5, 00       ; Test for overflow
  jump    Z, overflow  ; Branch with s5 == 0x00
  
  ; Same without compare
  add     s5, 01       ; Increment
  jump    C, overflow  ; Branch when add overflowed

Recognizing these opportunities to reduce the number of instructions used is important for fitting complex programs into the limited space available for PicoBlaze program storage.

Loops
~~~~~

The other major control structures are loops used to repetitively execute blocks of code. The most fundamental of these are the while loop and do-while loop which only differ in when the loop expression is evaluated: either before or after the block.

.. code-block:: c

  while(count < 20) {
    value = value + 4;
    count = count + 1;
  }
  
  do {
    value = value + 4;
    count = count + 1;
  } while(count < 20)

We can implement these in PicoBlaze assembly as follows:

.. code-block:: picoblaze

  fetch   s5, VALUE       ; Get value from scratchpad RAM
  load    s6, 00          ; Initialize count

  while_loop:
    compare s6, 20'd
    jump    NC, while_end ; End loop when s6 ≥ 20
    add     s5, 04
    add     s6, 01
    jump    while_loop
  while_end:


  fetch   s5, VALUE       ; Get value from scratchpad RAM
  load    s6, 00          ; Initialize count

  do_while_loop:
    add     s5, 04
    add     s6, 01
    compare s6, 20'd
    jump    C, do_while_loop ; Continue loop when s6 < 20

Note that the do-while loop requires one less instruction and is the more efficient form if you can arrange your program to work with that variant.

Subroutines
-----------

It is useful to have reusable code that can be executed from different locations in a program. This is done by creating a subroutine. These begin with a label like those used for :ref:`inst-jump` targets. The :ref:`inst-call` instruction will branch to the the label just like ``jump`` but it also saves the next address on to the hardware call stack. When the subroutine is finished the :ref:`inst-return` instruction pops the most recent address from the stack and resumes execution after the ``call`` instruction.

.. code-block:: picoblaze

  compute_something:
    <common code>
    return               ; Resume execution after call
    
  ...
  
  ; Main program
  call compute_something ; Branch to subroutine
  load s0, 01            ; Execution resumes here
  ...
  call compute_something ; Call it again

Nothing truly isolates subroutines from executing as normal code other than convention. You must make certain that the processor can't accidentally begin executing a subroutine outside of the ``call``/``return`` mechanism. If you ``jump`` into a subroutine and then execute ``return`` you will pop the wrong address from the call stack and have a malfunction. Likewise, you must not allow the processor to enter into a subroutine by normal sequential execution without a ``call`` to prepare the hardware stack.

.. code-block:: picoblaze

  ; Protect processor from executing subroutines
  ; as normal code.
  jump main
  
  ;======= SUBROUTINES FOLLOW =======

  compute_something:
    <common code>
    return

  ;======= MAIN PROGRAM =======
  main:
    ...
    call compute_something

Subroutines can call other subroutines up to the limit of the hardware stack which is 31 levels on PB3 and 30 levels on PB6.

The ``call`` instruction has a conditional form that works the same as a conditional ``jump``. This allows you to use a subroutine as the body of a control structure.

.. code-block:: picoblaze

  subroutine:
    ...
    return
    
    
    compare s5, 10
    call    Z, subroutine  ; Execute subroutine if s5 == 0x10
    

    ; Less efficient using jump:
    compare s5, 10
    jump    NZ, end_if     ; Skip subroutine if s5 != 0x10
    call    subroutine
  end_if:

.. _stack-variables:

stack variables
~~~~~~~~~~~~~~~

Initially you might start using registers in an ad hoc way. Inevitably you will end up in a situation where you don't have any free registers left to do your next task. Worse yet, you may have subtle bugs caused by accidentally overwriting a register that wasn't expected to change.

Higher level languages employ a calling convention where they save registers not deemed as temporaries onto a stack at the beginning of a subroutine and restore these saved values before returning. This allows you to reuse the same register for different purposes in your program. The stack is a region of memory that expands as more data is pushed onto it and shrinks as data is popped off. Most processors have special instructions to assist in managing such a stack in RAM but not the PicoBlaze. The hardware call stack is dedicated to storing only return addresses and is unavailable for general purpose use. It is possible, however, to create a stack in the scratchpad memory and emulate the behavior of push and pop operations.

We reserve a register to function as a stack pointer. It will hold an index into scratchpad memory that always points to the next free location on the stack. Pushes and pops will manipulate this pointer and move data to and from the scratchpad memory.

.. code-block:: picoblaze

  namereg sF, SP    ; Reserve sF as the stack pointer
  load  SP, 3F      ; Start stack at address 0x3F
  
  ; Push s5 register
  store s5, (SP)    ; Save to next location in stack
  sub   SP, 01      ; Move SP to next free location

  ; Push s6 register
  store s6, (SP)    ; Save to next location in stack
  sub   SP, 01      ; Move SP to next free location
  
  ; At this point SP points to address 0x3D
  ; s5 is saved at address 0x3F and s6 is at 0x3E
  
  ...
  load  s5, 42      ; Work with s5, altering its value
  add   s6, s5
  ...

  ; Pop s6 register
  add   SP, 01      ; Move SP back to last saved value
  fetch s6, (SP)    ; Restore saved value of s6
  
  ; Pop s5 register
  add   SP, 01      ; Move SP back to next saved value
  fetch s5, (SP)    ; Restore saved value of s5
  
  ; s5 and s6 are restored to their original values
  ; SP points at address 0x3F again, ready for new data

Each push operation is implemented as a pair of ``store`` and ``sub`` instructions and each pop is an ``add`` ``fetch``. You must pop registers in the reverse of the order they were pushed to restore them to their original state.

In most cases the stack is designed to grow down from higher addresses to lower addresses. This lets you place the stack at the upper end of the scratchpad and use the lower end for other purposes. You don't have to follow this convention and can have a stack grow from low to high if you wish. It is important that the stack never grows large enough to overwrite other data stored in scratchpad. 

The Opbasm macro library has :pb:macro:`push` and :pb:macro:`pop` macros as well as :ref:`other stack handling macros <stack-operations>` to simplify stack management when writing your programs.

With a stack in place you can use it to enforce a calling convention for your subroutines. Within a subroutine all modified registers must be saved to the stack before modification unless they are designated as temporaries that are never saved or a return value. When this convention is followed a subroutine caller never sees registers change before and after a :ref:`inst-call` except the return value register.

.. code-block:: picoblaze

  rotate:
    ; Push s5
    store   s5, (SP)
    sub     SP, 01
    
    load    sE, s1       ; Move argument into temporary we can modify
    load    s5, 01       ; s5 is available for use

    loop:
      compare sE, 00
      jump    Z, end_loop
      rl      s5
      sub     sE, 01
      jump    loop
    end_loop:

    ; Return result in s0
    load    s0, s5
    
    ; Pop s5
    add     SP, 01
    fetch   s5, (SP)
    return
  ...
    
  ; Set subroutine arguments 
  load    s1, 02
  call    rotate
  ; s5 is unchanged, s0 has result, sE is altered


External I/O
------------

With the basic foundations of writing assembly it comes time to actually do something useful with the PicoBlaze. Since it is implemented as a soft-core within an FPGA there will usually be additional logic outside of the PicoBlaze that you need to interact with. This is accomplished through the I/O ports. There are 256 input and output ports which are multiplexed together onto an 8-bit address bus.These ports are accessed with the :ref:`inst-input` and :ref:`inst-output` instructions.

.. code-block:: picoblaze

  input  s5, 01  ; Input from port 0x01
  add    s5, 01
  output s5, 01  ; Write back to port 0x01
  
To use the port interface you will have to implement decode logic to handle port operations based on their addresses. Refer to the official PicoBlaze documentation for examples of how this can be done. If you want to completely decode the ports and save their state in registers, one general purpose solution would be to instantiate the `generic register file component <http://kevinpt.github.io/vhdl-extras/rst/packages.html#reg-file>`_ from the VHDL-extras library.

External events
---------------

The final topic to explore in developing for the PicoBlaze are interrupts. This is a mechanism where external hardware can interrupt normal program execution to cause special code to run known as an interrupt service routine (ISR). The PicoBlaze has a single interrupt input and supports a single ISR.

Interrupts are optional. You do not have to use them in your designs. Their main benefit is that they let you avoid polling for changes in the hardware state through the I/O ports and you can respond to external events with the lowest, most deterministic delay possible.

Interrupt handling is controled by an internal flag. Interrupts are off by default. You can use the :ref:`inst-enable` instruction to enable the the ISR. The :ref:`inst-disable` instruction disables interrupts.

When the interrupt input to the PicoBlaze goes high it saves the current instruction address on the hardware stack like a normal subroutine call. It also saves the values of the Z, C flags, and on PB6, saves the active register bank. The processor then executes the instruction located at the interrupt vector address. This address is fixed at 0x3FF for PB3 and can be modified for PB6 in its generic block. This instruction is usually a :ref:`inst-jump` into the body of the ISR:

.. code-block:: picoblaze

  my_ISR:
    <Save registers in scratchpad RAM or switch register banks>
    <ISR body>
    <Restore registers>
    returni enable      ; Return with interrupts enabled
    
    returni disable     ; Return with interrupts disabled
    
  ...
  
  ; Jump into the ISR from the default interrupt vector
  ; at the end of 1K address space.
  address 3FF
  jump my_ISR
  
The ISR is created like a special subroutine starting with a label as usual. You must exit from the ISR using the :ref:`inst-returni` instruction instead of :ref:`inst-return`. Interrupts must remain disabled during the entire ISR. You can choose whether to re-enable them with the ``returni`` instruction or later on with the :ref:`inst-enable` instruction. The ``returni`` resumes execution at the address saved upon the start of the interrupt. The saved Z, C and register bank are restored to their previous values. Execution can then proceed as normal.

You must be careful not to let the ISR disrupt processor state such that execution fails after resuming normal execution. This means that you can't change any registers needed by the main program. An easy but inconvenient solution is to reserve some registers for exclusive use by the ISR. On PB6, you can employ the second register bank for the ISR if it isn't already in use. Otherwise you must implement a stack as described above and push all registers that will be modified before changing them. Similarly, the ISR should only modify scratchpad locations it has exclusive write access to so as to avoid corrupting the normal program in progress.

The ISR can execute at any time and it could potentially interrupt a timing critical task that can't afford long delays. For this reason it is best to minimize the amount of code in the ISR to minimize its execution time. The most critical sections of code that can't tolerate an interrupt should be guarded by turning interrupts off around them using the :ref:`inst-disable` and :ref:`inst-enable` instructions.

.. code-block:: picoblaze

  disable interrupt
  <Critical code here>
  enable interrupt

With only a single interrupt input, you have to take extra steps to handle multiple external events. External logic can be added that captures multiple interrupt sources and that can be checked through an I/O port to determine which interupt launched the ISR.


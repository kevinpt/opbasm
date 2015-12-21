======
Opbsim
======

Opbsim is a command line simulator for PicoBlaze-3 and PicoBlaze-6. The full instruction set for both targets is supported. The primary purpose of Opbsim is to run the automated test suite that validates the Opbasm macro library. However, it is designed to be simple and run fast to permit testing of long running programs and, as such, may be of of general use as well. Opbsim can run faster than real-time when it is built in release mode. The simulator is simple enough (900 SLOC) to permit easy modifications and a modular object system is in place to simulate various external peripherals.

Building Opbsim
---------------

Opbsim is written in the `Nim <http://nim-lang.org/>`_ programming language. To build it you will need the nim compiler `installed <http://nim-lang.org/download.html>`_ and working. The Opbasm installation script does not build Opbsim for you. Its sources are located in the "sim" directory. The test suite expects to find the compiled executable in the same location.

Opbsim is built with the Nim compiler using the following command:

.. code-block:: sh

  > cd sim
  > nim c -d:release opbsim.nim
  
This will produce an executable ``opbsim`` program that should be placed at a location on your search path. Nim does not include 64-bit DLLs. When building on 64-bit Windows you will need to copy the 64-bit ``pcre.dll`` from the "sim/win64" directory to the same location as ``opbsim.exe``.

It you have the Nimble package manager installed you can build and install Opbsim with the following:

.. code-block:: sh

  > cd sim
  > nimble install
  
This will create a release build and install it to the following default locations.

On `*nix`:
  A symbolic link at ~/.nimble/bin/opbsim
  
On Windows:
  A batch wrapper at C:\\Users\\<username>\\.nimble\\bin\\opbsim.bat

Using Opbsim
------------

Opbsim is run with the following options:

::

  Open PicoBlaze simulator
  Usage: opbsim [OPTIONS]

  Options:
    -m:MEM_FILE --mem:MEM_FILE    Input mem file
    -i:JSON_IN  --input:JSON_IN   JSON input string
    --log:LOG_FILE                Log file with symbol table
    -L:NUM        --limit:NUM     Limit to NUM instructions executed
    -v            --verbose       Verbose output
    -t            --trace         Print execution trace
    -q            --quiet         Quiet output
    -j            --json          JSON report [forces quiet too]
    -p            --list-periphs  Print peripheral information
    --pb3                         Simulate PicoBlaze-3 code
    --pb6                         Simulate PicoBlaze-6 code [default]

You have to provide a MEM file with the ``-m`` option. As a guard against infinite loops, there is a default execution limit of 100 million instructions (4 seconds of simulation time at 50 MHz). You can increase or decrease this limit with ``-L``. PB6 is the default processor target. You have to use the ``--pb3`` option to simulate PB3 code. Note that Opbsim cannot detect the difference between PB3 and PB6 code and will produce erroneous results if you simulate with the wrong target.

.. parsed-literal::

  > opbasm -6 carry_flag.psm4
  ...
  > opbsim --pb6 -m:carry_flag.mem
  PicoBlaze simulator
  Running in PicoBlaze-6 mode
  Input: carry_flag.mem
  Read 4096 words

  Found 8 symbols in carry_flag.log

  Quitting simulation


  Executed 16 instructions (640 ns @ 50 MHz, CPU time = 15 us, 0.04x realtime)
  16 instructions visited of 27 total (59%)
  0 testbench error(s)
  Normal termination
  
The execution time estimate is based on a 50 MHz clock. The overhead of setting up the simulation and priming the processor cache causes the realtime performance comparison to be extremely low for small programs. The simulator will exceed realtime performance on longer programs where the overhead is a smaller percentage of the total execution time.

The JSON output mode is used for automated testing. Once simulation ends, a dictionary is output containing the final state of registers, ports, simulated console output, and additional information. You can read this data into any tool that can parse JSON to evaluate the simulation results. Quiet mode is forced on when JSON mode is selected.

.. parsed-literal::

  > opbsim --pb6 -m:carry_flag.mem -j

  {"regs_a": [253, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63],
   "regs_b": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "scratchpad": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "ports_in": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "ports_out": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "total_insts": 16, "cpu_runtime": 4.999999999999796e-06, "executed": 16,
    "inst_count": 27, "termination": "termNormal", "console": ""}
    
When run manually, Opbsim will provide basic information about the program executed. You can use ``--verbose`` to get more detailed output of the final processor state including registers, scratchpad memory, and the output port values. The ``--trace`` option will provide a trace of all instructions executed. This will slow down simulation significantly in long programs due to the large amount of text output so it is best used to debug short code snippets. Opbsim will look for a log file with the same base name as the MEM file. When found, it will load all of the address symbols and display these names next to JUMP and CALL instructions.

.. parsed-literal::

  > opbsim --pb6 -m:carry_flag.mem -t
  
  PicoBlaze simulator
  Running in PicoBlaze-6 mode
  Input: carry_flag.mem
  Read 4096 words

  Found 8 symbols in carry_flag.log

  000 01F3F Load  sF = 0x3F
  001 01D00 Load  sD = 0x00
  002 22006 Jump  init_done (carry_flag.psm4)
  006 02EE0 And  sE & 0x00
  007 3E00A Jump NC  true  GE_f1_0001 (carry_flag.psm4)
  00A 14E80 Hwbuild sE = 00
  00B 3A00E Jump C  true  LT_f1_0003 (carry_flag.psm4)
  00E 010FF Load  s0 = 0xFF
  00F 11002 Add  s0 + 0x02
  010 3A013 Jump C  true  LT_f1_0005 (carry_flag.psm4)
  013 19002 Sub  s0 - 0x02
  014 3A017 Jump C  true  LT_f1_0007 (carry_flag.psm4)
  017 19002 Sub  s0 - 0x02
  018 3E01B Jump NC  true  GE_f1_0009 (carry_flag.psm4)
  01B 22003 Jump  terminate (carry_flag.psm4)
  003 2DDFF Output  port[FF] = sD
  Quitting simulation

  Executed 16 instructions (640 ns @ 50 MHz, CPU time = 109 us, 0.01x realtime)
  16 instructions visited of 27 total (59%)
  0 testbench error(s)
  Normal termination



The Opbsim program returns zero on success and one when an unrecoverable internal error occurs. Non-fatal errors in simulation are reported through the "termination" status value and the simulator returns zero.

You can provide input data by passing a JSON array with the ``-i`` option. The first 256 values will be assigned to the input ports during initialization. No other interactive behavior is provided.

.. code-block:: sh

  > opbsim --pb6 -m:foo.mem -i:"[0,1,2,3,4,5,6]"


Peripherals
-----------

PicoBlaze firmware eventually needs to interact with external hardware and Opbsim provides a mechanism to simulate attached peripherals.

Included with Opbsim are a number of basic peripherals that have general purpose utility:

console
  This is a simulated terminal that collects ASCII characters written to port 0xFE into a buffer. When a newline character is
  received the buffer is written to stdout. The entire log of console data is captured and reported when JSON mode is active.  

quit
  This is a special peripheral used to terminate the simulation. A write to port 0xFF ends execution. The value written is reported
  to the output as the number of testbench errors. 

loopback
  A range of output ports from 0x00 to 0x0F are setup to copy whatever is written to them back to the corresponding input port.

ROM
  To access INST directive data packed in the program memory, a dual ported ROM is simulated. An address to read is written to
  0xFA (high byte), 0xFB (low byte) and the low 16-bits of program memory are read back through the same input ports.

IntGen
  Simulated interrupts can be generated by writing to port 0xFC. This will trigger the ISR for testing.

Peripheral Interface
~~~~~~~~~~~~~~~~~~~~

You can add your own peripherals by modifying the Opbsim source. They are implemented as objects initialized with a sequence of input and output ports they are attached to. Whenever an ``input`` or ``output`` instruction is executed, the associated port is checked for attached peripherals and portRead or portWrite method is called on the perioheral object. The methods have access to the processor state and can modify port values as needed. Multiple peripherals can be attached to a single port. They will be called in the order that they were defined during initialization of the processor state.


Test suite
----------

Opbsim is used as part of an automated test suite. The tests are designed to run using the Python unittest framework with auto test discovery. If necessary, each test runs Opbasm to assemble a program for both PB6 and PB3 targets. The assembled code is then run in the simulator and the result is checked for any failures. You need to have Opbasm installed and accessible from your command line search path. Opbsim must be compiled and available from the /sim directory of the project or your search path. Run the tests from the root directory of the project with the following command:

.. code-block:: sh

  > python -m unittest discover -v



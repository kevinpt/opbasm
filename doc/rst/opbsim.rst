======
Opbsim
======

.. note::

  Opbsim is slated for official release in v1.3. Until then it is only available for download through a Git clone.

Opbsim is a command line simulator for PicoBlaze-3 and PicoBlaze-6. The full instruction set for both targets is supported. The primary purpose of Opbsim is to run the automated test suite that validates the macro library. However, it is designed to be simple and run fast to permit testing of long running programs and, as such, may be of of general use as well. Opbsim can run faster than real-time when it is built in release mode. The simulator is simple enough to permit easy modifications and a modular object system is in place to simulate various external peripherals.

Building Opbsim
---------------

Opbsim is written in the `Nim <http://nim-lang.org/>`_ programming language. To build it you will need the nim compiler `installed <http://nim-lang.org/download.html>`_ and working. The Opbasm installation script does not build Opbsim for you. Its sources are located in the "sim" directory. The test suite expects to find the compiled executable in the same location.

Opbsim is built with the Nim compiler using the following command:

.. code-block:: sh

  > nim c -d:release opbsim.nim
  
This will produce an executable ``opbsim`` program that should be placed at a location on your search path.

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
    --pb3                         Simulate PicoBlase-3 code
    --pb6                         Simulate PicoBlaze-6 code [default]

You have to provide a MEM file with the ``-m`` option. As a guard against infinite loops, there is a default execution limit of 100 million instructions (4 seconds of simulation time at 50 MHz). You can increase or decrease this limit with ``-L``. PB6 is the default processor target. You have to use the ``--pb3`` option to simulate PB3 code. Note that Opbsim cannot detect the difference between PB3 and PB6 code and will produce erroneous results if you simulate with the wrong target.

The JSON output mode is used for automated testing. Once simulation ends, a dictionary is output containing the final state of registers, ports, simulated console output, and additional information. You can read this data into any tool that can parse JSON to evaluate the simulation results. Quiet mode is forced on when JSON mode is selected.

When run manually, Opbsim will provide basic information about the program executed. You can use ``--verbose`` to get more detailed output and ``--trace`` to get a trace of all instructions executed. The latter will slow down simulation significantly in long programs due to the large amount of text output so it is best used to debug short code snippets. Opbsim will look for a log file with the same base name as the MEM file. When found, it will load all of the address symbols and display these names next to JUMP and CALL instructions.

The Opbsim program returns zero on success and one when an unrecoverable internal error occurs. Non-fatal errors in simulation are reported through the "termination" status value and the simulator returns zero.

You can provide input data by passing a JSON array with the ``-i`` option. The first 256 values will be assigned to the input ports during initialization. No other interactive behavior is provided.


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

You can add your own peripherals by modifying the Opbsim source. They are implemented as objects that have a sequence of input and putput ports they are attached to. Whenever an ``input`` or ``output`` instruction is executed, the associated port is checked for attached peripherals and an associated portRead or portWrite method is called. The methods have access to the processor state and can modify port values as needed. Multiple peripherals can be attached to a single port. They will be called in the order that they were definied during initialization of the processor state.


Test suite
----------

Opbsim is used as part of an automated test suite. The tests are designed to run using the Python unittest framework with auto test discovery. If necessary, each test runs Opbasm to assemble a program for both PB6 and PB3 targets. The assembled code is then run in the simulator and the result is checked for any failures. You need to have Opbasm installed and accessible from your command line search path. Opbsim must be compiled and available from the /sim directory of the project. Run the tests from the root directory of the project with the following command:

.. code-block:: sh

  > python -m unittest discover -v



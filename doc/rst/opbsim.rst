======
Opbsim
======

.. note::

  Opbsim is slated for official release in v1.3. Until then it is only available for download through a Git clone.

Opbsim is a command line simulator for PicoBlaze-3 and PicoBlaze-6. The primary purpose of Opbsim is to run the automated test suite that validates the macro library. It has been designed to be simple and run fast to permit testing of long running programs and as such may be of of general use as well. Opbsim can run faster than real-time when it is built in release mode. The simulator is simple enough to permit easy modifications and a modular object system is in place to simulate various external peripherals.

Building Opbsim
---------------

Opbsim is implemented in the `Nim <http://nim-lang.org/>`_ programming language. To build it you will have to have the nim compiler `installed <http://nim-lang.org/download.html>`_ and working. The Opbasm installation script does not build it for you. Its sources are located in the "sim" directory. The test suite expects to find the compiled executable in the same location.

Opbsim is built with the Nim compiler using the following command:

.. code-block:: sh

  > nim c -d:release opbsim.nim

Using Opbsim
------------

Opbsim is run with the following options:

.. code-block:: sh

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
    --pb3                         Simulate PicoBlase-3 code
    --pb6                         Simulate PicoBlaze-6 code [default]

You have to provide a MEM file with the ``-m`` option. As a guard against infinite loops, there is a default execution limit of 100 million instructions (4 seconds of simulation time at 50 MHz). You can increase or decrease this limit with ``-L``. PB6 is the default processor target. You have to use the ``--pb3`` option to simulate PB3 code.

The JSON output mode is used for automated testing. Once simulation ends, a dictionary is output containing the final state of registers, ports, simulated console output, and additional information. You can read this data into any tool that can parse JSON to evaluate the simulation results. Quiet mode is forced on when JSON mode is selected.

When run manually, Opbsim will provide basic information about the program executed. You can use ``--verbose`` to get more detailed output and ``--trace`` to get a trace of all instructions executed. The latter will slow down simulation significantly in long programs due to the large amount of text output so it is best used to debug short code snippets. Opbsim will look for a log file with the same base name as the MEM file. When found, it will load all of the address symbols and display the names next to JUMP and CALL instructions.

The Opbsim program returns zero on success and one when an unrecoverable internal error occurs. Non-fatal errors in simulation are reported through the "termination" status value and the simulator returns zero.

You can provide input data by passing a JSON array with the ``-i`` option. The first 256 values will be assigned to the input ports during initialization. No other interactive behavior is provided.

Peripherals
-----------


Peripheral Interface
~~~~~~~~~~~~~~~~~~~~



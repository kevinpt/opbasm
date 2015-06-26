.. image:: http://kevinpt.github.io/opbasm/_static/opbasm_logo.png

========================
Open PicoBlaze Assembler
========================

Opbasm is a free cross-platform assembler for the PicoBlaze-3 (PB3) and
PicoBlaze-6 (PB6) microcontrollers `provided by Xilinx
<http://www.xilinx.com/products/intellectual-property/picoblaze.htm>`_. It will
run readily on any platform with a functional Python interpreter. Opbasm
provides a better performing solution to assembling PicoBlaze code without
resorting to DOS or Windows emulation to run the native KCPSM assemblers.


**Special features of Opbasm:**

 * Optional `m4 preprocessor macros
   <http://kevinpt.github.io/opbasm/rst/m4.html>`_ are available when the m4
   program is installed. An extensive set of built-in macros provide more
   advanced features than the base language. For example, converting
   temperature scales becomes as easy as this:

  .. parsed-literal::

    reg16(rx, s4,s5)                ; Create a virtual 16-bit register pair

    c_to_f:
      load reglower(rx), s0         ; Load 8-bit Celsius temperature
      signex(rx)                    ; Sign extend to 16-bits
      expr2s(rx := rx * 9 / 5 + 32) ; Perform 16x8-bit signed arithmetic to
                                    ; get Fahrenheit
      return


 * `Static code analysis
   <http://kevinpt.github.io/opbasm/#static-code-analysis>`_ to identify dead
   code and optionally remove it. This permits the development of code
   libraries that can be included without wasting memory on unused functions.

 * Code block annotations with `user defined PRAGMA meta-comments <http://kevinpt.github.io/opbasm/#user-defined-pragma-meta-comments>`_.


Support for the full PicoBlaze-6 syntax is provided as well as `enabling most
of the new PB6 syntax enhancements in PicoBlaze-3 code
<http://kevinpt.github.io/opbasm/#enabling-most-of-the-new-pb6-syntax-enhancements-in-picoblaze-3-code>`_.
The original templating system for ROM components is supported as well as a
more flexible `generic ROM component
<http://kevinpt.github.io/opbasm/#generic-rom-component>`_ that can read *.mem*
and *.hex* files directly during synthesis and simulation. A utility script is
included that permits `updating the ROM contents of a bitstream file
<http://kevinpt.github.io/opbasm/#updating-the-rom-contents-of-a-bitstream-file>`_
without requiring resynthesis as was formerly supplied by the DOS-based KCPSM3
tools.

Files generated on non-Windows platforms will not have DOS line endings and
PicoBlaze-3 files are not restricted to 8.3 file names. Opbasm also runs
significantly faster than the native implementation:

.. image:: http://kevinpt.github.io/opbasm/_images/opbasm_perf.png

Requirements
------------

Opbasm requires either Python 2.7 or Python 3.x and no additional libraries.
The installation script depends on setuptools which will be installed if it
isn't currently present in your Python distribution. Optional macro support is
provided when m4 is installed. You can get optional colorized output from the
scripts by installing the Python colorama package. The source is written in
Python 2.7 syntax but will convert cleanly to Python 3 when the installer
passes it through 2to3.


Download
--------

You can access the Opbasm Git repository from `Github
<https://github.com/kevinpt/opbasm>`_. `Packaged source code
<https://drive.google.com/folderview?id=0B5jin2146-EXd0hBTlAzem1ybmM&usp=sharing>`_
is also available for download. You can install direct from PyPI with the "pip"
command if you have it available.


Documentation
-------------

The full documentation is available online at the `main Opbasm site
<http://kevinpt.github.io/opbasm/>`_.

========================
Open Picoblaze Assembler
========================

Opbasm is a free cross-platform assembler for the Picoblaze-3 (PB3) and Picoblaze-6 (PB6) microcontrollers `provided by Xilinx <http://www.xilinx.com/products/intellectual-property/picoblaze.htm>`_. It will run readily on any platform with a functional Python intepreter. Opbasm provides a better perfoming solution to assembling Picoblaze code without resorting to DOS or Windows emulation to run the native KCPSM assemblers. 

**Special features of Opbasm:**


 * Optional `m4 preprocessor macros <http://code.google.com/p/opbasm/wiki/m4>`_ are available when the m4 program is installed. An extensive set of builtin macros provide more advanced features than the base language.

 * `Static code analysis <http://code.google.com/p/opbasm/#Static_code_analysis>`_ can be performed to identify dead code and optionally remove it. This permits the development of code libraries that can be included without wasting memory on unused functions.

 * Code block annotations with `user defined PRAGMA meta-comments <http://code.google.com/p/opbasm/#User_defined_PRAGMAs>`_.


Support for the full Picoblaze-6 syntax is provided as well as `enabling most of the new PB6 syntax enhancements in Picoblaze-3 code <http://code.google.com/p/opbasm/#Picoblaze-3_enhancements>`_. The original templating system for ROM components is supported as well as a more flexible `generic ROM component <https://code.google.com/p/opbasm/#Generic_ROM_component>`_ that can read *.mem* and *.hex* files directly during synthesis and simulation. A utility script is included that permits updating the ROM contents of a bitstream file without requiring resynthesis as was formerly supplied by the DOS-based KCPSM3 tools.

As a bonus, files generated on non-Windows platforms will not have DOS line endings and Picoblaze-3 files are not restricted to 8.3 file names. Opbasm also runs significantly faster than the native implementation:

.. image:: http://opbasm.googlecode.com/hg/doc/opbasm_perf.png

Requirements
------------

Opbasm requires the pyparsing library and either Python 2.7 or Python 3.x. The installation script depends on setuptools which will be installed if it isn't currently present in your Python distribution. Optional macro support is provided when m4 is installed. You can get optional colorized output from the scripts by installing the Python colorama package. The source is written in Python 2.7 syntax but will convert cleanly to Python 3 when the installer passes it through 2to3.


Download
--------

You can access the Opbasm Mercurial repository from `Google Code <http://code.google.com/p/opbasm/source/checkout>`_. `Packaged source code <https://drive.google.com/folderview?id=0B5jin2146-EXd0hBTlAzem1ybmM&usp=sharing>`_ is also available for download.


Documentation
-------------

The full documentation is available online at the `main Opbasm site <http://code.google.com/p/opbasm>`_.


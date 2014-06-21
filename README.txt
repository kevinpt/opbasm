=Open Picoblaze Assembler=

Opbasm is a free cross-platform assembler for the Picoblaze-3 (PB3) and Picoblaze-6 (PB6) microcontrollers [http://www.xilinx.com/products/intellectual-property/picoblaze.htm provided by Xilinx]. It should run readily on any platform with a functional Python intepreter. Opbasm provides a better perfoming solution to assembling Picoblaze code without resorting to DOS or Windows emulation to run the native KCPSM assemblers.

Support for the full Picoblaze-6 syntax is provided as well as [#Picoblaze-3_Enhancements enabling most of the new PB6 syntax enhancements in Picoblaze-3 code]. The original templating system for ROM components is supported as well as a more flexible [#Generic_ROM_component generic ROM component] that can read _.mem_ and _.hex_ files directly during synthesis and simulation. A utility script is included that permits updating the ROM contents of a bitstream file without requiring resynthesis as was formerly supplied by the DOS-based KCPSM3 tools.

Files generated on non-Windows platforms will not have DOS line endings and Picoblaze-3 files are not restricted to 8.3 file names.


==Requirements==

Opbasm requires Python 2.7 and the pyparsing library. The installation script depends on setuptools which will be installed if it isn't currently present in your Python distribution.


==Download==

You can access the Opbasm Mercurial repository from [https://code.google.com/p/opbasm/source/checkout Google Code]. [https://drive.google.com/folderview?id=0B5jin2146-EXd0hBTlAzem1ybmM&usp=sharing Packaged source code] is also available for download.


==Installation==

If your OS has a package manager, it may be preferable to install Python setuptools and pyparsing through that tool before attempting to install Opbasm. Otherwise, the installation script will install these packages directly without registering them with the OS package manager.

After extracting the archive you can install Opbasm with the following command:

{{{
> python setup.py install
}}}

On Linux systems you may need to install with root privileges using the _sudo_ command.

After a successful install the Opbasm scripts will be available. On Linux they should be immediately accessible from your current search path. On Windows you will need to make sure that the <Python root>\Scripts directory is in your %PATH% environment variable.


==Using Opbasm==

After installation you are ready to use Opbasm. The native KCPSM assemblers rely on HDL templates to carry assembled ROM data into synthesis. You can continue to use that process by using the provided Spartan-3 template or using a template from the KCPSM6 distribution. You can alternately use the [https://code.google.com/p/opbasm/source/browse/templates/picoblaze_rom.vhdl picoblaze_rom.vhdl] component which provides a generic resizable ROM that reads _.mem_ and _.hex_ files directly without requiring a template. See below for more information on the templating options.

The assembler is invoked with the _opbasm_ script. It supports the following command line syntax:

{{{
Usage: opbasm [-i] <input file> [-n <name>] [-t <template>] [-6] [-m <mem size>] [-s <scratch size>]
       opbasm -g

Options:
  -h, --help            show this help message and exit
  -i INPUT_FILE, --input=INPUT_FILE
                        Input file
  -n MODULE_NAME, --name=MODULE_NAME
                        Module or entity name (defaults to input file name)
  -t TEMPLATE_FILE, --template=TEMPLATE_FILE
                        Template file
  -6, --pb6             Assemble Picoblaze-6 code
  -m MEM_SIZE, --mem-size=MEM_SIZE
                        Program memory size
  -s SCRATCH_SIZE, --scratch-size=SCRATCH_SIZE
                        Scratchpad memory size
  -x, --hex             Write HEX in place of MEM file
  -c, --color-log       Colorize log file
  -g, --get-templates   Get default template files
  -v, --version         Show OPBASM version
}}}

To compile to Picoblaze-3 opcodes, use the following:

{{{
> opbasm foo.psm
OPBASM - Open Picoblaze Assembler
Running in Picoblaze-3 mode
  Device configuration:
    Memory size: 1024, Scratchpad size: 64

  Reading source: foo.psm

  Assembling code... SUCCESS
    15 instructions out of 1024 (1%)
    Highest occupied address: 00E hex

  Found template:
    ROM_form.vhdl

  Writing output
        mem map: foo.mem
       log file: foo.log

  Formatted source:
    foo.fmt
}}}

To compile to Picoblaze-6, use the following:

{{{
> opbasm -6 foo.psm
OPBASM - Open Picoblaze Assembler
Running in Picoblaze-6 mode
...
}}}

By default, Opbasm outputs _.mem_ format ROM listings as produced by KCPSM3. If you want to output the _.hex_ format listings produced by KCPSM6 pass the _-x_ option. The only difference is that _.mem_ format includes an "@nnn" address directive setting the starting offset for the memory.


===Picoblaze-3 Enhancements===

You can use all Picoblaze-6 syntax extensions in Picoblaze-3 code that don't
depend on PB6 specific instructions.

For Picoblaze-3 you can use the following syntax extensions from Picoblaze-6:
  * Decimal, binary, and character literals (`41'd, 01000001'b, "A"`)
  * Predefined char constants and date/time stamp fields (`CR, LF, HT, datestamp_day`, etc.)
  * Inverted constants ( `~my_const` )
  * Environment variable constants ( `constant foo, %my_env_const` )
  * INCLUDE, DEFAULT_JUMP, and INST directives
  * Address label constants ( `my_label'upper  my_label'lower` )

For Picoblaze-3 you _CANNOT_ use the following:
  * STRING and TABLE directives
  * Picoblaze-6 instructions (CALL@, COMPARECY, HWBUILD, JUMP@, LOAD&RETURN, OUTPUTK, REGBANK, STAR, TESTCY)

Refer to the file "all_kcpsm6_syntax.psm" distributed with KCPSM6 for a detailed
explanation of the new Picoblaze-6 syntax.


===Picoblaze-6 Enhancements===

The native PB6 assembler KCPSM6.exe has a -c switch to limit the size of memory. OPBASM provides -m to do the same as well as -s to limit the scratchpad memory size to 64 or 128 bytes. MEM format files are output by default. KCPSM6-style HEX format is activated with -x.


===Templating===

All of the official KCPSM-provided HDL templates are supported. Any custom templates you have created can be used unchanged. Because of improvements to XST's support for synthesis of BRAM generics since the original release of KCPSM3, an updated Spartan-3 template [https://code.google.com/p/opbasm/source/browse/templates/ROM_form_S3_1K.vhdl ROM_form_S3_1K.vhdl] is included that eliminates the warnings from redundant attribute declarations. Templates for Picoblaze-6 devices can be found in the KCPSM6 distribution.

Because Opbasm is more flexible in the naming of output files, the original template system's assumption that the "{name}" field matches the input source file isn't necessarily valid. a new field "{source file}" is added that clearly indicates the original top level source file used to populate a template. This field is optional and only used in a comment so it is not critical to include it in your templates.

The native KCPSM assemblers are hard coded to look for a template named _ROM_form.vhd_ or _ROM_form.v_. Opbasm searches for templates by those names (as well as _ROM_form.vhdl_) but you can also pass the _-t <template file>_ option to specify a different template. If your OS supports symbolic links it is recommended to maintain a link from the original template to _ROM_form.xxx_ rather than renaming it to one of the generic defaults.

To save the bother of hunting down templates when you start a new project, you can generate copies of the default templates included with Opbasm using the following command:

{{{
> obpasm -g
Retrieving default templates...
ROM_form_S3_1K.vhdl,picoblaze_rom.vhdl
  COPYING:  /usr/local/lib/python2.7/dist-packages/opbasm-1.0-py2.7.egg/templates/ROM_form_S3_1K.vhdl
  COPYING:  /usr/local/lib/python2.7/dist-packages/opbasm-1.0-py2.7.egg/templates/picoblaze_rom.vhdl
}}}


===Generic ROM component===

As an alternative to the templating system, a generic, synthesizable VHDL ROM is provided in the [https://code.google.com/p/opbasm/source/browse/templates/picoblaze_rom.vhdl picoblaze_rom.vhdl] file. This component uses XSTs limited support for textio during synthesis to read a _.mem_ or _.hex_ file directly without the use of a template file. It takes advantage of XSTs support for automatically partitioning memories that exceed the maximum size of a BRAM. This provides a simplification of the synthesis flow and you do not need to manually switch to different template files if you change the size of the ROM for Picoblaze-6 designs. For simulation, this component has the advantage that it doesn't have to be recompiled for every change to the Picoblaze source code in a design. It automatically re-reads the latest .mem or .hex whenever the simulation is reset.

XST doesn't infer the most efficient partition for a 4Kx18 ROM on Spartan-6. The "`ROM_form_S6_4K_<date>.vhd`" template distributed with KCPSM6 uses only 4 BRAMs rather than 5 and may be a better option.


==Updating bit files==

The KCPSM3 assembler included a program and batch file that automated the process of updating a Picoblaze ROM in a bit file without requiring a resynthesis. For Picoblaze-6 that process has been abandoned in favor of using the JTAG loader.

Because some platforms don't readily support the use of the JTAG loader, the old system of updating bit files has been reimplemented as a utility script _pb_update_. You will need to have a new _.mem_ file along with the top level _.ncd_ and _.bit_ files for the design. The Xilinx ISE tools _xdl_ and _data2mem_ must be accessible from your command line path.

When run, the _pb_update_ script will convert the _.ncd_ netlist to the textual _.xdl_ format and scan it for any block RAM instances. If one BRAM is found it is assumed to be the picoblaze ROM. If multiple BRAMs are in the design, a list is provided with their instance names and you are prompted to select which one is the Picoblaze ROM. This system currently only works for ROMs that are implemented as a single 18-bit wide BRAM. The _pb_update_ script does not work if the ROM has been split across multiple BRAMs or is implemented in distributed RAM. If you know the instance name of the BRAM you want to update, you can pass it with the _-r <RAM instance>_ parameter to bypass the prompt.

{{{
> pb_update -m foo.mem -n foo.ncd
Running XDL...
Release 14.5 - xdl P.58f (lin64)
Copyright (c) 1995-2012 Xilinx, Inc.  All rights reserved.

Loading device for application Rf_Device from file '3s500e.nph' in environment /usr/local/packages/Xilinx/14.5/ISE_DS/ISE/.
   "foo" is an NCD, version 3.2, device xc3s500e, package fg320, speed -5
Successfully converted design 'foo.ncd' to 'foo.xdl'.

Selected instance: rom/Mrom__varindex0000 placed at X1Y4
Running data2mem...
  data2mem -bm foo.bmm -bd foo.mem -bt foo.bit -o b new_foo.bit
Generated updated bit file: new_foo.bit
}}}

The updated bit file is created with the prefix "`new_`".


==Syntax highlighting==

Picoblaze syntax highlighting rules for gedit and notepad++ have been included in the "highlight" directory in the source distribution.


==Licensing==

Opbasm and the included VHDL source is licensed for free commercial and non-commercial use under the terms of the MIT license.


Instructions for building the Opbasm example project.

The example picoblaze code demonstrates the use of a number of m4 macros to
manage reading data from a dual port ROM.


1) Download Picoblaze-6 distribution and copy kcpsm6.vhd to this directory

2) Assemble Picoblaze code

  opbasm -6 -o build example.psm4

3) Set up Modelsim libraries

  vlib build/work
  vmap work build/work
  vlib build/extras
  vmap extras build/extras

4) Retrieve picoblaze_rom.vhdl

  opbasm -g

5) Compile VHDL

  vcom -work extras sizing.vhdl timing_ops.vhdl
  vcom kcpsm6.vhd picoblaze_rom.vhdl example.vhdl

6) Start simulation

  vsim work.example

7) Load example_wave.do in waveform window and run the simulation. You can verify words
   from the CONST_DATA array are read out of the second ROM port, stored in scratchpad
   RAM and then pushed onto the stack.

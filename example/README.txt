
Assemble Picoblaze code

  opbasm -6 -o build example.psm4


Set up Modelsim libraries

  vlib build/work
  vmap work build/work
  vlib build/extras
  vmap extras build/extras


Retrieve picoblaze_rom.vhdl

  opbasm -g


Compile VHDL

  vcom -work extras sizing.vhdl timing_ops.vhdl
  vcom kcpsm6.vhd picoblaze_rom.vhdl example.vhdl

Run simulation

  vsim work.example

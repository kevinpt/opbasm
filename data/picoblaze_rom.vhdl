--# picoblaze_rom.vhdl - Flexible synthesizable ROM for Picoblaze
--#
--# Copyright © 2014 Kevin Thibedeau
--#
--# Permission is hereby granted, free of charge, to any person obtaining a
--# copy of this software and associated documentation files (the "Software"),
--# to deal in the Software without restriction, including without limitation
--# the rights to use, copy, modify, merge, publish, distribute, sublicense,
--# and/or sell copies of the Software, and to permit persons to whom the
--# Software is furnished to do so, subject to the following conditions:
--#
--# The above copyright notice and this permission notice shall be included in
--# all copies or substantial portions of the Software.
--#
--# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--# DEALINGS IN THE SOFTWARE.
--#
--# DEPENDENCIES: none
--#
--# DESCRIPTION:
--# This is a generic synthesizable ROM for the Picoblaze-3 and Picoblaze-6
--# processors. It can be used as an alternative to the templating ROM system
--# normally used by the Picoblaze assemblers. Instead of generating a custom
--# ROM component from a template, this package provides a general purpose ROM
--# that can read the .mem or .hex memory listings during synthesis. It is
--# known to work correctly with ISE XST 14.5.
--#
--# For simulation, this component has the advantage that it doesn't have to be
--# recompiled for every change to the Picoblaze source code in a design. It
--# automatically re-reads the latest .mem or .hex whenever the simulation is
--# reset.
--#
--# The picoblaze_rom component provides a similar port interface as the KCPSM
--# templates with the following differences:
--# * ROM_FILE generic is used to specify the name of the .mem or .hex file
--#   with the ROM contents.
--# * Address is an unconstrained array. The size of the inferred ROM is
--#   established by the size of the signal connected to the Address input. for
--#   1K it must be (9 downto 0), for 2K (10 downto 0), and 4K (11 downto 0).
--#   With KCPSM6, the 12-bit address bus must be sliced down to implement ROMs
--#   smaller than 4K.
--# * For Picoblaze-3 the Enable input must be tied high.
--#
--# Supported ROM configurations:
--#
--#                      ROM size
--# Architecture     1K            2K             4K
--# -------------------------------------------------------------
--#                   __
--# Spartan-3     1K |HL|
--#                  '--'
--#                   18
--#                   __           _   _
--# Spartan-6     1K |HL|      2K |H| |L|    4K || || || || ||
--#                  '--'         |H| |L|       || || || || ||
--#                   18          '-' '-'       || || || || ||
--#                                9   9        || || || || ||
--#                                             4  4  4  4  2
--#                   __           __            _   _
--# Virtex-6      1K |HL|      2K |HL|       4K |H| |L|
--# 7-Series         '--'         |HL|          |H| |L|
--#                   18          '--'          |H| |L|
--#                                18           |H| |L|
--#                                             '-' '-'
--#                                              9   9
--#
--# Note: XST doesn't infer the most efficient partition for 4Kx18 ROM on
--# Spartan-6. The ROM_form_S6_4K_<date>.vhd template distributed with KCPSM6
--# uses only 4 BRAMs and may be a better option.

library ieee;
use ieee.std_logic_1164.all;

package picoblaze_rom_pkg is

  component picoblaze_rom is
    generic (
      ROM_FILE : string -- ROM memory contents in .mem or .hex format
    );
    port (
      Clock       : in std_logic;
      Enable      : in std_logic;
      Address     : in std_logic_vector;
      Instruction : out std_logic_vector(17 downto 0)
    );
  end component;

end package;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;

use std.textio.all;

entity picoblaze_rom is
  generic (
    ROM_FILE : string -- ROM memory contents in .mem or .hex format
  );
  port (
    Clock       : in std_logic;
    Enable      : in std_logic;
    Address     : in std_logic_vector;
    Instruction : out std_logic_vector(17 downto 0)
  );
end entity;

architecture rtl of picoblaze_rom is
  constant MEM_SIZE : positive := 2 ** Address'length;
  type rom_mem is array (0 to MEM_SIZE-1) of bit_vector(Instruction'length-1 downto 0);

  impure function read_mem_file(File_name: string) return rom_mem is
    -- Read a .mem or .hex file as produced by KCPSM3 and KCPSM6 assemblers
    file fh       : text open read_mode is File_name;
    variable ln   : line;
    variable addr : natural := 0;
    variable word : std_logic_vector(Instruction'length-1 downto 0);
    variable rom  : rom_mem;

    procedure read_hex(ln : inout line; hex : out std_logic_vector) is
      -- The standard hread() procedure doesn't work well when the target bit vector
      -- is not a multiple of four. This wrapper provides better behavior.
      variable hex4 : std_logic_vector(((hex'length + 3) / 4) * 4 - 1 downto 0);
    begin
      hread(ln, hex4);
      hex := hex4(hex'length-1 downto 0); -- Trim upper bits
    end procedure;

  begin

    while addr < MEM_SIZE loop
      if endfile(fh) then
        exit;
      end if;

      readline(fh, ln);
      if ln(1) = '@' then -- Skip .mem address line; Assume memory starts at 0
        next;
      end if;

      read_hex(ln, word); -- Convert hex string to bits
      rom(addr) := to_bitvector(word);

      addr := addr + 1;
    end loop;

    return rom;
  end function;

  -- Initialize ROM with file contents
  signal pb_rom : rom_mem := read_mem_file(ROM_FILE);
begin

  -- Infer ROM with synchronous enable and read port
  rd: process(Clock)
  begin
    if rising_edge(Clock) then
      if Enable = '1' then
        Instruction <= to_stdlogicvector(pb_rom(to_integer(unsigned(Address))));
      end if;
    end if;
  end process;

end architecture;


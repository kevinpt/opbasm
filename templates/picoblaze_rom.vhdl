--# picoblaze_rom.vhdl - Flexible synthesizable ROM for Picoblaze
--#
--# Freely available from Opbasm (http://code.google.com/p/opbasm)
--#
--# Copyright © 2014 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
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
--# An additional picoblaze_dp_rom component is available which is a
--# dual-ported implementation with a second read/write port. Both ports run
--# on the same clock domain for simplicity but the design can be trivially
--# altered to use separate clocks if desired. The second port provides access
--# to data packed with INST directives and the ability to use a portion of the
--# memory as RAM.
--#
--# The ROM can be implemented as a block RAM (BRAM) or as LUT-based distributed
--# RAM for use when BRAMs have been exhausted or only a small ROM is needed. The
--# STYLE generic is used to control the inferred RAM style. It defaults to the
--# string "BLOCK". Set it to "DISTRIBUTED" to infer distributed RAM. Any size
--# distributed RAM can be created that is a power of 2.
--#
--# Supported BRAM-based ROM configurations:
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
      ROM_FILE : string;           -- ROM memory contents in .mem or .hex format
      STYLE    : string := "BLOCK" -- Set to "DISTRIBUTED" to use distributed RAM
    );
    port (
      Clock       : in  std_logic;
      Enable      : in  std_logic;
      Address     : in  std_logic_vector;
      Instruction : out std_logic_vector(17 downto 0)
    );
  end component;

  component picoblaze_dp_rom is
    generic (
      ROM_FILE : string;           -- ROM memory contents in .mem or .hex format
      STYLE    : string := "BLOCK" -- Set to "DISTRIBUTED" to use distributed RAM
    );
    port (
      Clock           : in  std_logic;
      Enable          : in  std_logic;
      Address         : in  std_logic_vector;
      Instruction     : out std_logic_vector(17 downto 0);

      -- Second Read/Write port
      Address2        : in  std_logic_vector;
      Instruction2    : out std_logic_vector(17 downto 0);
      We              : in  std_logic;
      Wr_instruction2 : in  std_logic_vector(17 downto 0)
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
    ROM_FILE : string;           -- ROM memory contents in .mem or .hex format
    STYLE    : string := "BLOCK" -- Set to "DISTRIBUTED" to use distributed RAM
  );
  port (
    Clock       : in  std_logic;
    Enable      : in  std_logic;
    Address     : in  std_logic_vector;
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
    variable word : std_logic_vector(Instruction'length-1 downto 0);
    variable rom  : rom_mem;

    procedure read_hex(ln : inout line; hex : out std_logic_vector) is
      -- The hread() procedure doesn't work well when the target bit vector
      -- is not a multiple of four. This wrapper provides better behavior.
      variable hex4 : std_logic_vector(((hex'length + 3) / 4) * 4 - 1 downto 0);
    begin
      hread(ln, hex4);
      hex := hex4(hex'length-1 downto 0); -- Trim upper bits
    end procedure;

    -- Convert a string to lower case
    function to_lower( source : string ) return string is
      variable r : string(source'range) := source;
    begin
      for c in r'range loop
        if character'pos(r(c)) >= character'pos('A')
            or character'pos(r(c)) <= character'pos('Z') then

          -- This would work except that XST has regressed into not supporting
          -- character'val. Presumably this is "fixed" in Vivado and will never get
          -- corrected in poor old XST.
          r(c) := character'val(character'pos(r(c)) + 16#20#);
        end if;
      end loop;

      return r;
    end function;
  begin

    -- Can't call to_lower() for case-insensitive comparison because of XST limitation
    --if to_lower(File_name(File_name'length-3 to File_name'length)) = ".mem" then
    if File_name(File_name'length-3 to File_name'length) = ".mem" then
      -- Read the first address line of a .mem file and discard it
      -- Assume memory starts at 0
      readline(fh, ln);
    end if;


    -- XST isn't happy with a while loop because of its low default iteration limit setting
    -- so we have to use a for loop.
    for addr in 0 to MEM_SIZE-1 loop
      if endfile(fh) then
        exit;
      end if;

      readline(fh, ln);

      read_hex(ln, word); -- Convert hex string to bits
      rom(addr) := to_bitvector(word);

    end loop;

    return rom;
  end function;

  -- Initialize ROM with file contents
  signal pb_rom : rom_mem := read_mem_file(ROM_FILE);

  attribute RAM_STYLE : string;
  attribute RAM_STYLE of pb_rom : signal is STYLE;
begin

  -- Infer BRAM-based ROM with synchronous enable and read port
  rd: process(Clock)
  begin
    if rising_edge(Clock) then
      if Enable = '1' then
        Instruction <= to_stdlogicvector(pb_rom(to_integer(unsigned(Address))));
      end if;
    end if;
  end process;

end architecture;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;

use std.textio.all;

entity picoblaze_dp_rom is
  generic (
    ROM_FILE : string;           -- ROM memory contents in .mem or .hex format
    STYLE    : string := "BLOCK" -- Set to "DISTRIBUTED" to use distributed RAM
  );
  port (
    Clock           : in  std_logic;
    Enable          : in  std_logic;
    Address         : in  std_logic_vector;
    Instruction     : out std_logic_vector(17 downto 0);

    -- Second Read/Write port
    Address2        : in  std_logic_vector;
    Instruction2    : out std_logic_vector(17 downto 0);
    We              : in  std_logic;
    Wr_instruction2 : in  std_logic_vector(17 downto 0)
  );
end entity;

architecture rtl of picoblaze_dp_rom is
  constant MEM_SIZE : positive := 2 ** Address'length;
  type rom_mem is array (0 to MEM_SIZE-1) of bit_vector(Instruction'length-1 downto 0);

  impure function read_mem_file(File_name: string) return rom_mem is
    -- Read a .mem or .hex file as produced by KCPSM3 and KCPSM6 assemblers
    file fh       : text open read_mode is File_name;
    variable ln   : line;
    variable word : std_logic_vector(Instruction'length-1 downto 0);
    variable rom  : rom_mem;

    procedure read_hex(ln : inout line; hex : out std_logic_vector) is
      -- The hread() procedure doesn't work well when the target bit vector
      -- is not a multiple of four. This wrapper provides better behavior.
      variable hex4 : std_logic_vector(((hex'length + 3) / 4) * 4 - 1 downto 0);
    begin
      hread(ln, hex4);
      hex := hex4(hex'length-1 downto 0); -- Trim upper bits
    end procedure;

    -- Convert a string to lower case
    function to_lower( source : string ) return string is
      variable r : string(source'range) := source;
    begin
      for c in r'range loop
        if character'pos(r(c)) >= character'pos('A')
            or character'pos(r(c)) <= character'pos('Z') then

          -- This would work except that XST has regressed into not supporting
          -- character'val. Presumably this is "fixed" in Vivado and will never get
          -- corrected in poor old XST.
          r(c) := character'val(character'pos(r(c)) + 16#20#);
        end if;
      end loop;

      return r;
    end function;
  begin

    -- Can't call to_lower() for case-insensitive comparison because of XST limitation
    --if to_lower(File_name(File_name'length-3 to File_name'length)) = ".mem" then
    if File_name(File_name'length-3 to File_name'length) = ".mem" then
      -- Read the first address line of a .mem file and discard it
      -- Assume memory starts at 0
      readline(fh, ln);
    end if;


    -- XST isn't happy with a while loop because of its low default iteration limit setting
    -- so we have to use a for loop.
    for addr in 0 to MEM_SIZE-1 loop
      if endfile(fh) then
        exit;
      end if;

      readline(fh, ln);

      read_hex(ln, word); -- Convert hex string to bits
      rom(addr) := to_bitvector(word);

    end loop;

    return rom;
  end function;

  -- Initialize ROM with file contents
  signal pb_rom : rom_mem := read_mem_file(ROM_FILE);

  attribute RAM_STYLE : string;
  attribute RAM_STYLE of pb_rom : signal is STYLE;
begin

  -- Infer ROM with synchronous enable and dual read port
  rd: process(Clock)
  begin
    if rising_edge(Clock) then
      if Enable = '1' then
        -- Read port 1
        Instruction  <= to_stdlogicvector(pb_rom(to_integer(unsigned(Address))));

        -- Read/write port 2
        Instruction2 <= to_stdlogicvector(pb_rom(to_integer(unsigned(Address2))));
        if We = '1' then
          pb_rom(to_integer(unsigned(Address2))) <= to_bitvector(Wr_instruction2);
        end if;
      end if;
    end if;
  end process;

end architecture;



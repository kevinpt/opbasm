
library ieee;
use ieee.std_logic_1164.all;

library extras;
use extras.timing_ops.all;

use work.picoblaze_rom_pkg.all;

entity example is
  generic (
    ROM_FILE : string := "build/example.mem"
    );
end entity;

architecture rtl of example is

  signal   sim_done   : boolean      := false;
  constant CLOCK_FREQ : frequency    := 50 MHz;
  constant CPERIOD    : delay_length := to_period(CLOCK_FREQ);


  component kcpsm6 is
    generic(
      hwbuild                 : std_logic_vector(7 downto 0)  := X"00";
      interrupt_vector        : std_logic_vector(11 downto 0) := X"3FF";
      scratch_pad_memory_size : integer                       := 64);
    port (
      address        : out std_logic_vector(11 downto 0);
      instruction    : in  std_logic_vector(17 downto 0);
      bram_enable    : out std_logic;
      in_port        : in  std_logic_vector(7 downto 0);
      out_port       : out std_logic_vector(7 downto 0);
      port_id        : out std_logic_vector(7 downto 0);
      write_strobe   : out std_logic;
      k_write_strobe : out std_logic;
      read_strobe    : out std_logic;
      interrupt      : in  std_logic;
      interrupt_ack  : out std_logic;
      sleep          : in  std_logic;
      reset          : in  std_logic;
      clk            : in  std_logic);
  end component;

  signal address        : std_logic_vector(11 downto 0);
  signal instruction    : std_logic_vector(17 downto 0);
  signal bram_enable    : std_logic;
  signal in_port        : std_logic_vector(7 downto 0);
  signal out_port       : std_logic_vector(7 downto 0);
  signal port_id        : std_logic_vector(7 downto 0);
  signal write_strobe   : std_logic;
  signal k_write_strobe : std_logic;
  signal read_strobe    : std_logic;
  signal interrupt      : std_logic;
  signal interrupt_ack  : std_logic;
  signal sleep          : std_logic;
  signal reset          : std_logic;
  signal clk            : std_logic;

  signal address2     : std_logic_vector(address'range);
  signal instruction2 : std_logic_vector(instruction'range);
begin

  stim : process
  begin
    reset     <= '1', '0' after CPERIOD;
    interrupt <= '0';
    sleep     <= '0';

    -- Wait for write to port FF signaling end of the program
    wait until rising_edge(write_strobe) and port_id = X"FF";

    sim_done <= true;
    wait;
  end process;

  pb6 : kcpsm6
    port map (
      address        => address,
      instruction    => instruction,
      bram_enable    => bram_enable,
      in_port        => in_port,
      out_port       => out_port,
      port_id        => port_id,
      write_strobe   => write_strobe,
      k_write_strobe => k_write_strobe,
      read_strobe    => read_strobe,
      interrupt      => interrupt,
      interrupt_ack  => interrupt_ack,
      sleep          => sleep,
      reset          => reset,
      clk            => clk
      );

  rom : picoblaze_dp_rom
    generic map (
      ROM_FILE => ROM_FILE
      ) port map (
        Clock       => clk,
        Enable      => bram_enable,
        Address     => address,
        Instruction => instruction,

        Address2        => address2,
        Instruction2    => instruction2,
        We              => '0',
        Wr_instruction2 => (others => '0')
        );

  -- Connect second port of ROM to Picoblaze
  port_reg : process(clk, reset) is
  begin
    if reset = '1' then
      address2 <= (others => '0');
      in_port  <= (others => '0');
    elsif rising_edge(clk) then
      if write_strobe = '1' then
        if port_id = X"00" then
          address2(7 downto 0) <= out_port;
        end if;
        if port_id = X"01" then
          address2(11 downto 8) <= out_port(3 downto 0);
        end if;
      end if;

      if port_id = X"02" then
        in_port <= instruction2(7 downto 0);
      elsif port_id = X"03" then
        in_port <= instruction2(15 downto 8);
      end if;
    end if;
  end process;

  cgen : process
  begin
    clock_gen(clk, sim_done, CLOCK_FREQ);
    wait;
  end process;
end architecture;

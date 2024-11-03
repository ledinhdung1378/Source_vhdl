library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity counter_00_99_tb is
end entity;

architecture testbench of counter_00_99_tb is
    -- Component declaration
    component counter_00_99
        port(
            clk : in std_logic ;
            clr : in std_logic ; 
            led_dv : out std_logic_vector (6 downto 0);
            led_ch : out std_logic_vector (6 downto 0);
            value_dv : out std_logic_vector (3 downto 0);
            value_ch : out std_logic_vector (3 downto 0)
        );
    end component;

    -- Signals for inputs and outputs
    signal clk : std_logic := '0';
    signal clr : std_logic := '0';
    signal led_dv : std_logic_vector(6 downto 0);
    signal led_ch : std_logic_vector(6 downto 0);
    signal value_dv: std_logic_vector (3 downto 0);
    signal value_ch: std_logic_vector (3 downto 0);
    
    -- Clock period
    constant clk_period : time := 4 ns;

begin
    -- Instantiate the counter
    uut: counter_00_99
        port map(
            clk => clk,
            clr => clr,
            led_dv => led_dv,
            led_ch => led_ch,
            value_dv => value_dv,
            value_ch => value_ch
        );

    -- Clock process
    clk_process: process
    begin
        clk <= '0';
        wait for clk_period / 2;
        clk <= '1';
        wait for clk_period / 2;
    end process;

    -- Stimulus process
    stim_proc: process
    begin
        -- Clear signal active
        clr <= '1';
        wait for clk_period;
        clr <= '0';

        -- Let the counter run for 1 ms to observe changes
        

        -- End the simulation
        wait;
    end process;
end architecture;

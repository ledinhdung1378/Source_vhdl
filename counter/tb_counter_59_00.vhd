library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity tb_counter_59_00 is 
end entity ;

architecture Behavioral of tb_counter_59_00 is 

component counter_59_00 is 
     port(
        clk : in std_logic ; 
        clr : in std_logic ; 
        led_ch : out std_logic_vector (6 downto 0) ; 
        led_dv : out std_logic_vector (6 downto 0);
        value_ch : out std_logic_vector (3 downto 0) ; 
        value_dv : out std_logic_vector (3 downto 0)

    );
end component ; 

signal clk,clr : std_logic ; 
signal led_ch,led_dv: std_logic_vector (6 downto 0);
signal value_ch,value_dv:std_logic_vector (3 downto 0);
begin 
    uut : counter_59_00 port map (clk=>clk,clr=>clr,led_ch=>led_ch,led_dv=>led_dv,value_ch=>value_ch,value_dv=>value_dv);
    -- tao xung clock 
    process 
        begin 
            clk <= '1' ; 
            wait for 5 ns;
            clk <= '0';
            wait for 5 ns ;
        end process ;
    -- test 
    process 
        begin 
            clr <= '0';
            wait for 40 ns;
            clr <= '1';
            wait for 10 ns; 
            clr <= '0'; 
            wait ; 
        end process ;
end Behavioral ; 
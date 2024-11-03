library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity tb_nhay_trai_phai_S is
end entity;

architecture Behavioral of tb_nhay_trai_phai_S is

component nhay_trai_phai_S is
       port(
        clk : in std_logic;
        clr: in std_logic;
        S : in std_logic;
        led_out : out std_logic_vector (7 downto 0)
    ); 
end component ;
signal clr, clk , S : std_logic ;
signal led_out : std_logic_vector (7 downto 0);

begin 
    uut : nhay_trai_phai_S port map (clk => clk, clr => clr, S =>S ,led_out => led_out);
    -- tao xung clock 
    process 
        begin 
            clk <= '1'; 
            wait for 5 ns ;
            clk <= '0'; 
            wait for 5 ns ; 
        end process; 
    -- test
    process 
        begin 
            S <= '0';
            clr <='0';
            wait for 30 ns ;
            clr <='1';
            wait for 10 ns; 
            clr <='0';
            wait for 90 ns; 
            S <= '1';
            wait;
        end process;
end Behavioral ; 

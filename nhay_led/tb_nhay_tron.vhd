library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity tb_nhay_tron is 
end entity;

architecture Behavioral of tb_nhay_tron is 

component nhay_tron is
    port(
        clk : in std_logic ;
        clr : in std_logic ;
        led_out : out std_logic_vector(7 downto 0) 
    );
end component ; 

signal clr, clk : std_logic ;
signal led_out : std_logic_vector (7 downto 0);

begin 
    uut: nhay_tron port map (clk=>clk,clr=>clr,led_out=>led_out);
    process 
        begin 
            clk <= '1' ; wait for 5 ns ; 
            clk <= '0' ; wait for 5 ns ; 
        end process ; 
    -- test 
    process 
        begin 
            clr <='0';
            wait for 30 ns ; 
            clr <='1';
            wait for 20 ns ;
            clr <='0';
            wait ;
        end process ; 

end Behavioral ; 


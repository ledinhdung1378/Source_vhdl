library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


entity nhay_trai_phai_S is
    port(
        clk : in std_logic;
        clr: in std_logic;
        S : in std_logic;
        led_out : out std_logic_vector (7 downto 0)
    ); 
end entity; 

architecture Behavioral of nhay_trai_phai_S is

signal state_led : std_logic_vector (7 downto 0):="10000000";

begin 
    process(clk,clr)
    begin
        if clr = '1' then   
            state_led <= "10000000";
        elsif rising_edge(clk) then 
            if S = '0' then 
                state_led <= state_led(0) & state_led(7 downto 1);
            else 
                state_led <= state_led(6 downto 0) & state_led(7);
            end if;
        end if ; 
                
    end process; 
    led_out <= state_led ; 
    
end Behavioral; 
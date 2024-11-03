library ieee ; 
use ieee.std_logic_1164.all;
use ieee.std_logic_vector_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity nhay_jonhson is 
port (
    clk : in std_logic ; 
    clr : in std_logic ; 
    led_out : out std_logic_vector (7 downto 0)
); 
end entity ; 

architecture Behavioral of nhay_jonhson is 

signal state_led : std_logic_vector (7 downto 0) := "00000000" ; 

begin 

    process (clk, clr)
        begin 
            if clr = '1' then 
                state_led <= "00000000" ; 
            elsif rising_edge (clk) then 
                state_led <= state_led(6 downto 0) & not(state_led(7)); 
            end if ; 
        end process ; 
end Behavioral ; 
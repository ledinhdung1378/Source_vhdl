library ieee; 
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity nhay_tron is 
    port(
        clk : in std_logic ;
        clr : in std_logic ; 
        led_out : out std_logic_vector(7 downto 0) 
    );
end entity ; 

architecture Behavioral of nhay_tron is 

signal led_shift : std_logic_vector (7 downto 0) := "10000000" ; 

begin 

    process (clk,clr)
        begin
            if clr ='1' then 
                led_shift <= "10000000"; 
            elsif rising_edge(clk) then 
                led_shift <= led_shift(0) & led_shift(7 downto 1) ; 
            end if ; 
        end process ; 

    led_out <= led_shift ; 
end Behavioral  ; 

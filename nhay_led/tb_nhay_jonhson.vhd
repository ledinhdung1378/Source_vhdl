library ieee ; 
use ieee.std_logic_1164.all;
use ieee.std_logic_vector_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity tb_nhay_jonhson is 
end entity ; 

architecture Behavioral of tb_nhay_jonhson is 

component nhay_jonhson is 
port (
    clk : in std_logic ; 
    clr : in std_logic ; 
    led_out : out std_logic_vector (7 downto 0)
);
end component ;

signal clr, clk : std_logic ;
signal led_out : std_logic_vector (7 downto 0);

begin 
    uut : nhay_jonhson port map (clk => clk, clr => clr, led_out => led_out) ; 
    -- tao xung clock 
     
        process 
        begin 
            clk = '1' ; 
            wait for 5 ns; 
            clk = '0' ;
            wait for 5 ns;
        end process ;
    -- qua trinh test 
    process 
        begin 

        wait ; 
        end process ; 
end Behavioral ; 
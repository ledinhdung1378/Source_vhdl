library ieee ; 
use ieee.std_logic_1164.all 
use ieee.std_logic_arith.all 
use ieee.std_logic_unsigned.all 

entity alu_4bit is 
    generric n = 4 ; 
    port 
    (
        a,b : in std_logic_vector (n-1 downto 0) ;
        alu_sel : in std_logic_vector (n-1 downto 0) ; 
        alu_out : out std_logic_vector (n-1 downto 0) 
    ) ; 
end entity ; 
architecture Behavioral of alu_4bit is 

signal tmp : std_logic_vector (n downto 0)  ; 

begin 
    process (a,b,alu_sel) 
        begin 
            case (alu_sel)
                begin
                    when 001 =>  tmp <= (0 & a) + (0 & b)  ;
                    when 010 =>  tmp <= a - b  ;
                    when 011 =>  tmp <= a >> 1  ;
                    when 100 =>  tmp <= a << 1  ;
                    when 101 =>  tmp <= a and b  ;
                    when 110 =>  tmp <= a or b  ;
                    when 111 =>  tmp <= a xor b ; 
                end case  ; 
        end process ; 

alu_out <= tmp(n-1 downto 0) ; 

end Behavioral ; 
        


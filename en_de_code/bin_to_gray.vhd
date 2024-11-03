library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity bin_to_gray is
    generic (n:integer:=4);
    port (
        bin_in : in std_logic_vector (n-1 downto 0);
        gray_out : out std_logic_vector (n-1 downto 0)
    );
end entity ;

architecture Behavioral of bin_to_gray is 

begin 
    process (bin_in)
        begin 
            gray_out(n-1) <= bin_in(n-1) ; 
            for i in n-2 downto 0 loop
                gray_out(i) <= bin_in(i+1) xor bin_in(i) ; 
            end loop; 
        end process ;

end Behavioral ;
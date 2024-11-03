library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity comparator_nbit is 
    generic (n: integer:=4);
    port(
        a,b : in std_logic_vector(n-1 downto 0);
        a_eq_b : out std_logic ;
        a_gt_b : out std_logic ;
        a_lt_b : out std_logic 
    );
end entity ; 

architecture Behavioral of comparator_nbit is 

begin 
    a_eq_b <= '1' when a = b else '0';
    a_gt_b <= '1' when a > b else '0';
    a_lt_b <= '1' when a < b else '0';

end Behavioral ; 

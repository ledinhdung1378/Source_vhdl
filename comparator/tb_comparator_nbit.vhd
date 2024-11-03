library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity tb_comparator_nbit is 
end entity ;

architecture Behavioral of tb_comparator_nbit is 

component comparator_nbit is 
    generic (n: integer:=4);
     port(
        a,b : in std_logic_vector(n-1 downto 0);
        a_eq_b : out std_logic ;
        a_gt_b : out std_logic ;
        a_lt_b : out std_logic 
    );
end component ; 
constant n :integer := 4 ;
signal a,b : std_logic_vector (n-1 downto 0);
signal a_eq_b,a_gt_b,a_lt_b : std_logic ;

begin 
    uut : comparator_nbit port map(a=>a,b=>b,a_eq_b=>a_eq_b,a_gt_b=>a_gt_b,a_lt_b=>a_lt_b);
    process 
        begin
            a <= "1000";
            b <= "0001";
            wait for  20 ns;
            a <= "1010";
            b <= "1101";
            wait for  20 ns;
            a <= "1000";
            b <= "0011";
            wait for  20 ns;
        end process ; 
        
end Behavioral;
        
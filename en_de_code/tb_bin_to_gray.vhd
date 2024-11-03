library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity tb_bin_to_gray is 
end entity ;

architecture Behavioral of tb_bin_to_gray is 

component bin_to_gray is 
    generic (n:integer:=4);
    port (
        bin_in : in std_logic_vector (n-1 downto 0);
        gray_out : out std_logic_vector (n-1 downto 0)
    );
end component ;

constant n:integer:=4 ; 
signal bin_in,gray_out : std_logic_vector (n-1 downto 0);

begin 
    uut : bin_to_gray port map (bin_in=>bin_in, gray_out=>gray_out);
    process 
        begin 
            bin_in <= "1001";
            wait for 20 ns ;
            bin_in <= "1011";
            wait for 20 ns ;
            bin_in <= "0001";
            wait for 20 ns ;
            bin_in <= "1000";
            wait for 20 ns ;
            wait; 
        end process ;
end Behavioral ;



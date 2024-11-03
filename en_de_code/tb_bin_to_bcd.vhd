library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity tb_bin_to_bcd is 
    generic (n:integer :=4) ;
    generic (m:integer :=4) ;
end entity ;

architecture Behavioral of tb_bin_to_bcd is 

component tb_bin_to_bcd is 
    port(
        bin_in : in std_logic_vector (n-1 downto 0);
        dv : out std_logic_vector (m-1 downto 0);
        ch : out std_logic_vector (m-1 downto 0)
    );
end component;

constant (m : integer:=4); 
signal bin_in : std_logic_vector (n-1 downto 0);
signal dv : std_logic_vector (m-1 downto 0);
signal ch : std_logic_vector (m-1 downto 0);

begin 
    uut : tb_bin_to_bcd port map (bin_in=>bin_in,dv=>dv,ch=>ch) ;
    process     
        begin   
            bin_in <= "1000" ; 
            wait for 20 ns ; 
            bin_in <= "1010" ; 
            wait for 20 ns ; 
            bin_in <= "1101" ; 
            wait for 20 ns ; 
            bin_in <= "1111" ; 
            wait for 20 ns ; 
            bin_in <= "0011" ; 
            wait for 20 ns ; 
            bin_in <= "0001" ; 
            wait for 20 ns ; 
            wait;
        end process ;
end Behavioral ; 





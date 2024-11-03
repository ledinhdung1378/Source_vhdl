library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity bin_to_bcd is 
    generic (n:integer :=4) ; -- viec khai bao generic o day co the su dung trong architecture
    generic (m:integer :=4) ; -- vs cac phep toan phuc tap thi kg the dung bien generic trong entity nhung co the dung trong arrchitec
    port(
        bin_in : in std_logic_vector (n-1 downto 0);
        dv : out std_logic_vector (m-1 downto 0);
        ch : out std_logic_vector (m-1 downto 0)
    );
end entity ;

architecture Behavioral of bin_to_bcd is 

begin 
    process (bin_in)
        variable bcd_bin : std_logic_vector (n+m-1 downto 0);
        variable cnt : integer range 0 to 3 ;
        begin   
            bcd_bin := "00000000" & bin_in ; 
            dem := n-1 ; 
            while dem > 0 
                bcd_bin := bcd_bin(10 downto 0) & '0'; 
                dem := dem-1 ;
                if bcd_bin(7 downto 4) >= "0101" then 
                    bcd_bin(7 downto 4) := bcd_bin(7 downto 4) + "0011" ; 
                end if; 
            end loop ; 
            bcd_bin := bcd_bin(10 downto 0) & '0' ; 
            dv <= bcd_in (11 downto 8) ;
            ch <= bcd_in (7 downto 4) ;
        end process ; 
end Behavioral ; 


library ieee;
use ieee.std_logic_arith.all;
use ieee.std_logic_1164.all ; 

entity triger_jk is 
port(
    clk : in std_logic ;
    clr : in std_logic ;
    pre : in std_logic ;
    q : in std_logic ;
    j : in std_logic ;
    k : in std_logic ;
)
end entity ; 

architecture Behavioral of triger_jk is 
signal qreg : std_logic := '0' ; 

begin 

    process (clk,clr)
        begin 
            if rising_edge (clk) then 
                if clr = '1' then 
                    qreg <= '0';
                elsif pre = '1' then 
                    qreg <= '1'; 
                else 
                    qreg <= j and (not(qreg)) or (not(k) and qreg) ; 
                end if ;
            end if ;
        end process ; 
    q <= qreg ; 
end Behavioral ; 






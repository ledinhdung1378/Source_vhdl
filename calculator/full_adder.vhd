library ieee;
use ieee.std_logic_1164.all;

entity full_adder is
    port (
        a,b,cin : in std_logic ;
        sum,cout : out std_logic 
    ); 
end entity ;

architecture Behavioral of full_adder is 

begin 
    sum <= a xor b xor cin ; 
    cout <= ((a xor b) and cin) or (a and b) ; 

end Behavioral ; 



library ieee;
use ieee.std_logic_1164.all;

entity subtractor is 
    port (
        a,b : in std_logic_vector(n-1 downto 0) ; 
        cin : in std_logic;
        cout : in std_logic;
        subt : in std_logic_vector(n-1 downto 0) 
    ); 
end entity  ;

architecture Structure of subtractor is 

component full_adder is 
    port (
        a,b,cin : in std_logic ;
        sum,cout : out std_logic 
    ); 
end component ;

signal c : std_logic_vector (n-1 downto 0); 
    cin <= '1' ; 
    c(0) <= cin ; 
    gen : for i in 0 to n-1 generate 
        uut : full_adder port map (a=>a(i),b=>not(b(i)),cin=>c(i),cout=>c(i+1)) ;
    end generate ; 
    cout <= c(3); 

end Structure ; 

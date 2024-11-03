library ieee;
use ieee.std_logic_1164.all;

entity adder_nbit is 
    generic (n:integer:=4);
    port (
        a,b : in std_logic_vector (n-1 downto 0);
        cin : in std_logic ;
        cout : out std_logic ;
        sum : out std_logic_vector (n-1 downto 0)
    );
end entity ; 

architecture Structure of adder_nbit is 
signal c : std_logic_vector (n-1 downto 0 ) ; 

component full_adder is 
    port (
        a,b,cin : in std_logic ;
        sum,cout : out std_logic 
    ); 
end component ;

begin  
    c(0) <= cin ; 
    gen: for i in 0 to n-1 generate 
        uut: full_adder port map(a=>a(i),b=>b(i),cin=>c(i),sum=>sum(i),cout=>c(i+1));
    end generate ;
    cout <= c(3) ; 
end Structure ; 

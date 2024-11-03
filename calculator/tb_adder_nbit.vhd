library ieee;
use ieee.std_logic_1164.all;

entity tb_adder_nbit is 
    generic(n:integer:=4);
end entity ; 

architecture Behavioral of tb_adder_nbit is 

component adder_nbit is 
    port (
        a,b : in std_logic_vector (n-1 downto 0);
        cin : in std_logic ;
        cout : out std_logic ;
        sum : out std_logic_vector (n-1 downto 0)
    );
end component ;

signal a,b : std_logic_vector (n-1 downto 0) ; 
signal cin : std_logic := '0';
signal cout : std_logic ;
signal sum : std_logic_vector (n-1 downto 0); 

begin 
    uut : adder_nbit port map(a=>a,b=>b,cin=>cin,cout=>cout,sum=>sum);

    process 
        begin 
            a <= "1000" ; 
            b <= "1001" ; 
            wait for 20 ns; 
            a <= "1100" ; 
            b <= "1101" ; 
            wait for 20 ns; 
            a <= "0000" ; 
            b <= "1001" ; 
            wait for 20 ns; 
            a <= "1000" ; 
            b <= "0010" ; 
            wait for 20 ns; 
            wait ; 
         end process ; 
end Behavioral  ; 
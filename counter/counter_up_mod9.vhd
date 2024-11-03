library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity counter_up_mod9 is 
    port(
        clk : in std_logic ;
        clr : in std_logic ;
        pause : in std_logic ;
        qout : out std_logic_vector (3 downto 0)
    );
end entity ; 

architecture Structure of counter_up_mod9 is 

signal q : std_logic_vector (3 downto 0) := "0000" ; 
signal qd : std_logic_vector (3 downto 0) ;
signal x,y,z : std_logic ;
component triger_jk_pause is 
 port(
    clk : in std_logic ;
    clr : in std_logic ;
    pause : in std_logic ;
    q : out std_logic ;
    qd : out std_logic ;
    j : in std_logic ;
    k : in std_logic 
);
end component ; 

begin 
    x <= q(0) and q(1);
    y <= x and q(2);

    F1: triger_jk_pause port map(clk=>clk,clr=>clr,pause=>pause,j=>qd(3),k=>'1',q=>q(0),qd=>qd(0));
    F2: triger_jk_pause port map(clk=>clk,clr=>clr,pause=>pause,j=>q(0),k=>q(0),q=>q(1),qd=>qd(1));
    F3: triger_jk_pause port map(clk=>clk,clr=>clr,pause=>pause,j=>x,k=>x,q=>q(2),qd=>qd(2));
    F4: triger_jk_pause port map(clk=>clk,clr=>clr,pause=>pause,j=>y,k=>'1',q=>q(3),qd=>qd(3));

    qout <= q ;

end Structure ; 
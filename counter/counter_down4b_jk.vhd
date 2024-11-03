library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 
    
entity counter_down4b_jk is 
port(
    clr : in std_logic ;
    clk : in std_logic ;
    pause : in std_logic ;
    qout : out std_logic_vector (3 downto 0) 
); 
end entity ; 

architecture Structure of counter_down4b_jk is 

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

signal q : std_logic_vector (3 downto 0) :="0000";
signal qd : std_logic_vector (3 downto 0) ;
signal x,y : std_logic ;

begin 
    x <= qd(0) and qd(1) ; 
    y <= qd(2) and x ; 

    F1: triger_jk_pause port map (clk =>clk, clr =>clr, pause =>pause, j =>'1', k =>'1', q =>qout(0),qd =>qd(0));
    F2: triger_jk_pause port map (clk =>clk, clr =>clr, pause =>pause, j =>qd(0), k =>qd(0), q =>qout(1),qd =>qd(1));
    F3: triger_jk_pause port map (clk =>clk, clr =>clr, pause =>pause, j =>x, k =>x, q =>qout(2),qd =>qd(2));
    F4: triger_jk_pause port map (clk =>clk, clr =>clr, pause =>pause, j =>y, k =>y, q =>qout(3),qd =>qd(3));

end Structure ; 
    
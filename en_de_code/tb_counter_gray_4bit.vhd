library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity tb_counter_gray_4bit is 
end entity ;

architecture Behavioral of tb_counter_gray_4bit is 

component counter_gray_4bit is 
    port (
        clk : in std_logic ;
        clr : in std_logic ;
        pause : in std_logic ;
        count_gr : out std_logic_vector (3 downto 0) 
    );
end component ;

signal clr,clk,pause : std_logic ;
signal count_gr : std_logic_vector(3 downto 0);

begin 
    uut : counter_gray_4bit port map (clr=>clr,clk=>clk,pause=>pause,count_gr=>count_gr);

    -- tao xung clock 
    process 
        begin 
            clk <= '1'; 
            wait for 5 ns ;
            clk <= '0'; 
            wait for 5 ns ;
        end process ;
    -- test 
    process 
        begin 
            clr <= '0' ;
            pause <= '0' ;
            wait for 40 ns ;
            clr <= '1'; 
            wait for 10 ns ; 
            clr <= '0' ; 
            wait for 30 ns ; 
            pause <= '1'; 
            wait for 20 ns; 
            pause <= '0' ; 
            wait ; 
        end process ;
end Behavioral ; 

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity tb_counter_down4b_jk is 
end entity ; 

architecture Behavioral of tb_counter_down4b_jk is 

component counter_down4b_jk is 
    port(
        clr : in std_logic ;
        clk : in std_logic ;
        pause : in std_logic ;
        qout : out  std_logic_vector (3 downto 0) 
    ); 

end component ; 

signal clk,clr,pause : std_logic ;
signal qout : std_logic_vector (3 downto 0);

begin 
    uut : counter_down4b_jk port map (clk=>clk,clr=>clr,pause=>pause,qout=>qout);

    -- tao xung clokc 
    process 
        begin 
            clk <= '1' ; 
            wait for 5 ns ;
            clk <= '0'  ;
            wait for 5 ns ;
        end process ; 
    -- test 
    process 
        begin 
            clr <='0';
            pause <='0';
            wait for 20 ns ; 
            clr <= '1' ; 
            wait for 20 ns ; 
            clr <= '0'   ; 
            wait for 30 ns ;
            pause <= '1'; 
            wait for 20 ns ; 
            pause <= '0';
            wait;
        end process ;
end Behavioral ; 
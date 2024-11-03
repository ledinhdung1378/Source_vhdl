library IEEE ;
use STD_LOGIC_1164.ALL; 
use STD_LOGIC_VECTOR_1164.ALL; 
use STD_LOGIC_UNSIGNED.ALL; 
use STD_LOGIC_ARITH.ALL; 

entity tb_nhay_trai_phai is 
end tb_nhay_trai_phai;

architecture Behavioral of tb_nhay_trai_phai is

component nhay_trai_phai is 
    port(
        clr : in std_logic ; 
        clk : in std_logic ; 
        led_out : in std_logic_vector (7 downto 0)
    ); 
signal clr, clk : std_logic ;
signal led_out : std_logic_vector (7 downto 0);

begin 
    uut : nhay_trai_phai port map (clr => clr, clk => clk, led_out => led_out);

    -- tao xung clock voi chu ki 10 ns ;
    process 
        begin 
            clk = '1' ; 
            wait for 5 ns; 
            clk = '0' ;
            wait for 5 ns;
        end process ;
    -- qua trinh test 
    process 
        begin 
            clr = '1' ;
            wait for 10 ns ;
            wait ; 
        end process ;   
end;
library IEEE ;
use STD_LOGIC_1164.ALL; 
use STD_LOGIC_VECTOR_1164.ALL; 
use STD_LOGIC_UNSIGNED.ALL; 
use STD_LOGIC_ARITH.ALL; 

entity nhay_trai_phai is
    port(
        clr : in std_logic ; 
        clk : in std_logic ; 
        led_out : in std_logic_vector (7 downto 0);
    ) 
end nhay_trai_phai;

architecture Behavioral of nhay_trai_phai is 
signal led_shift : std_logic_vector (7 downto 0);

begin 
    process(clk,clr)
    begin
        if clr = '1' then 
            led_shift <= "10000000";
        elsif rising_edge (clk) then 
            if led_shift = "00000001" then 
                led_shift = "10000000"
            else 
                led_shift <= '0' & led_shift(7 downto 1);
            end if ; 
        end if ;
    end process;

    led_out <= led_shift ; 

end
        
    
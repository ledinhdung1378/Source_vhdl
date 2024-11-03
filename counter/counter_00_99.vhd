library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity counter_00_99 is 
port (
    clk : in std_logic ;
    clr : in std_logic ; 
    led_dv : out std_logic_vector (6 downto 0);
    led_ch : out std_logic_vector (6 downto 0);
    value_dv : out std_logic_vector (3 downto 0);
    value_ch : out std_logic_vector (3 downto 0)
); 
end entity ; 
architecture Behavioral of counter_00_99 is 
-- ma hoa bcd sang led 7 doan a chung 
function bcd_to_7seg(bcd: std_logic_vector(3 downto 0)) return std_logic_vector is 
    begin 
        case bcd is 
            when "0000" => return "1000000"; -- 0
            when "0001" => return "1111001"; -- 1
            when "0010" => return "0100100"; -- 2
            when "0011" => return "0110000"; -- 3
            when "0100" => return "0011001"; -- 4
            when "0101" => return "0010010"; -- 5
            when "0110" => return "0000010"; -- 6
            when "0111" => return "1111000"; -- 7
            when "1000" => return "0000000"; -- 8
            when "1001" => return "0010000"; -- 9
            when others => return "1111111"; -- Mã lỗi
        end case ; 
    end function ; 

signal dv : std_logic_vector(3 downto 0) :="0000"; 
signal ch : std_logic_vector(3 downto 0):="0000"; 

begin 

    process (clk,clr) 
        begin 
            if clr = '1' then 
                dv <= "0000";
                ch <= "0000";
            elsif rising_edge(clk) then 
                if dv = "1001" then 
                    dv <= "0000" ; 
                    if ch = "1001" then 
                        ch <= "0000";
                    else 
                        ch <= ch + '1'; 
                    end if;
                else 
                    dv <= dv + '1' ; 
                end if ; 
            end if ;
        end process ;
    value_dv <= dv ; 
    value_ch <= ch ;
    led_dv <= bcd_to_7seg(dv) ; 
    led_ch <= bcd_to_7seg(ch) ; 
end Behavioral ; 
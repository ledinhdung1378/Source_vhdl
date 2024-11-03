library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all ; 
use ieee.std_logic_arith.all ; 

entity counter_gray_4bit is 
    port (
        clk : in std_logic ;
        clr : in std_logic ;
        pause : in std_logic ;
        count_gr : out std_logic_vector (3 downto 0) 
    );
end entity ;

architecture Behavioral of counter_gray_4bit is 

type state_type is (s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15);
signal current_state,next_state : state_type ; 

function encode_gray(s:state_type) return std_logic_vector is 
    begin 
        case s is 
            when S0  => return "0000";
            when S1  => return "0001";
            when S2  => return "0011";
            when S3  => return "0010";
            when S4  => return "0110";
            when S5  => return "0111";
            when S6  => return "0101";
            when S7  => return "0100";
            when S8  => return "1100";
            when S9  => return "1101";
            when S10 => return "1111";
            when S11 => return "1110";
            when S12 => return "1010";
            when S13 => return "1011";
            when S14 => return "1001";
            when S15 => return "1000";
            when others => return "0000"; -- Default case
        end case ;
    end function ;

begin 
    process(clr,clk)
        begin 
            if clr = '1' then 
                current_state <= s0;
            elsif rising_edge (clk) then 
                if pause = '0' then 
                    current_state <= next_state ;
                end if ; 
            end if; 
        end process;    
    
    process (current_state)
        begin 
            case current_state is 
                when S0  => next_state <= S1;
                when S1  => next_state <= S2;
                when S2  => next_state <= S3;
                when S3  => next_state <= S4;
                when S4  => next_state <= S5;
                when S5  => next_state <= S6;
                when S6  => next_state <= S7;
                when S7  => next_state <= S8;
                when S8  => next_state <= S9;
                when S9  => next_state <= S10;
                when S10 => next_state <= S11;
                when S11 => next_state <= S12;
                when S12 => next_state <= S13;
                when S13 => next_state <= S14;
                when S14 => next_state <= S15;
                when S15 => next_state <= S0;  -- Reset về S0
                when others => next_state <= S0; -- Default case
            end case ;
        end process ; 

    count_gr <= encode_gray (current_state) ; 

end Behavioral ; 



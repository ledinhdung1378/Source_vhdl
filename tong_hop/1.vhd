-- GRAY TO BIN -- 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity gray_to_bin is
    Port ( gray : in  STD_LOGIC_VECTOR (3 downto 0); -- Tín hiệu vào mã Gray 4-bit
           binary : out STD_LOGIC_VECTOR (3 downto 0) -- Tín hiệu ra mã Binary 4-bit
         );
end gray_to_bin;

architecture concurrent of gray_to_bin is
begin
    -- Sử dụng lệnh song song để chuyển mã Gray sang Binary
    binary(3) <= gray(3);                               -- Bit cao nhất giữ nguyên
    binary(2) <= gray(3) XOR gray(2);                  -- Bit thứ 2
    binary(1) <= gray(3) XOR gray(2) XOR gray(1);      -- Bit thứ 1
    binary(0) <= gray(3) XOR gray(2) XOR gray(1) XOR gray(0); -- Bit thấp nhất
end concurrent;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb_gray_to_bin is
end tb_gray_to_bin;

architecture behavior of tb_gray_to_bin is
    -- Component declaration
    component gray_to_bin
        Port ( gray : in  STD_LOGIC_VECTOR (3 downto 0);
               binary : out STD_LOGIC_VECTOR (3 downto 0));
    end component;

    -- Signals
    signal gray   : STD_LOGIC_VECTOR (3 downto 0);
    signal binary : STD_LOGIC_VECTOR (3 downto 0);
begin
    -- Instantiate the Unit Under Test (UUT)
    uut: gray_to_bin PORT MAP (
          gray => gray,
          binary => binary
        );

    -- Stimulus process
    process
    begin
        -- Test các giá trị mã Gray
        gray <= "0000"; wait for 10 ns;
        gray <= "0001"; wait for 10 ns;
        gray <= "0011"; wait for 10 ns;
        gray <= "0010"; wait for 10 ns;
        gray <= "0110"; wait for 10 ns;
        gray <= "0111"; wait for 10 ns;
        gray <= "0101"; wait for 10 ns;
        gray <= "0100"; wait for 10 ns;
        gray <= "1100"; wait for 10 ns;
        gray <= "1101"; wait for 10 ns;
        gray <= "1111"; wait for 10 ns;
        gray <= "1110"; wait for 10 ns;
        wait;
    end process;
end behavior;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity gray_to_bin is
    Port ( gray : in  STD_LOGIC_VECTOR (3 downto 0);  -- Tín hiệu vào mã Gray 4-bit
           binary : out STD_LOGIC_VECTOR (3 downto 0) -- Tín hiệu ra mã Binary 4-bit
         );
end gray_to_bin;

architecture sequential of gray_to_bin is
begin
    process(gray)
        variable temp_binary : STD_LOGIC_VECTOR (3 downto 0); -- Biến tạm lưu kết quả
    begin
        -- Bit cao nhất của binary giữ nguyên
        temp_binary(3) := gray(3);
        
        -- Tính các bit còn lại bằng cách XOR tuần tự
        temp_binary(2) := temp_binary(3) XOR gray(2);
        temp_binary(1) := temp_binary(2) XOR gray(1);
        temp_binary(0) := temp_binary(1) XOR gray(0);

        -- Gán kết quả cho output
        binary <= temp_binary;
    end process;
end sequential;

--dem nhi phan 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity binary_counter is
    Port ( CLK  : in  STD_LOGIC;                  -- Tín hiệu xung nhịp
           CLR  : in  STD_LOGIC;                  -- Tín hiệu xóa
           Q    : out STD_LOGIC_VECTOR (3 downto 0) -- Đầu ra đếm 4-bit
         );
end binary_counter;

architecture behavioral of binary_counter is
    signal count : STD_LOGIC_VECTOR (3 downto 0) := "0000"; -- Biến lưu giá trị đếm
begin
    process(CLK, CLR)
    begin
        if CLR = '1' then
            count <= "0000"; -- Đặt lại bộ đếm khi tín hiệu CLR kích hoạt
        elsif rising_edge(CLK) then
            count <= count + 1; -- Tăng giá trị đếm ở cạnh lên của CLK
        end if;
    end process;

    Q <= count; -- Gán giá trị đếm cho đầu ra
end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb_binary_counter is
end tb_binary_counter;

architecture behavior of tb_binary_counter is
    -- Component declaration
    component binary_counter
        Port ( CLK  : in  STD_LOGIC;
               CLR  : in  STD_LOGIC;
               Q    : out STD_LOGIC_VECTOR (3 downto 0));
    end component;

    -- Signals
    signal CLK  : STD_LOGIC := '0';
    signal CLR  : STD_LOGIC := '0';
    signal Q    : STD_LOGIC_VECTOR (3 downto 0);

    constant CLK_PERIOD : time := 10 ns; -- Chu kỳ xung nhịp
begin
    -- Instantiate the Unit Under Test (UUT)
    uut: binary_counter PORT MAP (
          CLK => CLK,
          CLR => CLR,
          Q   => Q
        );

    -- Clock process
    clk_process : process
    begin
        CLK <= '0';
        wait for CLK_PERIOD / 2;
        CLK <= '1';
        wait for CLK_PERIOD / 2;
    end process;

    -- Stimulus process
    stim_proc: process
    begin
        -- Reset counter
        CLR <= '1';
        wait for CLK_PERIOD;
        CLR <= '0';

        -- Let the counter count for some cycles
        wait for 100 ns;

        -- Trigger reset
        CLR <= '1';
        wait for CLK_PERIOD;
        CLR <= '0';

        -- Continue counting
        wait for 100 ns;

        wait;
    end process;
end behavior;


-- bo dem tien 2 digit

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity decimal_counter is
    Port (
        CLK : in STD_LOGIC;                    -- Tín hiệu xung nhịp
        CLR : in STD_LOGIC;                    -- Tín hiệu xóa
        DIGIT1 : out STD_LOGIC_VECTOR (3 downto 0); -- Chữ số hàng đơn vị
        DIGIT2 : out STD_LOGIC_VECTOR (3 downto 0)  -- Chữ số hàng chục
    );
end decimal_counter;

architecture behavioral of decimal_counter is
    signal count_unit : STD_LOGIC_VECTOR (3 downto 0) := "0000"; -- Hàng đơn vị
    signal count_tens : STD_LOGIC_VECTOR (3 downto 0) := "0000"; -- Hàng chục
begin
    process(CLK, CLR)
    begin
        if CLR = '1' then
            count_unit <= "0000"; -- Reset hàng đơn vị
            count_tens <= "0000"; -- Reset hàng chục
        elsif rising_edge(CLK) then
            -- Tăng hàng đơn vị
            if count_unit = "1001" then
                count_unit <= "0000"; -- Reset hàng đơn vị khi đạt 9
                -- Tăng hàng chục
                if count_tens = "1001" then
                    count_tens <= "0000"; -- Reset hàng chục khi đạt 9
                else
                    count_tens <= count_tens + 1; -- Tăng hàng chục
                end if;
            else
                count_unit <= count_unit + 1; -- Tăng hàng đơn vị
            end if;
        end if;
    end process;

    DIGIT1 <= count_unit; -- Gán hàng đơn vị cho đầu ra
    DIGIT2 <= count_tens; -- Gán hàng chục cho đầu ra
end behavioral;


-- dem lui mode 60 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity decimal_counter_2digit_down is
    Port (
        CLK         : in  STD_LOGIC;                       -- Clock
        CLR         : in  STD_LOGIC;                       -- Reset đồng bộ
        LOAD        : in  STD_LOGIC;                       -- Nạp giá trị đồng bộ
        LOAD_VAL    : in  STD_LOGIC_VECTOR (7 downto 0);   -- Giá trị nạp (2 digit)
        CE          : in  STD_LOGIC;                       -- Tín hiệu cho phép
        DIGIT1      : out STD_LOGIC_VECTOR (6 downto 0);   -- LED 7 đoạn (hàng đơn vị)
        DIGIT2      : out STD_LOGIC_VECTOR (6 downto 0)    -- LED 7 đoạn (hàng chục)
    );
end decimal_counter_2digit_down;

architecture Behavioral of decimal_counter_2digit_down is
    signal count_unit : STD_LOGIC_VECTOR(3 downto 0) := "0000"; -- Hàng đơn vị
    signal count_tens : STD_LOGIC_VECTOR(3 downto 0) := "0000"; -- Hàng chục
begin
    process(CLK)
    begin
        if rising_edge(CLK) then
            if CLR = '1' then
                count_unit <= "0000"; -- Reset hàng đơn vị
                count_tens <= "0000"; -- Reset hàng chục
            elsif LOAD = '1' then
                count_unit <= LOAD_VAL(3 downto 0);   -- Nạp hàng đơn vị
                count_tens <= LOAD_VAL(7 downto 4);   -- Nạp hàng chục
            elsif CE = '1' then
                if count_unit = "0000" then
                    count_unit <= "1001"; -- Quay về 9
                    if count_tens = "0000" then
                        count_tens <= "0101"; -- Quay về 5 (mode 60)
                    else
                        count_tens <= count_tens - 1; -- Giảm hàng chục
                    end if;
                else
                    count_unit <= count_unit - 1; -- Giảm hàng đơn vị
                end if;
            end if;
        end if;
    end process;

    -- Hiển thị LED 7 đoạn
    DIGIT1 <= decode_7segment(count_unit);
    DIGIT2 <= decode_7segment(count_tens);

end Behavioral;

-- Hàm giải mã LED 7 đoạn
function decode_7segment(input : STD_LOGIC_VECTOR(3 downto 0)) return STD_LOGIC_VECTOR is
    variable segments : STD_LOGIC_VECTOR(6 downto 0);
begin
    case input is
        when "0000" => segments := "1000000"; -- 0
        when "0001" => segments := "1111001"; -- 1
        when "0010" => segments := "0100100"; -- 2
        when "0011" => segments := "0110000"; -- 3
        when "0100" => segments := "0011001"; -- 4
        when "0101" => segments := "0010010"; -- 5
        when "0110" => segments := "0000010"; -- 6
        when "0111" => segments := "1111000"; -- 7
        when "1000" => segments := "0000000"; -- 8
        when "1001" => segments := "0010000"; -- 9
        when others => segments := "1111111"; -- Blank
    end case;
    return segments;
end decode_7segment;


-- triger jk 

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity jk_flip_flop is
    Port (
        CLK : in STD_LOGIC;   -- Clock signal
        CLR : in STD_LOGIC;   -- Synchronous clear
        J   : in STD_LOGIC;   -- Input J
        K   : in STD_LOGIC;   -- Input K
        Q   : out STD_LOGIC;  -- Output Q
        QD  : out STD_LOGIC   -- Output Q complement
    );
end jk_flip_flop;

architecture Behavioral of jk_flip_flop is
    signal q_int : STD_LOGIC := '0'; -- Internal signal to hold state
begin

    process (CLK)
    begin
        if rising_edge(CLK) then
            if CLR = '1' then
                q_int <= '0'; -- Reset state to 0
            else
                if (J = '0' and K = '0') then
                    q_int <= q_int; -- Hold the current state
                elsif (J = '1' and K = '0') then
                    q_int <= '1'; -- Set state to 1
                elsif (J = '0' and K = '1') then
                    q_int <= '0'; -- Reset state to 0
                elsif (J = '1' and K = '1') then
                    q_int <= not q_int; -- Toggle the state
                end if;
            end if;
        end if;
    end process;

    -- Output assignments
    Q <= q_int;
    QD <= not q_int;

end Behavioral;


--- fms

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity sequence_detector_mealy_overlap is
    Port (
        CLK : in STD_LOGIC;        -- Clock signal
        RST : in STD_LOGIC;        -- Synchronous reset
        IN  : in STD_LOGIC;        -- Input bit stream
        Z   : out STD_LOGIC        -- Output: 1 when "1101" is detected
    );
end sequence_detector_mealy_overlap;

architecture Behavioral of sequence_detector_mealy_overlap is
    type state_type is (S0, S1, S2, S3); -- FSM states
    signal current_state, next_state: state_type;
begin

    -- State transition process
    process (CLK)
    begin
        if rising_edge(CLK) then
            if RST = '1' then
                current_state <= S0; -- Reset to initial state
            else
                current_state <= next_state; -- Move to next state
            end if;
        end if;
    end process;

    -- Next state and output logic
    process (current_state, IN)
    begin
        -- Default values
        next_state <= S0;
        Z <= '0';

        case current_state is
            when S0 =>
                if IN = '1' then
                    next_state <= S1;
                else
                    next_state <= S0;
                end if;

            when S1 =>
                if IN = '1' then
                    next_state <= S2;
                else
                    next_state <= S0;
                end if;

            when S2 =>
                if IN = '0' then
                    next_state <= S3;
                else
                    next_state <= S2;
                end if;

            when S3 =>
                if IN = '1' then
                    next_state <= S1;
                    Z <= '1'; -- Output 1 when "1101" is detected
                else
                    next_state <= S0;
                end if;

            when others =>
                next_state <= S0;
        end case;
    end process;

end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity sequence_detector is
    Port (
        CLK : in STD_LOGIC;         -- Clock signal
        RST : in STD_LOGIC;         -- Synchronous reset
        IN  : in STD_LOGIC;         -- Input bit stream
        Z   : out STD_LOGIC         -- Output: 1 when "1101" is detected
    );
end sequence_detector;

architecture Behavioral of sequence_detector is
    type state_type is (S0, S1, S2, S3, S4); -- FSM states
    signal current_state, next_state: state_type;
begin

    -- State transition process
    process (CLK)
    begin
        if rising_edge(CLK) then
            if RST = '1' then
                current_state <= S0; -- Reset to initial state
            else
                current_state <= next_state; -- Move to next state
            end if;
        end if;
    end process;

    -- Next state logic
    process (current_state, IN)
    begin
        case current_state is
            when S0 =>
                if IN = '1' then
                    next_state <= S1;
                else
                    next_state <= S0;
                end if;
            
            when S1 =>
                if IN = '1' then
                    next_state <= S2;
                else
                    next_state <= S0;
                end if;

            when S2 =>
                if IN = '0' then
                    next_state <= S3;
                else
                    next_state <= S1;
                end if;

            when S3 =>
                if IN = '1' then
                    next_state <= S4;
                else
                    next_state <= S0;
                end if;

            when S4 =>
                next_state <= S0; -- Return to initial state

            when others =>
                next_state <= S0;
        end case;
    end process;

    -- Output logic
    process (current_state)
    begin
        case current_state is
            when S4 =>
                Z <= '1'; -- Sequence "1101" detected
            when others =>
                Z <= '0'; -- Default output
        end case;
    end process;

end Behavioral;






library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Spi_comp2 is
    Port ( CLK 		: in  STD_LOGIC; --System CLK (100MHz)
		   RST 		: in  STD_LOGIC; --Global RST (Synchronous)
		   SPI_EN 	: in  STD_LOGIC; --SPI block enable pin
		   SPI_DATA : in  STD_LOGIC_VECTOR (7 downto 0); --Byte to be sent
		   CS		: out STD_LOGIC; --Chip Select
           SDO 		: out STD_LOGIC; --SPI data out
           SCLK 	: out STD_LOGIC; --SPI clock
		   SPI_FIN	: out STD_LOGIC);--SPI finish flag
end Spi_comp2;

architecture Behavioral of Spi_comp2 is
    
    type states is (Idle,
				Send,
				Done);
	signal curr_state : states := Idle ;
    signal sclk_temp : STD_LOGIC := '1' ;
    signal counter : integer := 0 ;
    signal send_counter : integer := 0 ;
    signal temp_sdo            : STD_LOGIC := '1';
    signal temp_data : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000" ;
    signal falling : STD_LOGIC := '1' ;
begin
    SDO <= temp_sdo;
    SCLK <= sclk_temp ;
    CS <= '1' when (curr_state = Idle and SPI_EN = '0') else
            '0';
    SPI_FIN <= '1' when (curr_state = Done) else
                '0';
    process (CLK)
        begin
            if(rising_edge(CLK)) then
                if(RST = '1') then --Synchronous RST
                    curr_state <= Idle;
                else
                    case (curr_state) is
                        when Idle => --Wait for SPI_EN to go high
                            if(SPI_EN = '1') then
                                curr_state <= Send;
                            end if;
                            counter <= 0 ;
                            send_counter <= 0 ;
                            temp_data <= SPI_DATA ;
                            temp_sdo <= '1' ;
                        when Send => --Start sending bits, transition out when all bits are sent and SCLK is high
                            if(send_counter = 8 and falling = '1') then
                                curr_state <= Done;
                            end if ;
                            if(counter = 0) then
                                falling <= '1' ;
                            elsif(counter = 15) then
                                sclk_temp <= '0' ;
                            elsif(counter = 16  and falling = '1') then
                                    temp_sdo <= temp_data(7 - send_counter) ;
                                    falling <= '0' ;
                                    send_counter <= send_counter + 1 ;
                            elsif(counter = 31) then
                                sclk_temp <= '1' ;
                            end if ;
                            
                            if(counter = 31) then
                                counter <= 0 ;
                            else
                                counter <= counter+ 1 ;
                            end if ;
--                        when Hold1 => --Hold CS low for a bit
--                            current_state <= Hold2;
--                        when Hold2 => --Hold CS low for a bit
--                            current_state <= Hold3;
--                        when Hold3 => --Hold CS low for a bit
--                            current_state <= Hold4;
--                        when Hold4 => --Hold CS low for a bit
--                            current_state <= Done;
                        when Done => --Finish SPI transimission wait for SPI_EN to go low
                            if(SPI_EN = '0') then
                                curr_state <= Idle;
                            end if;
                        when others =>
                            curr_state <= Idle;
                    end case;
                end if;
            end if;
        end process;

end Behavioral;

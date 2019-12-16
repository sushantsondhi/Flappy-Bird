----------------------------------------------
library IEEE; 
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.ALL;
use ieee.std_logic_arith.all;

entity PmodOLEDCtrl is
	Port ( 
		CLK 	: in  STD_LOGIC;
		RST 	: in  STD_LOGIC;
		JUMP : in STD_LOGIC ;
		PAUSE : in STD_LOGIC ;
		PLAY : in STD_LOGIC ;
		CS  	: out STD_LOGIC;
		SDIN	: out STD_LOGIC;
		SCLK	: out STD_LOGIC;
		DC		: out STD_LOGIC;
		RES	: out STD_LOGIC;
		VBAT	: out STD_LOGIC;
		VDD	: out STD_LOGIC ;
		DONE3 : out STD_LOGIC ;
		DONE4 : out STD_LOGIC ;
        co : out STD_LOGIC_VECTOR(6 DOWNTO 0)  ;
        ao : out STD_LOGIC_VECTOR(3 DOWNTO 0) );

end PmodOLEDCtrl;

architecture Behavioral of PmodOLEDCtrl is

component OledInit is
Port ( CLK 	: in  STD_LOGIC;
		RST 	: in	STD_LOGIC;
		EN		: in  STD_LOGIC;
		CS  	: out STD_LOGIC;
		SDO	: out STD_LOGIC;
		SCLK	: out STD_LOGIC;
		DC		: out STD_LOGIC;
		RES	: out STD_LOGIC;
		VBAT	: out STD_LOGIC;
		VDD	: out STD_LOGIC;
		FIN  : out STD_LOGIC);
end component;



component bird is
    Port ( CLK 	: in  STD_LOGIC; --System CLK
            JUMP : in STD_LOGIC ;
            PAUSE : in STD_LOGIC ;
                    PLAY : in STD_LOGIC ;
			  RST 	: in	STD_LOGIC; --Synchronous Reset
			  EN		: in  STD_LOGIC; --Example block enable pin
			  CS  	: out STD_LOGIC; --SPI Chip Select
			  SDO		: out STD_LOGIC; --SPI Data out
			  SCLK	: out STD_LOGIC; --SPI Clock
			  DC		: out STD_LOGIC; --Data/Command Controller
			  DONE  	: out STD_LOGIC;--Finish flag for example block
--			  NUM : out STD_LOGIC );
                           c : out STD_LOGIC_VECTOR(6 DOWNTO 0)  ;
            a : out STD_LOGIC_VECTOR(3 DOWNTO 0) ); 
end component ;

type states is (Idle,
					OledInitialize,
					OledExample,BirdTest,Wait1,Wait2,
					Done,checkRestart);

signal current_state 	: states := Idle;

signal init_en				: STD_LOGIC := '0';
signal init_done			: STD_LOGIC := '0' ;
signal init_cs				: STD_LOGIC;
signal init_sdo			: STD_LOGIC;
signal init_sclk			: STD_LOGIC;
signal init_dc				: STD_LOGIC;

signal example_en			: STD_LOGIC := '0';
signal example_cs			: STD_LOGIC;
signal example_sdo		: STD_LOGIC;
signal example_sclk		: STD_LOGIC;
signal example_dc			: STD_LOGIC;
signal example_done		: STD_LOGIC := '0';
signal bird_cs              : STD_LOGIC ;
signal bird_sclk		: STD_LOGIC;
signal bird_sdo		: STD_LOGIC;
signal bird_dc			: STD_LOGIC;
signal bird_done		: STD_LOGIC := '0' ;
signal bird_en : STD_LOGIC := '0' ;
signal count : integer := 0 ;
--signal num2 : STD_LOGIC ;
begin

	Init: OledInit port map(CLK, RST, init_en, init_cs, init_sdo, init_sclk, init_dc, RES, VBAT, VDD, init_done);
	OledBird : bird Port map(CLK,JUMP,PAUSE,PLAY,RST,bird_en,bird_cs,bird_sdo,bird_sclk,bird_dc,bird_done, co, ao) ;
	--MUXes to indicate which outputs are routed out depending on which block is enabled
	CS <= init_cs when(current_state = OledInitialize) else
			bird_cs;
			
	SDIN <= init_sdo when(current_state = OledInitialize) else  
                bird_sdo;
	SCLK <= init_sclk when(current_state = OledInitialize) else  
                bird_sclk;
	DC <= init_dc when(current_state = OledInitialize) else 
          bird_dc;
	--END output MUXes
	
	--MUXes that enable blocks when in the proper states
	init_en <= '1' when (current_state = OledInitialize) else
					'0';
	example_en <= '1' when (current_state = OledExample) else
					'0';
	bird_en <= '1' when (current_state = BirdTest) else '0' ; 
	--END enable MUXes
--	DONE1 <= init_done ;
	DONE3 <= bird_done ;
--    NUM <= num2 ;
    DONE4 <= '1' when (current_state = Wait1) else '0' ;
	process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(RST = '1') then
				current_state <= Idle;
			else
				case(current_state) is
					when Idle =>
						current_state <= OledInitialize;
					--Go through the initialization sequence
					when OledInitialize =>
						if(init_done = '1') then
							current_state <= Wait1;
						end if;
					-- Waiting one second before starting the game
					when Wait1 =>
					   if(JUMP = '1') then
					       current_state <= Wait2 ; 
					   end if ; 
					when Wait2 =>
					   count <= count + 1 ;
					   if(count = 100000000) then
					       current_state <= BirdTest ;
					       count <= 0 ;
					   end if ;    
					when BirdTest =>
					    if(bird_done = '1') then
					       current_state <= checkRestart ;
					    end if ;
					when checkRestart =>
					       if(RST = '1') then
					           current_state <= Wait2 ;
					       end if ;
					when Done =>
						current_state <= Done;
					when others =>
						current_state <= Idle;
				end case;
			end if;
		end if;
	end process;
	
	
end Behavioral;


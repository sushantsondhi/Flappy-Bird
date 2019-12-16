

--library IEEE;
--use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
--use IEEE.STD_LOGIC_UNSIGNED.ALL;


---- Uncomment the following library declaration if using
---- arithmetic functions with Signed or Unsigned values
----use IEEE.NUMERIC_STD.ALL;

---- Uncomment the following library declaration if instantiating
---- any Xilinx leaf cells in this code.
----library UNISIM;
----use UNISIM.VComponents.all;

--entity configStart is
--    Port( CLK : in STD_LOGIC;
--          EN : in STD_LOGIC ;
--          page: in STD_LOGIC_VECTOR(1 DOWNTO 0) ;
--          col : in STD_LOGIC_VECTOR(7 DOWNTO 0) ;
--          DC : out STD_LOGIC ;
--          FIN : out STD_LOGIC ;
--          CS : OUT  std_logic;
--          SDO : OUT  std_logic;
--          SCLK : OUT  std_logic);
          
          
--end configStart ;

--architecture Behavioral of configStart is
    
--    COMPONENT SpiCtrl
--    PORT(
--         CLK : IN  std_logic;
--         RST : IN  std_logic;
--         SPI_EN : IN  std_logic;
--         SPI_DATA : IN  std_logic_vector(7 downto 0);
--         CS : OUT  std_logic;
--         SDO : OUT  std_logic;
--         SCLK : OUT  std_logic;
--         SPI_FIN : OUT  std_logic
--        );
--    END COMPONENT;
    
--    signal stage : integer := 0 ;
--    signal temp_rst : STD_LOGIC := '0' ;
--    signal spi_data : STD_LOGIC_VECTOR(7 DOWNTO 0);
--    signal spi_en : STD_LOGIC := '0' ;
--    signal temp_cs : STD_LOGIC := '0' ;
--    signal temp_sdo : STD_LOGIC := '0' ;
--    signal temp_sclk : STD_LOGIC := '0' ;
--    signal configFin : STD_LOGIC := '0' ;
--    signal spi_fin : STD_LOGIC := '0' ; 
--    begin    
--        SPI_COMP: SpiCtrl PORT MAP (
--                  CLK => CLK,
--                  RST => temp_rst,
--                  SPI_EN => spi_en,
--                  SPI_DATA => spi_data,
--                  CS => CS,
--                  SDO => SDO,
--                  SCLK => SCLK,
--                  SPI_FIN => spi_fin
--                );
--        FIN <= configFin ;     
--         process(CLK)
--            begin
--                if(CLK = '1' and CLK'EVENT) then
--                    if(EN = '1') then
--                        CASE(stage) is
                            
--                            when others =>
--                                stage <= 0 ;
--                        end case ;
--                    end if ;
--                end if ;
--             end process ;
             
-- end Behavioral ;
             

        
        library IEEE;
        use IEEE.STD_LOGIC_1164.ALL;
        USE ieee.numeric_std.all;
--        USE ieee.std_logic_unsigned.all;
        
        
        ENTITY red_freq IS
            PORT( clk_in: IN STD_LOGIC ;
                  clk_out: OUT STD_LOGIC ;
                  limit: IN integer  
            );
        end red_freq;
        
        ARCHITECTURE red_freq_arch OF red_freq IS
        SIGNAL count: integer := 0 ;
        SIGNAL temp: STD_LOGIC:= '0' ;
        BEGIN
            PROCESS(clk_in) BEGIN
                IF (clk_in = '1' AND clk_in'EVENT) THEN
                    count <= count + 1;
                    IF(count = limit/2) THEN
                        temp <= NOT temp ;
                        count <= 0 ;
                    END IF ;
                END IF ;
                clk_out <= temp ; 
             END PROCESS ;
        END red_freq_arch;
        

        
        library IEEE;
                use IEEE.STD_LOGIC_1164.ALL;
                USE ieee.numeric_std.all;
--                USE ieee.std_logic_unsigned.all;

                
                entity Debounce is
                            Port ( 
                                clk: IN STD_LOGIC ;
                                reseto : IN STD_LOGIC;
                                pauseo : IN STD_LOGIC ;
                                playo : IN STD_LOGIC ;
                                pause : OUT STD_LOGIC ;
                                play : OUT STD_LOGIC ;
                                reset: OUT STD_LOGIC 
                                
                                
                            );
                end Debounce;
                
                Architecture arch_d of Debounce is
                    Begin
                        Process (clk)
                            BEGIN
                                if(clk = '1' AND clk'EVENT) THEN
                                    pause <= pauseo ;
                                    play <= playo ;
                                    reset <= reseto;
                                END IF;
                        END PROCESS;
                end arch_d;                                    
                
 library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.all;
USE ieee.std_logic_unsigned.all;

ENTITY seven_seg IS
    PORT( output :OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
         input: IN STD_LOGIC_VECTOR(3 DOWNTO 0);
         clk : IN STD_LOGIC := '0'   );
END seven_seg;

ARCHITECTURE seven_seg_arch OF seven_seg IS
signal output2 :STD_LOGIC_VECTOR(6 DOWNTO 0) := "0000000";
BEGIN 
    Process(clk)
    begin
--   if(clk='1' and clk'event) then
--    if (en1 ='1') then
    case input is
     WHEN "0000" => output2 <= "0000001";
     WHEN "0001" => output2 <= "1111001";
     WHEN "0010" => output2 <= "0010010";
     WHEN "0011" => output2 <= "0110000";
     WHEN "0100" => output2 <= "1101000";
     WHEN "0101" => output2 <= "0100100";
     WHEN "0110" => output2 <= "0000100"; 
     WHEN "0111" => output2 <= "1110001";
     WHEN "1000" => output2 <= "0000000";
     WHEN "1001" => output2 <= "0100000";
     WHEN "1010" => output2 <= "0001000";
     WHEN "1011" => output2 <= "1100000";
     WHEN "1100" => output2 <= "0110001";
     WHEN "1101" => output2 <= "1000010";
     WHEN "1110" => output2 <= "0110000";
     WHEN "1111" => output2 <= "0111000";      

         end case;

--    WITH input SELECT
--    output <= "0000001" WHEN "0000",
--              "1001111" WHEN "0001",
--              "0010010" WHEN "0010",
--              "0000110" WHEN "0011",
--              "1001100" WHEN "0100",
--              "0100100" WHEN "0101",
--              "0100000" WHEN "0110",
--              "0001111" WHEN "0111",
--               "0000000" WHEN "1000",
--               "0000100" WHEN "1001",
--               "0001000" WHEN "1010",
--               "1100000" WHEN "1011",
--               "0110001" WHEN "1100",
--               "1000010" WHEN "1101",
--               "0110000" WHEN "1110",
--               "0111000" WHEN OTHERS ;
              
--     end if;
--     end if;
     end process; 
     output <= output2;        
END seven_seg_arch;       

library IEEE;

use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
USE ieee.numeric_std.all;


entity bird is
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
             a : out STD_LOGIC_VECTOR(3 DOWNTO 0)  );
end bird;



architecture Behavioral of bird is
    
    COMPONENT Spi_comp2
    PORT(
         CLK : IN  std_logic;
         RST : IN  std_logic;
         SPI_EN : IN  std_logic;
         SPI_DATA : IN  std_logic_vector(7 downto 0);
         CS : OUT  std_logic;
         SDO : OUT  std_logic;
         SCLK : OUT  std_logic;
         SPI_FIN : OUT  std_logic
        );
    END COMPONENT;

--Delay Controller Component
    
    signal birdX : integer := 48 ;
    signal birdY : integer := 7  ;
    signal i,j : integer ;
    signal page : STD_LOGIC_VECTOR(1 DOWNTO 0) ;
    signal col : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";
--    signal config_en : STD_LOGIC := '0' ;
--    signal config_fin : STD_LOGIC := '0' ;
    signal spi_data : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000" ;
    signal spi_en : STD_LOGIC := '0' ;
    signal spi_fin : STD_LOGIC := '0' ;
    signal temp_done : STD_LOGIC := '0' ;
    type Matrix is array(0 to 3, 0 to 127) of STD_LOGIC_VECTOR(7 DOWNTO 0)                               ;
    type states is (Idle,game_over,paused,pauseTransition, make_empty,make_line,UpdateBird,UpdateMatrix,sendMatrix,checkNeg,checkNeg2,Config1,Config2,Config3,Config4,Config5,Config6,Config7,Config8,Config9,Config10,WaitForSend,Temp,WaitJump, check_collision) ;
    
    
    
    type jump_states is(Idle,NoJump,Jumped,Wait1) ;
    signal jump_state : jump_states := Idle ;
    
    signal switch_state : states := Idle ;
    signal state : states := Idle ;
    signal Display : Matrix ;
--    signal num2 : STD_LOGIC := '0' ;    
    SIGNAL limit: integer := 10000000  ;
    SIGNAL dx_clock : STD_LOGIC :='0' ;
    SIGNAL jump2 : STD_LOGIC := '0';
    signal deb_pause,deb_play : STD_LOGIC := '0' ;
   SIGNAL dx : STD_LOGIC := '0';
  Signal disp_clk : STD_LOGIC := '0';
  signal limit4 : integer := 100000;
  
  SIGNAL limit2 : integer := 1000;
    SIGNAL temp_dc : STD_LOGIC := '1' ; 
    constant jump_val : integer := 70 ;
    Signal v : integer := jump_val ; -- Velocity of the bird ;
    Signal acc : integer := 2 ;
    Signal real_y : integer := 16000 ; 
    signal jump_temp :STD_LOGIC := '0';
    signal jump_temp2: STD_LOGIC := '0';
    
    signal num_pillars : integer := 2 ;
    type int_array is array(0 to 1) of integer ;
    signal pillars : int_array := (127,199) ;
      signal pillars_height : int_array := (11,13) ;
   
    type all_heights is array(0 to 4) of integer ;
    signal heights_set : all_heights := (5, 10, 12, 16, 20) ;
    signal height_temp : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0001" ;                                                                                     
    signal height_count: integer := 0;
    
    signal limit3 : integer := 5000000 ;
    SIGNAL dx_pillar : STD_LOGIC := '0' ;                                       
    SIGNAL pillarHeight : integer := 11 ;
    Signal pillarWidth : integer := 5 ;
    Signal pillarGap : integer := 12 ;
    Signal main_clk : STD_LOGIC := '0' ;
    Signal main_lim : integer := 4 ;
    Signal make_fig_en : STD_LOGIC := '0';
    Signal make_x : integer := 0;
    signal make_y : integer := 0;
    signal length : integer := 0;
    signal type_l : integer := 0;
    signal temp_i : integer := 0;
    signal temp_j : integer := 0;
    signal done_make : STD_LOGIC := '1';
    signal seg_count : integer := 0;
    signal complete : STD_LoGIC := '0';
    signal total_score : STD_LOGIC_VECTOR(3 downto 0) := "0000";
    signal scored : STD_LOGIC :='0';
    signal toggle : STD_LOGIC := '0';
    signal total_score_1 : STD_LOGIC_VECTOR(3 downto 0) := "0000";
    signal anode : STD_LOGIC_VECTOR(3 downto 0) := "1110";
    signal cathode : STD_LOGIC_VECTOR(3 downto 0) := "0000";
    signal wait_count : integer := 0 ;
    
    
    begin
--    config: ENTITY WORK.configStart(Behavioral)
--                port map(CLK,config_en,page,col,DC,config_fin);
    DONE <= temp_done ;
--    NUM <= num2 ;
    DC <= temp_dc ;
--    birdY <= real_y/1000 ;
    SPI_COMP: Spi_comp2 PORT MAP (
                          CLK => CLK,
                          RST => RST,
                          SPI_EN => spi_en,
                          SPI_DATA => spi_data,
                          CS => CS,
                          SDO => SDO,
                          SCLK => SCLK,
                          SPI_FIN => spi_fin
                        );
                        
                     Get_seven_seg_0: ENTITY WORK.seven_seg(seven_seg_arch)
                        PORT MAp(c, cathode,disp_clk);
                        a<= anode;
--                         Get_seven_seg_1: ENTITY WORK.seven_seg(seven_seg_arch)
--                                             PORT MAp(c, total_score_1, en_seg, dx_clock);
                     Get_main_freq: ENTITY WORK.red_freq(red_freq_arch)
                                                   PORT MAP(CLK,main_clk,main_lim) ;    
                    Get_red_20_Hz: ENTITY WORK.red_freq(red_freq_arch)
                            PORT MAP(CLK,dx_clock,limit) ;
                   Get_red_01_Hz: ENTITY WORK.red_freq(red_freq_arch)
                                                      PORT MAP(CLK,dx,limit2) ;
                       Get_disp_clk: ENTITY WORK.red_freq(red_freq_arch)
                                     PORT MAP(CLK,disp_clk,limit4) ;
                                        Debounce :  ENTITY WORK.Debounce(arch_d)
                                PORT MAP(dx, JUMP,PAUSE,PLAY, deb_pause,deb_play,jump2);
                   Get_red_25_Hz: ENTITY WORK.red_freq(red_freq_arch)
                           PORT MAP(CLK,dx_pillar,limit3) ;
                                
        process(dx_clock)
            begin
                if(RST = '1') then
                      birdY <= 7 ;
                      jump_temp2 <= '0' ;
                elsif(dx_clock = '1' and dx_clock'EVENT) then
                      if(EN = '1' and state /= paused) then
--                    case(jump_state) is
--                        when Idle =>
--                            v <= v + acc ;
--                            if(jump2 = '1') then
--                                jump_state <= Jumped ;
                                
--                            else
--                                jump_state <= NoJump ;
--                            end if ;
--                        when NoJump =>
--                            v <= v + acc ;
--                            jump_state <= Wait1 ;
--                        when Jumped =>
--                            v <= jump_val ;
--                            jump_state <= Wait1 ;
--                        when Wait1 =>
--                            v <= v + acc ;
--                            if(jump2 = '0') then
--                                jump_state <= Idle ;
--                            end if ;
--                        end case ;
                        
--                        birdY <= birdY + v ;
                            if (jump_temp= '1') then 
                            jump_temp2<= '1';
                            birdY <= birdY - 3 ; 
                            else 
                            jump_temp2 <= '0'; 
                            birdY <= birdY + 1 ;
                            end if;
                        end if ;
                           
                 end if ;
                    
            end process ;
        
        process(dx_pillar)
            begin
                if(RST = '1') then
                       pillars(0) <= 127 ;
                       pillars(1) <= 191 ;
                elsif(dx_pillar = '1' and dx_pillar'EVENT) then
                      if(EN = '1'and state /= paused) then
                            
                            if(pillars(0)= 0) then
                                if(height_count <5) then
--                                     pillars_height(0) <= heights_set(height_count);
                                     pillars_height(0) <= to_integer(unsigned(height_temp));
                                     
--                                     height_count <= height_count +1;
                                else
--                                     pillars_height(0) <= heights_set(0);
                                     pillars_height(0) <= to_integer(unsigned(height_temp));
--                                     height_count <= 1;                             
                                end if;
                                height_temp(2 DOWNTO 0) <= height_temp(3 DOWNTO 1) ;
                                height_temp(3) <= (height_temp(3) AND not height_temp(0)) OR (height_temp(0) AND not height_temp(3)) ;
                                pillars(0) <= 127 ;
                            else
                                pillars(0) <= pillars(0) - 1 ;
                            end if ;
                                
                            if(pillars(1) = 0) then
                                if(height_count <5) then
                                     pillars_height(1) <= heights_set(height_count);
                                     height_count <= height_count +1;
                                else
                                     pillars_height(1) <= heights_set(0);
                                     height_count <= 1;                             
                                end if;
                                    pillars(1) <= 127 ;
                            else
                                pillars(1) <= pillars(1) - 1 ;
                            end if ;
                        end if ;
                 end if ;
                    
            end process ;
         
         process(disp_clk,RST)
            begin
            if(RST = '1') then
                  anode <= "1110" ;
                  cathode <= "0000" ; 
                  toggle <= '0';
            elsif(disp_clk = '1' and disp_clk'EVENT) then
            
                if (toggle ='0') then
                    anode<= "1110";
                    cathode <= total_score;
                    toggle <='1';
                elsif (toggle ='1') then
                     anode<= "1101";
                               cathode <= total_score_1;
                               toggle <= '0';
            end if;
            end if;
            end process;   
   

        process(main_clk)
            begin 
                if(RST = '1') then
                    total_score <= "0000" ;
                    total_score_1 <= "0000" ; 
                    limit3 <= 5000000 ;
                    temp_done <= '0' ;
                    complete <= '0' ;
                    scored <= '0' ;
                    wait_count <= 0 ;
                elsif(main_clk = '1' and main_clk'EVENT) then
                    
                    if(jump2 = '1' AND jump_temp2 = '0') then
                        jump_temp <= '1' ;
                        elsif (jump_temp2 = '1') then jump_temp <= '0' ;
                    end if ;
                    
                    
                    case(state) is
                        when Idle =>
                            complete <= '0';
                            if (scored = '0')then
                                if (birdX > (pillars(0)+pillarWidth)) then 
                                    scored <= '1';
                                    if (total_score= 9) then
                                        total_score<= "0000";
                                        total_score_1<= total_score_1+1;
                                        limit3 <= (limit3*4)/5 ;
                                    else
                                   
                                    total_score<= total_score+1;
                                    end if;
                                 end if;
                             elsif (scored <= '1') then
                                     if (birdX > (pillars(1)+ pillarWidth)) then 
                                         scored <= '0';
                                        if (total_score= 9) then
                                                                             total_score<= "0000";
                                                                             total_score_1<= total_score_1+1;
                                                                             limit3 <= (limit3*4)/5 ;
                                                                             else
                                                                            
                                                                             total_score<= total_score+1;
                                                                             end if;
                                      end if;
                           
                            
                            end if;
                            if(EN = '1') then
--                                num2 <= '0' ;
                                if(deb_pause = '1') then
                                    state <= pauseTransition ;
                                else
                                    state <= checkNeg ;
                                end if ;
                            end if ;
--                        when UpdateBird =>
--                            if(jump2 = '1') then
--                                birdY <= birdY - 1 ;
--                                state <= WaitJump ;
--                                i <= 0 ;
--                                j <= 0 ;
--                            end if ;
                        when pauseTransition =>
                            if(deb_pause = '0') then
                                state <= paused ; 
                            end if ;
                       when paused =>
                            if(deb_play = '1') then
                                state <= checkNeg ;
                            elsif(deb_pause = '1') then
                                state <= checkNeg2 ;
                                temp_done <= '1' ;
                            end if ;
                            
                        when WaitJump =>
                            if(jump2 = '0') then
                                state <= checkNeg ;
                            end if ;
                        when checkNeg =>
                            if(birdY < 0 or birdY > 31) then
--                                num2 <= '1' ;
                                temp_done <= '1' ;
                                state <= checkNeg2 ;
                            else
                                state <= UpdateMatrix ;
                                i <= 0 ;
                                j <= 0 ;
                            end if ;
                       when checkNeg2 =>
                            if(EN = '0') then
                                state <= game_over  ;
                                wait_count <= 0 ; 
                            end if ;
--                     when DisplayPillars =>
                            
                            
                       
                        when UpdateMatrix =>
                            
                            if(i = 32) then
--                                state <= sendMatrix ;
                                page <= "00" ;
                       
                                state <= Config1 ;
                                i <= 0 ;
                                j <= 0 ;
                            elsif(j = 128) then
                                j <= 0 ;
                                i <= i + 1 ;
                            else
                                j <= j+ 1 ;
                                if(complete = '1') then
                                    Display(i/8,j)(i - 8*(i/8)) <= '0' ;
                                elsif((i <= pillars_height(1) or i > pillars_height(1) + pillarGap) and (j >= pillars(1) and j < pillars(1) + pillarWidth)) then
                                    Display(i/8,j)(i - 8*(i/8)) <= '0' ;
                                elsif((i <= pillars_height(0) or i > pillars_height(0) + pillarGap) and (j >= pillars(0)and j < pillars(0) + pillarWidth)) then
                                     Display(i/8,j)(i - 8*(i/8)) <= '0' ;
                                elsif(i >= birdY and i-birdY < 3 and j >= birdX and j-birdX < 4) then
                                    Display(i/8,j)(i - 8*(i/8)) <= '0' ;
                                else
                                     Display(i/8,j)(i - 8*(i/8)) <= '1' ;
                                end if ;
                            end if ;
                            
                        when check_collision=>
                           if(i = 32) then
         --                                state <= sendMatrix ;
                                        
         --                                config_en <= '1' ;
                                         state <= Idle ;
                                         i <= 0 ;
                                         j <= 0 ;
                                     elsif(j = 128) then
                                         j <= 0 ;
                                         i <= i + 1 ;
                                     else
                                         j <= j+ 1 ;
                                         
                                         if((i <= pillars_height(1) or i > pillars_height(1) + pillarGap) and  (j >= pillars(1) and j < pillars(1) + pillarWidth)) then
                                           if(i >= birdY and i-birdY < 3 and j >= birdX and j-birdX < 4) then
--                                                 temp_done <= '1' ;
--                                                  state <= make_empty ;
--                                                  done_make<= '1';
                                                     state<= checkNeg2;
                                                     temp_done <= '1' ;
                                        
                                            end if;
                                         elsif((i <= pillars_height(0) or i > pillars_height(0) + pillarGap) and (j >= pillars(0)and j < pillars(0) + pillarWidth)) then
                                             if(i >= birdY and i-birdY < 3 and j >= birdX and j-birdX < 4) then
--                                                  temp_done <= '1' ;
--                                                    state <= make_empty ;
--                                                    done_make<= '1';
                                                    state<= checkNeg2;
                                                    temp_done <= '1' ;
                                         
                                            end if ;
                                     end if ;
                        
                        end if;
                        when make_empty =>
                            if(i = 32) then
                                 state <= game_over ;
                                 i <= 0 ;
                                 j <= 0 ;
                             elsif(j = 128) then
                                 j <= 0 ;
                                 i <= i + 1 ;
                             else
                                 j <= j+ 1 ;
                               
                                    Display(i/8,j)(i - 8*(i/8)) <= '0' ;
                              
                             end if;
                         when make_line =>
                                 if (make_fig_en = '1') then 
                                    
                                        if (type_l =1) then 
                                             if (length>= temp_j) then
                                                Display(temp_i/8,temp_j)(temp_i - 8*(temp_i/8)) <= '1' ;
                                                temp_j<= temp_j +1;
                                             else 
                                                done_make <= '1';
                                                make_fig_en <= '0';
                                                state <= game_over;
                                            end if;
                                            
                                          elsif (type_l=2) then 
                                            if (length >= temp_i) then
                                                  Display(temp_i/8,temp_j)(temp_i - 8*(temp_i/8)) <= '1' ;
                                                  temp_i<= temp_i +1;
                                               else 
                                                  done_make <= '1';
                                                  make_fig_en <= '0';
                                                  state <= game_over;
                                              end if;
                                    end if;
                                    
                                    end if;
                        when game_over =>
                            complete <= '1';
                            if(wait_count = 50000000) then
                                state <= UpdateMatrix;
                                i <= 0 ;
                                j <= 0 ;
--                                limit3 <= 5000000 ;
--                                total_score <= "0000" ;
--                                total_score_1 <= "0000" ;
--                                anode <= "1110" ;
--                                cathode <= "0000" ;
--                                birdY <= 16 ;
                            else
                                wait_count <= wait_count + 1;
                            end if ;
--                            if (done_make ='1')then
--                                if (seg_count= 1) then
--                                    seg_count <= seg_count+1;
--                                    make_fig_en <= '1';
--                                    done_make <= '0';
--                                    temp_i <=11;
--                                    temp_j <= 21;
--                                    type_l <= 1;
--                                    length <= 28;
--                                    state <= make_line;
--                                elsif (seg_count =2) then
--                                    seg_count <= seg_count+1;
--                                    make_fig_en <= '1';
--                                    done_make <= '0';
--                                    temp_i <=11;
--                                    temp_j <= 21;
--                                    type_l <= 2;
--                                    length <= 20;
--                                     state <= make_line;
--                                  elsif (seg_count =3) then
--                                     seg_count <= seg_count+1;
--                                     make_fig_en <= '1';
--                                     done_make <= '0';
--                                     temp_i <=20;
--                                     temp_j <= 21;
--                                     type_l <= 1;
--                                     length <= 28;
--                                      state <= make_line;
--                                   elsif (seg_count =4) then
--                                      seg_count <= seg_count+1;
--                                      make_fig_en <= '1';
--                                      done_make <= '0';
--                                      temp_i <=15;
--                                      temp_j <= 28;
--                                      type_l <= 2;
--                                      length <= 20;
--                                       state <= make_line;
--                                     elsif (seg_count =5) then
--                                       seg_count <= seg_count+1;
--                                       make_fig_en <= '1';
--                                       done_make <= '0';
--                                       temp_i <=11;
--                                       temp_j <= 32;
--                                       type_l <= 2;
--                                       length <= 20;
--                                        state <= make_line;
--                                      elsif (seg_count =6) then
--                                        seg_count <= seg_count+1;
--                                        make_fig_en <= '1';
--                                        done_make <= '0';
--                                        temp_i <=11;
--                                        temp_j <= 32;
--                                        type_l <= 1;
--                                        length <= 39;
--                                         state <= make_line;
--                                       elsif (seg_count =7) then
--                                                                             seg_count <= seg_count+1;
--                                                                             make_fig_en <= '1';
--                                                                             done_make <= '0';
--                                                                             temp_i <=11;
--                                                                             temp_j <= 21;
--                                                                             type_l <= 1;
--                                                                             length <= 28;
--                                                                              state <= make_line;
--                                 else 
--                                    page <= "00" ;
                                                        
--                                     state <= Config1 ;
                                     
--                                     i <= 0 ;
--                                     j <= 0 ;
--                                     make_fig_en <= '0';
--                                     done_make <= '1';
--                                     temp_i <=0;
--                                     temp_j <= 0;
--                                     type_l <= 0;
--                                     length <= 0;
--                                end if;
--                            end if;

                               
--                            end if;
--                            if(i = 32) then
--                             state <= Idle ;
--                             i <= 0 ;
--                             j <= 0 ;
--                         elsif(j = 128) then
--                             j <= 0 ;
--                             i <= i + 1 ;
--                         else
--                             j <= j+ 1 ;
                             
--                             if((i=11 and ((j>=21 and j<=28) or (j>= 32 and j<= 39) or j= 43 or j=44 or j=49 or j=50 or (j>= 54 and j<=61)  or(j>=72) ) ))then
--                             else
--                                Display(i/8,j)(i - 8*(i/8)) <= '0' ;
--                         end if ;
--                         end if;
                                                           
                       when sendMatrix =>
                             
                             
                             if(j = 128) then
                                if(i = 3) then
                                    if (complete = '1') then
                                    state <= idle;
                                    elsif (complete = '0') then
                                     state <= check_collision ;
                                     end if;
                                     i <= 0 ;
                                     j <= 0 ;
                                else
                                    page <= page + 1 ;
--                                    config_en <= '1' ;
--                                    state <= Config1 ;
                                    j <= 0 ;
                                    i <= i + 1 ;
                                end if ;
                             else
                                j <= j+ 1 ;
                                spi_data <= Display(i,j) ;
                                switch_state <= sendMatrix ;
                                spi_en <= '1' ;
                                state <= WaitForSend ;
                             end if ;
                      when Config1 =>
                          temp_dc <= '0' ;
                          state <= Config2 ;
                     when Config2 =>
                        spi_data <= "00100000";
                        spi_en <= '1' ;
                        switch_state <= Config3 ;
                        state <= WaitForSend ;
                     when Config3 =>
                         spi_data <= "00000000";
                         spi_en <= '1' ;
                         switch_state <= Config4 ;
                         state <= WaitForSend ;
                     when Config4 =>
                       spi_data <= "00100001";
                       spi_en <= '1' ;
                       switch_state <= Config5 ;
                       state <= WaitForSend ;
                    when Config5 =>
                       spi_data <= col;
                       switch_state <= Config6 ;
                       spi_en <= '1' ;
                       state <= WaitForSend ;
                    when Config6 =>
                         spi_data <= "01111111";
                         spi_en <= '1' ;
                         switch_state <= Config7 ;
                         state <= WaitForSend ;
                    when Config7 =>
                          spi_data <= "00100010";
                          spi_en <= '1' ;
                          switch_state <= Config8 ;
                          state <= WaitForSend ;
                   when Config8 =>
                           spi_data <= "00000000";
                           spi_en <= '1' ;
                           switch_state <= Config9 ;
                           state <= WaitForSend ;
                   when Config9 =>
                            spi_data <= "00000011";
                            spi_en <= '1' ;
                            switch_state <= Config10 ;
                            state <= WaitForSend ;
                     when Config10 =>
                         temp_dc <= '1' ;
                         state <= sendMatrix ;    
           
                      when WaitForSend =>
                         if(spi_fin = '1') then
                                state <= Temp ;
                          end if; 
                      when Temp =>
                            spi_en <= '0' ;
                            state <= switch_state ; 
                      when others =>
                            state <= Idle ;
                              
                      end case;   
                        
                end if ;
            end process ;
            
end Behavioral;

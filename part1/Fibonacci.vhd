-- execute I and R type instructions

library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity Fibonacci is
    port (reset : in  std_logic;
          clk   : in  std_logic;
			 y : out word);
end Fibonacci;

architecture behavioral of Fibonacci is

signal alu_func : alu_func_t := ALU_NONE;
signal alu_A : word := x"00000000";
signal alu_B : word := x"00000000";
signal alu_out : word := x"00000000";
signal reg_B : word := x"00000000";
signal imm : word:= x"00000000";
signal imm_rd : word := x"00000000";
signal ir : word := x"00000000";
signal dmem_out : word := x"00000000";
signal wb_wdata : word := x"00000000";
signal rf_wdata : word := x"00000000";
signal branch_imm : unsigned(word'range) := x"00000000";
signal jal_imm : unsigned(word'range) := x"00000000";
signal jalr_imm : unsigned(word'range) := x"00000000";
signal lui_imm : std_logic_vector(19 downto 0);


-- instruction fields
signal opcode : opcode_t;
signal funct3 : std_logic_vector(2 downto 0);
signal funct7 : std_logic_vector(6 downto 0);
signal rs1 : std_logic_vector(4 downto 0);
signal rs2 : std_logic_vector(4 downto 0);
signal rd : std_logic_vector(4 downto 0);
signal pc : unsigned(word'range) := x"00000000";
signal nextpc : unsigned(word'range) := x"00000000";

-- control signals
signal regwrite : std_logic;
signal wbsel : std_logic;
signal memwrite : std_logic;
signal op2sel : std_logic_vector(1 downto 0);
signal PCsel : std_logic_vector(1 downto 0);
signal LUIsel : std_logic;
component alu is
port (alu_func : in  alu_func_t;
		op1      : in  word;
		op2      : in  word;
		result   : out word);
end component alu;

component imem is 
port(    
	addr : in std_logic_vector(4 downto 0);
	dout : out word);
end component imem;

component dmem is
port (reset : in  std_logic;
      clk   : in  std_logic;
      raddr : in  std_logic_vector(5 downto 0);
      dout  : out word;
      waddr : in  std_logic_vector(5 downto 0);
      din : in  word;
      we    : in  std_logic);
end component dmem;

component regfile is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addra : in  std_logic_vector(4 downto 0);
      addrb : in  std_logic_vector(4 downto 0);
      rega  : out word;
      regb  : out word;
      addrw : in  std_logic_vector(4 downto 0);
      dataw : in  word;
      we    : in  std_logic);
end component regfile;

begin
	-- datapath
	alu0: alu port map(
		alu_func => alu_func,
            op1 => alu_A,
            op2 => alu_B,
			result => alu_out);
		  
	imem0: imem port map(    
        	addr => std_logic_vector(pc(6 downto 2)),
        	dout => ir);

	dmem0: dmem port map(
	reset => reset,
		clk => clk,
    raddr => alu_out(7 downto 2),
		dout => dmem_out,
		waddr => alu_out(7 downto 2),
		din => reg_B,
		we => memwrite);

	rf0: regfile port map(
	reset => reset,
		clk => clk,
        addra => rs1,
        addrb => rs2,
		rega => alu_A,
		regb => reg_B,
		addrw => rd,
		dataw => rf_wdata,
		we => regwrite);

	alu_B <= reg_B when op2sel = "00" else 
			imm when op2sel = "01" else
			imm_rd;
  wb_wdata <= alu_out when wbsel = '0' else 
      dmem_out;
  
  lui_proc : process (LUIsel,rf_wdata ,wb_wdata,lui_imm) is
  begin
    if( LUIsel = '0') then
      rf_wdata <= wb_wdata;
    elsif( LUIsel = '1') then
       rf_wdata(31 downto 12) <= lui_imm;
        rf_wdata(11 downto 0) <= (others => '0');     
  end if;
end process;
      
  nextpc_proc : process(PCsel,nextpc,pc,branch_imm,jal_imm,alu_A,pc) is
  begin
      if(PCsel = "01")then
        nextpc <= pc + branch_imm;   
      elsif (PCsel = "10")then
			   nextpc <= pc + jal_imm;  
		   elsif (PCsel = "11")then
			   nextpc <= jalr_imm + unsigned(alu_A);     
			  else
			   nextpc <=pc + 4;
			  
  end if;
end process;
		  
	-- instruction fields
	imm(31 downto 12) <= (others => ir(31));
	imm(11 downto 0) <= ir(31 downto 20);
	imm_rd(31 downto 12) <= (others => funct7(6));
	imm_rd(11 downto 5) <= funct7;
	imm_rd(4 downto 0) <= rd;
   rs1 <= ir(19 downto 15);
   rs2 <= ir(24 downto 20);
	rd <= ir(11 downto 7);
	funct3 <= ir(14 downto 12);
	funct7 <= ir(31 downto 25);
	opcode <= ir(6 downto 0);
	branch_imm(31 downto 13) <= (others => ir(31));
	branch_imm(12 downto 0) <= unsigned(ir(31) & ir(7) & 
								ir(30 downto 25) & ir(11 downto 8) & '0');
	jal_imm(31 downto 21) <= (others => ir(31));
	jal_imm(20 downto 0) <= unsigned(ir(31) & ir(19 downto 12) & ir(20) & ir(30 downto 21)&'0' );
	jalr_imm(31 downto 12) <= (others =>ir(31));
  jalr_imm(11 downto 0) <= unsigned(ir(31)& ir(30 downto 20));
  lui_imm (19 downto 0) <= ir(31 downto 12);
  
   decode_proc : process (ir, funct7, funct3, alu_A,reg_B) is
	begin
		regwrite <= '0';
		op2sel <= "00";
		memwrite <= '0';
		wbsel <= '0';
		PCsel <= "00";
		LUIsel <= '0';
		alu_func <= ALU_NONE;
		
		case opcode is
			when OP_ITYPE =>
				regwrite <= '1';
				op2sel <= "01";
				case (funct3) is
                    when "000" => alu_func <= ALU_ADD;
                    when "001" => alu_func <= ALU_SLL;
                    when "010" => alu_func <= ALU_SLT;
                    when "011" => alu_func <= ALU_SLTU;
                    when "100" => alu_func <= ALU_XOR;
                    when "110" => alu_func <= ALU_OR;
                    when "111" => alu_func <= ALU_AND;
                    when "101" =>
                        if (ir(30) = '1') then
                            alu_func <= ALU_SRA;
                        else
                            alu_func <= ALU_SRL;
                        end if;

                    when others => null;
                end case;

			when OP_RTYPE =>
				regwrite <= '1';
				case (funct3) is
					when "000" =>
						if (ir(30) = '1') then
							 alu_func <= ALU_SUB;
						else
							 alu_func <= ALU_ADD;
						end if;
					when "001" => alu_func <= ALU_SLL;
					when "010" => alu_func <= ALU_SLT;
					when "011" => alu_func <= ALU_SLTU;
					when "100" => alu_func <= ALU_XOR;
					when "101" =>
						if (ir(30) = '1') then
							 alu_func <= ALU_SRA;
						else
							 alu_func <= ALU_SRL;
						end if;
					when "110"  => alu_func <= ALU_OR;
					when "111"  => alu_func <= ALU_AND;
					when others => null;
				end case;
			when OP_STORE =>
			  regwrite <= '0';
			  memwrite <= '1';
        op2sel <= "11";
        wbsel <= '0';
        alu_func<= ALU_ADD;
      when OP_BRANCH =>
        case (funct3) is
          when BNE => 
            if(alu_A /= reg_B) then
               PCsel <= "01";
            else
               PCsel <= "00";
            end if;
          when BEQ =>
            if(alu_A = reg_B) then
              PCsel <= "01";
            else
              PCsel <= "00";
            end if;
          when others => null;
         end case;
      when OP_JAL =>
        regwrite <= '0';
        memwrite <='0';
        PCsel <= "10";
        wbsel <= '1';
     when OP_JALR =>
        regwrite <= '0';
        memwrite <='0';
        PCsel <= "11";
        wbsel <= '1';
    when OP_LUI =>
        regwrite <= '1';
        LUIsel <= '1';
        
       
      when others => null;
		end case;
    end process;

	y <= alu_out;
	
	acc: process(reset, clk) 
	begin 
		if (reset = '1') then 
			pc <= (others => '0');
		elsif (clk = '0') then 
			   
			    pc <= nextpc ;
			
		end if; 
	end process; 
end architecture;

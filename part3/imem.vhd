library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library work;
use work.common.all;

entity imem is
    port(   
        addr : in std_logic_vector(4 downto 0);
        dout : out word);
end imem;

architecture behavioral of imem is
type rom_arr is array(0 to 19) of word;

constant mem:rom_arr:=
    ( 
		 x"00000793",   --0000 93070000              li      a5,0
     x"00000693",   --0004 93060000              li      a3,0
     x"00100713",   --0008 13071000              li      a4,1
     x"00000613",   --000c 13060000              li      a2,0
     x"01E00593",      --0010 9305E001              li      a1,30
     x"00100513",                   --0014 13051000              li      a0,1
     x"0140006F",                   --0018 6F004001              j       .L2
                        --.L4:
     x"00E606B3",                   --001c B306E600              add     a3,a2,a4
     x"02A78263",                 --0020 6382A702              beq     a5,a0,.L5
     x"00070613",                   --0024 13060700              mv      a2,a4
     x"00068713",                   --0028 13870600              mv      a4,a3
                        --.L2:
     x"00178793",                   --002c 93871700              add     a5,a5,1
     x"FEB796E3",                   --0030 E396B7FE              bne     a5,a1,.L4
     x"0000F7B7",                   --0034 B7F70000              li      a5,61440
     x"00D7A023",                   --0038 23A0D700              sw      a3,0(a5)
     x"00000513",                   --003c 13050000              li      a0,0
     x"00008067",                   --0040 67800000              ret
                        --.L5
     x"00100693",                   --0044 93061000              li      a3,1
     x"FE5FF06F",                   --0048                            j       .L2
     x"00000013"                     --004c NOP
     );

begin
	dout<=mem(conv_integer(addr));
end behavioral;

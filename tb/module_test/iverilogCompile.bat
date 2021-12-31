



iverilog.exe -W all -o ../build/tl_mem_tb.iverilog ^
-y ../../src/main/resources ^
-y ../../src/test/resources ^
-I ./ ^
./tl_mem_tb.v 

@pause

vvp.exe  -N ../build/tl_mem_tb.iverilog -lxt2






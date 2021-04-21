@REM @Author: Ruige Lee
@REM @Date:   2020-11-02 11:29:57
@REM @Last Modified by:   Ruige Lee
@REM Modified time: 2021-04-21 10:43:04




iverilog.exe -W all -o ../build/tl_mem_tb.iverilog ^
-y ../../src/main/resources ^
-y ../../src/test/resources ^
-I ./ ^
./tl_mem_tb.v 

@pause

vvp.exe  -N ../build/tl_mem_tb.iverilog -lxt2

rem @pause


rem gtkwave.exe ../build/axi_ccm.vcd




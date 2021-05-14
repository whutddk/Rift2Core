@REM @Author: Ruige Lee
@REM @Date:   2020-11-02 11:29:57
@REM @Last Modified by:   Ruige Lee
@REM Modified time: 2021-05-12 16:39:48




iverilog.exe -Wall ^
-o ./build/wave.iverilog  ^
-y ./ ^
-y ./vtb/ ^
-y ../src/test/resources ^
-y ../generated/ ^
-I ../generated/ ^
-D RANDOMIZE_MEM_INIT ^
../tb/riftChip_DS.v 

@pause

vvp.exe  -N ./build/wave.iverilog -lxt2

rem @pause



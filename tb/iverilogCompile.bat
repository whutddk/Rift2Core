@REM @Author: Ruige Lee
@REM @Date:   2020-11-02 11:29:57
@REM @Last Modified by:   Ruige Lee
@REM Modified time: 2021-04-21 15:33:58




iverilog.exe -Wall ^
-o ./build/wave.iverilog  ^
-y ./ ^
-y ../src/test/resources ^
-y ../generated/ ^
-I ../generated/ ^
../tb/rift2chip.v 

@pause

vvp.exe  -N ./build/wave.iverilog -lxt2

rem @pause



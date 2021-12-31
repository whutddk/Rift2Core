



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



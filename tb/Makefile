# verilator version
verilator      ?= verilator
R2        ?= ${R2}


.DEFAULT_GOAL = sim


clean:
	rm ./build/*.cpp
	rm ./build/*.o
	rm ./build/*.d
	rm ./build/*.h
	rm ./build/*.mk


sim:
	verilator -Wno-fatal  \
	--timescale "1 ns / 1 ps" \
	-y ${R2}/generated/ \
	-y ${R2}/tb/ \
	-y ${R2}/tb/vtb/ \
	--top-module test \
	--trace \
	--cc ${R2}/tb/verilator/test.v  \
	--exe --build ./verilator/sim_main.cpp  \
	-Mdir build \
	-j 8
# 	rm ./obj_dir/*.d ./obj_dir/*.o ./obj_dir/*.cpp

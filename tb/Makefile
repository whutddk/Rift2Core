# verilator version
verilator      ?= verilator
R2        ?= ${R2}


.DEFAULT_GOAL = sim


clean:
	rm ./obj_dir/*


sim:
	verilator -Wno-fatal  \
	--timescale "1 ns / 1 ps" \
	-y ${R2}/generated/ \
	-y ${R2}/tb/ \
	-y ${R2}/tb/vtb/ \
	--top-module SimTop \
	--trace \
	--cc ${R2}/tb/verilator/SimTop.v  \
	--exe --build ./verilator/sim_main.cpp  \
	-j 8
# 	rm ./obj_dir/*.d ./obj_dir/*.o ./obj_dir/*.cpp
